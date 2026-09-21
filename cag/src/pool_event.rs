/*
 * Copyright (c) 2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjusted_value::AdjustedValue;
use crate::adjustment::Adjustment;
use crate::cag_configuration::MatchMethod;
use crate::holding::{DealHolding, DealHoldingSummary};
use itertools::Itertools;
use journ_core::amount::Amount;
use journ_core::datetime::JDateTimeRange;
use journ_core::metadata::Metadata;
use journ_core::unit::Unit;
use linked_hash_set::LinkedHashSet;
use smartstring::alias::String as SS;
use std::fmt;
use std::fmt::Write;
use std::sync::atomic;
use std::sync::atomic::AtomicUsize;

//#[derive(PartialEq, Eq)]
pub struct MatchDetails<'h> {
    /// If `None`, this indicates an aggregation of multiple matches where the methods were not all the same.
    match_method: Option<MatchMethod>,
    /// The part of the pool that was matched
    target: DealHolding<'h>,
    /// The incoming transaction that matched the pool. This will be a single deal usually, optionally adjusted.
    /// This may be multiple deals if multiple MatchDetails have been aggregated, such as when report.
    originator: DealHolding<'h>,
}

impl<'h> MatchDetails<'h> {
    pub fn new(
        match_method: MatchMethod,
        target: DealHolding<'h>,
        originator: DealHolding<'h>,
    ) -> MatchDetails<'h> {
        assert!(!originator.amount().is_zero());
        debug_assert!((originator.amount() + target.amount()).is_zero());

        MatchDetails { match_method: Some(match_method), target, originator }
    }

    pub fn function(&self) -> Option<MatchMethod> {
        self.match_method
    }

    pub fn originator(&self) -> &DealHolding<'h> {
        &self.originator
    }

    pub fn target(&self) -> &DealHolding<'h> {
        &self.target
    }

    /// Gets the buy holding of this match.
    pub fn buy_holding(&self) -> &DealHolding<'h> {
        if self.originator.amount().is_positive() { &self.originator } else { &self.target }
    }

    /// Gets the holding on the sell side of the match.
    pub fn sell_holding(&self) -> &DealHolding<'h> {
        if self.originator.amount().is_negative() { &self.originator } else { &self.target }
    }

    /// The actual purchase cost in proportion to the amount disposed including expenses.
    /// The actual cost is rounded.
    pub fn actual_cost(&self) -> Amount<'h> {
        let buy_holding = self.buy_holding();
        match buy_holding.split_parent() {
            Some(parent) => {
                // If the parent is available, we can calculate according to the correct formula to avoid rounding issues.
                let disposed = self.sell_holding().amount().abs();
                let acquired_origin_amount = parent.amount();
                let acquired_origin_cost = parent.value();
                (acquired_origin_cost * (disposed / acquired_origin_amount).quantity()).rounded()
            }
            _ => buy_holding.value().rounded(),
        }
    }

    /// The effective actual cost takes into account the actual reported gain/loss of the match.
    ///
    /// The usual formula for calculating the gain is:
    /// `gain = net proceeds - actual cost.`
    /// Rearranging this gives: `effective actual cost = net proceeds - gain`.
    /// Usually, the effective actual cost is the same as the actual cost, but if, at the deal level, the gain has been overridden, a difference would result.
    pub fn effective_actual_cost(&self) -> Amount<'h> {
        self.net_proceeds() - self.gain()
    }

    pub fn net_proceeds(&self) -> Amount<'h> {
        self.sell_holding().value()
    }

    /// Gets the gain (or loss if negative) of this match.
    /// This is the sell total - the buy total which is the gain in both long and short contexts.
    ///
    /// The gain may be overridden on disposal deals, in which case the overridden value is used.
    /// If not overridden, the gain is calculated as the `net proceeds - actual cost`.
    pub fn gain(&self) -> Amount<'h> {
        let total_actual_cost = self.actual_cost();
        let total_sold_amount = self.sell_holding().amount();

        // Work on each deal individually to allow for each deal overriding its own taxable gain and sum them up.
        self.sell_holding()
            .deal_iter()
            .map(|deal| match deal.taxable_gain() {
                Some(gain) => gain,
                None => {
                    // We don't round here to avoid accumulating rounding errors.
                    let deal_actual_cost = total_actual_cost
                        * (deal.amount().quantity() / total_sold_amount.quantity());
                    let deal_net_proceeds = deal.value();
                    deal_net_proceeds - deal_actual_cost
                }
            })
            .sum::<Amount<'h>>()
            .rounded()
    }

    pub fn notes(&self) -> LinkedHashSet<&str> {
        let mut notes = LinkedHashSet::new();
        notes.extend(self.originator.notes());
        notes.extend(self.target.notes());
        notes
    }
}

/*
impl<'h> Add for MatchDetails<'h> {
    type Output = Result<MatchDetails<'h>, (MatchDetails<'h>, MatchDetails<'h>)>;

    fn add(self, rhs: Self) -> Self::Output {
        if self.target().unit() != rhs.target().unit() {
            return Err((self, rhs));
        }
        let matched = match self.sell_holding().clone() + rhs.sell_holding().clone() {
            Ok(dh) => dh,
            Err((l_dh, r_dh)) => {
                return Err((
                    MatchDetails { target: l_dh, ..self },
                    MatchDetails { target: r_dh, ..rhs },
                ));
            }
        };
        // Will always succeed if self.matched could be added.
        let deal = (self.buy_holding().clone() + rhs.buy_holding().clone()).unwrap();

        let match_method = match (self.match_method, rhs.match_method) {
            (Some(m1), Some(m2)) if m1 == m2 => Some(m1),
            _ => None,
        };
        Ok(MatchDetails { match_method, target: matched, originator: deal })
    }
}*/

//#[derive(PartialEq, Eq)]
pub enum PoolEventKind<'h> {
    /// A change to the pool amount and/or value has taken place
    Adjustment(Adjustment<'h>),
    /// A deal was added to the pool
    PooledDeal(DealHoldingSummary<'h>),
    /// A deal was moved from the specified `DealHolding` and to the specified pool name
    MovedFrom(DealHoldingSummary<'h>, &'h str),
    /// A deal was moved out of the specified `DealHolding` and matched with the specified pool name
    //MatchedFrom(HoldingHandle<'h>, &'h str),
    /// A deal was moved to the specified `DealHolding`, and from the specified pool name
    MovedTo(DealHoldingSummary<'h>, &'h str),
    /// An acquisition was matched against a disposal
    Match(MatchDetails<'h>),
}

impl fmt::Display for PoolEventKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            PoolEventKind::PooledDeal(dh) => {
                let rem = if dh.parent().is_some() { "(remainder) " } else { "" };
                write!(f, "Pooled {}{}", rem, dh.amount())
            }

            PoolEventKind::MovedFrom(dh, to) => write!(f, "Moved {} to '{to}'", dh.amount()),
            /*PoolEventKind::MatchedFrom(dh, to) => {
                write!(f, "Matched from {} to {to}", dh.value())
            }*/
            PoolEventKind::MovedTo(dh, from) => {
                write!(f, "Moved {} from '{from}'", dh.amount())
            }
            PoolEventKind::Match(details) => {
                write!(f, "Matched {}", details.originator().amount(),)
            }
            PoolEventKind::Adjustment(adj) => write!(f, "Adjusted {}", adj),
        }
    }
}

/*
impl PartialOrd for PoolEventKind<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PoolEventKind<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        fn ordinal(kind: &PoolEventKind) -> u8 {
            match kind {
                PoolEventKind::Adjustment(_) => 0,
                PoolEventKind::PooledDeal(_) => 1,
                //PoolEventKind::MatchedFrom(..) => 2,
                PoolEventKind::MovedFrom(..) => 3,
                PoolEventKind::MovedTo(..) => 4,
                PoolEventKind::Match(_) => 5,
            }
        }
        ordinal(self).cmp(&ordinal(other))
    }
}*/

pub struct PoolEvent<'h> {
    /// A unique, incrementing sequence number to be able to correctly determine order when comparing events.
    sequence: usize,
    pool_name: &'h str,
    /// The date and time when the event took place in the report timezone. When events are folded,
    /// they form a range.
    /// This should show its time component, depending on whether the associated deal shows its time
    /// component.
    date: JDateTimeRange,
    event_kind: PoolEventKind<'h>,
    balance_before: AdjustedValue<'h>,
    balance_after: AdjustedValue<'h>,
}

impl<'h> PoolEvent<'h> {
    pub fn new(
        pool_name: &'h str,
        date: JDateTimeRange,
        kind: PoolEventKind<'h>,
        bal_before: AdjustedValue<'h>,
        bal_after: AdjustedValue<'h>,
    ) -> Self {
        static SEQ: AtomicUsize = AtomicUsize::new(0);

        PoolEvent {
            sequence: SEQ.fetch_add(1, atomic::Ordering::Relaxed),
            pool_name,
            date,
            event_kind: kind,
            balance_before: bal_before,
            balance_after: bal_after,
        }
    }

    pub fn unit(&self) -> &'h Unit<'h> {
        match &self.event_kind {
            PoolEventKind::PooledDeal(dh) => dh.amount().unit(),
            PoolEventKind::MovedFrom(dh, _)
            | PoolEventKind::MovedTo(dh, _)
            /*| PoolEventKind::MatchedFrom(dh, _)*/ => dh.amount().unit(),
            PoolEventKind::Match(details) => details.originator().unit(),
            PoolEventKind::Adjustment(adj) => adj.unit(),
        }
    }

    pub fn pool_name(&self) -> &'h str {
        self.pool_name
    }

    pub fn kind(&self) -> &PoolEventKind<'h> {
        &self.event_kind
    }

    pub fn balance_before(&self) -> AdjustedValue<'h> {
        self.balance_before
    }

    pub fn balance_after(&self) -> AdjustedValue<'h> {
        self.balance_after
    }

    /// The date of the event in the report timezone. If events have been combined, this will cover
    /// the date range of those events.
    pub fn event_datetime(&self) -> JDateTimeRange {
        self.date
    }

    /// The date of the deal in the report timezone.
    pub fn deal_datetime(&self) -> JDateTimeRange {
        match &self.event_kind {
            PoolEventKind::PooledDeal(dh) => dh.datetime(),
            PoolEventKind::MovedFrom(dh, _)
            | PoolEventKind::MovedTo(dh, _)
            /*| PoolEventKind::MatchedFrom(dh, _)*/ => dh.datetime(),
            // The details.originator() might seem more natural, but we want to group by the sell date
            // when displaying the default report. Hopefully this is not surprising in other contexts.
            PoolEventKind::Match(details) => details.sell_holding().datetime(),
            PoolEventKind::Adjustment(adj) => adj.datetime(),
        }
    }

    /// The 'amount' can be viewed as the change in the pool balance for any given event.
    /// However, because this amount gets repeated for moves, or can be cancelled out by a match, it
    /// is not useful whilst aggregating.
    ///
    /// The original amount seeks to be more useful in these situations as it is an amount that is
    /// not repeated.
    pub fn original_amount(&self) -> Option<Amount<'h>> {
        match &self.event_kind {
            PoolEventKind::PooledDeal(_) | PoolEventKind::Adjustment(_) => {
                Some(self.balance_after.amount() - self.balance_before.amount())
            }
            PoolEventKind::Match(details) if self.date == details.originator().datetime() => {
                Some(self.balance_after.amount() - self.balance_before.amount())
            }
            _ => None,
        }
    }

    /// Gets the base acquisition cost.
    pub fn acquired(&self) -> Option<AdjustedValue<'h>> {
        match &self.event_kind {
            PoolEventKind::PooledDeal(dh)
                if dh.datetime().start() == self.event_datetime().start() && dh.value() > 0 =>
            {
                Some(dh.adjusted_value())
            }
            _ => None,
        }
    }

    pub fn consideration(&self) -> Option<Amount<'h>> {
        match &self.event_kind {
            PoolEventKind::Match(details) => Some(details.buy_holding().consideration()),
            _ => None,
        }
    }

    /// Gets the disposal if this event was a match. The disposal is what was disposed and
    /// the proceeds received.
    pub fn disposed(&self) -> Option<AdjustedValue<'h>> {
        match &self.event_kind {
            PoolEventKind::Match(details) => Some(details.sell_holding().adjusted_value()),
            _ => None,
        }
    }

    /// Gets the disposal value after the expense adjustment.
    pub fn net_proceeds(&self) -> Option<Amount<'h>> {
        match &self.event_kind {
            PoolEventKind::Match(details) => {
                let net_proceeds = details.net_proceeds();
                Some(net_proceeds)
            }
            _ => None,
        }
    }

    pub fn expenses(&self) -> Option<Amount<'h>> {
        match &self.event_kind {
            PoolEventKind::Match(details) => match details.sell_holding().expenses() {
                exp if !exp.is_zero() => Some(exp),
                _ => None,
            },
            PoolEventKind::PooledDeal(dh)
                if self.event_datetime().start() == dh.datetime().start() =>
            {
                match dh.expenses() {
                    exp if !exp.is_zero() => Some(exp),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn actual_cost(&self) -> Option<Amount<'h>> {
        match &self.event_kind {
            PoolEventKind::Match(details) => Some(details.actual_cost()),
            _ => None,
        }
    }

    pub fn gain(&self) -> Option<Amount<'h>> {
        match &self.event_kind {
            PoolEventKind::Match(details) if details.gain() > 0 => Some(details.gain()),
            _ => None,
        }
    }

    pub fn losses(&self) -> Option<Amount<'h>> {
        match &self.event_kind {
            PoolEventKind::Match(details) if details.gain() < 0 => Some(details.gain()),
            _ => None,
        }
    }

    pub fn description(&self) -> SS {
        let mut ss = SS::new();
        write!(ss, "{} ", self.event_symbolic_description()).unwrap();
        write!(
            ss,
            "{}",
            match &self.event_kind {
                PoolEventKind::PooledDeal(dh) => dh.entry_description(),
                PoolEventKind::MovedFrom(dh, _)
                | PoolEventKind::MovedTo(dh, _)
                /*| PoolEventKind::MatchedFrom(dh, _)*/ => dh.entry_description(),
                PoolEventKind::Match(details) => details.originator().description(),
                PoolEventKind::Adjustment(adj) => {
                    let mut desc = LinkedHashSet::new();
                    desc.insert(adj.entry().description().trim_start());
                    desc
                }
            }
            .iter()
            .join(", ")
        )
        .unwrap();
        ss
    }

    pub fn event_symbolic_description(&self) -> &'static str {
        match &self.event_kind {
            PoolEventKind::PooledDeal(..) => "-->",
            PoolEventKind::MovedFrom(..) => "<--",
            //PoolEventKind::MatchedFrom(..) => "<==",
            PoolEventKind::MovedTo(..) => "-->",
            PoolEventKind::Match(_) => "==>",
            PoolEventKind::Adjustment(_) => "@@@",
        }
    }

    pub fn entry_metadata_by_key(&self, key: &str) -> LinkedHashSet<&Metadata<'h>> {
        match &self.event_kind {
            PoolEventKind::PooledDeal(dh) => dh.entry_metadata_by_key(key),
            PoolEventKind::MovedFrom(dh, _)
            | PoolEventKind::MovedTo(dh, _)
            /*| PoolEventKind::MatchedFrom(dh, _) */ => dh.entry_metadata_by_key(key),
            PoolEventKind::Match(details) => details.sell_holding().entry_metadata_by_key(key),
            PoolEventKind::Adjustment(adj) => {
                adj.entry().metadata_by_key(key).into_iter().collect()
            }
        }
    }
}

impl fmt::Display for PoolEvent<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} {}. Pool '{}' now {:?}",
            self.event_datetime().start(),
            self.event_kind,
            self.pool_name,
            self.balance_after
        )
    }
}

impl PartialEq for PoolEvent<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.sequence == other.sequence
    }
}

impl Eq for PoolEvent<'_> {}

impl PartialOrd for PoolEvent<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PoolEvent<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.sequence.cmp(&other.sequence)
    }
}
