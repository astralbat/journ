/*
 * Copyright (c) 2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjustment::Adjustment;
use crate::cgt_configuration::{CagCommand, CapitalGainsColumn, EventFilter, MatchMethod};
use crate::deal_holding::{AbsDealHolding, DealHolding};
use crate::pool::PoolBalance;
use itertools::Itertools;
use journ_core::arguments::Arguments;
use journ_core::configuration::Filter;
use journ_core::date_and_time::JDateTimeRange;
use journ_core::reporting::table::Cell;
use journ_core::reporting::table::Row;
use journ_core::reporting::term_style::{Colour, Weight};
use journ_core::unit::Unit;
use journ_core::valued_amount::ValuedAmount;
use linked_hash_set::LinkedHashSet;
use std::borrow::Cow;
use std::ops::Add;
use std::sync::atomic;
use std::sync::atomic::AtomicUsize;
use std::{fmt, iter};
use yaml_rust::Yaml;
use yaml_rust::yaml::Hash;

#[derive(Clone, PartialEq, Eq)]
pub struct MatchDetails<'h> {
    /// If `None`, this indicates an aggregation of multiple matches where the methods were not all the same.
    match_method: Option<MatchMethod>,
    /// The part of the pool that was matched
    target: DealHolding<'h>,
    /// The incoming transaction that matched the pool. This will be a single deal usually, optionally adjusted.
    /// This may be multiple deals if multiple MatchDetails have been aggregated, such as when reporting.
    originator: DealHolding<'h>,
}

impl<'h> MatchDetails<'h> {
    pub fn new(
        match_method: MatchMethod,
        target: DealHolding<'h>,
        originator: DealHolding<'h>,
    ) -> MatchDetails<'h> {
        assert!(!originator.total().is_zero());
        debug_assert!((originator.total().amount() + target.total().amount()).is_zero());

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
        if self.originator.total().amount().is_positive() { &self.originator } else { &self.target }
    }

    /// Gets the holding on the sell side of the match.
    pub fn sell_holding(&self) -> &DealHolding<'h> {
        if self.originator.total().amount().is_negative() { &self.originator } else { &self.target }
    }

    /// The actual purchase cost in proportion to the amount disposed including expenses.
    /// The actual cost is rounded.
    pub fn actual_cost(&self) -> ValuedAmount<'h> {
        let buy_holding = self.buy_holding();
        match buy_holding.split_parent() {
            Some(parent) => {
                // If the parent is available, we can calculate according to the correct formula to avoid rounding issues.
                let disposed = self.sell_holding().total().amount().abs();
                let acquired_origin_amount = parent.total().amount();
                let acquired_origin_cost =
                    parent.total().valued_amount().clone().without_unit(buy_holding.unit());
                (&acquired_origin_cost * (disposed / acquired_origin_amount).quantity()).rounded()
            }
            _ => buy_holding
                .total()
                .valued_amount()
                .clone()
                .without_unit(buy_holding.unit())
                .rounded(),
        }
    }

    pub fn net_proceeds(&self) -> ValuedAmount<'h> {
        // Use gross - expenses calculation rather than the holding's total. If we use the latter and negate it,
        // it will not be able to handle negative net_proceeds which can indeed happen.
        let mut gross_to_net = self
            .sell_holding()
            .total_before_expenses()
            .without_unit(self.originator().unit())
            .negated();
        let expenses = self.sell_holding().expenses();
        gross_to_net.sub_from(&expenses);
        gross_to_net.rounded()
    }

    /// Gets the gain (or loss if negative) of this match.
    /// This is the sell total - the buy total which is the gain in both long and short contexts.
    pub fn gain(&self) -> ValuedAmount<'h> {
        if let Some(gain) = self.sell_holding().explicit_taxable_gain() {
            return gain;
        }
        (self.net_proceeds() - self.actual_cost()).unwrap()
    }
}

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
}

impl From<&MatchDetails<'_>> for Yaml {
    fn from(value: &MatchDetails<'_>) -> Self {
        let mut map = Hash::new();

        // Include originator and its parent one level deep
        let mut originator = Hash::new();
        originator.extend(Yaml::from(value.originator()).into_hash().unwrap());
        if let Some(originator_parent) = value.originator().split_parent() {
            originator.insert(Yaml::String("parent".to_string()), originator_parent.into());
        }
        map.insert(Yaml::String("originator".to_string()), Yaml::Hash(originator));

        // Include target and its parent one level deep
        let mut target = Hash::new();
        target.extend(Yaml::from(value.target()).into_hash().unwrap());
        if let Some(target_parent) = value.target().split_parent() {
            target.insert(Yaml::String("parent".to_string()), target_parent.into());
        }
        map.insert(Yaml::String("target".to_string()), Yaml::Hash(target));

        // Indicate the buy side and sell side of the match.
        if value.target().total().amount().is_positive() {
            map.insert(Yaml::String("buy_side".to_string()), Yaml::String("target".to_string()));
            map.insert(
                Yaml::String("sell_side".to_string()),
                Yaml::String("originator".to_string()),
            );
        } else {
            map.insert(
                Yaml::String("buy_side".to_string()),
                Yaml::String("originator".to_string()),
            );
            map.insert(Yaml::String("sell_side".to_string()), Yaml::String("target".to_string()));
        }
        if let Some(method) = value.match_method {
            map.insert(Yaml::String("method".to_string()), Yaml::String(method.to_string()));
        }
        Yaml::Hash(map)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum PoolEventKind<'h> {
    /// A change to the pool amount and/or value has taken place
    Adjustment(Adjustment<'h>),
    /// A deal was added to the pool
    PooledDeal(DealHolding<'h>),
    /// A deal was extracted from the pool
    UnpooledDeal(DealHolding<'h>),
    /// An acquisition was matched against a disposal
    Match(MatchDetails<'h>),
}

impl fmt::Display for PoolEventKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            PoolEventKind::PooledDeal(dh) => {
                let rem = if dh.split_parent().is_some() { "(remainder) " } else { "" };
                write!(f, "Pooled {}{}", rem, dh.total())
            }
            PoolEventKind::UnpooledDeal(dh) => write!(f, "Unpooled {}", dh.total()),
            PoolEventKind::Match(details) => {
                write!(
                    f,
                    "Matched {} --> {}",
                    details.originator().total(),
                    details.target().total()
                )
            }
            PoolEventKind::Adjustment(adj) => write!(f, "Adjusted {}", adj),
        }
    }
}

macro_rules! impl_from_kind_for_cell {
    ($kind:ty) => {
        impl<'h, 'a> From<$kind> for Cell<'a>
        where
            'h: 'a,
        {
            fn from(value: $kind) -> Self {
                let mut cell: Cell = match &value {
                    PoolEventKind::PooledDeal(dh) => {
                        [Cell::from(&"Pooled "), dh.total().clone().into()].into()
                    }
                    PoolEventKind::UnpooledDeal(dh) => {
                        [Cell::from(&"Unpooled "), dh.total().clone().into()].into()
                    }
                    PoolEventKind::Match(details) => [
                        Cell::from(&"Matched "),
                        [Cell::from(details.target().total().amount())].into(),
                    ]
                    .into(),
                    PoolEventKind::Adjustment(adj) => [Cell::from(&"Adjusted "), adj.into()].into(),
                };
                cell.iter_mut().next().unwrap().set_foreground(Some(Colour::Blue));
                cell
            }
        }
    };
}
impl_from_kind_for_cell!(PoolEventKind<'h>);
impl_from_kind_for_cell!(&PoolEventKind<'h>);

impl<'h> From<&PoolEventKind<'h>> for Yaml {
    fn from(value: &PoolEventKind<'h>) -> Self {
        match value {
            PoolEventKind::PooledDeal(dh) => {
                let mut map = Hash::new();
                map.insert(Yaml::String("type".to_string()), Yaml::String("Pooled".to_string()));
                let mut pooled_map = Hash::new();
                if dh.total().amount().is_negative() {
                    pooled_map
                        .insert(Yaml::String("disposal".to_string()), AbsDealHolding(dh).into());
                } else {
                    pooled_map
                        .insert(Yaml::String("acquisition".to_string()), AbsDealHolding(dh).into());
                }
                map.insert(Yaml::String("Pooled".to_string()), Yaml::Hash(pooled_map));
                Yaml::Hash(map)
            }
            PoolEventKind::UnpooledDeal(dh) => {
                let mut map = Hash::new();
                map.insert(Yaml::String("type".to_string()), Yaml::String("Unpooled".to_string()));
                let mut unpooled_map = Hash::new();
                if dh.total().amount().is_negative() {
                    unpooled_map
                        .insert(Yaml::String("disposal".to_string()), AbsDealHolding(dh).into());
                } else {
                    unpooled_map
                        .insert(Yaml::String("acquisition".to_string()), AbsDealHolding(dh).into());
                }
                map.insert(Yaml::String("Unpooled".to_string()), Yaml::Hash(unpooled_map));
                Yaml::Hash(map)
            }
            PoolEventKind::Match(details) => {
                let mut map = Hash::new();
                map.insert(Yaml::String("type".to_string()), Yaml::String("Matched".to_string()));
                map.insert(Yaml::String("Matched".to_string()), details.into());

                // Include 'acquired' and 'disposed'. These are the same as the 'deal' and
                // 'matched' field within the details, but are non-negative and indicate easily
                // for report writers whether the deal/matched field is an acquisition or disposal.
                /*map.insert(
                    Yaml::String("acquired".to_string()),
                    AbsDealHolding(details.buy_holding()).into(),
                );
                map.insert(
                    Yaml::String("disposed".to_string()),
                    AbsDealHolding(details.sell_holding()).into(),
                );*/
                Yaml::Hash(map)
            }
            PoolEventKind::Adjustment(adj) => {
                let mut map = Hash::new();
                map.insert(
                    Yaml::String("type".to_string()),
                    Yaml::String("Adjustment".to_string()),
                );
                map.insert(Yaml::String("Adjustment".to_string()), adj.into());
                Yaml::Hash(map)
            }
        }
    }
}

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
                PoolEventKind::UnpooledDeal(_) => 2,
                PoolEventKind::Match(_) => 3,
            }
        }
        ordinal(self).cmp(&ordinal(other))
    }
}

impl<'h> Add for PoolEventKind<'h> {
    type Output = Option<PoolEventKind<'h>>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (PoolEventKind::PooledDeal(self_deal), PoolEventKind::PooledDeal(rhs_deal)) => {
                if self_deal.unit() != rhs_deal.unit() {
                    return None;
                }
                (self_deal + rhs_deal).ok().map(PoolEventKind::PooledDeal)
            }
            (PoolEventKind::UnpooledDeal(self_deal), PoolEventKind::UnpooledDeal(rhs_deal)) => {
                if self_deal.unit() != rhs_deal.unit() {
                    return None;
                }
                (self_deal + rhs_deal).ok().map(PoolEventKind::UnpooledDeal)
            }
            (PoolEventKind::Match(self_match), PoolEventKind::Match(rhs_match)) => {
                if self_match.target().unit() != rhs_match.target().unit() {
                    return None;
                }
                (self_match + rhs_match).ok().map(PoolEventKind::Match)
            }
            _ => None,
        }
    }
}

#[derive(Clone)]
pub struct PoolEvent<'h> {
    /// A unique, incrementing sequence number to be able to correctly determine order when comparing events.
    sequence: usize,
    pool_name: &'h str,
    /// The date and time when the event took place in the reporting timezone. When events are folded,
    /// they form a range.
    /// This should show its time component, depending on whether the associated deal shows its time
    /// component.
    date: JDateTimeRange<'h>,
    event_kind: PoolEventKind<'h>,
    balance_before: PoolBalance<'h>,
    balance_after: PoolBalance<'h>,
}

impl<'h> PoolEvent<'h> {
    pub fn new(
        pool_name: &'h str,
        date: JDateTimeRange<'h>,
        kind: PoolEventKind<'h>,
        bal_before: PoolBalance<'h>,
        bal_after: PoolBalance<'h>,
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
            PoolEventKind::PooledDeal(dh) => dh.unit(),
            PoolEventKind::UnpooledDeal(dh) => dh.unit(),
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

    pub fn balance_before(&self) -> &PoolBalance<'h> {
        &self.balance_before
    }

    pub fn balance_after(&self) -> &PoolBalance<'h> {
        &self.balance_after
    }

    /// The date of the event in the reporting timezone. If events have been combined, this will cover
    /// the date range of those events.
    pub fn event_datetime(&self) -> JDateTimeRange<'h> {
        self.date
    }

    /// The date of the deal in the reporting timezone.
    pub fn deal_datetime(&self) -> JDateTimeRange<'h> {
        match &self.event_kind {
            PoolEventKind::PooledDeal(dh) => dh.datetime(),
            PoolEventKind::UnpooledDeal(dh) => dh.datetime(),
            PoolEventKind::Match(details) => details.sell_holding().datetime(),
            PoolEventKind::Adjustment(adj) => adj.datetime(),
        }
    }

    /*
    /// Iterates through the deals in the event, if any.
    ///
    /// Adjustment events do not have deals.
    pub fn deal_iter(&self) -> Box<dyn Iterator<Item = &Deal<'h>> + '_> {
        match &self.event_kind {
            PoolEventKind::PooledDeal(dh) => dh.deal_iter(),
            PoolEventKind::UnpooledDeal(dh) => dh.deal_iter(),
            PoolEventKind::Match(details) => details.originator().deal_iter(),
            PoolEventKind::Adjustment(_) => Box::new(iter::empty()),
        }
    }*/

    /// Gets the base acquisition cost.
    pub fn acquired(&self) -> Option<ValuedAmount<'h>> {
        match &self.event_kind {
            PoolEventKind::PooledDeal(details)
                if details.datetime().start() == self.event_datetime().start()
                    && details.total().amount() > 0 =>
            {
                Some(details.total_before_expenses())
            }
            _ => None,
        }
    }

    pub fn total_cost(&self) -> Option<ValuedAmount<'h>> {
        match &self.event_kind {
            PoolEventKind::PooledDeal(pooled)
                if pooled.datetime().start() == self.event_datetime().start()
                    && pooled.total().amount() > 0 =>
            {
                Some(pooled.total().valued_amount().clone())
            }
            PoolEventKind::Adjustment(_adj) => Some(
                (&self.balance_after - &self.balance_before)
                    .valued_amount()
                    .clone()
                    .without_unit(self.unit()),
            ),
            _ => None,
        }
    }

    /// Gets the disposal if this event was a match. The disposal is what was disposed and
    /// the proceeds received.
    pub fn disposed(&self) -> Option<ValuedAmount<'h>> {
        match &self.event_kind {
            PoolEventKind::Match(details) => Some(details.sell_holding().total_before_expenses()),
            _ => None,
        }
    }

    /// Gets the disposal value after the expense adjustment.
    pub fn net_proceeds(&self) -> Option<ValuedAmount<'h>> {
        match &self.event_kind {
            PoolEventKind::Match(details) => {
                let net_proceeds = details.net_proceeds();
                //if !net_proceeds.is_zero() {
                Some(net_proceeds)
                //} else {
                //    None
                //}
            }
            _ => None,
        }
    }

    pub fn expenses(&self) -> Option<ValuedAmount<'h>> {
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

    pub fn actual_cost(&self) -> Option<ValuedAmount<'h>> {
        match &self.event_kind {
            PoolEventKind::Match(details) => Some(details.actual_cost()),
            _ => None,
        }
    }

    pub fn gain(&self) -> Option<ValuedAmount<'h>> {
        match &self.event_kind {
            PoolEventKind::Match(details) if details.gain().amount() > 0 => Some(details.gain()),
            _ => None,
        }
    }

    pub fn losses(&self) -> Option<ValuedAmount<'h>> {
        match &self.event_kind {
            PoolEventKind::Match(details) if details.gain().amount() < 0 => Some(details.gain()),
            _ => None,
        }
    }

    pub fn description(&self) -> LinkedHashSet<&'h str> {
        match &self.event_kind {
            PoolEventKind::PooledDeal(dh) => dh.description(),
            PoolEventKind::UnpooledDeal(dh) => dh.description(),
            PoolEventKind::Match(details) => details.originator().description(),
            PoolEventKind::Adjustment(adj) => {
                let mut desc = LinkedHashSet::new();
                desc.insert(adj.entry().description().trim_start());
                desc
            }
        }
    }

    pub fn metadata(&self, tag: &str) -> LinkedHashSet<String> {
        match &self.event_kind {
            PoolEventKind::PooledDeal(dh) => dh.metadata(tag),
            PoolEventKind::UnpooledDeal(dh) => dh.metadata(tag),
            PoolEventKind::Match(details) => details.sell_holding().metadata(tag),
            PoolEventKind::Adjustment(adj) => adj.entry().metadata_tag_values(tag),
        }
    }
}

impl fmt::Display for PoolEvent<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} on pool: '{}' -> {:?}",
            self.event_datetime().start(),
            self.event_kind,
            self.pool_name,
            self.balance_after
        )
    }
}

/*
impl From<&PoolEvent<'_>> for Yaml {
    fn from(e: &PoolEvent) -> Self {
        let cag = Arguments::get().cast_cmd::<CapitalGainsCommand>().unwrap();
        let mut map = Hash::new();
        let mut pool_map = Hash::new();

        for col in cag.columns.iter() {
            match col {
                CapitalGainsColumn::Description => {
                    map.insert(
                        Yaml::String("description".to_string()),
                        Yaml::String(e.description().to_string()),
                    );
                }
                CapitalGainsColumn::EventDate => {
                    map.insert(Yaml::String("event_date".to_string()), e.event_datetime().into());
                }
                CapitalGainsColumn::DealDate => {
                    map.insert(Yaml::String("deal_date".to_string()), e.deal_datetime().into());
                }
                CapitalGainsColumn::Unit => {
                    map.insert(
                        Yaml::String("unit".to_string()),
                        Yaml::String(e.unit().code().to_string()),
                    );
                }
                CapitalGainsColumn::Event => {
                    let kind = match e.kind() {
                        PoolEventKind::PooledDeal(dh) => {
                            map.insert(Yaml::String("Pooled".to_string()), dh.into());
                            "Pooled"
                        }
                        PoolEventKind::UnpooledDeal(dh) => {
                            map.insert(Yaml::String("Unpooled".to_string()), dh.into());
                            "Unpooled"
                        }
                        PoolEventKind::Match(details) => {
                            map.insert(Yaml::String("Match".to_string()), details.target().into());
                            "Match"
                        }
                        PoolEventKind::Adjustment(adj) => {
                            map.insert(Yaml::String("Adjustment".to_string()), adj.into());
                            "Adjustment"
                        }
                    };
                    map.insert(Yaml::String("type".to_string()), Yaml::String(kind.to_string()));
                }
                CapitalGainsColumn::Acquired => {
                    if let Some(acquired) = e.acquired() {
                        map.insert(Yaml::String("acquired".to_string()), (&acquired).into());
                    }
                }
                CapitalGainsColumn::TotalCost => {
                    if let Some(total_cost) = e.total_cost() {
                        map.insert(Yaml::String("total_cost".to_string()), (&total_cost).into());
                    }
                }
                CapitalGainsColumn::Disposed => {
                    if let Some(disposed) = e.disposed() {
                        map.insert(Yaml::String("disposed".to_string()), (&disposed.abs()).into());
                    }
                }
                CapitalGainsColumn::NetProceeds => {
                    if let Some(net_proceeds) = e.net_proceeds() {
                        map.insert(
                            Yaml::String("net_proceeds".to_string()),
                            (&net_proceeds).into(),
                        );
                    }
                }
                CapitalGainsColumn::Expenses => {
                    if let Some(expenses) = e.expenses() {
                        map.insert(Yaml::String("expenses".to_string()), (&expenses).into());
                    }
                }
                CapitalGainsColumn::ActualCost => {
                    if let Some(actual_cost) = e.actual_cost() {
                        map.insert(Yaml::String("actual_cost".to_string()), (&actual_cost).into());
                    }
                }
                CapitalGainsColumn::Gain => {
                    if let Some(gain) = e.gain() {
                        map.insert(Yaml::String("gain".to_string()), (&gain).into());
                    }
                }
                CapitalGainsColumn::Loss => {
                    if let Some(loss) = e.losses() {
                        map.insert(Yaml::String("loss".to_string()), (&loss.abs()).into());
                    }
                }
                CapitalGainsColumn::Pool => {
                    pool_map.insert(
                        Yaml::String("name".to_string()),
                        Yaml::String(e.pool_name().to_string()),
                    );
                }
                CapitalGainsColumn::PoolBalBefore => {
                    pool_map.insert(
                        Yaml::String("balance_before".to_string()),
                        e.balance_before().valued_amount().unwrap_or(&ValuedAmount::nil()).into(),
                    );
                }
                CapitalGainsColumn::PoolBalAfter => {
                    pool_map.insert(
                        Yaml::String("balance_after".to_string()),
                        e.balance_after().valued_amount().unwrap_or(&ValuedAmount::nil()).into(),
                    );
                }
                CapitalGainsColumn::Metadata(tag) => {
                    map.insert(
                        Yaml::String(format!("+{}", tag)),
                        Yaml::Array(
                            e.metadata(tag).into_iter().map(Yaml::String).collect::<Vec<_>>(),
                        ),
                    );
                }
            }
        }
        if !pool_map.is_empty() {
            map.insert(Yaml::String("pool".to_string()), Yaml::Hash(pool_map));
        }
        Yaml::Hash(map)
    }
}*/

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

impl<'h> Add<Self> for PoolEvent<'h> {
    type Output = Result<PoolEvent<'h>, (PoolEvent<'h>, PoolEvent<'h>)>;

    fn add(mut self, mut rhs: Self) -> Self::Output {
        if self.pool_name != rhs.pool_name {
            return Err((self, rhs));
        }
        assert_eq!(self.pool_name, rhs.pool_name);

        let combine_kind_res = match (self.event_kind, rhs.event_kind) {
            (PoolEventKind::PooledDeal(self_deal), PoolEventKind::PooledDeal(rhs_deal)) => {
                match self_deal + rhs_deal {
                    Ok(deal) => Ok(PoolEventKind::PooledDeal(deal)),
                    Err((l_dh, r_dh)) => {
                        Err((PoolEventKind::PooledDeal(l_dh), PoolEventKind::PooledDeal(r_dh)))
                    }
                }
            }
            (PoolEventKind::UnpooledDeal(self_deal), PoolEventKind::UnpooledDeal(rhs_deal)) => {
                match self_deal + rhs_deal {
                    Ok(deal) => Ok(PoolEventKind::UnpooledDeal(deal)),
                    Err((l_dh, r_dh)) => {
                        Err((PoolEventKind::UnpooledDeal(l_dh), PoolEventKind::UnpooledDeal(r_dh)))
                    }
                }
            }
            (PoolEventKind::Match(self_match), PoolEventKind::Match(rhs_match)) => {
                match self_match + rhs_match {
                    Ok(matched) => Ok(PoolEventKind::Match(matched)),
                    Err((l_match, r_match)) => {
                        Err((PoolEventKind::Match(l_match), PoolEventKind::Match(r_match)))
                    }
                }
            }
            (self_kind, rhs_kind) => Err((self_kind, rhs_kind)),
        };
        match combine_kind_res {
            Ok(kind) => self.event_kind = kind,
            Err((self_kind, rhs_kind)) => {
                self.event_kind = self_kind;
                rhs.event_kind = rhs_kind;
                return Err((self, rhs));
            }
        }
        self.date = self.date + rhs.date;

        match self.sequence.cmp(&rhs.sequence) {
            std::cmp::Ordering::Less => {
                self.balance_after = rhs.balance_after;
            }
            _ => {
                self.balance_before = rhs.balance_before;
            }
        };
        Ok(self)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum AggregatedPoolEvent<'h, 'e> {
    One(&'e PoolEvent<'h>),
    Many(Vec<AggregatedPoolEvent<'h, 'e>>),
}

impl<'h, 'e> AggregatedPoolEvent<'h, 'e> {
    pub fn event_iter(&self) -> Box<dyn DoubleEndedIterator<Item = &'e PoolEvent<'h>> + '_> {
        match self {
            AggregatedPoolEvent::One(e) => Box::new(iter::once(*e)),
            AggregatedPoolEvent::Many(es) => {
                Box::new(es.iter().flat_map(AggregatedPoolEvent::event_iter))
            }
        }
    }

    pub fn first_chrono_event(&self) -> &'e PoolEvent<'h> {
        self.min_event(|a, b| a.event_datetime().start().cmp(&b.event_datetime().start()))
    }

    pub fn last_chrono_event(&self) -> &'e PoolEvent<'h> {
        self.max_event(|a, b| a.event_datetime().start().cmp(&b.event_datetime().start()))
    }

    fn min_event<F>(&self, cmp: F) -> &'e PoolEvent<'h>
    where
        F: Fn(&PoolEvent<'h>, &PoolEvent<'h>) -> std::cmp::Ordering,
    {
        match self {
            AggregatedPoolEvent::One(e) => e,
            AggregatedPoolEvent::Many(_es) => {
                let mut iter = self.event_iter();
                let mut min = iter.next().unwrap();
                for e in iter {
                    if cmp(e, min) == std::cmp::Ordering::Less {
                        min = e;
                    }
                }
                min
            }
        }
    }

    fn max_event<F>(&self, cmp: F) -> &'e PoolEvent<'h>
    where
        F: Fn(&PoolEvent<'h>, &PoolEvent<'h>) -> std::cmp::Ordering,
    {
        match self {
            AggregatedPoolEvent::One(e) => e,
            AggregatedPoolEvent::Many(_es) => {
                let mut iter = self.event_iter().rev();
                let mut max = iter.next().unwrap();
                for e in iter {
                    if cmp(e, max) == std::cmp::Ordering::Greater {
                        max = e;
                    }
                }
                max
            }
        }
    }

    /// Gets the unit iff a single unit is common to all events.
    pub fn unit(&self) -> Option<&'h Unit<'h>> {
        match self {
            AggregatedPoolEvent::One(e) => Some(e.unit()),
            AggregatedPoolEvent::Many(es) => {
                let first_unit = es.first().unwrap().unit();
                if es.iter().skip(1).all(|ape| ape.unit() == first_unit) {
                    first_unit
                } else {
                    None
                }
            }
        }
    }

    pub fn pool_name(&self) -> Option<&str> {
        match self {
            AggregatedPoolEvent::One(e) => Some(e.pool_name()),
            AggregatedPoolEvent::Many(es) => {
                let first_pool_name = es.first().unwrap().pool_name();
                if es.iter().skip(1).all(|ape| ape.pool_name() == first_pool_name) {
                    first_pool_name
                } else {
                    None
                }
            }
        }
    }

    pub fn event_datetime(&self) -> JDateTimeRange<'h> {
        match self {
            AggregatedPoolEvent::One(e) => (*e).event_datetime(),
            AggregatedPoolEvent::Many(_es) => JDateTimeRange::new(
                self.first_chrono_event().event_datetime().start(),
                Some(self.last_chrono_event().event_datetime().end()),
            ),
        }
    }

    pub fn deal_datetime(&self) -> JDateTimeRange<'h> {
        match self {
            AggregatedPoolEvent::One(e) => e.deal_datetime(),
            AggregatedPoolEvent::Many(_es) => {
                let cmp = |a: &PoolEvent<'h>, b: &PoolEvent<'h>| {
                    a.deal_datetime().start().cmp(&b.deal_datetime().start())
                };
                JDateTimeRange::new(
                    self.min_event(cmp).deal_datetime().start(),
                    Some(self.max_event(cmp).deal_datetime().end()),
                )
            }
        }
    }

    pub fn kind(&self) -> Option<impl AsRef<PoolEventKind<'h>> + '_> {
        match self {
            AggregatedPoolEvent::One(e) => Some(Cow::Borrowed(e.kind())),
            AggregatedPoolEvent::Many(_ape) => {
                let mut iter = self.event_iter().map(PoolEvent::kind).cloned();
                let mut acc = iter.next().unwrap();
                for kind in iter {
                    acc = (acc + kind)?;
                }
                Some(Cow::Owned(acc))
            }
        }
    }

    pub fn balance_before(&self) -> Option<&PoolBalance<'h>> {
        match self {
            AggregatedPoolEvent::One(e) => Some(e.balance_before()),
            AggregatedPoolEvent::Many(_ape_list) => {
                // If the pool name is common, the balance before will be the balance
                // of that pool.
                self.pool_name().map(|_| self.first_chrono_event().balance_before())
            }
        }
    }

    pub fn balance_after(&self) -> Option<&PoolBalance<'h>> {
        match self {
            AggregatedPoolEvent::One(e) => Some(e.balance_after()),
            AggregatedPoolEvent::Many(_ape_list) => {
                // If the pool name is common, the balance after will be the balance
                // of that pool.
                self.pool_name().map(|_| self.last_chrono_event().balance_after())
            }
        }
    }

    /*
    /// An iterator one level down.
    pub fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = &'a AggregatedPoolEvent<'h>> + 'a> {
        match self {
            AggregatedPoolEvent::One(e) => Box::new(iter::once(e)),
            AggregatedPoolEvent::Many(es) => Box::new(es.iter()),
        }
    }*/

    pub fn filter_events(&self, event_filter: &EventFilter) -> Option<Self> {
        match self {
            AggregatedPoolEvent::One(e) => {
                if event_filter.is_included(e) {
                    Some(AggregatedPoolEvent::One(e))
                } else {
                    None
                }
            }
            AggregatedPoolEvent::Many(es) => {
                let mut filtered = vec![];
                for e in es.iter() {
                    if let Some(e) = e.filter_events(event_filter) {
                        filtered.push(e);
                    }
                }
                if filtered.is_empty() {
                    None
                } else if filtered.len() == 1 {
                    Some(filtered.into_iter().next().unwrap())
                } else {
                    Some(AggregatedPoolEvent::Many(filtered))
                }
            }
        }
    }

    pub fn acquired(&self) -> Option<ValuedAmount<'h>> {
        match self {
            AggregatedPoolEvent::One(e) => e.acquired(),
            AggregatedPoolEvent::Many(es) => {
                let sum = es.iter().filter_map(|e| e.acquired()).sum::<Option<ValuedAmount>>()?;
                Some(sum)
            }
        }
    }

    /// Gets the actual cost of the disposal.
    pub fn actual_cost(&self) -> Option<ValuedAmount<'h>> {
        match self {
            AggregatedPoolEvent::One(e) => e.actual_cost(),
            AggregatedPoolEvent::Many(es) => {
                let sum =
                    es.iter().filter_map(|e| e.actual_cost()).sum::<Option<ValuedAmount>>()?;
                Some(sum)
            }
        }
    }

    pub fn total_cost(&self) -> Option<ValuedAmount<'h>> {
        match self {
            AggregatedPoolEvent::One(e) => e.total_cost(),
            AggregatedPoolEvent::Many(es) => {
                let sum = es.iter().filter_map(|e| e.total_cost()).sum::<Option<ValuedAmount>>()?;
                Some(sum)
            }
        }
    }

    pub fn disposed(&self) -> Option<ValuedAmount<'h>> {
        match self {
            AggregatedPoolEvent::One(e) => e.disposed(),
            AggregatedPoolEvent::Many(es) => {
                let sum = es.iter().filter_map(|e| e.disposed()).sum::<Option<ValuedAmount>>()?;
                Some(sum)
            }
        }
    }

    pub fn net_proceeds(&self) -> Option<ValuedAmount<'h>> {
        match self {
            AggregatedPoolEvent::One(e) => e.net_proceeds(),
            AggregatedPoolEvent::Many(es) => {
                let sum =
                    es.iter().filter_map(|e| e.net_proceeds()).sum::<Option<ValuedAmount>>()?;
                Some(sum)
            }
        }
    }

    pub fn expenses(&self) -> Option<ValuedAmount<'h>> {
        match self {
            AggregatedPoolEvent::One(e) => e.expenses(),
            AggregatedPoolEvent::Many(es) => {
                let sum = es.iter().filter_map(|e| e.expenses()).sum::<Option<ValuedAmount>>()?;
                Some(sum)
            }
        }
    }

    pub fn gain(&self) -> Option<ValuedAmount<'h>> {
        match self {
            AggregatedPoolEvent::One(e) => e.gain(),
            AggregatedPoolEvent::Many(es) => {
                let sum = es.iter().filter_map(|e| e.gain()).sum::<Option<ValuedAmount>>()?;
                Some(sum)
            }
        }
    }

    pub fn loss(&self) -> Option<ValuedAmount<'h>> {
        match self {
            AggregatedPoolEvent::One(e) => e.losses(),
            AggregatedPoolEvent::Many(es) => {
                let sum = es.iter().filter_map(|e| e.loss()).sum::<Option<ValuedAmount>>()?;
                Some(sum)
            }
        }
    }

    pub fn description(&self) -> LinkedHashSet<&'h str> {
        match self {
            AggregatedPoolEvent::One(e) => e.description(),
            AggregatedPoolEvent::Many(es) => {
                let mut descs = LinkedHashSet::with_capacity(es.len());
                for vals in es.iter().map(|e| e.description()) {
                    for val in vals {
                        descs.insert_if_absent(val);
                    }
                }
                descs
            }
        }
    }

    pub fn metadata(&self, tag: &str) -> LinkedHashSet<String> {
        match self {
            AggregatedPoolEvent::One(e) => e.metadata(tag),
            AggregatedPoolEvent::Many(es) => {
                let mut md_vals = LinkedHashSet::new();
                for vals in es.iter().map(|e| e.metadata(tag)) {
                    for val in vals {
                        md_vals.insert_if_absent(val);
                    }
                }
                md_vals
            }
        }
    }

    pub fn sort(&mut self) {
        match self {
            AggregatedPoolEvent::One(_) => (),
            AggregatedPoolEvent::Many(es) => es.sort(),
        }
    }

    pub fn sort_by<F>(&mut self, compare: F)
    where
        F: Fn(&Self, &Self) -> std::cmp::Ordering,
    {
        match self {
            AggregatedPoolEvent::One(_) => (),
            AggregatedPoolEvent::Many(es) => es.sort_by(|a, b| compare(a, b)),
        }
    }

    pub fn as_table_rows(&self, include_sub_events: bool, total_row: bool) -> Vec<Row> {
        let cag = Arguments::get().cast_cmd::<CagCommand>().unwrap();
        let mut event_date = self.event_datetime();
        //event_date.with_brief_format(true);
        let mut deal_date = self.deal_datetime();
        if cag.show_time {
            event_date = event_date.with_time_format(&cag.datetime_args.time_format);
            deal_date = deal_date.with_time_format(&cag.datetime_args.time_format);
        } else {
            event_date = event_date.without_time();
            deal_date = deal_date.without_time();
        };
        let mut column_data = vec![];
        for col in cag.columns.iter() {
            match col {
                CapitalGainsColumn::EventDate => column_data.push(event_date.into()),
                CapitalGainsColumn::DealDate => column_data.push(deal_date.into()),
                CapitalGainsColumn::Unit => {
                    column_data.push(self.unit().map(Unit::code).map(Cell::new).into())
                }
                CapitalGainsColumn::Pool => {
                    column_data.push(self.pool_name().map(Cell::new).into())
                }
                CapitalGainsColumn::Event => {
                    column_data.push(Cell::from(self.kind().as_ref().map(|k| k.as_ref())))
                }
                CapitalGainsColumn::Acquired => column_data.push(self.acquired().into()),
                CapitalGainsColumn::TotalCost => column_data.push(self.total_cost().into()),
                CapitalGainsColumn::ActualCost => column_data.push(self.actual_cost().into()),
                CapitalGainsColumn::Disposed => column_data.push(self.disposed().into()),
                CapitalGainsColumn::NetProceeds => column_data.push(self.net_proceeds().into()),
                CapitalGainsColumn::Expenses => column_data.push(self.expenses().into()),
                CapitalGainsColumn::Gain => column_data.push(self.gain().into()),
                CapitalGainsColumn::Loss => column_data.push(self.loss().into()),
                CapitalGainsColumn::PoolBalBefore => column_data.push(self.balance_before().into()),
                CapitalGainsColumn::PoolBalAfter => column_data.push(self.balance_after().into()),
                CapitalGainsColumn::Description => {
                    if total_row {
                        column_data.push(Cell::default())
                    } else {
                        column_data.push(self.description().iter().join(", ").into())
                    }
                }
                CapitalGainsColumn::Metadata(tag) => {
                    if total_row {
                        column_data.push(Cell::default())
                    } else {
                        column_data.push(self.metadata(tag).iter().join(", ").into())
                    }
                }
            }
        }

        let mut rows = vec![Row::from(column_data)];
        if let AggregatedPoolEvent::Many(es) = self {
            if include_sub_events {
                for e in es {
                    let mut inner_rows = e.as_table_rows(include_sub_events, total_row);
                    inner_rows.first_mut().iter_mut().for_each(|r| {
                        r.set_weight(Weight::Faint);
                    });
                    rows.extend(inner_rows);
                }
            }
        }
        rows
    }

    pub fn as_yaml(&self, render_children: bool) -> Yaml {
        let cmd = Arguments::get().cast_cmd::<CagCommand>().unwrap();
        let unit = self.unit();
        let event_datetime = self.event_datetime();
        let deal_datetime = self.deal_datetime();
        let pool_name = self.pool_name().map(str::to_string).map(Yaml::String);
        let balance_before = if pool_name.is_some() {
            Some(Yaml::from(self.balance_before().unwrap().valued_amount()))
        } else {
            None
        };
        let balance_after = if pool_name.is_some() {
            Some(Yaml::from(self.balance_after().unwrap().valued_amount()))
        } else {
            None
        };

        let mut map = Hash::new();
        let mut pool_map = Hash::new();
        for col in cmd.columns.iter() {
            match col {
                CapitalGainsColumn::Description => {
                    map.insert(
                        Yaml::String("description".to_string()),
                        Yaml::String(self.description().iter().join(", ")),
                    );
                }
                CapitalGainsColumn::EventDate => {
                    map.insert(Yaml::String("event_date".to_string()), event_datetime.into());
                }
                CapitalGainsColumn::DealDate => {
                    map.insert(Yaml::String("deal_date".to_string()), deal_datetime.into());
                }
                CapitalGainsColumn::Unit => {
                    if let Some(unit) = unit {
                        map.insert(
                            Yaml::String("unit".to_string()),
                            Yaml::String(unit.code().to_string()),
                        );
                    }
                }
                CapitalGainsColumn::Event => {
                    let kind = self.kind().map(|k| match &k.as_ref() {
                        PoolEventKind::PooledDeal(dh) => {
                            map.insert(Yaml::String("pooled".to_string()), dh.into());
                            "pooled"
                        }
                        PoolEventKind::UnpooledDeal(dh) => {
                            map.insert(Yaml::String("unpooled".to_string()), dh.into());
                            "unpooled"
                        }
                        PoolEventKind::Match(details) => {
                            map.insert(Yaml::String("match".to_string()), details.into());
                            "match"
                        }
                        PoolEventKind::Adjustment(adj) => {
                            map.insert(Yaml::String("adjustment".to_string()), adj.into());
                            "adjustment"
                        }
                    });
                    if let Some(kind) = kind {
                        map.insert(
                            Yaml::String("type".to_string()),
                            Yaml::String(kind.to_string()),
                        );
                    }
                }
                CapitalGainsColumn::Acquired => {
                    if let Some(acquired) = self.acquired() {
                        map.insert(Yaml::String("acquired".to_string()), (&acquired).into());
                    }
                }
                CapitalGainsColumn::TotalCost => {
                    if let Some(total_cost) = self.total_cost() {
                        map.insert(Yaml::String("total_cost".to_string()), (&total_cost).into());
                    }
                }
                CapitalGainsColumn::Disposed => {
                    if let Some(disposed) = self.disposed() {
                        map.insert(Yaml::String("disposed".to_string()), (&disposed.abs()).into());
                    }
                }
                CapitalGainsColumn::NetProceeds => {
                    if let Some(net_proceeds) = self.net_proceeds() {
                        map.insert(
                            Yaml::String("net_proceeds".to_string()),
                            (&net_proceeds).into(),
                        );
                    }
                }
                CapitalGainsColumn::Expenses => {
                    if let Some(expenses) = self.expenses() {
                        map.insert(Yaml::String("expenses".to_string()), (&expenses).into());
                    }
                }
                CapitalGainsColumn::ActualCost => {
                    if let Some(actual_cost) = self.actual_cost() {
                        map.insert(Yaml::String("actual_cost".to_string()), (&actual_cost).into());
                    }
                }
                CapitalGainsColumn::Gain => {
                    if let Some(gain) = self.gain() {
                        map.insert(Yaml::String("gain".to_string()), (&gain).into());
                    }
                }
                CapitalGainsColumn::Loss => {
                    if let Some(loss) = self.loss() {
                        map.insert(Yaml::String("loss".to_string()), (&loss.abs()).into());
                    }
                }
                CapitalGainsColumn::Pool => {
                    if let Some(pool_name) = self.pool_name() {
                        pool_map.insert(
                            Yaml::String("name".to_string()),
                            Yaml::String(pool_name.to_string()),
                        );
                    }
                }
                CapitalGainsColumn::PoolBalBefore => {
                    if let Some(bal_before) = &balance_before {
                        pool_map
                            .insert(Yaml::String("balance_before".to_string()), bal_before.clone());
                    }
                }
                CapitalGainsColumn::PoolBalAfter => {
                    if let Some(bal_after) = &balance_after {
                        pool_map
                            .insert(Yaml::String("balance_after".to_string()), bal_after.clone());
                    }
                }
                CapitalGainsColumn::Metadata(tag) => {
                    let values = self.metadata(tag);
                    if !values.is_empty() {
                        map.insert(
                            Yaml::String(format!("+{}", tag)),
                            Yaml::Array(values.into_iter().map(Yaml::String).collect::<Vec<_>>()),
                        );
                    }
                }
            }
        }
        if !pool_map.is_empty() {
            map.insert(Yaml::String("pool".to_string()), Yaml::Hash(pool_map));
        }

        if render_children {
            if let AggregatedPoolEvent::Many(es) = self {
                map.insert(
                    Yaml::String("events".to_string()),
                    Yaml::Array(es.iter().map(|e| e.as_yaml(true)).collect()),
                );
            }
        }
        Yaml::Hash(map)
    }
}

impl Ord for AggregatedPoolEvent<'_, '_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.event_datetime().start().cmp(&other.event_datetime().start())
    }
}

impl PartialOrd for AggregatedPoolEvent<'_, '_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'h, 'e> From<&'e PoolEvent<'h>> for AggregatedPoolEvent<'h, 'e> {
    fn from(event: &'e PoolEvent<'h>) -> Self {
        AggregatedPoolEvent::One(event)
    }
}

impl<'h, 'e> From<Vec<&'e PoolEvent<'h>>> for AggregatedPoolEvent<'h, 'e> {
    fn from(events: Vec<&'e PoolEvent<'h>>) -> Self {
        if events.len() == 1 {
            AggregatedPoolEvent::One(events.into_iter().next().unwrap())
        } else {
            AggregatedPoolEvent::Many(events.into_iter().map(AggregatedPoolEvent::One).collect())
        }
    }
}

/*
impl<'h, 'e, 'a> From<&'a AggregatedPoolEvent<'h, 'e>> for Vec<Row<'a>>
where
    'h: 'e,
    'e: 'a,
{
    fn from(event: &'a AggregatedPoolEvent<'h, 'e>) -> Self {
        let cag = Arguments::get().cast_cmd::<CapitalGainsCommand>().unwrap();
        let mut event_date = event.event_datetime();
        event_date.with_brief_format(true);
        let mut deal_date = event.deal_datetime().with_brief_format(true);
        if cag.show_time {
            event_date = event_date.with_time_format(&cag.datetime_args.time_format);
            deal_date = deal_date.with_time_format(&cag.datetime_args.time_format);
        } else {
            event_date = event_date.without_time();
            deal_date = deal_date.without_time();
        };
        let mut column_data = vec![];
        for col in cag.columns.iter() {
            match col {
                CapitalGainsColumn::EventDate => {
                    column_data.push(Cell::new(event_date.to_string()))
                }
                CapitalGainsColumn::DealDate => column_data.push(Cell::new(deal_date.to_string())),
                CapitalGainsColumn::Unit => {
                    column_data.push(event.unit().map(Unit::code).map(Cell::new).into())
                }
                CapitalGainsColumn::Pool => {
                    column_data.push(event.pool_name().map(Cell::new).into())
                }
                CapitalGainsColumn::Event => column_data.push(Cell::from(event.kind())),
                CapitalGainsColumn::Acquired => column_data.push(event.acquired().into()),
                CapitalGainsColumn::TotalCost => column_data.push(event.total_cost().into()),
                CapitalGainsColumn::ActualCost => column_data.push(event.actual_cost().into()),
                CapitalGainsColumn::Disposed => column_data.push(event.disposed().into()),
                CapitalGainsColumn::NetProceeds => column_data.push(event.net_proceeds().into()),
                CapitalGainsColumn::Gain => column_data.push(event.gain().into()),
                CapitalGainsColumn::Loss => column_data.push(event.loss().into()),
                CapitalGainsColumn::PoolBalBefore => {
                    column_data.push(event.balance_before().into())
                }
                CapitalGainsColumn::PoolBalAfter => column_data.push(event.balance_after().into()),
                CapitalGainsColumn::Metadata(tag) => {
                    column_data.push(event.metadata(tag).iter().join(", ").into())
                }
            }
        }

        let mut rows = vec![Row::from(column_data)];
        if let AggregatedPoolEvent::Many(es, render) = event {
            if *render {
                for e in es {
                    let mut inner_rows = Vec::from(e);
                    inner_rows.first_mut().iter_mut().for_each(|r| {
                        r.set_faint(true);
                    });
                    rows.extend(inner_rows);
                }
            }
        }
        rows
    }
}*/

impl Add for AggregatedPoolEvent<'_, '_> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            AggregatedPoolEvent::One(e) => match rhs {
                AggregatedPoolEvent::One(rhs) => AggregatedPoolEvent::Many(vec![
                    AggregatedPoolEvent::One(e),
                    AggregatedPoolEvent::One(rhs),
                ]),
                AggregatedPoolEvent::Many(mut list) => {
                    list.insert(0, AggregatedPoolEvent::One(e));
                    AggregatedPoolEvent::Many(list)
                }
            },
            AggregatedPoolEvent::Many(mut list) => {
                list.push(rhs);
                AggregatedPoolEvent::Many(list)
            }
        }
    }
}
