/*
 * Copyright (c) 2023-2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjusted_value::AdjustedValue;
use crate::adjustment::Adjustment;
use crate::cag_configuration::MatchMethod;
use crate::deal::Deal;
use crate::holding::DealHolding::Single;
use crate::holding::average::AverageDealHolding;
use crate::holding::deal_holding::DealHolding::{Average, Sequence};
use crate::holding::sequence::SequenceDealHolding;
use crate::holding::single::SingleDealHolding;
use crate::holding::summary::DealHoldingSummary;
use crate::ruleset::{AgeUnit, Condition, DealKind};
use chrono::{DateTime, Duration};
use chrono_tz::Tz;
use journ_core::amount::{Amount, Quantity};
use journ_core::configuration::Configuration;
use journ_core::datetime::{JDateTime, JDateTimeRange};
use journ_core::error::{JournError, JournResult};
use journ_core::journal_entry::JournalEntry;
use journ_core::metadata::Metadata;
use journ_core::unit::Unit;
use linked_hash_set::LinkedHashSet;
use std::cell::{Ref, RefCell, RefMut};
use std::fmt::Debug;
use std::rc::Rc;
use std::{fmt, iter};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HoldingHandle<'h> {
    holding: Rc<RefCell<Option<DealHolding<'h>>>>,
}
impl<'h> HoldingHandle<'h> {
    pub fn new(holding: DealHolding<'h>) -> Self {
        Self { holding: Rc::new(RefCell::new(Some(holding))) }
    }

    pub fn borrow(&self) -> Ref<DealHolding<'h>> {
        Ref::map(self.holding.borrow(), |h| h.as_ref().unwrap())
    }

    pub fn borrow_mut(&self) -> RefMut<DealHolding<'h>> {
        RefMut::map(self.holding.borrow_mut(), |h| h.as_mut().unwrap())
    }

    pub fn take_with<F: FnOnce(DealHolding<'h>) -> DealHolding<'h>>(&self, f: F) {
        let taken = self.holding.borrow_mut().take().unwrap();
        let new_holding = f(taken);
        *self.holding.borrow_mut() = Some(new_holding);
    }

    pub fn take(self) -> DealHolding<'h> {
        self.holding.borrow_mut().take().unwrap()
    }
}

pub(super) static DEAL_HOLDING_ID_COUNTER: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(1);

/// A unifying type for the different kinds of deal holdings (containers). A DealHolding is a tree structure with
/// [Deal] types at its leaves, with the root of the tree being managed by a [`Pool`].
/// Thus, separate DealHoldings are required for each asset unit.
///
/// A DealHolding can never be empty; it will always have at least one deal, which itself may be zero.
#[derive(PartialEq, Eq)]
pub enum DealHolding<'h> {
    Single(SingleDealHolding<'h>),
    Sequence(SequenceDealHolding<'h>),
    Average(AverageDealHolding<'h>),
}

impl<'h> DealHolding<'h> {
    pub fn id(&self) -> usize {
        match self {
            Single(single) => single.id(),
            Sequence(seq) => seq.id(),
            Average(avg) => avg.id(),
        }
    }

    pub fn into_single(self) -> Option<SingleDealHolding<'h>> {
        match self {
            Single(single) => Some(single),
            _ => None,
        }
    }

    pub fn into_sequence(self) -> Option<SequenceDealHolding<'h>> {
        match self {
            Sequence(seq) => Some(seq),
            _ => None,
        }
    }

    pub fn into_average(self) -> Option<AverageDealHolding<'h>> {
        match self {
            Average(avg) => Some(avg),
            _ => None,
        }
    }

    pub fn amount(&self) -> Amount<'h> {
        match &self {
            Single(single) => single.amount(),
            Sequence(sh) => sh.amount(),
            Average(ah) => ah.amount(),
        }
    }

    /// Gets the adjusted value of the holding, which is the total value of the deals in the holding, after factoring in
    /// expenses and including any adjustments.
    pub fn value(&self) -> Amount<'h> {
        match &self {
            Single(single) => single.value(),
            Sequence(sh) => sh.value(),
            Average(ah) => ah.value(),
        }
    }

    /// Gets the deal totals without expenses. This will include the primary unit
    /// as the amount and the amount in the unit of account as the first valuation.
    pub fn consideration(&self) -> Amount<'h> {
        match self {
            Single(single) => single.consideration(),
            Sequence(seq) => seq.consideration(),
            Average(avg) => avg.consideration(),
        }
    }

    pub fn adjusted_value(&self) -> AdjustedValue<'h> {
        match self {
            Single(single) => single.adjusted_value(),
            Sequence(seq) => seq.adjusted_value(),
            Average(avg) => avg.adjusted_value(),
        }
    }

    pub fn unit(&self) -> &'h Unit<'h> {
        match self {
            Single(single) => single.amount().unit(),
            Sequence(seq) => seq.amount().unit(),
            Average(avg) => avg.amount().unit(),
        }
    }

    pub fn datetime(&self) -> JDateTimeRange {
        match self {
            Single(single) => single.deal().datetime(),
            Sequence(seq) => seq.datetime(),
            Average(avg) => avg.datetime(),
        }
    }

    pub fn into_deal_iter(self) -> Box<dyn Iterator<Item = Deal<'h>> + 'h> {
        match self {
            Single(single) => Box::new(iter::once(single.into_deal())),
            Sequence(seq) => seq.into_deal_iter(),
            Average(avg) => Box::new(avg.into_deal_iter()),
        }
    }

    pub fn deal_iter(&self) -> Box<dyn Iterator<Item = &Deal<'h>> + '_> {
        match self {
            Single(single) => Box::new(iter::once(single.deal())),
            Sequence(seq) => seq.deal_iter(),
            Average(avg) => Box::new(avg.deal_iter()),
        }
    }

    /// Gets the origin of the deals within the holding. The origin is not necessarily an original deal from
    /// a journal entry, only the set of split parents of the deals contained within.
    pub fn split_parent(&self) -> Option<&Rc<DealHoldingSummary<'h>>> {
        match self {
            Single(single) => single.split_parent(),
            Sequence(seq) => seq.split_parent(),
            Average(avg) => avg.split_parent(),
        }
    }

    /// Gets the expenses of deals within the holding. Note that this will
    /// always return `None` for average holdings.
    pub fn expenses(&self) -> Amount<'h> {
        match self {
            Single(single) => single.expenses(),
            Sequence(seq) => seq.expenses(),
            Average(avg) => avg.expenses().clone(),
        }
    }

    /// Gets the total explicit taxable gain of all the deals in the holding, if any of them
    /// has an explicitly set taxable gain.
    pub fn explicit_taxable_gain(&self) -> Option<Amount<'h>> {
        let explicit_gain =
            self.deal_iter().map(|d| d.taxable_gain().unwrap_or(Amount::nil())).sum();
        match explicit_gain {
            Some(va) if !va.is_nil() => Some(va),
            _ => None,
        }
    }

    pub fn set_value_on_date(
        &mut self,
        uoa: &'h Unit<'h>,
        config: &Configuration<'h>,
        date: DateTime<Tz>,
    ) -> Result<(), JournError> {
        match self {
            Single(single) => single.set_value_on_date(uoa, config, date),
            Sequence(seq) => seq.set_value_on_date(uoa, config, date),
            Average(avg) => avg.set_value_on_date(uoa, config, date),
        }
    }

    pub fn ensure_valued(&mut self, uoa: &'h Unit<'h>) -> Result<(), JournError> {
        match self {
            Single(single) => single.ensure_valued(uoa)?,
            Sequence(seq) => seq.ensure_valued(uoa)?,
            Average(avg) => avg.ensure_valued(uoa)?,
        }
        Ok(())
    }

    /// Adds another holding to the end of this one, returning a new holding.
    pub fn push(self, holding: Self, match_method: MatchMethod) -> Self {
        match match_method {
            // Whether fifo or lifo, insertion is the same. The match_method is respected during the split_max() calls, not here.
            MatchMethod::Fifo | MatchMethod::Lifo => match self {
                Single(single) => {
                    Sequence(SequenceDealHolding::from_iterator([Single(single), holding]))
                }
                Sequence(mut seq) => {
                    seq.insert(holding);
                    Sequence(seq)
                }
                Average(avg) => {
                    Sequence(SequenceDealHolding::from_iterator([Average(avg), holding]))
                }
            },
            // Convert to an average holding immediately. The id of the whole holding will be the extraction id
            // for scheduled pool extractions. This is because once a holding is averaged, it is no longer possible to extract a single deal from it.
            MatchMethod::Average => match self {
                Average(mut avg) => {
                    for deal in holding.into_deal_iter() {
                        avg.add_deal(deal)
                    }
                    Average(avg)
                }
                _ => {
                    let self_id = self.id();
                    let mut avg: AverageDealHolding =
                        self.into_deal_iter().chain(holding.into_deal_iter()).collect();
                    // Force the id of the new avg holding to be the id of this holding so that any scheduled extractions will
                    // still work, albeit on the new average holding. A side effect will be multiple extractions attempts for the same holding, but this isn't a problem since
                    // non-successful extractions are ignored.
                    avg.id = self_id;
                    Average(avg)
                }
            },
        }
    }

    /// Splits the holding on the specified amount.
    /// `split_amount` must be in the same pos/neg sign as the holding's balance to be successful.
    ///
    /// On success the holding will be split into the matched and remaining portions where
    /// the matched part will only contain a single deal (if the holding is or contains a sequence).
    /// This should be called repeatedly until either the `split_amount` has been reached, or an
    /// `Err` is returned.
    pub fn split_max(
        self,
        split_amount: Quantity,
        method: MatchMethod,
    ) -> Result<(DealHolding<'h>, Option<DealHolding<'h>>), Box<DealHolding<'h>>> {
        match self {
            Single(sdh) => sdh
                .split_max(split_amount)
                .map(|(l, r)| (Single(l), r.map(Single)))
                .map_err(|e| Box::new(Single(e))),
            Sequence(seq_holding) => {
                seq_holding.split_max(split_amount, method).map_err(|e| Box::new(Sequence(e)))
            }
            Average(avg_holding) => avg_holding
                .split_max(split_amount)
                .map(|(l, r)| (Average(l), r.map(Average)))
                .map_err(|e| Box::new(Average(e))),
        }
    }

    /// Removes the deal sub-holding with the given `holding_id` from the holding.
    ///
    /// Returns the portion of the holding extracted, along with the remainder of the holding, if any.
    pub fn extract(self, holding_id: usize) -> Result<(DealHolding<'h>, Option<Self>), Self> {
        match self {
            Single(sdh) => {
                if sdh.id() == holding_id {
                    Ok((Single(sdh), None))
                } else {
                    Err(Single(sdh))
                }
            }
            Sequence(seq) => seq.extract(holding_id).map_err(Sequence),
            Average(avg) => {
                if avg.id() == holding_id {
                    Ok((Average(avg), None))
                } else {
                    Err(Average(avg))
                }
            }
        }
    }

    pub fn add_adjustment(&mut self, adj: Adjustment<'h>) -> JournResult<()> {
        match self {
            Single(single) => {
                single.add_adjustment(adj)?;
                Ok(())
            }
            Sequence(seq) => seq.add_adjustment(adj),
            Average(avg) => avg.add_adjustment(adj),
        }
    }

    pub fn description(&self) -> LinkedHashSet<&'h str> {
        match self {
            Single(single) => Some(single.deal().entry().description()).into_iter().collect(),
            Sequence(seq) => seq.description(),
            Average(_) => LinkedHashSet::new(),
        }
    }

    /// The journal entries behind the deals in the holding. This will be empty for average holdings.
    pub fn entries(&self) -> Box<dyn Iterator<Item = &'h JournalEntry<'h>> + '_> {
        match self {
            Single(single) => Box::new(iter::once(single.deal().entry())),
            Sequence(seq) => seq.entries(),
            Average(avg) => Box::new(avg.deal_iter().map(|d| d.entry())),
        }
    }

    /// Looks up metadata values on the associated entries.
    pub fn entry_metadata_by_key(&self, key: &str) -> LinkedHashSet<&Metadata<'h>> {
        match self {
            Single(single) => single.deal().entry().metadata_by_key(key).into_iter().collect(),
            Sequence(seq) => {
                let mut tag_vals = LinkedHashSet::new();
                for dh in seq.iter() {
                    for val in dh.entry_metadata_by_key(key) {
                        tag_vals.insert(val);
                    }
                }
                tag_vals
            }
            Average(_avg) => LinkedHashSet::new(),
        }
    }

    /// Gets whether this holding matches the specified condition at the specified clock time.
    pub fn matches(&self, cond: &Condition, clock: JDateTime) -> bool {
        let datetime = self.datetime();

        match cond {
            Condition::True => true,
            Condition::False => false,
            Condition::Age(age, age_unit) => match age_unit {
                AgeUnit::Days => {
                    clock.naive_utc() - datetime.start().datetime().naive_utc()
                        >= Duration::days(*age as i64)
                }
                AgeUnit::CalDays => {
                    clock.utc_date() - datetime.start().utc_date() >= Duration::days(*age as i64)
                }
            },
            Condition::Kind(kind) => match kind {
                DealKind::Buy => self.amount() > 0,
                DealKind::Sell => self.amount() < 0,
            },
            Condition::And(left, right) => self.matches(left, clock) && self.matches(right, clock),
        }
    }
}

impl Debug for DealHolding<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Single(single) => write!(f, "{:?}", single),
            Sequence(seq) => write!(f, "{:?}", seq),
            Average(avg) => write!(f, "{:?}", avg),
        }
    }
}

/*
impl<'h> Add for DealHolding<'h> {
    type Output = Result<DealHolding<'h>, (DealHolding<'h>, DealHolding<'h>)>;

    fn add(self, rhs: DealHolding<'h>) -> Self::Output {
        // Both need to be sign compatible to add. This is usually used to indicate
        // we're adding a long position to a short position or vice versa.
        // Avoiding this means we're not 'destroying' how much was acquired or disposed of in total.
        if self.adjusted_value().amount().is_negative()
            != rhs.adjusted_value().amount().is_negative()
        {
            return Err((self, rhs));
        }

        let combined = Sequence(SequenceDealHolding::from_iterator([self.clone(), rhs]));
        Ok(combined)
    }
}*/

/*
impl From<&DealHolding<'_>> for Yaml {
    fn from(value: &DealHolding<'_>) -> Self {
        let mut map = yaml_rust2::yaml::Hash::new();
        let expenses = value.expenses();
        let total = value.total().valued_amount().clone();
        let total_before_expenses = value.total_before_expenses();

        map.insert(Yaml::String("total".to_string()), (&total).into());
        map.insert(Yaml::String("expenses".to_string()), (&expenses).into());
        map.insert(
            Yaml::String("total_before_expenses".to_string()),
            (&total_before_expenses).into(),
        );
        map.insert(Yaml::String("datetime".to_string()), value.datetime().into());
        map.insert(
            Yaml::String("remainder".to_string()),
            Yaml::Boolean(match value.root_parent() {
                Some(parent) => parent.total().amount() != value.total().amount(),
                None => false,
            }),
        );
        Yaml::Hash(map)
    }
}*/

/*
/// A deal holding that displays absolute values
pub struct AbsDealHolding<'h, 'd>(pub &'d DealHolding<'h>);

impl From<AbsDealHolding<'_, '_>> for Yaml {
    fn from(value: AbsDealHolding<'_, '_>) -> Self {
        let mut map = yaml_rust2::yaml::Hash::new();
        let expenses = value.0.expenses();
        let total = value.0.adjusted_value().clone();
        let unadjusted_total = value.0.consideration();

        map.insert(Yaml::String("adjusted_total".to_string()), (&total.abs()).into());
        map.insert(Yaml::String("expenses".to_string()), (&expenses.abs()).into());
        map.insert(Yaml::String("unadjusted_total".to_string()), (&unadjusted_total.abs()).into());
        Yaml::Hash(map)
    }
}*/

/*
impl<'h> From<Deal<'h>> for DealHolding<'h> {
    fn from(deal: Deal<'h>) -> Self {
        Single(deal)
    }
}*/

/*
impl<'h> From<DealGroup<'h>> for DealHolding<'h> {
    fn from(group: DealGroup<'h>) -> Self {
        Group(group)
    }
}*/
