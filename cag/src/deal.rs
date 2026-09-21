/*
 * Copyright (c) 2022-2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjusted_value::AdjustedValue;
use crate::adjustment::Adjustment;
use crate::cag_configuration::CagConfiguration;
use crate::holding::Split;
use crate::metadata::CAG_NOTE;
use crate::module_init::MODULE_NAME;
use chrono::DateTime;
use chrono_tz::Tz;
use journ_core::amount::{Amount, Quantity};
use journ_core::configuration::Configuration;
use journ_core::datetime::{DateTimePrecision, JDateTime, JDateTimeRange};
use journ_core::error::JournResult;
use journ_core::journal_context::JContext;
use journ_core::journal_entry::JournalEntry;
use journ_core::journal_entry_flow::{Flow, LinkedFlow};
use journ_core::metadata::Metadata;
use journ_core::tree_id::TreeId;
use journ_core::unit::Unit;
use journ_core::valued_amount::ValuedAmount;
use journ_core::valuer::{SystemValuer, ValuationError, ValueError, ValueResult, Valuer};
use linked_hash_set::LinkedHashSet;
use std::fmt;
use std::fmt::Formatter;
use std::rc::Rc;

pub const PROP_EXPENSES: &str = "Expenses";
pub const PROP_TAXABLE_GAIN: &str = "Taxable Gain";

pub type DealId = TreeId;

#[derive(Clone)]
pub enum DealOrigin<'h> {
    Entry(&'h JournalEntry<'h>),
    Flow(&'h JournalEntry<'h>, Flow<'h>),
}
impl<'h> DealOrigin<'h> {
    pub fn entry(&self) -> &'h JournalEntry<'h> {
        match self {
            DealOrigin::Entry(entry) => entry,
            DealOrigin::Flow(entry, _) => entry,
        }
    }
}

pub struct Deal<'h> {
    /// The entry from which the deal or belongs to.
    entry: &'h JournalEntry<'h>,
    /// The original flow from which the deal was originally derived.
    /// This will be `None` for explicitly created deals.
    linked_flow: Option<Rc<LinkedFlow<'h>>>,
    /// The amount transacted along with its total cost and expenses.
    adjusted_value: AdjustedValue<'h>,
    /// Sets the gain explicitly, rather than allowing the gain to be calculated. There are usually exceptional reasons
    /// within a tax code that may allow this.
    taxable_gain: Option<Amount<'h>>,
}

impl<'h> Deal<'h> {
    /// Creates a new deal.
    ///
    /// # Returns
    /// `Ok(Deal)` unless the `valued_amount` or `expenses` cannot be valued in the specified `uoa`, in which case a `ValueError::ValuationNeeded` is returned.
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        entry: &'h JournalEntry<'h>,
        valued_amount: ValuedAmount<'h>,
        expenses: ValuedAmount<'h>,
        linked_flow: Option<LinkedFlow<'h>>,
        taxable_gain: Option<ValuedAmount<'h>>,
        uoa: &'h Unit<'h>,
    ) -> ValueResult<'h, Self> {
        assert!(!valued_amount.is_nil());

        let amount = valued_amount.amount();
        let valuation =
            valued_amount.value_in(uoa).ok_or_else(|| ValueError::ValuationNeeded(uoa, amount))?;
        let expenses_valuation = expenses
            .value_in(uoa)
            .ok_or_else(|| ValueError::ValuationNeeded(uoa, expenses.amount()))?;

        let adjusted_value = AdjustedValue::new(
            amount,
            (valuation + expenses_valuation).abs(),
            expenses_valuation.abs(),
        );

        let taxable_gain = taxable_gain
            .map(|tg| tg.value_in(uoa).ok_or_else(|| ValueError::ValuationNeeded(uoa, tg.amount())))
            .transpose()?;

        Ok(Self { entry, linked_flow: linked_flow.map(Rc::new), taxable_gain, adjusted_value })
    }

    pub fn zero(unit: &'h Unit<'h>, entry: &'h JournalEntry<'h>, uoa: &'h Unit<'h>) -> Self {
        let allocator = JContext::get().allocator();
        Self::new(
            entry,
            ValuedAmount::new_in(unit.with_quantity(0), allocator),
            ValuedAmount::nil(),
            None,
            None,
            uoa,
        )
        .unwrap()
    }

    pub fn datetime(&self) -> JDateTimeRange {
        self.entry().datetime_range()
    }

    pub fn unit(&self) -> &'h Unit<'h> {
        self.adjusted_value.amount().unit()
    }

    pub fn unit_of_account(&self) -> &'h Unit<'h> {
        self.adjusted_value.unit_of_account()
    }

    pub fn is_acquisition(&self) -> bool {
        self.adjusted_value.amount() > 0
    }

    pub fn is_disposal(&self) -> bool {
        self.adjusted_value.amount() < 0
    }

    /// Ensure that all the deal's components can be valued in the specified `unit`.
    /// If not, attempt to perform a valuation on the entry which will first try to derive the valuation,
    /// and fallback to using the price lookup functionality.
    pub fn ensure_valued(&mut self, unit_of_account: &'h Unit<'h>) -> Result<(), ValuationError> {
        let mut system_valuer = SystemValuer::from(self.entry());
        let round_deals = self
            .entry()
            .config()
            .module_config::<CagConfiguration>(MODULE_NAME)
            .unwrap()
            .round_deal_values();
        self.value_with(&mut system_valuer, unit_of_account, round_deals)
    }

    pub fn set_value_on_date(
        &mut self,
        uoa: &'h Unit<'h>,
        config: &Configuration<'h>,
        date: DateTime<Tz>,
    ) -> Result<(), ValuationError> {
        let mut valuer =
            SystemValuer::on_date(config.clone(), JDateTime::new(date, DateTimePrecision::Second));
        let round_vals =
            config.module_config::<CagConfiguration>(MODULE_NAME).unwrap().round_deal_values();
        self.value_with(&mut valuer, uoa, round_vals)
    }

    pub fn value_with<V: Valuer<'h>>(
        &mut self,
        valuer: &mut V,
        quote_unit: &'h Unit<'h>,
        round_vals: bool,
    ) -> Result<(), ValuationError> {
        let mut new_val = valuer.value(quote_unit, self.adjusted_value.value())?.value();
        let mut new_expenses = valuer.value(quote_unit, self.adjusted_value.expenses())?.value();
        if round_vals {
            new_val = new_val.rounded();
            new_expenses = new_expenses.rounded();
        }

        self.adjusted_value =
            AdjustedValue::new(self.adjusted_value.amount(), new_val, new_expenses);
        self.taxable_gain = self
            .taxable_gain
            .map(|tg| valuer.value(quote_unit, tg).map(|v| v.value()))
            .transpose()?;

        Ok(())
    }

    pub fn adjusted_value(&self) -> AdjustedValue<'h> {
        self.adjusted_value
    }

    pub fn amount(&self) -> Amount<'h> {
        self.adjusted_value.amount()
    }

    /// The total after adjustments and expenses.
    pub fn value(&self) -> Amount<'h> {
        self.adjusted_value.value()
    }

    pub fn consideration(&self) -> Amount<'h> {
        self.adjusted_value.consideration()
    }

    pub fn expenses(&self) -> Amount<'h> {
        self.adjusted_value.expenses()
    }

    pub fn add_adjustment(&mut self, adjustment: Adjustment<'h>) -> JournResult<()> {
        adjustment.apply(&mut self.adjusted_value)
    }

    pub fn entry(&self) -> &'h JournalEntry<'h> {
        self.entry
    }

    pub fn taxable_gain(&self) -> Option<Amount<'h>> {
        self.taxable_gain
    }

    /// Split on the amount. Other components are split proportionally and rounded.
    pub fn split(self, amount: Quantity) -> (Deal<'h>, Option<Deal<'h>>) {
        let percent = amount / self.adjusted_value.amount().quantity();
        let (tg_left, tg_right) = match self.taxable_gain {
            Some(tg) => {
                let (left, right) = tg.split_percent(percent, Some(tg.max_scale()));
                (Some(left), Some(right))
            }
            None => (None, None),
        };

        let (left, right) = self.adjusted_value.split(amount);

        self.split_with((left, right), (tg_left, tg_right))
    }

    /// Splits all the deal's components using subtraction only.
    pub fn split_all(
        self,
        adjusted_value: AdjustedValue<'h>,
        taxable_gain: Option<Amount<'h>>,
    ) -> (Deal<'h>, Option<Deal<'h>>) {
        assert_eq!(
            adjusted_value.amount().unit(),
            self.adjusted_value.amount().unit(),
            "Cannot split a deal with an adjusted value in a different unit than the original deal's adjusted value"
        );
        assert_eq!(
            adjusted_value.amount().is_positive(),
            self.adjusted_value.amount().is_positive(),
            "Cannot split a deal with an adjusted value in a different direction than the original deal's adjusted value"
        );
        assert!(
            adjusted_value.amount().abs() <= self.adjusted_value.amount().abs(),
            "Cannot split a deal with an adjusted value greater than the original deal's adjusted value"
        );

        let (tg_left, tg_right) = match self.taxable_gain {
            Some(self_tg) => match taxable_gain {
                Some(tg) => {
                    assert!(
                        tg <= self_tg,
                        "Cannot split a deal with a taxable gain greater than the original deal's taxable gain"
                    );
                    let (left, right) = self_tg.split(tg.quantity());
                    (Some(left), Some(right))
                }
                None => (None, Some(self_tg)),
            },
            None => (None, taxable_gain),
        };

        let left = adjusted_value;
        let right = self.adjusted_value - adjusted_value;

        self.split_with(
            (left, if !right.amount().is_zero() { Some(right) } else { None }),
            (tg_left, tg_right),
        )
    }

    pub fn split_with_split(self, split: &mut Split<'h>) -> (Deal<'h>, Option<Deal<'h>>) {
        let split_res = split.split_off(&[
            self.adjusted_value.amount(),
            self.adjusted_value.value(),
            self.adjusted_value.expenses(),
            self.taxable_gain.unwrap_or(Amount::nil()),
        ]);

        let left = AdjustedValue::new(split_res.of()[0], split_res.of()[1], split_res.of()[2]);

        let right = if split_res.from()[0].is_zero() {
            None
        } else {
            Some(AdjustedValue::new(split_res.from()[0], split_res.from()[1], split_res.from()[2]))
        };
        let tg_left = if split_res.of()[3].is_zero() { None } else { Some(split_res.of()[3]) };
        let tg_right = if split_res.from()[3].is_zero() { None } else { Some(split_res.from()[3]) };
        self.split_with((left, right), (tg_left, tg_right))
    }

    /*
    /// Splits on the percentage of the amount. Other components are split proportionally.
    ///
    /// All values are rounded.
    pub fn split_percent(self, percent: Decimal) -> (Deal<'h>, Option<Deal<'h>>) {
        let (tg_left, tg_right) = match self.taxable_gain {
            Some(tg) => {
                let (left, right) = tg.split_percent(percent, Some(tg.max_scale()));
                (Some(left), Some(right))
            }
            None => (None, None),
        };

        let (left, right) =
            self.adjusted_value.split(self.adjusted_value.amount().quantity() * percent);

        self.split_with((left, right), (tg_left, tg_right))
    }*/

    fn split_with(
        self,
        (left, right): (AdjustedValue<'h>, Option<AdjustedValue<'h>>),
        (tg_left, tg_right): (Option<Amount<'h>>, Option<Amount<'h>>),
    ) -> (Deal<'h>, Option<Deal<'h>>) {
        // Create an Rc of self so that we can set the split_parent of the new deals to it.
        //let self_rc = Rc::new(self);

        //let mut orig_deal = &self_rc;

        let left_deal = Deal {
            entry: self.entry,
            linked_flow: self.linked_flow.clone(),
            taxable_gain: tg_left,
            adjusted_value: left,
        };
        let right_deal = right.map(|r| Deal {
            entry: self.entry,
            linked_flow: self.linked_flow.clone(),
            taxable_gain: tg_right,
            adjusted_value: r,
        });
        (left_deal, right_deal)
    }

    /// Gets all notes associated with the deal which includes:
    /// * Notes on the deal's entry
    /// * Notes on the flow's accounts
    pub fn notes(&self) -> LinkedHashSet<&str> {
        let mut notes = LinkedHashSet::new();
        notes.extend(self.entry.metadata_by_key(CAG_NOTE).into_iter().filter_map(Metadata::value));
        if let Some(linked_flow) = &self.linked_flow {
            notes.extend(
                linked_flow
                    .account_root()
                    .unwrap()
                    .metadata_by_key(CAG_NOTE)
                    .into_iter()
                    .filter_map(Metadata::value),
            );
            notes.extend(
                linked_flow
                    .linked()
                    .account_root()
                    .unwrap()
                    .metadata_by_key(CAG_NOTE)
                    .into_iter()
                    .filter_map(Metadata::value),
            );
        }
        notes
    }
}

impl fmt::Display for Deal<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.adjusted_value)?;
        if let Some(tg) = &self.taxable_gain {
            write!(f, " == {}", tg)?
        }
        Ok(())
    }
}

impl fmt::Debug for Deal<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

/*
impl PartialEq for Deal<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Deal<'_> {}*/

/*
impl PartialOrd for Deal<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Deal<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}*/

/*
/// Tries to add a deal to another deal. If the deals cannot be added, `None` is returned.
impl<'h> Add<&Deal<'h>> for &Deal<'h> {
    type Output = Deal<'h>;
    fn add(self, rhs: &Deal<'h>) -> Self::Output {
        assert_eq!(self.unit(), rhs.unit(), "Cannot add deals with different units: {} and {}", self.unit(), rhs.unit());
        assert_eq!(self.value().unit(), rhs.value().unit(), "Cannot add deals with different value units: {} and {}", self.value().unit(), rhs.value().unit());

        let tg = match (self.taxable_gain, rhs.taxable_gain) {
            (Some(tg1), Some(tg2)) => {
                Some(tg1 + tg2)
            }
            (Some(tg), None) | (None, Some(tg)) => {
                Some(tg)
            }
            (None, None) => None
        };
        let adj_value = self.adjusted_value + rhs.adjusted_value;

        Deal {
            id: BranchCountingTreeId::from(self.id().next_id()),
            entry: self.entry,
            taxable_gain: tg,
            split_parent: None,
            adjusted_value: adj_value,
        }
    }
}*/

/*
impl Clone for Deal<'_> {
    fn clone(&self) -> Self {
        Self {
            id: self.id.clone(),
            entry: self.entry,
            taxable_gain: self.taxable_gain.clone(),
            split_parent: self.split_parent.clone(),
            adjusted_value: self.adjusted_value.clone(),
        }
    }
}*/
