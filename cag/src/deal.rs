/*
 * Copyright (c) 2022-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjustment::Adjustment;
use crate::cgt_configuration::{CagCommand, CgtConfiguration};
use crate::deal_holding::DealHolding;
use crate::module_init::MODULE_NAME;
use crate::pool::PoolBalance;
use journ_core::alloc::HerdAllocator;
use journ_core::amount::AmountExpr;
use journ_core::arguments::Arguments;
use journ_core::date_and_time::JDateTimeRange;
use journ_core::error::JournResult;
use journ_core::journal_entry::{EntryId, JournalEntry};
use journ_core::reporting::table;
use journ_core::unit::Unit;
use journ_core::valued_amount::ValuedAmount;
use journ_core::valuer::SystemValuer;
use std::fmt;
use std::fmt::Formatter;
use std::ops::Add;
use std::sync::atomic::{AtomicU32, Ordering};
use yaml_rust::Yaml;
use yaml_rust::yaml::Hash;

pub const PROP_EXPENSES: &str = "Expenses";
pub const PROP_TAXABLE_GAIN: &str = "Taxable Gain";

/// The deal identifier uniquely identifies a deal within an entry.
/// Upon splitting, the two parts will both still have the same deal id. Therefore the deal_id is not always unique.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DealId<'h> {
    entry_id: EntryId<'h>,
    // The `extra` is the same as position when that is `Some`. When `None`, it is a unique sequence number.
    // This allows `DealId` to always be unique, and this field coming above `position` ensures
    // that existing ids with a position always come first.
    extra: u32,
    position: Option<u32>,
}

impl<'h> DealId<'h> {
    pub(crate) fn new(entry_id: EntryId<'h>, position: u32) -> Self {
        Self { entry_id, position: Some(position), extra: position }
    }
    fn allocate(entry_id: EntryId<'h>) -> Self {
        // Set to a sufficiently high starting value so that allocated deals are naturally sequenced
        // after parsed deals that will expected to be allocated manually.
        static DEAL_COUNTER: AtomicU32 = AtomicU32::new(2 ^ 16);
        let new_id = DEAL_COUNTER.fetch_add(1, Ordering::Relaxed);
        Self { entry_id, position: None, extra: new_id }
    }

    pub fn entry_id(&self) -> EntryId<'h> {
        self.entry_id
    }

    /// The position of the deal within the entry where the first deal is 0. This will be `None`
    /// when the deal was not read from an entry.
    pub fn position(&self) -> Option<u32> {
        self.position
    }
}

#[derive(Clone)]
pub struct Deal<'h> {
    /// The deal id. Sorting deals by their id will allow them to written to a `JournalEntry` in the
    /// correct order.
    id: DealId<'h>,
    /// The entry from which the deal or belongs to.
    entry: &'h JournalEntry<'h>,
    datetime: JDateTimeRange<'h>,
    /// Positive for acquisitions, negative for disposals. May be zero but never nil.
    /// The values represent the cost of the deal before allowable expenses for acquisitions; or it may reflect gross proceeds for disposals.
    /// A ValuedAmount type is expected to have at least one valuation in the unit of account set at the time, and valuations in other currencies
    /// may be useful if the unit of account changes and the containing `DealHolding` needs to be revalued.
    valued_amount: ValuedAmount<'h>,
    /// Set allowable expenses that may be added to the cost of a purchase or deducted from the gross proceeds.
    /// The expenses will be positive to indicate they should be added to to the `valued_amount`, or negative to indicate they
    /// should be deducted from the `valued_amount`.
    allowable_expenses: ValuedAmount<'h>,
    /// Any adjustments applied to the deal in the order they were applied.
    adjustments: Vec<Adjustment<'h>>,
    /// Sets the gain explicitly, rather than allowing the gain to be calculated. There are usually exceptional reasons
    /// within a tax code that may allow this.
    taxable_gain: Option<ValuedAmount<'h>>,
    /// Indicates whether the Deal was specified manually using a 'CAG-Deal' key. These deals
    /// will override the normal deal detection on the entry.
    required: bool,
    /// When split operations are performed on deals, this is set to the original deal before the split.
    /// This is useful to allow for reporting whether a deal has been split up, and the amount it was split from.
    split_parent: Option<Box<DealHolding<'h>, &'h HerdAllocator<'h>>>,
    /// The balance of the valued_amount plus allowable_expenses
    balance: PoolBalance<'h>,
}

impl<'h> Deal<'h> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        position: Option<u32>,
        entry: &'h JournalEntry<'h>,
        mut valued_amount: ValuedAmount<'h>,
        mut allowable_expenses: ValuedAmount<'h>,
        taxable_gain: Option<ValuedAmount<'h>>,
        required: bool,
    ) -> Self {
        assert!(!valued_amount.is_nil());
        assert!(
            allowable_expenses.is_nil()
                || allowable_expenses.units().all(|u| u != valued_amount.unit()),
            "Expenses must not have the same unit as the valued amount"
        );

        let id = position
            .map(|id| DealId::new(entry.id(), id))
            .unwrap_or_else(|| DealId::allocate(entry.id()));

        // Base the datetime of the deal on the argument's timezone rather than the entry's. This
        // allows us to compare deals effectively and prepare the deal for reporting.
        let args = Arguments::get();
        let datetime = entry
            .date_and_time()
            .datetime_range()
            .convert_datetime_range(&args.cast_cmd::<CagCommand>().unwrap().datetime_args);

        let round_deals = entry
            .config()
            .module_config::<CgtConfiguration>(MODULE_NAME)
            .unwrap()
            .round_deal_values();
        if round_deals {
            valued_amount.to_totalled_valuations();
            valued_amount.round_total_valuations();
            allowable_expenses.to_totalled_valuations();
            allowable_expenses.round();
        }

        let balance = Self::calc_balance(&valued_amount, &allowable_expenses, &[]);

        Self {
            id,
            datetime,
            //unit_of_account: None,
            entry,
            valued_amount,
            allowable_expenses,
            taxable_gain,
            required,
            //has_split: false,
            split_parent: None,
            balance,
            adjustments: vec![],
        }
    }

    pub fn zero(unit: &'h Unit<'h>, entry: &'h JournalEntry<'h>) -> Self {
        let allocator = entry.config().allocator();
        Self::new(
            None,
            entry,
            ValuedAmount::new_in(AmountExpr::from(unit.with_quantity(0)), allocator),
            ValuedAmount::nil(),
            None,
            false,
        )
    }

    pub fn id(&self) -> DealId<'h> {
        self.id
    }

    pub fn datetime(&self) -> JDateTimeRange<'h> {
        self.datetime
    }

    pub fn unit(&self) -> &'h Unit<'h> {
        self.valued_amount.amount().unit()
    }

    pub fn allocator(&self) -> &'h HerdAllocator<'h> {
        self.valued_amount.allocator().expect("Deal's valued amount cannot be nil")
    }

    pub fn is_acquisition(&self) -> bool {
        self.valued_amount.amount() > 0
    }

    pub fn is_disposal(&self) -> bool {
        self.valued_amount.amount() < 0
    }

    /// Gets whether this deal can match another based on one being an acquisition and the other, a disposal.
    pub fn is_match_compatible(&self, other: &Deal<'h>) -> bool {
        if self.unit() != other.unit() {
            return false;
        }
        if self.total().is_zero() || other.total().is_zero() {
            return true;
        }
        self.is_acquisition() != other.is_acquisition()
    }

    /// Ensure that all of the deal's components can be valued in the specified `unit`.
    /// If not, attempt to perform a valuation on the entry which will first try to derive the valuation,
    /// and fallback to using the price lookup functionality.
    pub fn value_with_system_valuer(&mut self, unit: &'h Unit<'h>) -> JournResult<()> {
        let mut system_valuer = SystemValuer::from(self.entry);

        self.valued_amount.value_in_or_value_with(unit, &mut system_valuer)?;
        self.allowable_expenses.value_in_or_value_with(unit, &mut system_valuer)?;
        self.taxable_gain
            .as_mut()
            .map(|tg| tg.value_in_or_value_with(unit, &mut system_valuer).map(Some))
            .unwrap_or(Ok(None))?;
        self.balance =
            Self::calc_balance(&self.valued_amount, &self.allowable_expenses, &self.adjustments);
        Ok(())
    }

    pub fn valued_amount(&self) -> &ValuedAmount<'h> {
        &self.valued_amount
    }

    /// The total after adjustments, without considering expenses.
    pub fn total_before_expenses(&self) -> ValuedAmount<'h> {
        let mut va = self.valued_amount.clone();
        for adj in &self.adjustments {
            adj.apply(&mut va).unwrap();
        }
        va
    }

    /// The total after adjustments and expenses.
    pub fn total(&self) -> &PoolBalance<'h> {
        &self.balance
    }

    /// Gets the `PoolBalance` as if a pool only contained this deal.
    ///
    /// # Example
    /// a deal of 5 BTC @@ $1000 ++ 0.1 BTC @@ $20 should have a balance of 5 BTC @@ $1020.
    fn calc_balance(
        valued_amount: &ValuedAmount<'h>,
        expenses: &ValuedAmount<'h>,
        adjustments: &[Adjustment<'h>],
    ) -> PoolBalance<'h> {
        let mut adj_va = valued_amount.clone();
        adj_va.add_from(expenses);
        for adj in adjustments {
            adj.apply(&mut adj_va).unwrap();
        }

        PoolBalance::new(adj_va)
    }

    pub fn entry(&self) -> &'h JournalEntry<'h> {
        self.entry
    }

    pub fn expenses(&self) -> &ValuedAmount<'h> {
        &self.allowable_expenses
    }

    pub fn taxable_gain(&self) -> Option<&ValuedAmount<'h>> {
        self.taxable_gain.as_ref()
    }

    pub fn is_required(&self) -> bool {
        self.required
    }

    /// Gets the parent deal that this deal was split from using [Deal::split_max()], if any.
    pub fn split_parent(&self) -> Option<&DealHolding<'h>> {
        self.split_parent.as_ref().map(|parent| &**parent)
    }

    /// Gets whether the deal has been split up from a bigger deal.
    pub fn is_remainder(&self) -> bool {
        match self.split_parent() {
            Some(parent) => {
                self.total_before_expenses().amount().quantity()
                    < parent.total_before_expenses().amount().quantity()
            }
            None => false,
        }
    }
}

impl fmt::Display for Deal<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.valued_amount,)?;
        if !self.allowable_expenses.amount().is_zero() {
            if self.allowable_expenses.amount().is_negative() {
                let mut neg_expenses = self.allowable_expenses.clone();
                neg_expenses.negate();
                write!(f, " -- {}", neg_expenses)?
            } else {
                write!(f, " ++ {}", self.allowable_expenses)?
            }
        }
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

impl<'a> From<&'a Deal<'_>> for table::Cell<'a> {
    fn from(value: &'a Deal<'_>) -> Self {
        let cell0 = (&value.valued_amount).into();
        let cell1 = if !value.allowable_expenses.amount().is_zero() {
            let cell11 = if value.allowable_expenses.amount().is_negative() {
                table::Cell::new(" -- ")
            } else {
                table::Cell::new(" ++ ")
            };
            let cell12 = value.allowable_expenses.amount().into();
            table::Cell::from([cell11, cell12])
        } else {
            table::Cell::default()
        };
        let cell2 = match &value.taxable_gain {
            Some(tg) => {
                let cell21 = table::Cell::new(" == ");
                let cell22 = tg.amount().into();
                table::Cell::from([cell21, cell22])
            }
            None => table::Cell::default(),
        };
        [cell0, cell1, cell2].into()
    }
}

impl From<&Deal<'_>> for Yaml {
    fn from(deal: &Deal<'_>) -> Self {
        let mut map = Hash::new();
        map.insert(Yaml::String("datetime".to_string()), deal.datetime.into());
        map.insert(Yaml::String("total".to_string()), deal.balance.valued_amount().into());
        map.insert(Yaml::String("amount".to_string()), (&deal.valued_amount).into());
        map.insert(Yaml::String("expenses".to_string()), (&deal.allowable_expenses).into());
        /*
        if let Some(parent) = deal.split_parent.as_ref() {
            map.insert(Yaml::String("split_parent".to_string()), (&**parent).into());
        }*/
        map.insert(Yaml::String("required".to_string()), Yaml::Boolean(deal.required));
        //map.insert(Yaml::String("remainder".to_string()), Yaml::Boolean(deal.is_remainder()));
        Yaml::Hash(map)
    }
}

impl PartialEq for Deal<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Deal<'_> {}

impl PartialOrd for Deal<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Deal<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

/// Tries to add a deal to another deal. If the deals cannot be added, `None` is returned.
impl<'h> Add<&Deal<'h>> for Deal<'h> {
    type Output = Option<Deal<'h>>;
    fn add(mut self, rhs: &Deal<'h>) -> Self::Output {
        if self.unit() != rhs.unit() {
            return None;
        }
        if self.taxable_gain.is_some() || rhs.taxable_gain.is_some() {
            return None;
        }

        self.adjustments.append(&mut rhs.adjustments.clone());
        self.valued_amount = (&self.valued_amount + &rhs.valued_amount).unwrap();
        self.allowable_expenses = match &self.allowable_expenses + &rhs.allowable_expenses {
            Some(expenses) => expenses,
            None => return None,
        };
        self.balance =
            Self::calc_balance(&self.valued_amount, &self.allowable_expenses, &self.adjustments);
        Some(self)
    }
}
