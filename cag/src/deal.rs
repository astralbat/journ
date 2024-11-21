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
use std::borrow::Cow;
use std::fmt;
use std::fmt::Formatter;
use std::ops::Add;
use std::sync::atomic::{AtomicU32, Ordering};
use yaml_rust::yaml::Hash;
use yaml_rust::Yaml;

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
    /*/// The deal will always have a unit of account: explicit or implied. Initially, this will be implied bo be the first valuation on the `valued_amount`,
    /// or if none, the primary unit of the `valued_amount`. When added to a pool, it should be changed to the pool's accounting unit.
    //unit_of_account: Option<&'h Unit<'h>>,*/
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
    /* /// Gets whether this deal is the result of a split operation. If `false`, this indicates that the deal
    /// is the original scanned deal from an entry.
    //has_split: bool,*/
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

        /*
        // Make sure the expenses does not have the same unit, this causes issues when adding the expenses to the valued amount.
        if allowable_expenses.amount() > 0
            && allowable_expenses.value_in(valued_amount.unit()).is_some()
        {
            allowable_expenses = allowable_expenses.without_unit(valued_amount.unit());
        }*/

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

    /*
    pub fn unit_of_account_or_assume(&self) -> &'h Unit<'h> {
        self.unit_of_account.unwrap_or_else(|| {
            self.valued_amount
                .valuations()
                .next()
                .map(|v| v.unit())
                .unwrap_or_else(|| self.valued_amount.amount().unit())
        })
    }*/

    /*
    /// Gets the unadjusted base amount transacted by this deal.
    ///
    /// The base amount is the amount before adding the expenses. For positive amounts (acquisitions),
    /// this is the amount before additional costs. For negative amounts (disposals), this
    /// is the gross amount before deducting expenses.
    ///
    /// E.g. Bought 1 Bitcoin (1 BTC) or Sold 1 Bitcoin (-1 BTC).
    pub fn amount(&self) -> Amount<'h> {
        self.valued_amount.amount()
    }*/

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

    /*
    /// Sets the unit of account for this deal.
    ///
    /// Check to see whether this deal can be valued in the specified `uoa` and `value_date`, and if not, attempt
    /// to perform such a valuation.
    pub fn set_unit_of_account_with_value_date(
        &mut self,
        uoa: &'h Unit<'h>,
        config: &Configuration<'h>,
        value_date: DateTime<Tz>,
    ) -> JournResult<()> {
        let date_valuer = |mut config: Configuration<'h>, value_date: DateTime<Tz>| {
            move |base_amount: Amount<'h>| {
                let price = valuer::get_value(&mut config, base_amount.unit(), uoa, value_date)?;
                Ok((base_amount * price.price()).rounded())
            }
        };
        self.valued_amount.value_with(date_valuer(config.clone(), value_date))?;
        self.allowable_expenses.value_with(date_valuer(config.clone(), value_date))?;
        self.taxable_gain.as_mut().map(|tg| tg.value_with(date_valuer(config.clone(), value_date)));
        self.unit_of_account = Some(uoa);
        self.balance =
            Self::calc_balance(&self.valued_amount, &self.allowable_expenses, &self.adjustments);
        Ok(())
    }*/

    /*
    /// Sets the unit of account for this deal.
    ///
    /// Check to see whether this deal can be valued in the specified unit, and if not, attempt
    /// to perform a valuation.
    pub fn set_unit_of_account(&mut self, uoa: &'h Unit<'h>) -> JournResult<()> {
        self.value_with_entry_valuer(uoa)?;
        self.unit_of_account = Some(uoa);

        // Put in terms of the uoa. This makes more sense when printing.
        self.allowable_expenses.in_terms_of(uoa);
        Ok(())
    }*/

    /*
    /// Gets the adjusted consideration, the consideration that includes expenses. For acquiring deals,
    /// the expenses should usually be positive (depending on how the user has specified them), so that
    /// the total can be thought of as the total cost of accepting the deal.
    /// For disposing deals, the expenses should ideally be negative in order to discount the additional costs
    /// of the transaction which can be thought of as the net proceeds or the value returned to the user after
    /// the costs have been paid.
    pub fn adjusted_consideration(&self) -> Amount<'h> {
        let expenses = self.allowable_expenses.value_in(self.unit_of_account_or_assume()).unwrap();
        let consideration = self.valued_amount.value_in(self.unit_of_account_or_assume()).unwrap();

        consideration + expenses
    }*/

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
        // Get the valued amount without the deal's unit before adding the expenses. This is important
        // to avoid the possibility of altering the deal's amount.
        //let adj_consideration_opt =
        //    &self.valued_amount.clone().without_unit(self.unit()) + &self.allowable_expenses;

        let mut adj_va = valued_amount.clone();
        adj_va.add_from(expenses);
        for adj in adjustments {
            adj.apply(&mut adj_va).unwrap();
        }

        /*
        debug_assert!(
            adj_consideration_opt.is_some(),
            "No common unit between deal amount and expenses"
        );

        let mut adj_consideration = adj_consideration_opt.unwrap();
        // Add the primary amount back if necessary
        if adj_consideration.amount().unit() != self.unit() {
            adj_consideration
                .set_valuation(Valuation::new_total(self.amount().abs(), false))
                .unwrap();
            adj_consideration.in_terms_of(self.amount().unit());
            //if self.amount().is_negative() {
            //    adj_consideration.set_amount(adj_consideration.amount() * -1);
            //}
        }*/
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

    /*
    /// Splits this deal in to two parts at the amount threshold.
    /// To be successful, the sign of the `amount` should be the same as the deal.
    /// An error is returned if the operation does not result in any split.
    pub(crate) fn split_max(self, amount: Quantity) -> Result<(Self, Option<Self>), Self> {
        if !amount.is_sign_compatible(self.total().amount().quantity()) {
            return Err(self);
        }
        let actual_amount = amount.min_abs(self.balance.amount().quantity());
        if actual_amount.is_zero() {
            return Err(self);
        }

        let allocator = self.allocator();
        let split_percent = actual_amount / self.balance.amount().quantity();

        // Return early without a split parent.
        // *** Commented out. Needs to be consistent with Avg holding. Split parent needs to always be set
        // to allow for accurate MatchDetails::actual_cost() calculation.
        //if split_percent.is_one() {
        //return Ok((self, None));
        //}

        let split_parent = DealHolding::Single(self.clone());

        // Use the balance for the split and round that. This is more accurate than splitting the
        // valued amount and expenses components, rounding them and adding them back together.
        let (bal_split, bal_rem) = self.balance.into_valued_amount().split(actual_amount);

        let (left_va, right_va) = self.valued_amount.split(actual_amount);

        // Split expenses.
        let mut left_exp = (&bal_split - &left_va).unwrap();
        let mut right_exp = (&bal_rem - &right_va).unwrap();
        // If the main expense amount is 0, but there are non-zero values, then remove the main amount.
        for exp in [&mut left_exp, &mut right_exp] {
            let unit = if exp.amount().is_zero() {
                if let Some(amount) = exp.totalled_valuations().find(|v| !v.is_zero()) {
                    Some(amount.unit())
                } else {
                    None
                }
            } else {
                None
            };
            if let Some(unit) = unit {
                let primary_unit = exp.unit();
                exp.in_terms_of(unit);
                exp.remove_valuation(primary_unit);
            }
        }

        // Split taxable gain
        let (left_tg, right_tg) = self
            .taxable_gain
            .map(|tg| {
                let tg_amount = (tg.amount() * split_percent).rounded().quantity();
                tg.split(tg_amount)
            })
            .map(|(tg1, tg2)| (Some(tg1), Some(tg2)))
            .unwrap_or((None, None));

        // Split adjustments
        let (left_adjs, right_adjs): (Vec<_>, Vec<_>) =
            self.adjustments.into_iter().map(|adj| adj.split(split_percent)).unzip();

        let left = Self {
            balance: PoolBalance::new(bal_split),
            id: self.id,
            entry: self.entry,
            datetime: self.datetime,
            valued_amount: left_va,
            allowable_expenses: left_exp,
            taxable_gain: left_tg,
            required: self.required,
            split_parent: Some(Box::new_in(split_parent.clone(), allocator)),
            /*
            // There is no split parent if we have taken the whole deal.
            split_parent: if right_va.is_zero() {
                None
            } else {
                Some(Box::new_in(split_parent.clone(), allocator))
            },*/
            adjustments: left_adjs,
            ..self
        };
        let right = if !bal_rem.is_zero() {
            Some(Self {
                balance: PoolBalance::new(bal_rem),
                id: self.id,
                entry: self.entry,
                datetime: self.datetime,
                valued_amount: right_va,
                allowable_expenses: right_exp,
                taxable_gain: right_tg,
                required: self.required,
                split_parent: Some(Box::new_in(split_parent, allocator)),
                adjustments: right_adjs,
                ..self
            })
        } else {
            None
        };
        Ok((left, right))
    }*/

    /*
    pub fn add_adjustment(&mut self, adjustment: Adjustment<'h>) -> JournResult<()> {
        // Check that the adjustment can be applied to the deal.
        adjustment.apply(&mut self.balance.valued_amount().clone())?;

        self.adjustments.push(adjustment);
        self.balance =
            Self::calc_balance(&self.valued_amount, &self.allowable_expenses, &self.adjustments);
        Ok(())
    }*/

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
