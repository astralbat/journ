/*
 * Copyright (c) 2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::cgt_configuration::{AssignExpenses, CagConfiguration};
use crate::module_init::MODULE_NAME;
use journ_core::journal_entry::JournalEntry;
use journ_core::metadata::{Metadata, MetadataKey};
use journ_core::unit::Unit;
use journ_core::valued_amount::ValuedAmount;
use journ_core::valuer::ValueResult;
use rust_decimal::Decimal;
use smallvec::SmallVec;
use std::borrow::Cow;

pub const SHARED_EXPENSE_TAG: MetadataKey = MetadataKey(Cow::Borrowed("CAG-DealExpense"));
pub const ACQUISITION_EXPENSE_TAG: MetadataKey =
    MetadataKey(Cow::Borrowed("CAG-AcquisitionExpense"));
pub const DISPOSAL_EXPENSE_TAG: MetadataKey = MetadataKey(Cow::Borrowed("CAG-DisposalExpense"));

/// A scanner and holder of entry expenses.
///
/// When expenses are scanned, they need to fulfill a requirement that they can be valued in either two distinct units,
/// or in a single `value_unit` which is usually the predicted unit of account.
/// This requirement is necessary, as when expenses are used to create deals, the deal's unit will be removed from
/// the expenses.
pub struct EntryExpenses<'h> {
    acquisition_expenses: ValuedAmount<'h>,
    disposal_expenses: ValuedAmount<'h>,
    shared_expenses: ValuedAmount<'h>,
    assign_expenses: AssignExpenses,
    value_unit: &'h Unit<'h>,
}

impl<'h> EntryExpenses<'h> {
    pub fn scan_and_divide<'e>(
        entry: &JournalEntry<'h>,
        value_unit: &'h Unit<'h>,
        equity_flows: impl Iterator<Item = &'e ValuedAmount<'h>> + Clone,
    ) -> ValueResult<'h, ExpensesDivision<'h>>
    where
        'h: 'e,
    {
        let expenses = Self::scan(entry, value_unit)?;
        expenses.divide_between(equity_flows)
    }

    /// Scans the entry for all expenses, making sure that they can be valued in either two distinct units, or the single `value_unit`.
    pub fn scan(
        entry: &JournalEntry<'h>,
        value_unit: &'h Unit<'h>,
    ) -> ValueResult<'h, EntryExpenses<'h>> {
        let mut disposal_expenses = ValuedAmount::nil();
        let mut acquisition_expenses = ValuedAmount::nil();
        let mut shared_expenses = ValuedAmount::nil();
        for pst in entry.postings() {
            let accum = match pst
                .account()
                .metadata_by_keys(&[
                    &DISPOSAL_EXPENSE_TAG,
                    &ACQUISITION_EXPENSE_TAG,
                    &SHARED_EXPENSE_TAG,
                ])
                .into_iter()
                .next()
                .map(Metadata::key)
            {
                Some(k) if k == &DISPOSAL_EXPENSE_TAG => &mut disposal_expenses,
                Some(k) if k == &ACQUISITION_EXPENSE_TAG => &mut acquisition_expenses,
                Some(k) if k == &SHARED_EXPENSE_TAG => &mut shared_expenses,
                Some(_) => unreachable!(),
                None => continue,
            };
            match &*accum + pst.valued_amount() {
                Some(val) if val.units().count() == 1 && val.value_in(value_unit).is_none() => {
                    ValueResult::ValuationNeeded(value_unit, val.amount())?;
                }
                Some(val) => *accum = val,
                None => {
                    ValueResult::ValuationNeeded(value_unit, pst.amount())?;
                }
            }
        }

        // All expenses groups need to be able to sum together.
        match [&acquisition_expenses, &disposal_expenses, &shared_expenses]
            .iter()
            .copied()
            .sum::<Option<ValuedAmount>>()
        {
            Some(sum) if sum.is_nil() => {}
            Some(sum) if sum.units().count() == 1 && sum.value_in(value_unit).is_none() => {
                ValueResult::ValuationNeeded(value_unit, sum.amount())?;
            }
            Some(_) => {}
            None => {
                for expense in [&acquisition_expenses, &disposal_expenses, &shared_expenses].iter()
                {
                    if expense.value_in(value_unit).is_none() {
                        ValueResult::ValuationNeeded(value_unit, expense.amount())?;
                    }
                }
            }
        }

        let assign_expenses = entry
            .config()
            .module_config::<CagConfiguration>(MODULE_NAME)
            .unwrap()
            .assign_expenses();

        let expenses = EntryExpenses {
            acquisition_expenses,
            disposal_expenses,
            shared_expenses,
            assign_expenses,
            value_unit,
        };
        ValueResult::Ok(expenses)
    }

    /// Creates an `ExpensesDivision` that allows the expenses to be divided between a particular list
    /// of `ValuedAmount's` - that should correspond to the equity flows of a particular entry.
    ///
    /// The division decides how to allocate the expenses based on configuration (the `assign_expenses` parameter)
    /// and which accounts they came from.
    pub fn divide_between<'a, I: Iterator<Item = &'a ValuedAmount<'h>> + Clone>(
        self,
        iter: I,
    ) -> ValueResult<'h, ExpensesDivision<'h>>
    where
        'h: 'a,
    {
        let has_disposal = iter.clone().any(|f| f.amount().is_negative());
        let has_acquisition = iter.clone().any(|f| f.amount().is_positive());
        let nil_amount = ValuedAmount::nil();

        let acquisition_filter =
            iter.clone().map(|f| if f.amount().is_positive() { f } else { &nil_amount });
        let disposal_filter =
            iter.clone().map(|f| if f.amount().is_negative() { f } else { &nil_amount });
        let shared_filter =
            iter.clone().map(|f| match self.assign_expenses {
                AssignExpenses::PreferAcquisition if has_acquisition => {
                    if f.amount().is_positive() { f } else { &nil_amount }
                }
                AssignExpenses::PreferDisposal if has_disposal => {
                    if f.amount().is_negative() {
                        f
                    } else {
                        &nil_amount
                    }
                }
                _ => f,
            });

        let (acquisition_weights, disposal_weights, shared_weights) = match (
            ValuedAmount::normalized_weights(acquisition_filter.clone()),
            ValuedAmount::normalized_weights(disposal_filter.clone()),
            ValuedAmount::normalized_weights(shared_filter.clone()),
        ) {
            (Some(w1), Some(w2), Some(w3)) => (w1, w2, w3),
            _ => {
                for val in iter.clone() {
                    if val.value_in(self.value_unit).is_none() {
                        ValueResult::ValuationNeeded(self.value_unit, val.amount())?;
                    }
                }
                unreachable!()
            }
        };

        ValueResult::Ok(ExpensesDivision {
            acquisition_expenses: self.acquisition_expenses,
            disposal_expenses: self.disposal_expenses,
            shared_expenses: self.shared_expenses,
            acquisition_weights,
            disposal_weights,
            shared_weights,
        })
    }
}

pub struct ExpensesDivision<'h> {
    acquisition_expenses: ValuedAmount<'h>,
    disposal_expenses: ValuedAmount<'h>,
    shared_expenses: ValuedAmount<'h>,
    acquisition_weights: SmallVec<[Decimal; 2]>,
    disposal_weights: SmallVec<[Decimal; 2]>,
    shared_weights: SmallVec<[Decimal; 2]>,
}

impl<'h> ExpensesDivision<'h> {
    pub fn get_expenses(&self, i: usize) -> ValuedAmount<'h> {
        [
            self.acquisition_expenses.split_weighted(&self.acquisition_weights, i),
            self.disposal_expenses.split_weighted(&self.disposal_weights, i),
            self.shared_expenses.split_weighted(&self.shared_weights, i),
        ]
        .iter()
        .sum::<Option<ValuedAmount<'h>>>()
        .unwrap()
    }
}
