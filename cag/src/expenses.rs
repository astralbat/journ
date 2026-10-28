/*
 * Copyright (c) 2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::cag_configuration::{AssignExpenses, CagConfiguration};
use crate::metadata::{ACQUISITION_EXPENSE_TAG, DISPOSAL_EXPENSE_TAG, SHARED_EXPENSE_TAG};
use crate::module_init::MODULE_NAME;
use journ_core::amount::Amount;
use journ_core::error::JournResult;
use journ_core::journal_entry::JournalEntry;
use journ_core::metadata::Metadata;
use journ_core::money_util::MoneyPot;
use journ_core::unit::Unit;
use journ_core::valued_amount::ValuedAmount;
use journ_core::valuer::Valuer;
use rust_decimal::Decimal;
use smallvec::SmallVec;

/// A scanner and holder of entry expenses.
///
/// When expenses are scanned, they need to fulfill a requirement that they can be valued in either two distinct units,
/// or in a single `value_unit` which is usually the predicted unit of account.
/// This requirement is necessary, as when expenses are used to create deals, the deal's unit will be removed from
/// the expenses.
pub struct EntryExpenses<'h> {
    acquisition_expenses: Amount<'h>,
    disposal_expenses: Amount<'h>,
    shared_expenses: Amount<'h>,
    assign_expenses: AssignExpenses,
}

impl<'h> EntryExpenses<'h> {
    pub fn scan_and_divide<'e>(
        valuer: &mut impl Valuer<'h>,
        entry: &JournalEntry<'h>,
        value_unit: &'h Unit<'h>,
        equity_flows: impl Iterator<Item = &'e ValuedAmount<'h>> + Clone,
    ) -> JournResult<ExpensesDivision<'h>>
    where
        'h: 'e,
    {
        let expenses = Self::scan(valuer, entry, value_unit)?;
        expenses.divide_between(equity_flows)
    }

    /// Scans the entry for all expenses, making sure that they can be valued in either two distinct units, or the single `value_unit`.
    pub fn scan(
        valuer: &mut impl Valuer<'h>,
        entry: &JournalEntry<'h>,
        value_unit: &'h Unit<'h>,
    ) -> JournResult<EntryExpenses<'h>> {
        let mut disposal_expenses = value_unit.with_quantity(0);
        let mut acquisition_expenses = value_unit.with_quantity(0);
        let mut shared_expenses = value_unit.with_quantity(0);
        for pst in entry.postings() {
            let mut accum = match pst
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
                Some(k) if k == DISPOSAL_EXPENSE_TAG => &mut disposal_expenses,
                Some(k) if k == ACQUISITION_EXPENSE_TAG => &mut acquisition_expenses,
                Some(k) if k == SHARED_EXPENSE_TAG => &mut shared_expenses,
                Some(_) => unreachable!(),
                None => continue,
            };
            accum += pst.valued_amount().value_in_or_value_with(value_unit, valuer)?.abs();
        }

        // All expenses groups need to be able to sum together.
        /*
        match [acquisition_expenses, disposal_expenses, shared_expenses]
            .into_iter()
            .sum::<Amount<'h>>()
        {
            Some(sum) if sum.is_nil() => {}
            Some(sum) if sum.units().count() == 1 && sum.value_in(value_unit).is_none() => {
                Err(ValueError::ValuationNeeded(value_unit, sum.amount()))?;
            }
            Some(_) => {}
            None => {
                for expense in [&acquisition_expenses, &disposal_expenses, &shared_expenses].iter()
                {
                    if expense.value_in(value_unit).is_none() {
                        Err(ValueError::ValuationNeeded(value_unit, expense.amount()))?;
                    }
                }
            }
        }*/

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
        };
        Ok(expenses)
    }

    /// Creates an `ExpensesDivision` that allows the expenses to be divided between a particular list
    /// of `ValuedAmount's` - that should correspond to the equity flows of a particular entry.
    ///
    /// The division decides how to allocate the expenses based on configuration (the `assign_expenses` parameter)
    /// and which accounts they came from.
    pub fn divide_between<'a, I: Iterator<Item = &'a ValuedAmount<'h>> + Clone>(
        self,
        iter: I,
    ) -> JournResult<ExpensesDivision<'h>>
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

        let (acquisition_weights, disposal_weights, shared_weights) = (
            ValuedAmount::normalized_weights(acquisition_filter.clone()).unwrap(),
            ValuedAmount::normalized_weights(disposal_filter.clone()).unwrap(),
            ValuedAmount::normalized_weights(shared_filter.clone()).unwrap(),
        );

        Ok(ExpensesDivision {
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
    acquisition_expenses: Amount<'h>,
    disposal_expenses: Amount<'h>,
    shared_expenses: Amount<'h>,
    acquisition_weights: SmallVec<[Decimal; 2]>,
    disposal_weights: SmallVec<[Decimal; 2]>,
    shared_weights: SmallVec<[Decimal; 2]>,
}

impl<'h> ExpensesDivision<'h> {
    pub fn get_expenses(&self, i: usize) -> Amount<'h> {
        let ae = if self.acquisition_expenses > 0 {
            MoneyPot::new(self.acquisition_expenses)
                .split_weighted(&self.acquisition_weights)
                .nth(i)
                .unwrap()
        } else {
            Amount::nil()
        };

        let de = if self.disposal_expenses > 0 {
            MoneyPot::new(self.disposal_expenses)
                .split_weighted(&self.disposal_weights)
                .nth(i)
                .unwrap()
        } else {
            Amount::nil()
        };

        let se = if self.shared_expenses > 0 {
            MoneyPot::new(self.shared_expenses).split_weighted(&self.shared_weights).nth(i).unwrap()
        } else {
            Amount::nil()
        };

        self.acquisition_expenses.unit().with_quantity((ae + de + se).quantity())
    }
}
