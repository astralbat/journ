/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use journ_core::amount::{Amount, Quantity};
use journ_core::journal_context::JContext;
use journ_core::unit::Unit;
use journ_core::valued_amount::ValuedAmount;
use rust_decimal::Decimal;
use std::fmt;
use std::iter::Sum;
use std::ops::{Add, AddAssign, Mul, Sub, SubAssign};
use yaml_rust2::Yaml;

/// Represents either a _total cost_ on an acquisition or holding; or _net proceeds_ on a disposal.
/// This may apply to `Deal`s or `DealHolding`'s alike.
///
/// This amount may also include further adjustments made.
///
/// # Examples
/// * 10 AAPL @@ \$1,000 ++ \$10 has a cost base of \$1,010 after incorporating the \$10 fee.
/// * -10 AAPL @@ \$1,000 ++ \$10 has a net proceeds of \$990 after incorporating the \$10 fee.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct AdjustedValue<'h> {
    amount: Amount<'h>,
    /// The value, incorporating all adjustments, including `expenses`. Must always be positive.
    value: Amount<'h>,
    /// The expenses component of `value`. Must be in the same unit as `value`. Must always be positive.
    expenses: Amount<'h>,
}

impl<'h> AdjustedValue<'h> {
    /// Creates a new `AdjustedValue` instance.
    ///
    /// # Arguments
    /// * `amount` - The amount of the asset. This can be positive (for acquisitions) or negative (for disposals).
    /// * `value` - The adjusted cost base, after expenses and adjustments. Must be positive.
    /// * `expenses` - The expenses component of the value. Must be positive and in the same unit as `value`.
    ///
    /// # Panics
    /// This function will panic if:
    /// * `value` is not positive.
    /// * `expenses` is not positive.
    /// * `amount` is nil.
    /// * `value` and `expenses` are not in the same unit.
    pub fn new(amount: Amount<'h>, value: Amount<'h>, expenses: Amount<'h>) -> Self {
        assert!(value.is_positive(), "value must be positive");
        assert!(expenses.is_positive(), "expenses must be positive");
        assert!(!amount.is_nil(), "amount must not be nil");
        assert_eq!(value.unit(), expenses.unit(), "value and expenses must be in the same unit");

        AdjustedValue { amount, value, expenses }
    }

    pub fn zero(unit: &'h Unit<'h>, uoa: &'h Unit<'h>) -> Self {
        AdjustedValue {
            amount: unit.with_quantity(0),
            value: uoa.with_quantity(0),
            expenses: uoa.with_quantity(0),
        }
    }

    pub fn is_zero(&self) -> bool {
        self.amount.is_zero()
    }

    /// The pool's amount. This will be positive to represent an acquisition, or negative to represent a disposal.
    pub fn amount(&self) -> Amount<'h> {
        self.amount
    }

    pub fn set_amount(&mut self, amount: Amount<'h>) {
        self.amount = amount;
    }

    pub fn unit_of_account(&self) -> &'h Unit<'h> {
        self.value.unit()
    }

    /// Gets the pool balance's cost in the unit of account.
    /// This will always be a positive amount.
    pub fn value(&self) -> Amount<'h> {
        self.value
    }

    pub fn set_value(&mut self, value: Amount<'h>) {
        assert!(value.is_positive(), "value must be positive");

        self.value = value;
    }

    /// The expenses component of the `value`.
    /// This will always be a positive amount.
    pub fn expenses(&self) -> Amount<'h> {
        self.expenses
    }

    pub fn set_expenses(&mut self, expenses: Amount<'h>) {
        assert!(expenses.is_positive(), "expenses must be positive");
        assert_eq!(
            self.value.unit(),
            expenses.unit(),
            "expenses must be in the same unit as value"
        );

        self.expenses = expenses;
    }

    /// Gets the value before expenses - as though the expenses were not incurred. This will always be a positive amount.
    pub fn consideration(&self) -> Amount<'h> {
        if self.amount().is_positive() {
            self.value - self.expenses
        } else {
            self.value + self.expenses
        }
    }

    pub fn abs(&self) -> Self {
        AdjustedValue { amount: self.amount.abs(), value: self.value, expenses: self.expenses }
    }

    pub fn as_valued_amount(&self) -> ValuedAmount<'h> {
        ValuedAmount::from_amounts([self.amount, self.value], JContext::get().allocator())
    }

    /// Splits the adjusted value into two parts, based on the specified split amount. All amounts are rounded according to the max scale
    /// permitted by their respective units.
    ///
    /// # Arguments
    /// * `split_amount` - The amount to split from the original adjusted value. Must not exceed the absolute value of the adjusted value's amount.
    ///
    /// # Returns
    /// A tuple containing two `AdjustedValue` instances: the left part and the right part of the split.
    /// The right part will be `None` if, after rounding, it has an amount of zero.
    ///
    /// # Panics
    /// If the `split_amount` exceeds the absolute value of the adjusted value's amount.
    pub fn split(&self, split_amount: Quantity) -> (AdjustedValue<'h>, Option<AdjustedValue<'h>>) {
        assert!(
            split_amount.abs() <= self.amount.abs().quantity(),
            "split amount must not exceed the absolute value of the adjusted value's amount"
        );

        let split_ratio = split_amount / self.amount.quantity();

        let (l_amount, r_amount) = self.amount.split(split_amount);
        //if rounded {
        //    l_amount = l_amount.rounded();
        //    r_amount = r_amount.rounded();
        //}
        let (l_value, r_value) =
            self.value.split_percent(split_ratio, Some(self.value().max_scale()));
        let (l_expenses, r_expenses) =
            self.expenses.split_percent(split_ratio, Some(self.expenses.max_scale()));

        (
            AdjustedValue::new(l_amount, l_value, l_expenses),
            if r_amount.is_zero() {
                None
            } else {
                Some(AdjustedValue::new(r_amount, r_value, r_expenses))
            },
        )
    }
}

impl fmt::Display for AdjustedValue<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} @@ {}", self.amount, self.value)?;
        if !self.expenses.is_zero() {
            // Since the amount includes expenses, we subtract or add the expenses to reach back to the consideration amount.
            if self.amount.is_positive() {
                write!(f, " -- {}", self.expenses)?;
            } else {
                write!(f, " ++ {}", self.expenses)?;
            }
        }
        Ok(())
    }
}

impl fmt::Debug for AdjustedValue<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.amount >= 0 {
            write!(f, "{:?} @@ {:?} -- {:?}", self.amount, self.value, self.expenses)
        } else {
            write!(f, "{:?} @@ {:?} ++ {:?}", self.amount, self.value, self.expenses)
        }
    }
}

impl From<&AdjustedValue<'_>> for Yaml {
    fn from(value: &AdjustedValue<'_>) -> Self {
        Yaml::from(&value.as_valued_amount())
    }
}

impl<'h> Add<AdjustedValue<'h>> for AdjustedValue<'h> {
    type Output = AdjustedValue<'h>;

    fn add(self, rhs: AdjustedValue<'h>) -> Self::Output {
        let amount = self.amount.add_precise(rhs.amount).unwrap();
        let value = self.value.add_precise(rhs.value).unwrap();
        let expenses = self.expenses.add_precise(rhs.expenses).unwrap();

        AdjustedValue::new(amount, value, expenses)
    }
}

impl AddAssign<Self> for AdjustedValue<'_> {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl<'h> Sum<AdjustedValue<'h>> for AdjustedValue<'h> {
    fn sum<I: Iterator<Item = AdjustedValue<'h>>>(mut iter: I) -> Self {
        let mut total = iter.next().unwrap();
        for pb in iter {
            total += pb;
        }
        total
    }
}

impl<'h> Sub<AdjustedValue<'h>> for AdjustedValue<'h> {
    type Output = AdjustedValue<'h>;

    /// Subtracts one `AdjustedValue` from another. The expenses and value may go negative to allow for rounding
    /// in average holding after split. But the amount must never change sign.
    fn sub(self, rhs: AdjustedValue<'h>) -> Self::Output {
        assert!(
            rhs.amount().abs() <= self.amount().abs(),
            "Cannot subtract an adjusted value with a greater absolute amount than the current adjusted value"
        );

        AdjustedValue {
            amount: self.amount - rhs.amount,
            value: self.value - rhs.value,
            expenses: self.expenses - rhs.expenses,
        }
    }
}

impl<'h> SubAssign<AdjustedValue<'h>> for AdjustedValue<'h> {
    fn sub_assign(&mut self, rhs: AdjustedValue<'h>) {
        *self = *self - rhs;
    }
}

impl<'h> Mul<Decimal> for AdjustedValue<'h> {
    type Output = AdjustedValue<'h>;

    fn mul(self, rhs: Decimal) -> Self::Output {
        AdjustedValue {
            amount: self.amount * rhs,
            value: self.value * rhs,
            expenses: self.expenses * rhs,
        }
    }
}
