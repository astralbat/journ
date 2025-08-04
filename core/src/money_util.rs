/*
 * Copyright (c) 2020-2023. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::Amount;
use crate::unit::{RoundingStrategy, Unit};
use rust_decimal::Decimal;
use rust_decimal_macros::*;
use std::slice::Iter;

/// A conceptual pot of amount.
pub struct MoneyPot<'h> {
    value: Amount<'h>,
}

impl<'h> MoneyPot<'h> {
    #[allow(non_fmt_panics)]
    pub fn new(value: Amount<'h>) -> Self {
        assert!(value.quantity() >= dec!(0), "MoneyPot initialised with a value < 0: {value}");

        MoneyPot { value }
    }

    pub fn value(&self) -> Amount<'h> {
        self.value
    }

    pub fn is_empty(&self) -> bool {
        self.value.is_zero()
    }

    pub fn add(&mut self, more_money: &Amount<'h>) {
        assert!(more_money >= &0, "Only positive amount can be added");

        self.value += more_money;
    }

    /// Take a percentage of amount out of the pot, diminishing the pot. The amount is rounded.
    /// It is not possible to take more amount than is in the pot and it is not possible to take
    /// a negative amount.
    pub fn take_percent(&mut self, percent: Decimal) -> Amount<'h> {
        assert!(percent <= dec!(1) && percent >= dec!(0));

        let taken = (self.value * percent).rounded();
        self.value -= taken;
        taken
    }

    pub fn take_all(&mut self) -> Amount<'h> {
        let taken = self.value;
        self.value = self.value.with_quantity(0);
        taken
    }

    /// Try to take amount out, or the remainder of the pot whichever is less.
    /// Returns the amount taken.
    pub fn take_max(&mut self, amount: Amount<'h>) -> Amount<'h> {
        assert!(amount >= 0, "Amount must be positive");

        if self.value >= amount {
            self.value -= amount;
            amount
        } else {
            let taken = self.value;
            self.value = self.value.unit().with_quantity(0);
            amount.with_quantity(taken.quantity())
        }
    }

    /// Divide up the pot according to the relative weights.
    ///
    /// # Example
    /// ```
    /// use crate::journ_core::money_util::MoneyPot;
    /// use crate::journ_core::amount::Amount;
    /// use crate::journ_core::unit::Unit;
    ///
    /// let dollars = Box::leak(Box::new(Unit::new("$")));
    /// let pot = MoneyPot::new(dollars.with_quantity(50));
    /// let weights = vec![40, 40, 20];
    /// let mut split = pot.split_weighted(&weights);
    /// assert_eq!(split.next(), Some(dollars.with_quantity(20)));
    /// assert_eq!(split.next(), Some(dollars.with_quantity(20)));
    /// assert_eq!(split.next(), Some(dollars.with_quantity(10)));
    /// assert_eq!(split.next(), None);
    /// ```
    pub fn split_weighted<'w, D>(
        mut self,
        weights: &'w [D],
    ) -> impl Iterator<Item = Amount<'h>> + 'w
    where
        D: Into<Decimal> + Copy,
        'h: 'w,
    {
        assert!(!weights.is_empty(), "from len must be > 0");

        let total_in_pot = self.value;
        let weight_sum: Decimal = weights.iter().copied().map(Into::into).sum();
        let num_portions = weights.len();
        weights.iter().enumerate().map(move |(i, weight)| match i {
            n if n == num_portions - 1 => self.take_all(),
            _ => self.take_max((total_in_pot * ((*weight).into() / weight_sum)).rounded()),
        })
    }

    /// Splits the amount pot n number of ways, taking the ith way.
    pub fn splitn_ways(mut self, n: usize, i: usize) -> Amount<'h> {
        assert!(n > 0);
        assert!(i < n);

        let upper_amount_each = (self.value / Decimal::new(n as i64, 0))
            .rounded_with_strategy(RoundingStrategy::AlwaysUp);
        let mut taken = self.take_max(upper_amount_each);
        for _ in 1..=i {
            taken = self.take_max(upper_amount_each);
        }
        taken
    }

    /// Take all the amount out of the pot. This destroys the pot
    pub fn consume(self) -> Amount<'h> {
        self.value
    }
}

/// Often while working with [Amount]s, it is desirable to have a [HashMap] structure with Unit keys. More often than not, these maps
/// are very small and so it is more performant to use a [Vec] which is less ergonomic. Enter the `AmountMap` which
/// adds a map-like interface to a `Vec`.
#[derive(Default, Debug)]
pub struct UnitAmountMap<'h> {
    inner: Vec<Amount<'h>>,
}
impl<'h> UnitAmountMap<'h> {
    pub fn with_capacity(capacity: usize) -> Self {
        UnitAmountMap { inner: Vec::with_capacity(capacity) }
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn get(&self, unit: &Unit<'h>) -> Option<Amount<'h>> {
        self.inner.iter().find(|a| a.unit() == unit).copied()
    }

    pub fn contains(&self, unit: &Unit<'h>) -> bool {
        self.inner.iter().map(|m| m.unit()).any(|c| c == unit)
    }

    pub fn insert(&mut self, amount: Amount<'h>) {
        match self.inner.iter_mut().find(|a| a.unit() == amount.unit()) {
            Some(existing) => *existing = amount,
            None => self.inner.push(amount),
        }
    }

    pub fn iter(&self) -> Iter<'_, Amount<'h>> {
        self.inner.iter()
    }
}

impl<'h> Extend<Amount<'h>> for UnitAmountMap<'h> {
    fn extend<T: IntoIterator<Item = Amount<'h>>>(&mut self, iter: T) {
        for amnt in iter {
            self.insert(amnt)
        }
    }
}

impl<'h> IntoIterator for UnitAmountMap<'h> {
    type Item = Amount<'h>;
    type IntoIter = <Vec<Amount<'h>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}
