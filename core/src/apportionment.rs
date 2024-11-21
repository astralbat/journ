/*
 * Copyright (c) 2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::Amount;
use crate::valued_amount::ValuedAmount;
use rust_decimal::Decimal;

/// Portions are used for apportionment operations which seek to divide some total amount
/// into various weighted parts. The naive approach to this problem is to simply use
/// the divide operator, but this can lead to rounding errors. E.g. 10 into 3 parts.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Portion<'h> {
    total: ValuedAmount<'h>,
    /// The part size may be > `total`. This seems counterintuitive, but may occur
    /// when a portion is split with negative amounts. E.g. a portion of 10 divided into [15, -5].
    part: ValuedAmount<'h>,
}

impl<'h> Portion<'h> {
    pub fn new(total: ValuedAmount<'h>) -> Self {
        Self { total: total.clone(), part: total }
    }

    pub fn total(&self) -> &ValuedAmount<'h> {
        &self.total
    }

    pub fn amount(&self) -> Amount<'h> {
        self.part.amount()
    }

    pub fn valued_amount(&self) -> &ValuedAmount<'h> {
        &self.part
    }

    pub fn is_zero(&self) -> bool {
        self.part.amount().is_zero()
    }

    /// Splits this portion at `size` threshold and returns the new portion and any remainder.
    /// The size of this portion is reduced by `size`.
    ///
    /// # Panics
    /// If `size` is greater than the size of this portion or `size` and `total` have
    /// different signs.
    pub fn split_off(mut self, size: Decimal) -> (Self, Self) {
        let (l, r) = self.part.split(size, false);
        self.part = r;
        (
            Self { total: self.total.clone(), part: l },
            Self { total: self.total.clone(), part: self.part },
        )

        /*
        let mut new_part = ValuedAmount::new_in(
            self.part.amount().unit().with_quantity(size).into(),
            self.part.allocator().unwrap(),
        );
        let mut new_remainder = ValuedAmount::new_in(
            self.part.amount().unit().with_quantity(self.part.amount().quantity() - size).into(),
            self.part.allocator().unwrap(),
        );
        for valuation in self.part.valuations() {
            let (part1, part2) = match valuation {
                Valuation::Unit(unit_val) => {
                    (Valuation::Unit(unit_val.clone()), Valuation::Unit(unit_val.clone()))
                }
                Valuation::Total(total_val, elided) => {
                    let total_val_part1 = (**total_val) * size / self.part.amount().quantity();
                    let total_val_part2 = (**total_val) - total_val_part1;
                    (
                        Valuation::Total(total_val_part1.into(), *elided),
                        Valuation::Total(total_val_part2.into(), *elided),
                    )
                }
            };
            new_part.set_valuation(part1).unwrap();
            new_remainder.set_valuation(part2).unwrap();
        }
        (
            Self { total: self.total.clone(), part: new_part },
            if new_remainder.amount().is_zero() {
                None
            } else {
                Some(Self { total: self.total.clone(), part: new_remainder })
            },
        )*/
    }

    /// Splits the portion at `percent`%.
    ///
    /// A normal percentage will be between 0 and 1 to take a fraction
    /// of the portion's part.
    /// If the percentage is greater than 1, a portion greater than the total
    /// will be taken and the remaining part will be negative.
    /// If the percentage is negative, the returned portion will also be negative.
    pub fn split_part_percentage(self, percent: Decimal) -> (Self, Self) {
        let size = (self.part.amount() * percent).rounded();
        self.split_off(size.quantity())
    }
}

/*
impl<'h> Mul<Amount<'h>> for Portion<'h> {
    type Output = Amount<'h>;

    fn mul(self, rhs: Amount<'h>) -> Self::Output {
        let mut rhs_portion = Portion::new(rhs);
        rhs_portion.last = self.last;
        let (l, r) = rhs_portion.split_percentage(self.size / self.total.quantity());

        rhs.quantity() * self.size / self.total.quantity();
        let quantity = self.size * rhs.quantity() / self.total.quantity();
        Amount::new(rhs.commodity(), quantity)
    }
}*/
