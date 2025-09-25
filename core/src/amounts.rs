/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::Amount;
use crate::unit::Unit;
use smallvec::SmallVec;
use std::ops::AddAssign;

/// Trait for collections of Amounts
pub trait Amounts<'h> {
    /// Gets the sum of all amounts in the collection with the specified unit
    fn balance(&self, unit: &Unit<'h>) -> Amount<'h>;

    fn len(&self) -> usize;

    fn is_empty(&self) -> bool;

    fn as_slice(&self) -> &[Amount<'h>];

    fn add_amounts<A>(&mut self, amounts: A)
    where
        A: IntoIterator<Item = Amount<'h>>;

    fn sub_amounts<A>(&mut self, amounts: A)
    where
        A: IntoIterator<Item = Amount<'h>>;

    fn mul_amounts<A>(&mut self, amounts: A)
    where
        A: IntoIterator<Item = Amount<'h>>;
}

macro_rules! impl_amounts_for_vec_like {
    ($vec_like:ty) => {
        impl<'h> Amounts<'h> for $vec_like {
            fn balance(&self, unit: &Unit<'h>) -> Amount<'h> {
                self.iter().filter(|a| a.unit() == unit).fold(Amount::nil(), |acc, m| acc + *m)
            }

            fn len(&self) -> usize {
                self.len()
            }

            fn is_empty(&self) -> bool {
                self.is_empty()
            }

            fn as_slice(&self) -> &[Amount<'h>] {
                self.as_slice()
            }

            fn add_amounts<A>(&mut self, amounts: A)
            where
                A: IntoIterator<Item = Amount<'h>>,
            {
                for amount in amounts {
                    match self.iter_mut().find(|b| b.unit() == amount.unit()) {
                        Some(existing) => *existing += amount,
                        None => self.push(amount),
                    }
                }
            }

            fn sub_amounts<A>(&mut self, amounts: A)
            where
                A: IntoIterator<Item = Amount<'h>>,
            {
                for amount in amounts {
                    match self.iter_mut().find(|b| b.unit() == amount.unit()) {
                        Some(existing) => *existing -= amount,
                        None => self.push(-amount),
                    }
                }
            }

            fn mul_amounts<A>(&mut self, amounts: A)
            where
                A: IntoIterator<Item = Amount<'h>>,
            {
                for amount in amounts {
                    match self.iter_mut().find(|b| b.unit() == amount.unit()) {
                        Some(existing) => *existing *= amount.quantity(),
                        None => self.push(Amount::nil()),
                    }
                }
            }
        }
    };
}
impl_amounts_for_vec_like!(Vec<Amount<'h>>);
impl_amounts_for_vec_like!(SmallVec<[Amount<'h>; 8]>);
impl_amounts_for_vec_like!(SmallVec<[Amount<'h>; 4]>);
impl_amounts_for_vec_like!(SmallVec<[Amount<'h>; 2]>);

macro_rules! add_assign_vec_like {
    ($vec_like:ty) => {
        impl<'h> AddAssign<Amount<'h>> for $vec_like {
            fn add_assign(&mut self, rhs: Amount<'h>) {
                match self.iter_mut().find(|b| b.unit() == rhs.unit()) {
                    Some(amount) => *amount += rhs,
                    None => self.push(rhs),
                }
            }
        }
    };
}
add_assign_vec_like!(Vec<Amount<'h>>);
add_assign_vec_like!(SmallVec<[Amount<'h>; 8]>);
add_assign_vec_like!(SmallVec<[Amount<'h>; 4]>);
add_assign_vec_like!(SmallVec<[Amount<'h>; 2]>);
