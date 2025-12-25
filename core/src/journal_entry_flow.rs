/*
 * Copyright (c) 2021-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::account::{Account, AccountType};
use crate::amount::Amount;
use crate::unit::Unit;
use crate::valued_amount::ValuedAmount;
use smallvec::SmallVec;
use std::fmt;
use std::fmt::Formatter;
use std::ops::{Add, AddAssign};
use std::sync::Arc;

pub type FlowVec<'h> = SmallVec<[Flow<'h>; 4]>;

pub trait Flows<'h>: IntoIterator<Item = Flow<'h>> {
    fn is_empty(&self) -> bool;

    fn len(&self) -> usize;

    fn as_slice(&self) -> &[Flow<'h>];

    fn as_mut_slice(&mut self) -> &mut [Flow<'h>];

    fn remove(&mut self, index: usize) -> Flow<'h>;

    fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&mut Flow<'h>) -> bool;

    fn truncate(&mut self, n: usize);

    /// Sums all flows
    ///
    /// # Panics
    /// If the flows are not homogenous
    fn sum(&self) -> Option<Flow<'h>> {
        let mut iter = self.as_slice().iter();
        let first = iter.next().cloned();
        iter.fold(first, |acc, b| acc.map(|a| &a + b))
    }

    /// Gets all flows which are debits
    ///
    /// # Panics
    /// If the flows are not homogenous
    fn debits(&self) -> SmallVec<[Flow<'h>; 4]> {
        self.as_slice().iter().filter(|f| f.is_debit()).cloned().collect()
    }

    /// Gets all flows which are credits
    ///
    /// # Panics
    /// If the flows are not homogenous
    fn credits(&self) -> SmallVec<[Flow<'h>; 4]> {
        self.as_slice().iter().filter(|f| f.is_credit()).cloned().collect()
    }

    /// Takes as much of the specified `amount` as possible from the flows, splitting the returned value
    /// in to those flows which have been taken, and those remaining.
    ///
    /// The `amount` should be positive to match debits and negative to match credits.
    fn take_amount(&self, amount: Amount<'h>) -> (FlowVec<'h>, FlowVec<'h>) {
        let mut taken = FlowVec::new();
        let mut remainder = FlowVec::new();
        let mut amount_remaining = amount;
        let is_debit = amount >= 0;

        for flow in self.as_slice().iter() {
            if flow.is_debit() == is_debit && amount_remaining != 0 {
                // Take the whole flow
                if (flow.is_debit() && flow.amount() <= amount_remaining)
                    || (flow.is_credit() && flow.amount() >= amount_remaining)
                {
                    amount_remaining -= flow.amount();
                    taken.push(flow.clone());
                // Take part of the flow
                } else {
                    let (f, rem) = flow.split(amount_remaining);
                    taken.push(f);
                    remainder.push(rem);
                }
            // Don't take the flow
            } else {
                remainder.push(flow.clone());
            }
        }
        (taken, remainder)
    }

    fn assets<'a>(&'a self) -> impl Iterator<Item = &'a Flow<'h>>
    where
        'h: 'a,
    {
        self.as_slice().iter().filter(|f| f.account_type() == Some(AccountType::Asset))
    }

    fn equity<'a>(&'a self) -> impl Iterator<Item = &'a Flow<'h>>
    where
        'h: 'a,
    {
        self.as_slice().iter().filter(|f| f.account_type() == Some(AccountType::Equity))
    }

    /// Gets whether the number of units is one.
    fn is_homogenous(&self) -> bool {
        let unit = self.as_slice().first().map(|f| f.unit());
        self.as_slice().iter().skip(1).all(|f| Some(f.unit()) == unit)
    }

    /// Gets all unique primary units in the flows.
    fn units<'a>(&'a self) -> impl Iterator<Item = &'h Unit<'h>> + 'a
    where
        'h: 'a,
    {
        let mut units: SmallVec<[&'h Unit<'h>; 4]> = SmallVec::new();
        for unit in self.as_slice().iter().map(|f| f.unit()) {
            if !units.contains(&unit) {
                units.push(unit);
            }
        }
        units.into_iter()
    }

    /// Gets Asset flows, searching for any equity flows in the same unit and adding the amounts.
    /// Otherwise, returning the original asset flow.
    /// Should the added amounts equal 0, they cancel each other out and are omitted. E.g. An asset
    /// transfer from equity (opening balance entry).
    fn assets_plus_equity<'a>(&'a self) -> impl Iterator<Item = Option<ValuedAmount<'h>>> + 'a
    where
        'h: 'a,
    {
        self.assets()
            .map(move |af| {
                self.as_slice()
                    .iter()
                    .find(|ef| {
                        ef.account_type() == Some(AccountType::Equity) && ef.unit() == af.unit()
                    })
                    .map(|ef| &af.net_amount + &ef.net_amount)
                    .unwrap_or(Some(af.net_amount.clone()))
            })
            .filter(|va| match va {
                Some(va) => !va.is_zero(),
                None => false,
            })
    }

    fn income<'a>(&'a self) -> impl Iterator<Item = &'a Flow<'h>>
    where
        'h: 'a,
    {
        self.as_slice().iter().filter(|f| f.account_type() == Some(AccountType::Income))
    }

    fn income_in_unit(&self, unit: &Unit<'h>) -> Option<&Flow<'h>> {
        self.as_slice()
            .iter()
            .find(|f| f.unit() == unit && f.account_type() == Some(AccountType::Income))
    }

    /// Reduce the collection by summing together flows sharing the same account type
    fn reduce_down_by_account_type(&mut self)
    where
        Self: Sized,
    {
        reduce_down(self, |a, b| a.account_type() == b.account_type());
    }
}

/// Reduce the flows by summing them together where:
/// * The two flows share the same unit (as is required by `add`).
/// * The two flows passed to `reduce_fn` evaluate to `true`.
fn reduce_down<'h, F, Func>(flows: &mut F, reduce_fn: Func)
where
    Func: Fn(&Flow, &Flow) -> bool,
    F: Flows<'h>,
{
    let mut_slice = flows.as_mut_slice();
    let mut new_len = mut_slice.len();
    let mut i = 0;
    while i < new_len {
        let mut j = i + 1;
        while j < new_len {
            if mut_slice[i].unit() == mut_slice[j].unit() && reduce_fn(&mut_slice[i], &mut_slice[j])
            {
                mut_slice[i] = &mut_slice[i] + &mut_slice[j];
                new_len -= 1;
                mut_slice.swap(j, new_len);
            } else {
                j += 1;
            }
        }
        i += 1;
    }
    flows.truncate(new_len);
}

impl<'h> Flows<'h> for Vec<Flow<'h>> {
    fn is_empty(&self) -> bool {
        self.is_empty()
    }

    fn len(&self) -> usize {
        self.len()
    }

    fn as_slice(&self) -> &[Flow<'h>] {
        self.as_slice()
    }

    fn as_mut_slice(&mut self) -> &mut [Flow<'h>] {
        self.as_mut_slice()
    }

    fn remove(&mut self, index: usize) -> Flow<'h> {
        self.remove(index)
    }

    fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&mut Flow<'h>) -> bool,
    {
        self.retain_mut(f)
    }

    fn truncate(&mut self, n: usize) {
        self.truncate(n);
    }
}

impl<'h> Flows<'h> for SmallVec<[Flow<'h>; 4]> {
    fn is_empty(&self) -> bool {
        self.is_empty()
    }

    fn len(&self) -> usize {
        self.len()
    }

    fn as_slice(&self) -> &[Flow<'h>] {
        self.as_slice()
    }
    fn as_mut_slice(&mut self) -> &mut [Flow<'h>] {
        self.as_mut_slice()
    }

    fn remove(&mut self, index: usize) -> Flow<'h> {
        self.remove(index)
    }

    fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&mut Flow<'h>) -> bool,
    {
        self.retain(f)
    }

    fn truncate(&mut self, n: usize) {
        self.truncate(n);
    }
}

/// A flow is similar to a posting, and indeed, a posting is a kind of flow. However, flows can be
/// added together if they share the same account type.
#[derive(Debug, Clone)]
pub struct Flow<'h> {
    /// The posting account if derived from a posting, or the common parent for summed flows, if one exists.
    account_root: Option<Arc<Account<'h>>>,
    net_amount: ValuedAmount<'h>,
}

impl<'h> Flow<'h> {
    pub fn new(account: Option<Arc<Account<'h>>>, net_amount: ValuedAmount<'h>) -> Self {
        Self { account_root: account, net_amount }
    }

    pub fn account_root(&self) -> Option<&Account<'h>> {
        self.account_root.as_deref()
    }

    pub fn account_type(&self) -> Option<AccountType> {
        self.account_root.as_ref()?.account_type()
    }

    pub fn unit(&self) -> &'h Unit<'h> {
        self.net_amount.amount().unit()
    }

    pub fn valued_amount(&self) -> &ValuedAmount<'h> {
        &self.net_amount
    }

    pub fn into_valued_amount(self) -> ValuedAmount<'h> {
        self.net_amount
    }

    pub fn amount(&self) -> Amount<'h> {
        self.net_amount.amount()
    }

    pub fn negate(&mut self) {
        self.net_amount.negate();
    }

    /// Tries to get the amount in the specified unit. This is either going to be the amount
    /// itself, or one of its valuations.
    pub fn amount_in(&self, in_curr: &'h Unit<'h>) -> Option<Amount<'h>> {
        self.net_amount.value_in(in_curr)
    }

    pub fn is_debit(&self) -> bool {
        self.net_amount.amount() > 0
    }

    pub fn is_credit(&self) -> bool {
        self.net_amount.amount() < 0
    }

    /// Splits the flow in two at the given threshold `amount`.
    ///
    /// # Panics
    /// If the amount is considered out of bounds of the flow.
    pub fn split(&self, amount: Amount<'h>) -> (Self, Self) {
        assert!(
            (self.is_debit() && amount >= 0 && amount <= self.amount())
                || (self.is_credit() && amount <= 0 && amount >= self.amount()),
            "Split amount is out of bounds"
        );

        let (left_amount, right_amount) = self.net_amount.clone().split(amount.quantity());

        (
            Flow::new(self.account_root.clone(), left_amount),
            Flow::new(self.account_root.clone(), right_amount),
        )
    }
}

impl<'h> Add<&Flow<'h>> for &Flow<'h> {
    type Output = Flow<'h>;

    fn add(self, rhs: &Flow<'h>) -> Self::Output {
        if self.unit() != rhs.unit() {
            panic!("Cannot add flows with different units: {:?} + {:?}", self, rhs);
        }
        let combined =
            (&self.net_amount + &rhs.net_amount).expect("Both sides have the same primary unit");
        Flow {
            net_amount: combined,
            account_root: self
                .account_root
                .as_ref()
                .and_then(|ar| {
                    rhs.account_root.as_ref().and_then(|rhs_ar| Account::common_parent(ar, rhs_ar))
                })
                .cloned(),
        }
    }
}

impl<'h> AddAssign<&Flow<'h>> for Flow<'h> {
    fn add_assign(&mut self, rhs: &Flow<'h>) {
        *self = &*self + rhs;
    }
}

impl<'h> PartialEq for Flow<'h> {
    fn eq(&self, other: &Self) -> bool {
        self.net_amount == other.net_amount
    }
}

impl<'h> Eq for Flow<'h> {}

impl<'h> PartialOrd for Flow<'h> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'h> Ord for Flow<'h> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.net_amount.cmp(&other.net_amount)
    }
}

impl fmt::Display for Flow<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {}", self.account_root, self.net_amount)
    }
}
