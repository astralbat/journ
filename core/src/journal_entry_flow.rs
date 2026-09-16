/*
 * Copyright (c) 2021-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::account::{Account, AccountType};
use crate::amount::{Amount, Quantity};
use crate::err;
use crate::error::JournResult;
use crate::unit::Unit;
use crate::valued_amount::{PostingValuation, ValuedAmount};
use crate::valuer::{ValuationError, Valuer};
use rust_decimal::prelude::Zero;
use smallvec::{SmallVec, smallvec};
use std::cmp::Ordering;
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

    fn insert(&mut self, index: usize, flow: Flow<'h>);

    /// Returns true if all flows are zero, which is naturally the case when there are no flows
    fn is_zero(&self) -> bool {
        for flow in self.as_slice().iter() {
            if !flow.amount().is_zero() {
                return false;
            }
        }
        true
    }

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

    /// Links debits and credits together, returning a vector of linked flows.
    fn linked<V: Valuer<'h>>(&self, valuer: &mut V) -> JournResult<SmallVec<[LinkedFlow<'h>; 4]>> {
        let mut res: SmallVec<[LinkedFlow<'h>; 4]> = smallvec![];
        let push_res =
            |res: &mut SmallVec<[LinkedFlow<'h>; 4]>, mut debit: Flow<'h>, mut credit: Flow<'h>| {
                // A non-explicit zero flow that would round to 0 is not something we want.
                if !debit.is_zero()
                    && debit.amount().rounded().is_zero()
                    && !credit.is_zero()
                    && credit.amount().rounded().is_zero()
                {
                    return;
                }
                // Ensure valuations are set for when flows are split up later.
                if credit.unit() != debit.unit() {
                    debit
                        .valued_amount_mut()
                        .set_valuation(PostingValuation::new_total(-credit.amount(), true));
                    credit
                        .valued_amount_mut()
                        .set_valuation(PostingValuation::new_total(-debit.amount(), true));
                }
                debit.round();
                credit.round();
                res.push(LinkedFlow::new(debit, credit));
            };

        let (mut debits, mut credits) = (self.debits(), self.credits());
        // When credits is empty, it could mean because a 0 amount has been interpreted as a debit when
        // it needs to be a credit.
        if credits.is_empty()
            && let Some(pos) = debits.iter().position(|d| d.is_zero())
        {
            credits.push(debits.remove(pos));
        }

        let mut same_unit_mode = true;
        let mut debits_rem: FlowVec<'h> = smallvec![];
        'restart_debits: loop {
            while let Some(debit) = debits.pop() {
                // If there's only 1 credit left, we just need to link with that
                if debits.is_empty() && credits.len() == 1 && !same_unit_mode {
                    push_res(&mut res, debit, credits.remove(0));
                    break;
                }

                // Find the best credit to link with. Bias towards positions at the end,
                // as we do with debits.
                let mut credit = {
                    let mut best_credit_pos = None;
                    for (i, credit) in credits.iter().enumerate().rev() {
                        if -credit.amount() == debit.amount() {
                            best_credit_pos = Some(i);
                            break;
                        }
                        if credit.unit() == debit.unit() && best_credit_pos.is_none() {
                            best_credit_pos = Some(i);
                        }
                    }
                    match best_credit_pos {
                        Some(pos) => credits.remove(pos),
                        // If we're only working in the same unit, we go to the next
                        // debit.
                        None if same_unit_mode => {
                            debits_rem.push(debit);
                            continue;
                        }
                        None => credits.pop().unwrap(),
                    }
                };

                // This can happen with bad valuations. Only thing we can
                // do is consume the credits and continue.
                if debit.amount().is_zero() && debits.is_empty() {
                    push_res(&mut res, debit, credit);
                    continue;
                }

                // Value the credit in the debit's unit and compare the valuations.
                // Don't round because:
                // - Sometimes we can end up rounding to 0, which then wrongly ends
                //   up taking the valuation.is_zero() branch below.
                // - Valuations are less accurate when set with rounded values
                match credit.net_amount.set_value_in_or_value_with(debit.unit(), valuer, false) {
                    // Beware 0 valuations.
                    Ok(credit_value) if (*credit_value).is_zero() => {
                        push_res(&mut res, debit, credit.clone().with_zero());
                        if !credit.amount().is_zero() {
                            credits.push(credit);
                        }
                    }
                    Ok(credit_value) => match (*credit_value).abs().cmp(&debit.amount()) {
                        Ordering::Greater => {
                            let (taken, credit_rem) = credit.split(-debit.amount());
                            push_res(&mut res, debit, taken);
                            if !credit_rem.amount().is_zero() {
                                credits.push(credit_rem);
                            }
                        }
                        Ordering::Less => {
                            let (taken, debit_rem) = debit.split(-*credit_value);
                            push_res(&mut res, taken, credit);
                            if !debit_rem.amount().is_zero() {
                                debits.push(debit_rem);
                            }
                        }
                        Ordering::Equal => {
                            push_res(&mut res, debit, credit);
                        }
                    },
                    Err(e) => return Err(err!(e)),
                }

                // We need to ensure that there is at least 1 debit/credit if there are more credits/debits.
                if !same_unit_mode && debits.is_empty() && !credits.is_empty() {
                    debits.push(res.last().unwrap().flow().clone().with_zero());
                } else if !same_unit_mode && credits.is_empty() && !debits.is_empty() {
                    credits.push(res.last().unwrap().linked().clone().with_zero());
                }
            }
            if same_unit_mode {
                same_unit_mode = false;
                debits = debits_rem;
                debits_rem = smallvec![]; // To please compiler
                continue 'restart_debits;
            } else {
                break;
            }
        }
        Ok(res)
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

    /// Tries to extract the amount from the flows, matching flows of the same unit.
    /// If amount is too large for any one flow, `None` is returned.
    fn try_take_amount(&mut self, amount: Amount<'h>) -> Option<Flow<'h>> {
        for (i, flow) in self.as_slice().iter().enumerate() {
            if flow.unit() == amount.unit()
                && ((flow.is_debit() && amount >= 0 && flow.amount() >= amount)
                    || (flow.is_credit() && amount <= 0 && flow.amount() <= amount))
            {
                let flow = self.remove(i);
                let (taken, rem) = flow.split(amount);
                if !rem.amount().is_zero() {
                    self.insert(i, rem)
                }
                return Some(taken);
            }
        }
        None
    }

    /// Tries to extract an equivalent value from the flows, splitting them up if necessary.
    fn try_take_value<V: Valuer<'h>>(
        &mut self,
        value: Amount<'h>,
        valuer: &mut V,
    ) -> Result<Option<Flow<'h>>, ValuationError>
    where
        Self: Sized,
    {
        let mut errs = vec![];
        for i in (0..self.len()).rev() {
            let mut flow = self.remove(i);
            match flow.net_amount.set_value_in_or_value_with(value.unit(), valuer, true) {
                Ok(v)
                    if (flow >= Quantity::zero() && value >= 0 && *v >= value)
                        || (flow <= Quantity::zero() && value <= 0 && *v <= value) =>
                {
                    let (taken, rem) = flow.split(value);
                    if !rem.is_zero() {
                        self.insert(i, rem);
                    }
                    return Ok(Some(taken));
                }
                Ok(_) => {
                    self.insert(i, flow);
                    continue;
                }
                Err(e) => {
                    self.insert(i, flow);
                    errs.push(e);
                }
            }
        }
        if errs.is_empty() { Ok(None) } else { Err(errs.pop().unwrap()) }
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

    /// Creates a series of sub flows for each unit within, where each sub flow
    /// is homogenous.
    fn by_unit(&self, unit: &Unit<'h>) -> impl Flows<'h> {
        let mut unit_group = smallvec![];

        for flow in self.as_slice().iter() {
            if flow.unit() == unit {
                unit_group.push(flow.clone());
            }
        }
        unit_group
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

    fn insert(&mut self, index: usize, flow: Flow<'h>) {
        self.insert(index, flow);
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

    fn insert(&mut self, index: usize, flow: Flow<'h>) {
        self.insert(index, flow);
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

    pub fn account_root(&self) -> Option<&Arc<Account<'h>>> {
        self.account_root.as_ref()
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

    pub fn valued_amount_mut(&mut self) -> &mut ValuedAmount<'h> {
        &mut self.net_amount
    }

    pub fn into_valued_amount(self) -> ValuedAmount<'h> {
        self.net_amount
    }

    pub fn amount(&self) -> Amount<'h> {
        self.net_amount.amount()
    }

    pub fn with_zero(mut self) -> Self {
        self.net_amount = ValuedAmount::zero(
            self.net_amount.amount().unit(),
            self.net_amount.allocator().unwrap(),
        );
        self
    }

    pub fn with_amount(mut self, amount: ValuedAmount<'h>) -> Self {
        self.net_amount = amount;
        self
    }

    pub fn negate(&mut self) {
        self.net_amount.negate();
    }

    pub fn round(&mut self) {
        self.net_amount.round();
    }

    /// Tries to get the amount in the specified unit. This is either going to be the amount
    /// itself, or one of its valuations.
    pub fn amount_in(&self, in_curr: &'h Unit<'h>) -> Option<Amount<'h>> {
        self.net_amount.value_in(in_curr)
    }

    /// Gets whether the flow is a debit, that is, its net_amount >= 0.
    pub fn is_debit(&self) -> bool {
        self.net_amount.amount() >= 0
    }

    /// Gets whether the flow is a credit, having a negative amount.
    pub fn is_credit(&self) -> bool {
        self.net_amount.amount() < 0
    }

    /// The flow is zero when all amounts are zero. A flow with a zero
    /// primary amount, but a non-zero valuation is _not_ zero.
    pub fn is_zero(&self) -> bool {
        self.net_amount.amounts().all(|amount| amount.is_zero())
    }

    /// Splits the flow in two at the given threshold `amount`.
    ///
    /// # Panics
    /// If the amount is considered out of bounds of the flow.
    pub fn split(&self, amount: Amount<'h>) -> (Self, Self) {
        assert!(
            (self >= &Quantity::zero()
                && amount >= 0
                && self.amount_in(amount.unit()).is_some_and(|a| amount <= a))
                || (self <= &Quantity::zero()
                    && amount <= 0
                    && self.amount_in(amount.unit()).is_some_and(|a| amount >= a)),
            "Split amount is out of bounds"
        );

        let (left_amount, right_amount) = self.net_amount.clone().split(amount);

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

impl<'h> PartialEq<Quantity> for Flow<'h> {
    fn eq(&self, other: &Quantity) -> bool {
        self.amount().quantity() == *other
    }
}

impl<'h> Eq for Flow<'h> {}

impl<'h> PartialOrd for Flow<'h> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'h> PartialOrd<Quantity> for Flow<'h> {
    fn partial_cmp(&self, other: &Quantity) -> Option<Ordering> {
        self.net_amount.amount().partial_cmp(other)
    }
}

impl<'h> Ord for Flow<'h> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.net_amount.cmp(&other.net_amount)
    }
}

impl fmt::Display for Flow<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {}", self.account_root, self.net_amount)
    }
}

pub struct LinkedFlow<'h> {
    flow: Flow<'h>,
    linked: Flow<'h>,
}

impl<'h> LinkedFlow<'h> {
    pub fn new(flow: Flow<'h>, linked: Flow<'h>) -> Self {
        assert!(
            (flow.amount().is_positive() && linked.amount().is_negative())
                || (flow.amount().is_negative() && linked.amount().is_positive()),
            "A flow must be between a debit and credit: {} and {}",
            flow.amount(),
            linked.amount()
        );

        Self { flow, linked }
    }

    pub fn flow(&self) -> &Flow<'h> {
        &self.flow
    }

    pub fn flow_mut(&mut self) -> &mut Flow<'h> {
        &mut self.flow
    }

    pub fn linked(&self) -> &Flow<'h> {
        &self.linked
    }

    pub fn linked_mut(&mut self) -> &mut Flow<'h> {
        &mut self.linked
    }

    pub fn invert(self) -> Self {
        Self { flow: self.linked, linked: self.flow }
    }
}
