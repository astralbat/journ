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
use std::ops::{Add, AddAssign, Deref};
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
    ///
    /// # Returns
    /// A list of linked flows where the `linked` flow are matching credits.
    /// Flows are linked in the order they appear with credits of the same unit and same amount being matched first.
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
        // Reverse for popping
        debits.reverse();
        // When credits is empty, it could mean because a 0 amount has been interpreted as a debit when
        // it needs to be a credit.
        if credits.is_empty()
            && let Some(pos) = debits.iter().position(|d| d.is_zero())
        {
            credits.push(debits.remove(pos));
        }

        while let Some(debit) = debits.pop() {
            // If there's only 1 credit left, we just need to link with that
            if debits.is_empty() && credits.len() == 1 {
                push_res(&mut res, debit, credits.remove(0));
                break;
            }
            // If there are no credits left, we need to create a zero credit to link with.
            if credits.is_empty() {
                let credit = debit.clone().with_zero();
                push_res(&mut res, debit, credit);
                continue;
            }

            // Find the best credit to link with in priority of:
            // * Same amount
            // * Same unit
            // * Same valuation
            let mut credit = {
                let best_credit_pos = credits
                    .iter()
                    .position(|cr| cr.amount() == debit.amount())
                    .or_else(|| credits.iter().position(|cr| cr.unit() == debit.unit()))
                    .or_else(|| {
                        credits
                            .iter()
                            .position(|cr| cr.valued_amount().eq_abs_value(debit.valued_amount()))
                    });
                match best_credit_pos {
                    Some(pos) => credits.remove(pos),
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
            /*
            if debits.is_empty() && !credits.is_empty() {
                debits.push(res.last().unwrap().flow().clone().with_zero());
            } else if credits.is_empty() && !debits.is_empty() {
                credits.push(res.last().unwrap().linked().clone().with_zero());
            }*/
        }
        debug_assert!(debits.is_empty());
        debug_assert!(credits.is_empty());
        Ok(res)
    }
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

#[derive(Clone)]
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

impl<'h> Deref for LinkedFlow<'h> {
    type Target = Flow<'h>;

    fn deref(&self) -> &Self::Target {
        &self.flow
    }
}

#[cfg(test)]
mod test {
    use crate::entry;
    use crate::journal_context::JContext;
    use crate::journal_entry_flow::Flows;
    use crate::test_util::with_entry;
    use crate::valuer::SystemValuer;
    use indoc::indoc;

    #[test]
    fn test_linked_with_zero_posting() {
        let entry = entry!(indoc! {r#"
        2000-01-01  Entry 1
            A  $100
            B  -$100
            C  $0
        "#});
        let mut valuer = SystemValuer::from(&entry);
        let linked = entry.flows().linked(&mut valuer);
        assert!(linked.is_ok());
        let linked = linked.unwrap();
        assert_eq!(linked.len(), 2);
        assert_eq!(&**linked[0].flow().account_root().unwrap(), "A");
        assert_eq!(&**linked[0].linked().account_root().unwrap(), "B");
        assert_eq!(&**linked[1].flow().account_root().unwrap(), "C");
        assert_eq!(&**linked[1].linked().account_root().unwrap(), "C");

        let entry = entry!(indoc! {r#"
        2000-01-01  Entry 1
            A  $100
            B  -$100
            C  -$0
        "#});
        let mut valuer = SystemValuer::from(&entry);
        let linked = entry.flows().linked(&mut valuer);
        assert!(linked.is_ok());
        let linked = linked.unwrap();
        assert_eq!(linked.len(), 2);
        assert_eq!(&**linked[0].flow().account_root().unwrap(), "A");
        assert_eq!(&**linked[0].linked().account_root().unwrap(), "B");
        assert_eq!(&**linked[1].flow().account_root().unwrap(), "C");
        assert_eq!(&**linked[1].linked().account_root().unwrap(), "C");
    }

    /// When units are different, the flows are linked by equal value if possible.
    #[test]
    fn test_link_by_value() {
        let entry = entry!(indoc! {r#"
        2000-01-01  Entry 1
            A  100 ABC @@ $100
            D  -100 XYZ @@ $200
            C  100 UVW @@ $200
            B  -100 DEF @@ $100
        "#});
        let mut valuer = SystemValuer::from(&entry);
        let linked = entry.flows().linked(&mut valuer);
        assert!(linked.is_ok());
        let linked = linked.unwrap();
        assert_eq!(linked.len(), 2);
        assert_eq!(&**linked[0].flow().account_root().unwrap(), "A");
        assert_eq!(&**linked[0].linked().account_root().unwrap(), "B");
        assert_eq!(&**linked[1].flow().account_root().unwrap(), "C");
        assert_eq!(&**linked[1].linked().account_root().unwrap(), "D");
    }

    /// Tests that the linked flows are in the same order as the postings, and that they are linked correctly.
    #[test]
    fn test_linked_order() {
        let entry = entry!(indoc! {r#"
        2000-01-01  Entry 1
            A  $100
            B  -$100
            C  $100
            D  -$100
            E  $100
            F  -$100
        "#});
        let mut valuer = SystemValuer::from(&entry);
        let linked = entry.flows().linked(&mut valuer);
        assert!(linked.is_ok());
        let linked = linked.unwrap();
        assert_eq!(linked.len(), 3);
        assert_eq!(&**linked[0].flow().account_root().unwrap(), "A");
        assert_eq!(&**linked[0].linked().account_root().unwrap(), "B");
        assert_eq!(&**linked[1].flow().account_root().unwrap(), "C");
        assert_eq!(&**linked[1].linked().account_root().unwrap(), "D");
        assert_eq!(&**linked[2].flow().account_root().unwrap(), "E");
        assert_eq!(&**linked[2].linked().account_root().unwrap(), "F");

        let entry = entry!(indoc! {r#"
        2000-01-01  Entry 1
            A  $100
            B  -100 USD @@ $100
            C  $100
            D  -$100
            E  $100
            F  -$100
        "#});
        let mut valuer = SystemValuer::from(&entry);
        let linked = entry.flows().linked(&mut valuer);
        assert!(linked.is_ok());
        let linked = linked.unwrap();
        assert_eq!(linked.len(), 3);
        assert_eq!(&**linked[0].flow().account_root().unwrap(), "A");
        assert_eq!(&**linked[0].linked().account_root().unwrap(), "D");
        assert_eq!(&**linked[1].flow().account_root().unwrap(), "C");
        assert_eq!(&**linked[1].linked().account_root().unwrap(), "F");
        assert_eq!(&**linked[2].flow().account_root().unwrap(), "E");
        assert_eq!(&**linked[2].linked().account_root().unwrap(), "B");
    }
}
