/*
 * Copyright (c) 2021-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::account::AccountType;
use crate::amount::Amount;
use crate::journal_entry::JournalEntry;
use crate::unit::Unit;
use crate::valued_amount::ValuedAmount;
use smallvec::SmallVec;
use std::fmt;
use std::fmt::Formatter;
use std::iter::Sum;
use std::ops::Add;

pub struct Flows<'h>(Vec<Flow<'h>>);
impl<'h> Flows<'h> {
    pub(crate) fn create(je: &JournalEntry<'h>) -> Self {
        let mut flows: Vec<Flow<'h>> = vec![];
        for pst in je.postings() {
            // Create a new flow for the posting without any pretext.
            let mut va = pst.valued_amount().clone();
            va.set_pretext("");
            let new_flow = Flow::new(pst.account().account_type(), va);
            match flows.iter_mut().find(|f| {
                f.account_type() == pst.account().account_type() && f.unit() == pst.unit()
            }) {
                Some(flow) => *flow = (&*flow + new_flow).unwrap(),
                None => flows.push(new_flow),
            }
        }
        // A flow of net 0 isn't a flow.
        flows.retain(|f| f.net_amount.amount() != 0);
        Flows(flows)
    }

    pub fn assets(&self) -> impl Iterator<Item = &Flow<'h>> {
        self.0.iter().filter(|f| f.account_type == AccountType::Asset)
    }

    pub fn equity(&self) -> impl Iterator<Item = &Flow<'h>> {
        self.0.iter().filter(|f| f.account_type == AccountType::Equity)
    }

    /// Gets all unique primary units in the flows.
    pub fn units(&self) -> impl Iterator<Item = &'h Unit<'h>> + '_ {
        let mut units: SmallVec<[&'h Unit<'h>; 4]> = SmallVec::new();
        for unit in self.0.iter().map(|f| f.unit()) {
            if !units.contains(&unit) {
                units.push(unit);
            }
        }
        units.into_iter()
    }

    /// Gets an iterator of the net equity flow in each unit.
    /// The net equity is the sum of all equity, asset and liability flows in the same unit.
    ///
    /// The net equity transfer can thus be considered an acquisition or a disposal, depending on
    /// whether the net equity flow is positive or negative.
    pub fn net_equity(&self) -> impl Iterator<Item = Flow<'h>> + '_ {
        self.units().filter_map(move |unit| {
            let mut accum = Flow::new(AccountType::Equity, ValuedAmount::nil());
            for flow in self.0.iter().filter(|f| f.unit() == unit) {
                match flow.account_type {
                    AccountType::Equity | AccountType::Asset | AccountType::Liability => {
                        accum.net_amount = (&accum.net_amount + &flow.net_amount).unwrap();
                    }
                    _ => {}
                }
            }
            if !accum.net_amount.is_zero() { Some(accum) } else { None }
        })
    }

    /// Gets Asset flows, searching for any equity flows in the same unit and adding the amounts.
    /// Otherwise, returning the original asset flow.
    /// Should the added amounts equal 0, they cancel each other out and are omitted. E.g. An asset
    /// transfer from equity (opening balance entry).
    pub fn assets_plus_equity(&self) -> impl Iterator<Item = Option<ValuedAmount<'h>>> + '_ {
        self.assets()
            .map(move |af| {
                self.0
                    .iter()
                    .find(|ef| ef.account_type == AccountType::Equity && ef.unit() == af.unit())
                    .map(|ef| &af.net_amount + &ef.net_amount)
                    .unwrap_or(Some(af.net_amount.clone()))
            })
            .filter(|va| match va {
                Some(va) => !va.is_zero(),
                None => false,
            })
    }

    pub fn income(&self) -> impl Iterator<Item = &Flow<'h>> {
        self.0.iter().filter(|f| f.account_type == AccountType::Income)
    }

    pub fn income_in_unit(&self, unit: &Unit<'h>) -> Option<&Flow<'h>> {
        self.0.iter().find(|f| f.unit() == unit && f.account_type == AccountType::Income)
    }
}

/*
pub trait JournalEntryFlows {
    fn create(je: &JournalEntry) -> Vec<Flow> {
        let mut flows: Vec<Flow> = vec![];
        for pst in je.postings() {
            let new_flow = Flow::new(pst.account().account_type(), pst.valued_amount().clone());
            match flows.iter_mut().find(|f| f.account_type() == pst.account().account_type() && f.unit() == pst.unit())
            {
                Some(flow) => *flow += new_flow,
                None => flows.push(new_flow),
            }
        }
        // A flow of net 0 isn't a flow.
        flows.retain(|f| f.net_amount.amount() != 0);
        flows
    }

    /// The total of all flows in the specified unit.
    fn total(&self, curr: &'static Unit) -> Flow;

    /// Gets the flow of net income. This is simply all Income + Expenses.
    fn net_income(&self, curr: &'static Unit) -> Flow;

    fn assets<P>(&self) -> iter::Filter<Iter<'_, Flow>, P>;

    fn asset_in_currency(&self, curr: &'static Unit) -> Option<&Flow>;

    fn income(&self) -> Vec<&Flow>;

    fn income_in_currency(&self, curr: &'static Unit) -> Option<&Flow>;

    fn asset_acquisitions(&self) -> Vec<&Flow>;

    fn asset_disposals(&self) -> Vec<&Flow>;
}

impl JournalEntryFlows for Vec<Flow> {
    fn total(&self, curr: &'static Unit) -> Flow {
        let mut accum = Flow::new(
            AccountType::None,
            ValuedAmount::new(AmountExpr::new(curr.with_quantity(0), "", None::<&'static str>)),
        );
        for flow in self.iter().filter(|f| f.unit() == curr) {
            accum += flow.clone();
        }
        accum
    }

    /// Gets the flow of net income. This is simply all Income + Expenses.
    fn net_income(&self, curr: &'static Unit) -> Flow {
        let mut accum = Flow::new(
            AccountType::Income,
            ValuedAmount::new(AmountExpr::new(curr.with_quantity(0), "", None::<&'static str>)),
        );
        for flow in self.iter().filter(|f| f.unit() == curr) {
            if flow.account_type == AccountType::Expense || flow.account_type == AccountType::Income {
                accum += flow.clone();
            }
        }
        accum
    }

    fn assets<P>(&self) -> iter::Filter<Iter<'_, Flow>, P> {
        self.iter().filter(|f: &Flow| f.account_type == AccountType::Asset)
    }

    fn asset_in_currency(&self, curr: &'static Unit) -> Option<&Flow> {
        self.iter().find(|f| f.unit() == curr && f.account_type == AccountType::Asset)
    }

    fn income(&self) -> Vec<&Flow> {
        self.iter().filter(|f| f.account_type == AccountType::Income).collect()
    }

    fn income_in_currency(&self, curr: &'static Unit) -> Option<&Flow> {
        self.iter().find(|f| f.unit() == curr && f.account_type == AccountType::Income)
    }

    fn asset_acquisitions(&self) -> Vec<&Flow> {
        self.iter().filter(|f| f.account_type == AccountType::Asset).filter(|f| f.net_amount.amount() > 0).collect()
    }

    fn asset_disposals(&self) -> Vec<&Flow> {
        self.iter().filter(|f| f.account_type == AccountType::Asset).filter(|f| f.net_amount.amount() < 0).collect()
    }
}*/

#[derive(Debug, Clone)]
pub struct Flow<'h> {
    account_type: AccountType,
    net_amount: ValuedAmount<'h>,
}

impl<'h> Flow<'h> {
    pub fn new(account_type: AccountType, net_amount: ValuedAmount<'h>) -> Self {
        Self { account_type, net_amount }
    }

    pub fn account_type(&self) -> AccountType {
        self.account_type
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
}

impl<'h> Add<Flow<'h>> for &Flow<'h> {
    type Output = Option<Flow<'h>>;

    fn add(self, rhs: Flow<'h>) -> Self::Output {
        if self.account_type != rhs.account_type {
            return None;
        }
        let combined = &self.net_amount + &rhs.net_amount;
        //if combined.is_zero() {
        //    None
        //} else {
        combined.map(|combined| Flow { net_amount: combined, account_type: self.account_type })
        //}
    }
}

impl<'h> Sum<Flow<'h>> for Option<Flow<'h>> {
    fn sum<I: Iterator<Item = Flow<'h>>>(iter: I) -> Self {
        let mut accum = None;
        for flow in iter {
            match accum {
                Some(ref mut a) => accum = &*a + flow,
                None => accum = Some(flow),
            }
        }
        accum
    }
}

/*
impl AddAssign<Flow> for Flow {
    fn add_assign(&mut self, rhs: Flow) {
        assert_eq!(self.account_type, rhs.account_type);

        self.net_amount += &rhs.net_amount;
    }
}*/

/*
impl SubAssign<Flow> for Flow {
    fn sub_assign(&mut self, mut rhs: Flow) {
        rhs.negate();
        *self += rhs
    }
}*/

impl fmt::Display for Flow<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {}", self.account_type, self.net_amount)
    }
}
