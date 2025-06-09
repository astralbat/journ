/*
 * Copyright (c) 2019-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::account::Account;
use crate::amount::Amount;
use crate::journal_entry::JournalEntry;
use crate::unit::Unit;
use crate::valued_amount::ValuedAmount;
use smallvec::SmallVec;
use std::cell::Ref;
use std::collections::HashMap;
use std::ops::AddAssign;
use std::sync::Arc;

pub trait Balance<'h> {
    fn balance(&self, unit: &'h Unit<'h>) -> Amount<'h>;
}

impl<'h> Balance<'h> for Vec<Amount<'h>> {
    fn balance(&self, unit: &'h Unit<'h>) -> Amount<'h> {
        self.iter().filter(|m| m.unit() == unit).fold(unit.with_quantity(0), |acc, m| acc + *m)
    }
}

impl<'h> Balance<'h> for HashMap<&'h Unit<'h>, Amount<'h>> {
    fn balance(&self, unit: &'h Unit<'h>) -> Amount<'h> {
        self.get(unit).copied().unwrap_or_else(|| unit.with_quantity(0))
    }
}

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

pub trait AccountBalance<'h> {
    fn balances(&mut self, account_filter: Option<&str>) -> AccountBalances<'h>;

    fn balance(
        &mut self,
        account_filter: &str,
        account: &Arc<Account<'h>>,
        unit: &'h Unit<'h>,
    ) -> Amount<'h> {
        self.balances(Some(account_filter)).balance(account, unit)
    }
}

pub struct AccountBalances<'h> {
    value_units: Vec<&'h Unit<'h>>,
    lines: Vec<BalanceLine<'h>>,
}
pub struct BalanceLine<'h> {
    account: Arc<Account<'h>>,
    valued_amount: ValuedAmount<'h>,
}

impl<'h> BalanceLine<'h> {
    fn new(account: Arc<Account<'h>>, valued_amount: ValuedAmount<'h>) -> BalanceLine<'h> {
        BalanceLine { account, valued_amount }
    }

    pub fn account(&self) -> &Arc<Account<'h>> {
        &self.account
    }

    pub fn valued_amount(&self) -> &ValuedAmount<'h> {
        &self.valued_amount
    }
}

impl<'h> AccountBalances<'h> {
    pub fn new(value_units: Vec<&'h Unit<'h>>) -> AccountBalances<'h> {
        AccountBalances { value_units, lines: vec![] }
    }

    pub fn account_balances<'a>(
        &'a self,
        account: &'a Arc<Account<'h>>,
    ) -> impl Iterator<Item = Amount<'h>> + 'a {
        self.lines
            .iter()
            .filter(move |l| &l.account == account)
            .flat_map(|l| l.valued_amount.amounts())
    }

    pub fn balance(&self, account: &Arc<Account<'h>>, unit: &'h Unit<'h>) -> Amount<'h> {
        self.find_line(account, unit)
            .map(|l| l.valued_amount.amount())
            .unwrap_or_else(|| unit.with_quantity(0))
    }

    pub fn units(&self) -> Vec<&'h Unit<'h>> {
        let mut v = vec![];
        for line in self.lines.iter().filter(|l| !l.valued_amount.is_zero()) {
            if !v.contains(&line.valued_amount.unit()) {
                v.push(line.valued_amount.unit());
            }
        }
        v
    }

    pub fn value_units(&self) -> &[&'h Unit<'h>] {
        &self.value_units
    }

    /// Updates the balance of the specified account.
    /// If replace is true, the balance will be set instead of added.
    pub fn update_balance(
        &mut self,
        account: &Arc<Account<'h>>,
        valued_amount: &ValuedAmount<'h>,
        replace: bool,
    ) {
        if valued_amount.is_zero() {
            return;
        }

        match self.find_line_mut(account, valued_amount.unit()) {
            Ok(line) => {
                if replace {
                    line.valued_amount = valued_amount.clone();
                } else {
                    line.valued_amount = (&line.valued_amount + valued_amount).unwrap();
                }
            }
            Err(insert_pos) => {
                self.lines.insert(
                    insert_pos,
                    BalanceLine::new(Arc::clone(account), valued_amount.clone()),
                );
            }
        }
    }

    pub fn len(&self) -> usize {
        self.lines.len()
    }

    pub fn is_empty(&self) -> bool {
        self.lines.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &BalanceLine<'h>> {
        self.lines.iter()
    }

    fn find_line(
        &self,
        account: &Arc<Account<'h>>,
        unit: &'h Unit<'h>,
    ) -> Option<&BalanceLine<'h>> {
        match self
            .lines
            .binary_search_by_key(&(account, unit), |l| (&l.account, l.valued_amount.unit()))
        {
            Ok(i) => Some(&self.lines[i]),
            Err(_) => None,
        }
    }

    fn find_line_mut(
        &mut self,
        account: &Arc<Account<'h>>,
        unit: &'h Unit<'h>,
    ) -> Result<&mut BalanceLine<'h>, usize> {
        match self
            .lines
            .binary_search_by_key(&(account, unit), |l| (&l.account, l.valued_amount.unit()))
        {
            Ok(i) => Ok(&mut self.lines[i]),
            Err(i) => Err(i),
        }
    }
}

impl<'h, 'a, T> AccountBalance<'h> for T
where
    T: Iterator<Item = Ref<'a, JournalEntry<'h>>>,
    'h: 'a,
{
    fn balances(&mut self, account_part: Option<&str>) -> AccountBalances<'h> {
        let mut bals = AccountBalances::new(vec![]);
        for entry in self {
            for pst in entry.postings() {
                if let Some(part) = &account_part {
                    if !pst.account().name().contains(part) {
                        continue;
                    }
                }
                bals.update_balance(
                    pst.account(),
                    pst.valued_amount(),
                    pst.balance_assertion().is_some(),
                );
            }
        }
        bals
    }
}
