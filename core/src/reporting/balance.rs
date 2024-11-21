/*
 * Copyright (c) 2019-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::account::Account;
use crate::amount::Amount;
use crate::arguments::{Arguments, BalCommand};
use crate::journal_entry::JournalEntry;
use crate::reporting::table::{Cell, Table};
use crate::reporting::term_style::Colour;
use crate::unit::Unit;
use crate::valued_amount::ValuedAmount;
use smallvec::SmallVec;
use std::cell::Ref;
use std::collections::HashMap;
use std::fmt;
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

pub struct AccountBalances<'h> {
    value_units: Vec<&'h Unit<'h>>,
    lines: Vec<BalanceLine<'h>>,
}
struct BalanceLine<'h> {
    account: Arc<Account<'h>>,
    valued_amount: ValuedAmount<'h>,
}

impl<'h> BalanceLine<'h> {
    fn new(account: Arc<Account<'h>>, valued_amount: ValuedAmount<'h>) -> BalanceLine<'h> {
        BalanceLine { account, valued_amount }
    }

    /*
    fn set_valuations(&mut self, prices: &[Arc<Price<'h>>], valuation_currencies: &[&'h Unit<'h>]) {
        self.valuations.clear();
        'next_quote_curr: for quote_curr in valuation_currencies {
            for p in prices {
                if p.base_unit() == self.amount.unit() && p.price().unit() == *quote_curr {
                    self.valuations.push(Some(p.base_valuation(self.amount)));
                    continue 'next_quote_curr;
                }
            }
            self.valuations.push(None);
        }
    }*/

    /*
    fn print(&self, val_cols: &[BalanceColumn<'h>], last_account: Option<&Arc<Account<'h>>>) {
        print!("{:la$}", self.amount, la = val_cols[0].width);

        for (i, col) in val_cols.iter().skip(1).enumerate() {
            if Some(self.amount.unit()) == col.unit {
                print!("  @@ {:lt$}", self.amount, lt = col.width);
            } else if let Some(value) = &self.valuations[i] {
                print!("  @@ {:lt$}", value, lt = col.width);
            } else {
                // No value, so print empty cell
                print!("     {:lt$}", "", lt = col.width)
            }
        }
        // Don't repeat the last account
        if last_account != Some(&self.account) {
            if atty::is(Stream::Stdout) {
                println!("  {}", Blue.paint(&self.account.to_string()));
            } else {
                println!("  {}", self.account);
            }
        } else {
            println!();
        }
    }*/
}

impl<'h> AccountBalances<'h> {
    pub fn new(value_units: Vec<&'h Unit<'h>>) -> AccountBalances<'h> {
        AccountBalances { value_units, lines: vec![] }
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

    /*
    /// Create a structure for each numerical column in these account balances.
    fn create_balance_columns(&self) -> Vec<BalanceColumn<'h>> {
        let mut bal_cols = Vec::with_capacity(self.prices.len());
        bal_cols.push(BalanceColumn { width: 0, unit: None });
        let mut buf = String::new();

        // Set the first column to the width of the longest total.
        for t in self.totals.iter().filter(|t| !t.is_zero()) {
            write!(&mut buf, "{t}").unwrap();
            if buf.chars().count() > bal_cols[0].width {
                bal_cols[0].width = buf.chars().count();
            }
            buf.clear();
        }

        let val_currs = self.valuation_units();
        let mut val_totals = Vec::with_capacity(val_currs.len());
        for val_curr in val_currs.iter() {
            val_totals.push(val_curr.with_quantity(0));
        }
        for l in self.lines.iter().filter(|l| !l.amount.is_zero()) {
            for (i, val_curr) in val_currs.iter().copied().enumerate() {
                val_totals += l.valuations[i].unwrap_or(val_curr.with_quantity(0))
            }
        }
        for t in val_totals.into_iter() {
            bal_cols.push(BalanceColumn { width: 0, unit: Some(t.unit()) });
            write!(&mut buf, "{t}").unwrap();
            let len = bal_cols.len() - 1;
            bal_cols[len].width = buf.chars().count();
            buf.clear();
        }
        bal_cols
    }*/

    /*
    pub fn print(&self) {
        let cols = self.create_balance_columns();

        // Print the lines
        let mut last_account = None;
        for line in self.lines.iter().filter(|l| !l.amount.is_zero()) {
            line.print(&cols, last_account);
            last_account = Some(&line.account);
        }

        // Print sub grand totals
        if self.lines.len() > 1 && last_account.is_some() {
            print!("{:-<la$}", "", la = cols[0].width);
            for col in cols.iter().skip(1) {
                print!("     {:-<lt$}", "", lt = col.width)
            }
            println!();
            for tot in self.totals.iter().filter(|t| !t.is_zero()) {
                print!("{:la$}", tot, la = cols[0].width);
                for (i, col) in cols.iter().enumerate().skip(1) {
                    let mut val_curr_sub_total = col.unit.as_ref().unwrap().with_quantity(0);
                    for l in self.lines.iter().filter(|l| !l.amount.is_zero()) {
                        if l.amount.unit() == tot.unit() {
                            if col.unit == Some(l.amount.unit()) {
                                val_curr_sub_total += l.amount
                            } else {
                                val_curr_sub_total +=
                                    l.valuations[i - 1].as_ref().map(|m| m.quantity()).unwrap_or(dec!(0));
                            }
                        }
                    }
                    print!("  @@ {:lt$}", val_curr_sub_total, lt = cols[i].width)
                }
                println!()
            }

            // Print grand valuations
            if cols.len() > 1 {
                print!("{:la$}", "", la = cols[0].width);
                for col in cols.iter().skip(1) {
                    print!("     {:-<lt$}", "", lt = col.width)
                }
                println!();
                print!("{:la$}", "", la = cols[0].width);
                for (i, col) in cols.iter().enumerate().skip(1) {
                    let mut val_curr_tot = col.unit.as_ref().unwrap().with_quantity(0);
                    for l in self.lines.iter().filter(|l| !l.amount.is_zero()) {
                        if col.unit == Some(l.amount.unit()) {
                            val_curr_tot += l.amount
                        } else {
                            val_curr_tot += l.valuations[i - 1].as_ref().map(|m| m.quantity()).unwrap_or(dec!(0));
                        }
                    }
                    print!("  @@ {:lt$}", val_curr_tot, lt = col.width)
                }
                println!()
            }
        }
    }*/

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

impl fmt::Display for AccountBalances<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let cmd = Arguments::get().cast_cmd::<BalCommand>().unwrap();
        let mut table = Table::default();

        // Heading Row
        if !self.lines.is_empty() {
            let mut headings = vec![Cell::from(&"Account"), Cell::from(&"Amount")];
            for value_unit in cmd.value_units() {
                headings.push(Cell::from(value_unit.to_string()));
            }
            table.set_heading_row(headings);
        }

        // Account Rows
        let mut num_rows = 0;
        for line in self.lines.iter().filter(|l| !l.valued_amount.is_zero()) {
            let mut row = vec![
                Cell::from(line.account.to_string()).with_foreground(Some(Colour::Blue)),
                Cell::from(line.valued_amount.amount()),
            ];
            for unit in self.value_units.iter() {
                row.push(Cell::from(line.valued_amount.value_in(unit).unwrap()));
            }
            num_rows += 1;
            table.add_row(row);
        }

        // Total rows for each unit
        if num_rows > 1 {
            table.add_separator_row('-', (2 + self.value_units.len()) as u16);
            let mut num_totals = 0;
            for unit in self.units() {
                let mut total = unit.with_quantity(0);
                let mut val_totals = vec![];
                for vu in self.value_units.iter() {
                    val_totals.push(vu.with_quantity(0));
                }
                for bl in self
                    .lines
                    .iter()
                    .filter(|l| !l.valued_amount.is_zero())
                    .filter(|l| l.valued_amount.unit() == unit)
                {
                    total += bl.valued_amount.amount();
                    for (i, val_curr) in self.value_units.iter().copied().enumerate() {
                        val_totals[i] += bl.valued_amount.value_in(val_curr).unwrap();
                    }
                }
                let mut row = vec![Cell::from(&"")];
                row.push(Cell::from(total));
                for val in val_totals {
                    row.push(Cell::from(val));
                }
                table.add_row(row);
                num_totals += 1;
            }

            // Grand total row
            if num_totals > 1 && !self.value_units.is_empty() {
                table.add_separator_row('-', (2 + self.value_units.len()) as u16);
                let mut val_totals = vec![];
                for vu in self.value_units.iter() {
                    val_totals.push(vu.with_quantity(0));
                }
                for bl in self.lines.iter().filter(|l| !l.valued_amount.is_zero()) {
                    for (i, val_curr) in self.value_units.iter().copied().enumerate() {
                        val_totals[i] += bl.valued_amount.value_in(val_curr).unwrap();
                    }
                }
                let mut row = vec![Cell::from(&""), Cell::from(&"")];
                for val in val_totals {
                    row.push(Cell::from(val));
                }
                table.add_row(row);
            }
        }

        table.print(f)
    }
}

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
