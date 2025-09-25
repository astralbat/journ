/*
 * Copyright (c) 2019-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::metadata::Metadata;
use crate::reporting::table2::fmt::CellFormatter;
use crate::reporting::table2::{
    Cell, CellWidth, ColumnWidth, PolicyWrappingCell, StyledCell, WrapEase, WrapPolicy,
};
use crate::reporting::term_style::{Colour, Style};
use std::hash::{Hash, Hasher};
use std::sync::Arc;
use std::{cmp, fmt};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AccountType {
    Asset,
    Liability,
    Income,
    Expense,
    Equity,
    /// All other accounts
    None,
}

#[derive(Debug, Eq)]
pub struct Account<'h> {
    /// The full name of the account, including any parents
    name: String,
    /// The parent account will never be virtual
    parent: Option<Arc<Account<'h>>>,
    metadata: Vec<Metadata<'h>>,
}

impl<'h> Account<'h> {
    pub fn new(
        name: String,
        parent: Option<Arc<Account<'h>>>,
        metadata: Vec<Metadata<'h>>,
    ) -> Account<'h> {
        Account { name, parent, metadata }
    }

    /// Gets the parent name of the account specified.
    ///
    /// # Examples
    /// ```
    /// use journ_core::account::Account;
    ///
    /// assert_eq!(Account::parent_str("A"), None);
    /// assert_eq!(Account::parent_str("A:B"), Some(String::from("A")));
    /// assert_eq!(Account::parent_str("(A:B:C)"), Some(String::from("(A:B)")));
    /// assert_eq!(Account::parent_str("[A:B:C:D]"), Some(String::from("[A:B:C]")));
    /// ```
    pub fn parent_str(full_name: &str) -> Option<String> {
        let round_brackets = full_name.starts_with('(') && full_name.ends_with(')');
        let square_brackets = full_name.starts_with('[') && full_name.ends_with(']');

        full_name.rfind(':').map(|pos| {
            let mut ret = full_name[0..pos].to_string();
            if round_brackets {
                ret += ")";
            } else if square_brackets {
                ret += "]";
            }
            ret
        })
    }

    /// The exact name of the account, including any brackets
    pub fn name_exact(&self) -> &str {
        &self.name
    }

    /// Gets the full name of the account
    pub fn name(&self) -> &str {
        if self.is_virtual() { &self.name[1..self.name.len() - 1] } else { &self.name }
    }

    pub fn parent(&self) -> Option<&Arc<Account<'h>>> {
        self.parent.as_ref()
    }

    pub fn account_type(&self) -> AccountType {
        if self.is_asset() {
            AccountType::Asset
        } else if self.is_liability() {
            AccountType::Liability
        } else if self.is_income() {
            AccountType::Income
        } else if self.is_expense() {
            AccountType::Expense
        } else if self.is_equity() {
            AccountType::Equity
        } else {
            AccountType::None
        }
    }

    pub fn is_asset(&self) -> bool {
        self.name.to_lowercase().starts_with("assets:")
    }

    pub fn is_liability(&self) -> bool {
        self.name.to_lowercase().starts_with("liability:")
    }

    pub fn is_equity(&self) -> bool {
        self.name.to_lowercase().starts_with("equity:")
    }

    pub fn is_income(&self) -> bool {
        self.name.to_lowercase().starts_with("income:")
    }

    pub fn is_expense(&self) -> bool {
        self.name.to_lowercase().starts_with("expenses:")
    }

    pub fn is_trading_fee(&self) -> bool {
        let s = self.name.to_lowercase();
        s.starts_with("expenses:") && (s.ends_with("tradingfees") || s.ends_with("fxfees"))
    }

    pub fn is_real(&self) -> bool {
        !self.is_virtual()
    }

    pub fn is_balanced(&self) -> bool {
        !self.is_virtual() || self.is_virtual_balanced()
    }

    pub fn is_virtual(&self) -> bool {
        self.is_virtual_balanced() || self.is_virtual_unbalanced()
    }

    pub fn is_virtual_unbalanced(&self) -> bool {
        self.name.starts_with('(') && self.name.ends_with(')')
    }

    pub fn is_virtual_balanced(&self) -> bool {
        self.name.starts_with('[') && self.name.ends_with(']')
    }

    pub fn metadata(&self) -> &Vec<Metadata<'h>> {
        &self.metadata
    }

    /// Gets whether this account or a parent has the specified metadata key.
    pub fn has_tag(&self, tag: &'static str) -> bool {
        if let Some(parent) = &self.parent {
            if parent.has_tag(tag) {
                return true;
            }
        }
        self.metadata.iter().any(|m| m.key() == tag)
    }

    /// Gets the most specific tag that this account or a parent has from a set of `tags`.
    pub fn get_best_tag(&self, tags: &[&'static str]) -> Option<&'static str> {
        for tag in tags {
            if self.metadata.iter().any(|m| m.key() == *tag) {
                return Some(tag);
            }
        }
        if let Some(parent) = &self.parent { parent.get_best_tag(tags) } else { None }
    }
}

impl fmt::Display for Account<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.pad(&self.name)
    }
}

impl PartialEq for Account<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name()
    }
}

impl Ord for Account<'_> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.name().cmp(other.name())
    }
}

impl PartialOrd for Account<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Hash for Account<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name().hash(state)
    }
}

impl<'t, S> From<S> for Account<'t>
where
    S: AsRef<str>,
{
    fn from(s: S) -> Account<'t> {
        let parent = Account::parent_str(s.as_ref()).map(|s| Arc::new(Account::from(s)));
        Account::new(s.as_ref().to_string(), parent, vec![])
    }
}

impl Cell for &Account<'_> {
    fn print<'format>(
        &self,
        f: &mut dyn CellFormatter,
        line: usize,
        _width: Option<ColumnWidth>,
    ) -> fmt::Result {
        if line == 0 { write!(f, "{}", self.name()) } else { Err(fmt::Error) }
    }

    fn width(&self) -> CellWidth {
        CellWidth::Unary(self.name().chars().count())
    }
}

impl<'a> From<&'a Account<'_>> for Box<dyn Cell + 'a> {
    fn from(account: &'a Account) -> Self {
        let mut wrapping_account =
            PolicyWrappingCell::new(Box::new(account), WrapPolicy::AfterStr(":"));
        wrapping_account.set_wrap_ease(WrapEase::Eager);

        Box::new(StyledCell::new(
            Box::new(wrapping_account),
            Style::default().with_fg(Colour::Blue),
        ))
    }
}

#[cfg(test)]
mod tests {}
