/*
 * Copyright (c) 2019-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::account::Account;
use crate::configuration::Configuration;
use crate::datetime::DateTimeFormat;
use crate::journal_entry::JournalEntry;
use crate::journal_node::JournalNode;
use crate::module::ModuleDirectiveObj;
use crate::parsing::text_block::{BlockObject, TextBlock, TextBlockBuf};
use crate::price::Price;
use crate::unit::Unit;
use chrono_tz::Tz;
use std::cmp::Ordering;
use std::fmt::{Debug, Write};
use std::sync::Arc;
//pub type ParsedJournalEntry<'h> = Arc<Mutex<ParsedDirective<'h, JournalEntry<'h>>>>;

#[derive(Debug)]
#[repr(u8)]
pub enum DirectiveKind<'h> {
    Entry(&'h JournalEntry<'h>) = 0,
    Price(Arc<Price<'h>>) = 1,
    Account(Arc<Account<'h>>) = 4,
    Accounts(Arc<Account<'h>>) = 3,
    Unit(&'h Unit<'h>) = 5,
    Units(&'h Unit<'h>) = 2,
    Include(&'h JournalNode<'h>) = 6,
    Branch(&'h JournalNode<'h>) = 7,
    Python(&'h TextBlock<'h>) = 8,
    DateFormat(&'h DateTimeFormat<'h>) = 9,
    TimeFormat(&'h DateTimeFormat<'h>) = 10,
    DateTimeFormat(&'h DateTimeFormat<'h>) = 11,
    TimeZone(Tz) = 12,
    Module(&'h dyn ModuleDirectiveObj) = 13,
    Comment(&'h str) = 14,
}

impl<'h> PartialEq<DirectiveKind<'h>> for DirectiveKind<'h> {
    fn eq(&self, other: &DirectiveKind<'h>) -> bool {
        match (self, other) {
            (DirectiveKind::Entry(entry), DirectiveKind::Entry(other_entry)) => {
                entry == other_entry
            }
            (DirectiveKind::Price(price), DirectiveKind::Price(other_price)) => {
                price == other_price
            }
            (DirectiveKind::Account(account), DirectiveKind::Account(other_account)) => {
                account == other_account
            }
            (DirectiveKind::Unit(unit), DirectiveKind::Unit(other_unit)) => unit == other_unit,
            (DirectiveKind::Units(unit), DirectiveKind::Units(other_unit)) => unit == other_unit,
            (DirectiveKind::Include(include), DirectiveKind::Include(other_include)) => {
                include == other_include
            }
            (DirectiveKind::Branch(branch), DirectiveKind::Branch(other_branch)) => {
                branch == other_branch
            }
            (DirectiveKind::Python(python), DirectiveKind::Python(other_python)) => {
                python == other_python
            }
            (
                DirectiveKind::DateFormat(date_format),
                DirectiveKind::DateFormat(other_date_format),
            ) => date_format == other_date_format,
            (
                DirectiveKind::TimeFormat(time_format),
                DirectiveKind::TimeFormat(other_time_format),
            ) => time_format == other_time_format,
            (DirectiveKind::TimeZone(time_zone), DirectiveKind::TimeZone(other_time_zone)) => {
                time_zone == other_time_zone
            }
            //(DirectiveKind::Cgt(cgt), DirectiveKind::Cgt(other_cgt)) => cgt == other_cgt,
            (DirectiveKind::Module(module_dir), DirectiveKind::Module(_other_module_dir)) => {
                &**module_dir == other
            }
            (DirectiveKind::Comment(comment), DirectiveKind::Comment(other_comment)) => {
                comment == other_comment
            }
            _ => false,
        }
    }
}

/// Some kinds of directive can be compared, for others, it doesn't make sense. This comparison
/// is used for sorting directives in a journal file such as prices, or inserting an entry in
/// date order.
impl PartialOrd for DirectiveKind<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (DirectiveKind::Entry(entry), DirectiveKind::Entry(other_entry)) => {
                Some(entry.cmp(other_entry))
            }
            (DirectiveKind::Price(price), DirectiveKind::Price(other_price)) => {
                Some(price.cmp(other_price))
            }
            (DirectiveKind::Account(account), DirectiveKind::Account(other_account)) => {
                Some(account.cmp(other_account))
            }
            (DirectiveKind::Unit(unit), DirectiveKind::Unit(other_unit)) => {
                Some(unit.cmp(other_unit))
            }
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct Directive<'h> {
    parsed: Option<&'h TextBlock<'h>>,
    kind: DirectiveKind<'h>,
}

impl<'h> Directive<'h> {
    pub fn new(raw: Option<&'h TextBlock<'h>>, kind: DirectiveKind<'h>) -> Self {
        Self { parsed: raw, kind }
    }

    /// Gets the raw text block from which this directive was parsed.
    /// A `None` indicates that the directive was not parsed from a raw text block, but rather
    /// created after parsing the journal.
    pub fn parsed(&self) -> Option<&'h TextBlock<'h>> {
        self.parsed
    }

    pub fn kind(&self) -> &DirectiveKind<'h> {
        &self.kind
    }

    pub fn kind_mut(&mut self) -> &mut DirectiveKind<'h> {
        &mut self.kind
    }

    pub fn into_inner(self) -> (Option<&'h TextBlock<'h>>, DirectiveKind<'h>) {
        (self.parsed, self.kind)
    }
}

impl<'h> PartialEq<Directive<'h>> for Directive<'h> {
    fn eq(&self, other: &Directive<'h>) -> bool {
        self.kind == other.kind
    }
}

impl<'h> Eq for Directive<'h> {}

impl<'h> PartialOrd<Directive<'h>> for Directive<'h> {
    fn partial_cmp(&self, other: &Directive<'h>) -> Option<Ordering> {
        self.kind.partial_cmp(&other.kind)
    }
}

impl BlockObject for Directive<'_> {
    fn write(&self, buf: &mut TextBlockBuf, config: Option<&Configuration>) {
        match self.parsed {
            Some(parsed) => write!(buf, "{}", parsed).unwrap(),
            None => match self.kind() {
                DirectiveKind::Entry(entry) => {
                    buf.write(*entry, config);
                }
                DirectiveKind::Price(price) => {
                    buf.write(&**price, config);
                }
                _ => panic!(
                    "Can't write {:?}. The only objects that can be written are entries and prices; write parsed text for other directives",
                    self.kind()
                ),
            },
        }
    }
}
