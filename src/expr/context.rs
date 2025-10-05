/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::{ColumnValue, GroupKey};
use journ_core::journal::Journal;
use journ_core::journal_entry::JournalEntry;
use journ_core::posting::Posting;
use smallvec::SmallVec;
use smartstring::alias::String as SS;
use std::ascii::AsciiExt;
use std::sync::Arc;

pub trait EvalContext<'h> {
    fn journal(&self) -> &Journal<'h>;

    fn as_posting_context(&self) -> Option<&PostingContext<'h>> {
        None
    }

    fn eval_aggregate(&self, _index: usize) -> Option<ColumnValue<'h>> {
        None
    }
}

pub trait IdentifierContext<'h>: EvalContext<'h> {
    fn eval_identifier(&self, identifier: &str) -> Option<ColumnValue<'h>>;

    fn set_identifier(&mut self, identifier: &str, value: ColumnValue<'h>);
}

pub struct LateContext<'h> {
    journal: &'h Journal<'h>,
    aggregate_values: Vec<ColumnValue<'h>>,
    group_key: GroupKey<'h>,
    variables: SmallVec<[(SS, ColumnValue<'h>); 1]>,
}
impl<'h> LateContext<'h> {
    pub fn new(
        journal: &'h Journal<'h>,
        group_key: GroupKey<'h>,
        aggregate_values: Vec<ColumnValue<'h>>,
    ) -> LateContext<'h> {
        let mut context =
            LateContext { journal, aggregate_values, group_key, variables: SmallVec::new() };

        // Make the --group-by aliases available as identifiers in the context. E.g. -o ALIAS
        let aliases_and_values =
            context.group_key.aliases().map(|(a, v)| (a.clone(), v.clone())).collect::<Vec<_>>();
        for (alias, val) in aliases_and_values {
            context.set_identifier(alias, val.clone());
        }
        context
    }

    pub fn group_key(&self) -> &GroupKey<'h> {
        &self.group_key
    }

    pub fn aggregate_values(&self) -> &[ColumnValue<'h>] {
        &self.aggregate_values
    }
}
impl<'h> EvalContext<'h> for LateContext<'h> {
    fn journal(&self) -> &Journal<'h> {
        self.journal
    }

    fn eval_aggregate(&self, index: usize) -> Option<ColumnValue<'h>> {
        self.aggregate_values.get(index).cloned()
    }
}
impl<'h> IdentifierContext<'h> for LateContext<'h> {
    fn eval_identifier(&self, identifier: &str) -> Option<ColumnValue<'h>> {
        self.variables.iter().find_map(|(s, v)| {
            if s.eq_ignore_ascii_case(identifier) { Some(v.clone()) } else { None }
        })
    }

    fn set_identifier(&mut self, identifier: &str, _value: ColumnValue<'h>) {
        if let Some((_, v)) =
            self.variables.iter_mut().find(|(k, _)| k.eq_ignore_ascii_case(identifier))
        {
            *v = _value;
        } else {
            self.variables.push((identifier.into(), _value));
        }
    }
}

pub struct TotalContext<'h> {
    journal: &'h Journal<'h>,
    aggregate_values: Vec<ColumnValue<'h>>,
}
impl<'h> TotalContext<'h> {
    pub fn new(
        journal: &'h Journal<'h>,
        aggregate_values: Vec<ColumnValue<'h>>,
    ) -> TotalContext<'h> {
        TotalContext { journal, aggregate_values }
    }
}
impl<'h> EvalContext<'h> for TotalContext<'h> {
    fn journal(&self) -> &Journal<'h> {
        self.journal
    }

    fn eval_aggregate(&self, index: usize) -> Option<ColumnValue<'h>> {
        self.aggregate_values.get(index).cloned()
    }
}
impl<'h> IdentifierContext<'h> for TotalContext<'h> {
    fn eval_identifier(&self, _identifier: &str) -> Option<ColumnValue<'h>> {
        Some(ColumnValue::Undefined)
    }
    fn set_identifier(&mut self, _identifier: &str, _value: ColumnValue<'h>) {
        // No-op
    }
}

pub struct PostingContext<'h> {
    journal: &'h Journal<'h>,
    entry: &'h JournalEntry<'h>,
    posting: &'h Posting<'h>,
    variables: SmallVec<[(SS, ColumnValue<'h>); 1]>,
}
impl<'h> PostingContext<'h> {
    pub fn new(
        journal: &'h Journal<'h>,
        entry: &'h JournalEntry<'h>,
        posting: &'h Posting<'h>,
    ) -> PostingContext<'h> {
        PostingContext { journal, entry, posting, variables: SmallVec::new() }
    }

    pub fn entry(&self) -> &JournalEntry<'h> {
        self.entry
    }

    pub fn posting(&self) -> &Posting<'h> {
        self.posting
    }
}

impl<'h> EvalContext<'h> for PostingContext<'h> {
    fn journal(&self) -> &Journal<'h> {
        self.journal
    }

    fn as_posting_context(&self) -> Option<&PostingContext<'h>> {
        Some(self)
    }
}

impl<'h> IdentifierContext<'h> for PostingContext<'h> {
    fn eval_identifier(&self, identifier: &str) -> Option<ColumnValue<'h>> {
        if identifier.eq_ignore_ascii_case("account") {
            Some(ColumnValue::Account(Arc::clone(self.posting.account())))
        } else if identifier.eq_ignore_ascii_case("date") {
            Some(ColumnValue::Date(self.entry.date_and_time().datetime_range().start().date()))
        } else if identifier.eq_ignore_ascii_case("datetime_from") {
            Some(ColumnValue::Datetime(self.entry.date_and_time().datetime_range().start()))
        } else if identifier.eq_ignore_ascii_case("datetime_to") {
            Some(ColumnValue::Datetime(self.entry.date_and_time().datetime_range().end()))
        } else if identifier.eq_ignore_ascii_case("datetime_mid") {
            Some(ColumnValue::Datetime(self.entry.date_and_time().average()))
        } else if identifier.eq_ignore_ascii_case("description") {
            Some(ColumnValue::Description(self.entry.description()))
        } else if identifier.eq_ignore_ascii_case("amount") {
            Some(ColumnValue::Amount(self.posting.amount()))
        } else if identifier.starts_with('+') {
            Some(
                self.entry
                    .metadata()
                    .find(|m| m.key() == &identifier[1..])
                    .and_then(|m| m.value().map(ColumnValue::StringRef))
                    .unwrap_or(ColumnValue::Undefined),
            )
        } else if let Some((_var, value)) =
            self.variables.iter().find(|(k, _)| (*k).eq_ignore_ascii_case(identifier))
        {
            Some(value.clone())
        } else {
            None
        }
    }

    fn set_identifier(&mut self, identifier: &str, value: ColumnValue<'h>) {
        if let Some((_, v)) = self.variables.iter_mut().find(|(k, _)| k == identifier) {
            *v = value;
        } else {
            self.variables.push((identifier.into(), value));
        }
    }
}
