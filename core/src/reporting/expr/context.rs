/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::journal::Journal;
use crate::journal_entry::JournalEntry;
use crate::posting::Posting;
use crate::reporting::expr::{ColumnValue, GroupKey};
use smartstring::alias::String as SS;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::sync::Arc;

pub trait EvalContext<'h> {
    fn journal(&self) -> &Journal<'h>;

    fn as_posting_context(&self) -> Option<&PostingContext<'h>> {
        None
    }

    fn as_posting_context_mut(&mut self) -> Option<&mut PostingContext<'h>> {
        None
    }

    fn eval_aggregate(&self, _index: usize) -> Option<ColumnValue<'h>> {
        None
    }
}

pub trait IdentifierContext<'h>: EvalContext<'h> {
    fn variables(&self) -> &HashMap<SS, ColumnValue<'h>>;

    fn variables_mut(&mut self) -> &mut HashMap<SS, ColumnValue<'h>>;

    fn eval_identifier(&self, identifier: &str) -> Option<ColumnValue<'h>> {
        self.variables().get(identifier.to_lowercase().as_str()).cloned()
    }

    fn set_identifier(&mut self, identifier: &str, value: ColumnValue<'h>) {
        self.variables_mut().insert(identifier.to_lowercase().into(), value);
    }

    fn append_identifier(&mut self, identifier: &str, value: ColumnValue<'h>) {
        let entry = self.variables_mut().entry(identifier.to_lowercase().into());
        match entry {
            Entry::Occupied(mut o) => {
                let existing = o.get_mut();
                match existing {
                    ColumnValue::List(vec) => {
                        value.as_list().iter().for_each(|v| vec.push(v.clone()));
                    }
                    _ => {
                        let mut vec = Vec::new();
                        vec.push(existing.clone());
                        value.as_list().iter().for_each(|v| vec.push(v.clone()));
                        *existing = ColumnValue::List(vec);
                    }
                }
            }
            Entry::Vacant(v) => {
                v.insert(value);
            }
        }
    }
}

pub struct LateContext<'h> {
    journal: &'h Journal<'h>,
    aggregate_values: Vec<ColumnValue<'h>>,
    group_key: GroupKey<'h>,
    variables: HashMap<SS, ColumnValue<'h>>,
}
impl<'h> LateContext<'h> {
    pub fn new(
        journal: &'h Journal<'h>,
        group_key: GroupKey<'h>,
        aggregate_values: Vec<ColumnValue<'h>>,
    ) -> LateContext<'h> {
        let mut context =
            LateContext { journal, aggregate_values, group_key, variables: HashMap::new() };

        // Make the --group-by aliases available as identifiers in the context. E.g. -o ALIAS
        let aliases_and_values =
            context.group_key.aliases().map(|(a, v)| (a, v.clone())).collect::<Vec<_>>();
        for (alias, val) in aliases_and_values {
            context.set_identifier(alias, val.clone());
        }
        context
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
    fn variables(&self) -> &HashMap<SS, ColumnValue<'h>> {
        &self.variables
    }

    fn variables_mut(&mut self) -> &mut HashMap<SS, ColumnValue<'h>> {
        &mut self.variables
    }
}

pub struct TotalContext<'h> {
    journal: &'h Journal<'h>,
    aggregate_values: Vec<ColumnValue<'h>>,
    variables: HashMap<SS, ColumnValue<'h>>,
}
impl<'h> TotalContext<'h> {
    pub fn new(
        journal: &'h Journal<'h>,
        aggregate_values: Vec<ColumnValue<'h>>,
    ) -> TotalContext<'h> {
        TotalContext { journal, aggregate_values, variables: HashMap::new() }
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
    fn variables(&self) -> &HashMap<SS, ColumnValue<'h>> {
        &self.variables
    }

    fn variables_mut(&mut self) -> &mut HashMap<SS, ColumnValue<'h>> {
        &mut self.variables
    }
}

pub struct PostingContext<'h> {
    journal: &'h Journal<'h>,
    entry: &'h JournalEntry<'h>,
    posting: &'h Posting<'h>,
    variables: HashMap<SS, ColumnValue<'h>>,
}
impl<'h> PostingContext<'h> {
    pub fn new(
        journal: &'h Journal<'h>,
        entry: &'h JournalEntry<'h>,
        posting: &'h Posting<'h>,
    ) -> PostingContext<'h> {
        PostingContext { journal, entry, posting, variables: HashMap::new() }
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

    fn as_posting_context_mut(&mut self) -> Option<&mut PostingContext<'h>> {
        Some(self)
    }
}

impl<'h> IdentifierContext<'h> for PostingContext<'h> {
    fn variables(&self) -> &HashMap<SS, ColumnValue<'h>> {
        &self.variables
    }

    fn variables_mut(&mut self) -> &mut HashMap<SS, ColumnValue<'h>> {
        &mut self.variables
    }

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
                    .and_then(|m| m.value().map(|m| ColumnValue::StringRef(m)))
                    .unwrap_or(ColumnValue::Undefined),
            )
        } else {
            self.variables.get(identifier.to_lowercase().as_str()).cloned()
        }
    }
}
