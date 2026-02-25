/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::configuration::Configuration;
use crate::datetime::JDateTime;
use crate::error::JournResult;
use crate::journal::Journal;
use crate::journal_entry::JournalEntry;
use crate::posting::Posting;
use crate::report::expr::{ColumnValue, GroupKey};
use crate::valuer::{SystemValuer, Valuer};
use smartstring::alias::String as SS;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::sync::Arc;

pub trait EvalContext<'h> {
    fn config(&self) -> &Configuration<'h>;

    fn as_valuer_context(&self) -> Option<&dyn ValuerContext<'h>> {
        None
    }

    fn as_valuer_context_mut(&mut self) -> Option<&mut dyn ValuerContext<'h>> {
        None
    }

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

pub trait ValuerContext<'h>: IdentifierContext<'h> {
    fn valuer<'a>(&'a self, date: Option<JDateTime>) -> JournResult<Box<dyn Valuer<'h> + 'a>>
    where
        'h: 'a,
    {
        match date {
            Some(date) => Ok(Box::new(SystemValuer::on_date(self.config().clone(), date))),
            None => Ok(Box::new(SystemValuer::on_date(self.config().clone(), JDateTime::now()))),
        }
    }
}

pub struct LateContext<'h, 'j> {
    journal: &'j Journal<'h>,
    aggregate_values: Vec<ColumnValue<'h>>,
    group_key: GroupKey<'h>,
    variables: HashMap<SS, ColumnValue<'h>>,
}
impl<'h, 'j> LateContext<'h, 'j> {
    pub fn new(
        journal: &'j Journal<'h>,
        group_key: GroupKey<'h>,
        aggregate_values: Vec<ColumnValue<'h>>,
    ) -> LateContext<'h, 'j> {
        let mut context =
            LateContext { journal, aggregate_values, group_key, variables: HashMap::new() };

        // Make the --group-by aliases available as identifiers in the context. E.g. -o ALIAS
        let aliases_and_values = context
            .group_key
            .aliases()
            .map(|(a, v)| (a.to_string(), v.clone()))
            .chain(context.group_key.values().iter().map(|(e, v)| (e.to_string(), v.clone())))
            .collect::<Vec<_>>();
        for (alias, val) in aliases_and_values {
            context.set_identifier(&alias, val.clone());
        }
        context
    }
}
impl<'h, 'j> EvalContext<'h> for LateContext<'h, 'j> {
    fn config(&self) -> &Configuration<'h> {
        self.journal.config()
    }

    fn as_valuer_context(&self) -> Option<&dyn ValuerContext<'h>> {
        Some(self)
    }

    fn as_valuer_context_mut(&mut self) -> Option<&mut dyn ValuerContext<'h>> {
        Some(self)
    }

    fn eval_aggregate(&self, index: usize) -> Option<ColumnValue<'h>> {
        self.aggregate_values.get(index).cloned()
    }
}
impl<'h, 'j> IdentifierContext<'h> for LateContext<'h, 'j> {
    fn variables(&self) -> &HashMap<SS, ColumnValue<'h>> {
        &self.variables
    }

    fn variables_mut(&mut self) -> &mut HashMap<SS, ColumnValue<'h>> {
        &mut self.variables
    }

    fn eval_identifier(&self, identifier: &str) -> Option<ColumnValue<'h>> {
        let mut split_dot = identifier.splitn(2, '.');
        let left_ident = split_dot.next().unwrap();

        let mut eval_next = |obj: ColumnValue<'h>| match split_dot.next() {
            Some(right_ident) => obj.eval_identifier(right_ident),
            None => Some(obj),
        };

        if let Some(val) = self.variables.get(left_ident.to_lowercase().as_str()).cloned() {
            eval_next(val)
        } else {
            None
        }
    }
}

impl<'h> ValuerContext<'h> for LateContext<'h, '_> {}

pub struct TotalContext<'h, 'j> {
    journal: &'j Journal<'h>,
    aggregate_values: Vec<ColumnValue<'h>>,
    variables: HashMap<SS, ColumnValue<'h>>,
}
impl<'h, 'j> TotalContext<'h, 'j> {
    pub fn new(journal: &'j Journal<'h>, aggregate_values: Vec<ColumnValue<'h>>) -> Self {
        TotalContext { journal, aggregate_values, variables: HashMap::new() }
    }
}
impl<'h> EvalContext<'h> for TotalContext<'h, '_> {
    fn config(&self) -> &Configuration<'h> {
        self.journal.config()
    }

    fn as_valuer_context(&self) -> Option<&dyn ValuerContext<'h>> {
        Some(self)
    }
    fn as_valuer_context_mut(&mut self) -> Option<&mut dyn ValuerContext<'h>> {
        Some(self)
    }

    fn eval_aggregate(&self, index: usize) -> Option<ColumnValue<'h>> {
        self.aggregate_values.get(index).cloned()
    }
}
impl<'h> IdentifierContext<'h> for TotalContext<'h, '_> {
    fn variables(&self) -> &HashMap<SS, ColumnValue<'h>> {
        &self.variables
    }

    fn variables_mut(&mut self) -> &mut HashMap<SS, ColumnValue<'h>> {
        &mut self.variables
    }
}

impl<'h> ValuerContext<'h> for TotalContext<'h, '_> {}

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

    pub fn journal(&self) -> &'h Journal<'h> {
        self.journal
    }

    pub fn entry(&self) -> &JournalEntry<'h> {
        self.entry
    }

    pub fn posting(&self) -> &Posting<'h> {
        self.posting
    }
}

impl<'h> EvalContext<'h> for PostingContext<'h> {
    fn config(&self) -> &Configuration<'h> {
        self.journal.config()
    }

    fn as_valuer_context(&self) -> Option<&dyn ValuerContext<'h>> {
        Some(self)
    }

    fn as_valuer_context_mut(&mut self) -> Option<&mut dyn ValuerContext<'h>> {
        Some(self)
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
            Some(ColumnValue::Description(self.entry.description().into()))
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

impl<'h> ValuerContext<'h> for PostingContext<'h> {
    fn valuer<'a>(&'a self, datetime: Option<JDateTime>) -> JournResult<Box<dyn Valuer<'h> + 'a>>
    where
        'h: 'a,
    {
        let sys_valuer = match datetime {
            Some(datetime) => SystemValuer::on_date(self.journal().config().clone(), datetime),
            None => SystemValuer::from(self.entry()),
        };
        Ok(Box::new(sys_valuer))
    }
}

#[macro_export]
macro_rules! eval_identifier {
    ($identifier:expr, $eval_ty:ty, $enum_ident:ident, $($member:pat $(if $part:expr)? => $handler:expr),+) => {{
        let mut split_dot = $identifier.splitn(2, '.');
        let left_ident = split_dot.next().unwrap();

        let mut eval_next = |obj: $eval_ty| match split_dot.next() {
            Some(right_ident) => obj.eval_identifier(right_ident),
            None => obj.eval_identifier(""),
        };

        let res = {
            match $enum_ident {
                $(
                    $member $(if left_ident.eq_ignore_ascii_case($part))? => {
                        eval_next($handler)
                    }
                )+,
                _ => None
            }
        };
        res
    }};

    ($identifier:expr, $eval_ty:ty, $($part:expr => $handler:expr),+) => {{
        let mut split_dot = $identifier.splitn(2, '.');
        let left_ident = split_dot.next().unwrap();

        let mut eval_next = |obj: $eval_ty| match split_dot.next() {
            Some(right_ident) => obj.eval_identifier(right_ident),
            None => obj.eval_identifier(""),
        };

        let res = 'matcher: {
            $(
                if left_ident.eq_ignore_ascii_case($part) {
                    break 'matcher eval_next($handler);
                }
            )+ else {
                None
            }
        };
        res
    }};
}
