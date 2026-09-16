/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::datetime::JDateTime;
use crate::error::JournResult;
use crate::journal_context::JContext;
use crate::journal_entry::JournalEntry;
use crate::journal_entry_flow::{Flow, LinkedFlow};
use crate::posting::Posting;
use crate::report::expr::{ColumnValue, GroupKey};
use crate::valuer::{SystemValuer, Valuer};
use smartstring::alias::String as SS;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::sync::Arc;

#[macro_export]
macro_rules! _eval_call_handler {
    // $part IS present: use handler as-is (guard already filters by left_ident)
    ($eval_next:expr, $handler:expr, $left_ident:expr, $part:expr) => {
        $eval_next($handler)
    };
    // $part is NOT present: pass left_ident into the handler
    ($eval_next:expr, $handler:expr, $left_ident:expr,) => {
        $handler($left_ident).and_then(|r| $eval_next(r))
    };
}

/// Evaluates an identifier in `obj.property` syntax. The macro takes an identifier string, the type of the object to evaluate on,
/// and a series of patterns and handlers. It splits the identifier on the first dot, matches the left part against
/// the provided patterns, and if a match is found, evaluates the right part (if any) on the corresponding handler object.
/// If no match is found, it returns None.
///
/// # Examples
/// `eval_identifier("account.name", ColumnValue)
#[macro_export]
macro_rules! eval_identifier {
    ($identifier:expr, $eval_ty:ty, $enum_ident:ident, $($member:pat $(if $part:expr)? => $handler:expr),+) => {{
        use $crate::_eval_call_handler;

        let split_times = if $identifier.starts_with('+') { 1 } else { 2 };
        let mut split_dot = $identifier.splitn(split_times, '.');
        let left_ident = split_dot.next().unwrap();

        let mut eval_next = |obj: $eval_ty| match split_dot.next() {
            Some(right_ident) => obj.eval_identifier(right_ident),
            None => obj.eval_identifier(""),
        };

        let res = {
            match $enum_ident {
                $(
                    $member $(if left_ident.eq_ignore_ascii_case($part))? => {
                        _eval_call_handler!(eval_next, $handler, left_ident, $($part)?)
                    }
                )+,
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

        let res = {
            #[allow(unreachable_patterns)]
            match left_ident {
                $(
                    s if s.eq_ignore_ascii_case($part) => {
                        eval_next($handler)
                    }
                )+,
                _ => None
            }
        };

        /*
        let res = 'matcher: {
            $(
                if left_ident.eq_ignore_ascii_case($part) {
                    break 'matcher eval_next($handler);
                }
            )+ else {
                None
            }
        };*/
        res
    }};
}

pub trait EvalContext<'h, 'a> {
    fn as_valuer_context(&self) -> Option<&dyn ValuerContext<'h, 'a>> {
        None
    }

    fn as_valuer_context_mut(&mut self) -> Option<&mut dyn ValuerContext<'h, 'a>> {
        None
    }

    fn as_posting_context(&self) -> Option<&PostingContext<'h, '_>> {
        None
    }

    fn as_posting_context_mut(&mut self) -> Option<&mut PostingContext<'h, 'a>> {
        None
    }

    fn eval_aggregate(&self, _index: usize) -> Option<ColumnValue<'h>> {
        None
    }
}

pub trait IdentifierContext<'h, 'a>: EvalContext<'h, 'a> {
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

pub trait ValuerContext<'h, 'p>: IdentifierContext<'h, 'p> {
    fn valuer<'a>(&'a self, date: Option<JDateTime>) -> JournResult<Box<dyn Valuer<'h> + 'a>>
    where
        'h: 'a,
    {
        match date {
            Some(date) => Ok(Box::new(SystemValuer::on_date(
                JContext::get().journal().config().clone(),
                date,
            ))),
            None => Ok(Box::new(SystemValuer::on_date(
                JContext::get().journal().config().clone(),
                JDateTime::now(),
            ))),
        }
    }
}

pub struct LateContext<'h> {
    aggregate_values: Vec<ColumnValue<'h>>,
    group_key: GroupKey<'h>,
    variables: HashMap<SS, ColumnValue<'h>>,
}
impl<'h> LateContext<'h> {
    pub fn new(group_key: GroupKey<'h>, aggregate_values: Vec<ColumnValue<'h>>) -> LateContext<'h> {
        let mut context = LateContext { aggregate_values, group_key, variables: HashMap::new() };

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
impl<'h, 'a> EvalContext<'h, 'a> for LateContext<'h> {
    fn as_valuer_context(&self) -> Option<&dyn ValuerContext<'h, 'a>> {
        Some(self)
    }

    fn as_valuer_context_mut(&mut self) -> Option<&mut dyn ValuerContext<'h, 'a>> {
        Some(self)
    }

    fn eval_aggregate(&self, index: usize) -> Option<ColumnValue<'h>> {
        self.aggregate_values.get(index).cloned()
    }
}
impl<'h, 'a> IdentifierContext<'h, 'a> for LateContext<'h> {
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

impl<'h, 'a> ValuerContext<'h, 'a> for LateContext<'h> {}

pub struct TotalContext<'h> {
    aggregate_values: Vec<ColumnValue<'h>>,
    variables: HashMap<SS, ColumnValue<'h>>,
}
impl<'h> TotalContext<'h> {
    pub fn new(aggregate_values: Vec<ColumnValue<'h>>) -> Self {
        TotalContext { aggregate_values, variables: HashMap::new() }
    }
}
impl<'h, 'a> EvalContext<'h, 'a> for TotalContext<'h> {
    fn as_valuer_context(&self) -> Option<&dyn ValuerContext<'h, 'a>> {
        Some(self)
    }
    fn as_valuer_context_mut(&mut self) -> Option<&mut dyn ValuerContext<'h, 'a>> {
        Some(self)
    }

    fn eval_aggregate(&self, index: usize) -> Option<ColumnValue<'h>> {
        self.aggregate_values.get(index).cloned()
    }
}
impl<'h, 'a> IdentifierContext<'h, 'a> for TotalContext<'h> {
    fn variables(&self) -> &HashMap<SS, ColumnValue<'h>> {
        &self.variables
    }

    fn variables_mut(&mut self) -> &mut HashMap<SS, ColumnValue<'h>> {
        &mut self.variables
    }
}

impl<'h, 'a> ValuerContext<'h, 'a> for TotalContext<'h> {}

pub struct PostingContext<'h, 'a> {
    entry: &'a JournalEntry<'h>,
    posting: &'a Posting<'h>,
    variables: HashMap<SS, ColumnValue<'h>>,
}
impl<'h, 'a> PostingContext<'h, 'a> {
    pub fn new(entry: &'a JournalEntry<'h>, posting: &'a Posting<'h>) -> PostingContext<'h, 'a> {
        PostingContext { entry, posting, variables: HashMap::new() }
    }

    pub fn entry(&self) -> &JournalEntry<'h> {
        self.entry
    }

    pub fn posting(&self) -> &'a Posting<'h> {
        self.posting
    }
}

impl<'h, 'a> EvalContext<'h, 'a> for PostingContext<'h, 'a> {
    fn as_valuer_context(&self) -> Option<&dyn ValuerContext<'h, 'a>> {
        Some(self)
    }

    fn as_valuer_context_mut(&mut self) -> Option<&mut dyn ValuerContext<'h, 'a>> {
        Some(self)
    }

    fn as_posting_context(&self) -> Option<&PostingContext<'h, 'a>> {
        Some(self)
    }

    fn as_posting_context_mut(&mut self) -> Option<&mut PostingContext<'h, 'a>> {
        Some(self)
    }
}

impl<'h, 'a> IdentifierContext<'h, 'a> for PostingContext<'h, 'a> {
    fn variables(&self) -> &HashMap<SS, ColumnValue<'h>> {
        &self.variables
    }

    fn variables_mut(&mut self) -> &mut HashMap<SS, ColumnValue<'h>> {
        &mut self.variables
    }

    fn eval_identifier(&self, identifier: &str) -> Option<ColumnValue<'h>> {
        use ColumnValue::*;
        let res = eval_identifier!(identifier, ColumnValue<'h>,
            "account" => Account(Arc::clone(self.posting.account())),
            //"date" => Date(self.entry.datetime_range().start().date()),
            "date" => DatetimeRange(self.entry.datetime_range()),
            "description" => Description(self.entry.description().into()),
            "amount" => Amount(self.posting.amount(), true),
            "file" => {
                JContext::get()
                    .journal()
                    .root()
                    .find_by_node_id(&self.entry.id().parent().unwrap())
                    .unwrap()
                    .nearest_filename()
                    .map(|p| String(p.to_str().unwrap().into()))
                    .unwrap_or(Undefined)
            }
        );
        res.or_else(|| {
            identifier.strip_prefix('+').and_then(|key| {
                self.entry
                    .metadata()
                    .find(|m| m.key() == key)
                    .map(|m| {
                        m.value().map(|v| String(SS::from(v))).unwrap_or_else(|| String(SS::new()))
                    })
                    .or(Some(Undefined))
            })
        })
        .or_else(|| self.variables.get(identifier.to_lowercase().as_str()).cloned())
    }
}

impl<'h, 'p> ValuerContext<'h, 'p> for PostingContext<'h, 'p> {
    fn valuer<'a>(&'a self, datetime: Option<JDateTime>) -> JournResult<Box<dyn Valuer<'h> + 'a>>
    where
        'h: 'a,
    {
        let sys_valuer = match datetime {
            Some(datetime) => {
                SystemValuer::on_date(JContext::get().journal().config().clone(), datetime)
            }
            None => SystemValuer::from(self.entry()),
        };
        Ok(Box::new(sys_valuer))
    }
}

impl<'h> Flow<'h> {
    pub fn eval_identifier(&self, identifier: &str) -> Option<ColumnValue<'h>> {
        use ColumnValue::*;

        eval_identifier!(identifier, ColumnValue<'h>,
            "account" => Account(Arc::clone(self.account_root().unwrap())),
            "amount" => Amount(self.amount(), true),
            "unit" => Unit(self.unit())
        )
    }
}

pub struct LinkedFlowContext<'h, 'a> {
    linked_flow: &'a LinkedFlow<'h>,
    variables: HashMap<SS, ColumnValue<'h>>,
}
impl<'h, 'a> LinkedFlowContext<'h, 'a> {
    pub fn new(linked_flow: &'a LinkedFlow<'h>) -> Self {
        Self { linked_flow, variables: HashMap::new() }
    }
}

impl<'h, 'a> EvalContext<'h, 'a> for LinkedFlowContext<'h, 'a> {
    fn as_valuer_context(&self) -> Option<&dyn ValuerContext<'h, 'a>> {
        None
    }
}

impl<'h, 'a> IdentifierContext<'h, 'a> for LinkedFlowContext<'h, 'a> {
    fn variables(&self) -> &HashMap<SS, ColumnValue<'h>> {
        &self.variables
    }

    fn variables_mut(&mut self) -> &mut HashMap<SS, ColumnValue<'h>> {
        &mut self.variables
    }

    fn eval_identifier(&self, identifier: &str) -> Option<ColumnValue<'h>> {
        let res = eval_identifier!(identifier, &Flow<'h>,
            "linked" => self.linked_flow.linked()
        );

        res.or_else(|| self.linked_flow.flow().eval_identifier(identifier))
    }
}
