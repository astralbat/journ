/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::err;
use crate::error::JournResult;
use crate::journal::Journal;
use crate::report::expr::parser::AggKind;
use crate::report::expr::{
    ColumnValue, Expr, GroupKey, GroupState, IdentifierContext, LateContext,
};
use std::collections::HashMap;
use std::collections::hash_map::Entry;

pub struct Plan<'h> {
    column_spec: Vec<Expr<'h>>,
    group_by: Vec<Expr<'h>>,
    // Any extra fields that need be evaluated that aren't columns
    additional: HashMap<&'static str, Expr<'h>>,
    agg_functions: Vec<AggKind<'h>>,
    sort_exprs: Vec<Expr<'h>>,
    sort_ascending: bool,
}

impl<'h> Plan<'h> {
    pub fn new(
        column_spec: Vec<Expr<'h>>,
        group_by: Vec<Expr<'h>>,
        additional: HashMap<&'static str, Expr<'h>>,
        agg_functions: Vec<AggKind<'h>>,
        sort_exprs: Vec<Expr<'h>>,
        sort_ascending: bool,
    ) -> Self {
        Plan { column_spec, group_by, agg_functions, additional, sort_exprs, sort_ascending }
    }

    pub fn column_expressions(&self) -> &[Expr<'h>] {
        &self.column_spec
    }

    pub fn agg_functions(&self) -> &[AggKind<'h>] {
        &self.agg_functions
    }

    pub fn group_by(&self) -> &[Expr<'h>] {
        &self.group_by
    }

    /// Checks whether aggregation functions are used correctly in the plan.
    /// - If `group_by` is empty, aggregates cannot be mixed with non-aggregates.
    /// - If `group_by` is not empty, all identifiers in `column_spec` must either appear in `group_by`,
    ///   refer to an alias of another column or be used within an aggregation function.
    /// - Nested aggregation functions are not allowed.
    pub fn validate(&self) -> JournResult<()> {
        if self.group_by.is_empty() {
            let is_agg = |expr: &Expr| expr.iter().any(|e| matches!(e, Expr::AggFunction { .. }));
            if self.column_spec.iter().chain(self.additional.values()).any(is_agg) {
                if !self.column_spec.iter().all(Self::validate_expr_is_agg_or_no_identifiers) {
                    return Err(err!(
                        "Aggregate functions cannot be mixed with identifiers unless using --group-by"
                    ));
                }
            }
            Ok(())
        } else {
            self.validate_no_nested_aggregates()?;
            Ok(())
        }
    }

    fn validate_expr_is_agg_or_no_identifiers(expr: &Expr) -> bool {
        if matches!(expr, Expr::Identifier(_)) {
            false
        } else if matches!(expr, Expr::AggFunction { .. }) {
            true
        } else {
            for c in expr.children() {
                if !Self::validate_expr_is_agg_or_no_identifiers(c) {
                    return false;
                }
            }
            true
        }
    }

    fn validate_no_nested_aggregates(&self) -> JournResult<()> {
        for expr in self.column_spec.iter().chain(self.additional.values()) {
            if expr.iter().any(|e| {
                matches!(e, Expr::AggFunction { .. })
                    && e.children().any(|inner| matches!(inner, Expr::AggFunction { .. }))
            }) {
                return Err(err!("Nested aggregate functions are not allowed"));
            }
        }
        Ok(())
    }

    pub fn execute<'e, 'j, E, F, C>(
        &self,
        journ: &'j Journal<'h>,
        items: impl Iterator<Item = E>,
        context_fn: F,
    ) -> JournResult<Vec<RowData<'h>>>
    where
        C: IdentifierContext<'h> + 'e,
        F: FnMut(E) -> C,
        E: 'e,
    {
        // Either we are executing in a grouping way or we are not. These two modes cannot
        // be mixed. Also, when aggregate functions have been specified but no group-by clause -
        // the whole dataset effectively becomes a single group.
        if !self.group_by.is_empty() || !self.agg_functions.is_empty() {
            self.execute_with_groups(journ, items, context_fn)
        } else {
            self.execute_without_groups(items, context_fn)
        }
    }

    pub fn execute_to_groups<'e, E, F, C>(
        &self,
        items: impl Iterator<Item = E>,
        mut context_fn: F,
        mut total_group: Option<&mut GroupState<'h>>,
    ) -> JournResult<HashMap<GroupKey<'h>, GroupState<'h>>>
    where
        C: IdentifierContext<'h> + 'e,
        F: FnMut(E) -> C,
        E: 'e,
    {
        // Aggregate events into groups
        let mut groups: HashMap<GroupKey, GroupState> = HashMap::new();
        for item in items {
            let mut context = context_fn(item);

            let key = GroupKey::new(
                self.group_by()
                    .iter()
                    .map(|e| {
                        e.eval(&mut context)
                            .map_err(|e| err!("Unable to create key from context").with_source(e))
                            .map(|v| (e.clone(), v))
                    })
                    .collect::<JournResult<Vec<_>>>()?,
            );
            let group = match groups.entry(key) {
                Entry::Occupied(e) => e.into_mut(),
                Entry::Vacant(e) => e.insert(GroupState::new(self)?),
            };
            group.add(&mut context)?;
            if let Some(total) = total_group.as_mut() {
                total.add(&mut context)?;
            }
        }
        Ok(groups)
    }

    fn execute_with_groups<'e, 'j, E, F, C>(
        &self,
        journ: &'j Journal<'h>,
        items: impl Iterator<Item = E>,
        context_fn: F,
    ) -> JournResult<Vec<RowData<'h>>>
    where
        C: IdentifierContext<'h> + 'e,
        F: FnMut(E) -> C,
        E: 'e,
    {
        let groups = self.execute_to_groups(items, context_fn, None)?;

        let mut rows = Vec::new();
        for (key, group) in groups {
            let mut context = LateContext::new(journ, key.clone(), group.finalize());
            let mut row_data = RowData::default();

            for sort_res in self.sort_exprs.iter().map(|k| {
                k.eval(&mut context)
                    .map_err(|e| err!(e; "Unable to evaluate sort key from context"))
            }) {
                row_data.push_sort_value(sort_res?);
            }

            for (additional_key, col) in self
                .column_expressions()
                .iter()
                .map(|e| (None, e))
                .chain(self.additional.iter().map(|(k, v)| (Some(*k), v)))
            {
                // Get the value from the group key if possible, otherwise evaluate the expression
                let value = key
                    .get(col)
                    .cloned()
                    .ok_or_else(|| err!("Unable to get group key for column: '{}'", col))
                    .or_else(|_| {
                        col.eval(&mut context).map_err(|e| {
                            err!("Unable to evaluate column: '{}'", col).with_source(e)
                        })
                    })?;
                match additional_key {
                    Some(key) => row_data.insert_additional(key, value),
                    None => row_data.push_column_value(value),
                }
            }
            rows.insert(self.row_insert_pos(&rows, &row_data), row_data);
        }
        Ok(rows)
    }

    fn execute_without_groups<'e, E, F, C>(
        &self,
        items: impl Iterator<Item = E>,
        mut context_fn: F,
    ) -> JournResult<Vec<RowData<'h>>>
    where
        C: IdentifierContext<'h> + 'e,
        F: FnMut(E) -> C,
        E: 'e,
    {
        let mut rows = Vec::new();
        for item in items {
            let mut context = context_fn(item);
            let mut row_data = RowData::default();

            for eval_res in self.sort_exprs.iter().map(|k| {
                k.eval(&mut context)
                    .map_err(|e| err!(e; "Unable to evaluate sort key from context"))
            }) {
                row_data.push_sort_value(eval_res?);
            }

            for (additional_key, col) in self
                .column_expressions()
                .iter()
                .map(|e| (None, e))
                .chain(self.additional.iter().map(|(k, v)| (Some(*k), v)))
            {
                let value = col
                    .eval(&mut context)
                    .map_err(|e| err!("Unable to evaluate column: '{}'", col).with_source(e))?;

                match additional_key {
                    Some(key) => row_data.insert_additional(key, value),
                    None => row_data.push_column_value(value),
                }
            }
            rows.insert(self.row_insert_pos(&rows, &row_data), row_data);
        }
        Ok(rows)
    }

    fn row_insert_pos(&self, rows: &Vec<RowData<'h>>, row: &RowData<'h>) -> usize {
        // No sort expression specified; insert at the end.
        if self.sort_exprs.is_empty() {
            return rows.len();
        }
        match rows.binary_search_by(|r: &RowData<'h>| {
            let cmp = r.sort_values.cmp(&row.sort_values);
            if !self.sort_ascending { cmp.reverse() } else { cmp }
        }) {
            Ok(i) | Err(i) => i,
        }
    }
}

#[derive(Default)]
pub struct RowData<'h> {
    pub column_values: Vec<ColumnValue<'h>>,
    pub additional: HashMap<&'static str, ColumnValue<'h>>,
    pub sort_values: Vec<ColumnValue<'h>>,
}
impl<'h> RowData<'h> {
    pub fn push_column_value(&mut self, val: ColumnValue<'h>) {
        self.column_values.push(val)
    }

    pub fn push_sort_value(&mut self, val: ColumnValue<'h>) {
        self.sort_values.push(val)
    }

    pub fn insert_additional(&mut self, key: &'static str, value: ColumnValue<'h>) {
        self.additional.insert(key, value);
    }

    pub fn remove_additional_value(&mut self, key: &'static str) -> Option<ColumnValue<'h>> {
        self.additional.remove(key)
    }
}
