/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::err;
use crate::error::JournResult;
use crate::report::expr::column_spec::ColumnSpec;
use crate::report::expr::{
    ColumnValue, Expr, GroupKey, GroupState, IdentifierContext, LateContext, TotalContext,
};
use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, HashMap};
use std::marker::PhantomData;

pub trait BalanceUpdater<'h, E> {
    fn update(&self, prev_rows: &[RowData<'h>], row: &mut RowData<'h>, item: E);
}

impl<'h, E, F> BalanceUpdater<'h, E> for F
where
    F: Fn(&[RowData<'h>], &mut RowData<'h>, E),
{
    fn update(&self, prev_rows: &[RowData<'h>], row: &mut RowData<'h>, item: E) {
        self(prev_rows, row, item);
    }
}

pub struct NullBalanceUpdater<'h, E> {
    _phantom: PhantomData<&'h E>,
}
impl<'h, E> BalanceUpdater<'h, E> for NullBalanceUpdater<'h, E> {
    fn update(&self, _: &[RowData<'h>], _row: &mut RowData<'h>, _item: E) {}
}

pub struct Plan<'h> {
    column_spec: ColumnSpec<'h>,
    where_conditions: Vec<Expr<'h>>,
    group_by: Vec<Expr<'h>>,
    show_total: bool,
    // Any extra fields that need be evaluated that aren't columns
    additional: HashMap<&'static str, Expr<'h>>,
    sort_exprs: Vec<Expr<'h>>,
    sort_ascending: bool,
}

impl<'h> Plan<'h> {
    pub fn new(
        column_spec: ColumnSpec<'h>,
        where_conditions: Vec<Expr<'h>>,
        show_total: bool,
        group_by: Vec<Expr<'h>>,
        additional: HashMap<&'static str, Expr<'h>>,
        sort_exprs: Vec<Expr<'h>>,
        sort_ascending: bool,
    ) -> Self {
        Plan {
            column_spec,
            where_conditions,
            show_total,
            group_by,
            additional,
            sort_exprs,
            sort_ascending,
        }
    }

    /// The specification for the data rows.
    pub fn column_spec(&self) -> &ColumnSpec<'h> {
        &self.column_spec
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
            Ok(())
        } else {
            self.validate_no_nested_aggregates()?;
            Ok(())
        }
    }

    fn validate_no_nested_aggregates(&self) -> JournResult<()> {
        for expr in self.column_spec.exprs().iter().chain(self.additional.values()) {
            if expr.iter().any(|e| {
                matches!(e, Expr::AggFunction { .. })
                    && e.children().any(|inner| matches!(inner, Expr::AggFunction { .. }))
            }) {
                return Err(err!("Nested aggregate functions are not allowed"));
            }
        }
        Ok(())
    }

    pub fn execute<'e, 'j, E, F, B, C>(
        &self,
        items: impl Iterator<Item = E>,
        context_fn: F,
        balance_updater: Option<B>,
    ) -> JournResult<Vec<RowData<'h>>>
    where
        B: BalanceUpdater<'h, E>,
        C: IdentifierContext<'h> + 'e,
        F: FnMut(E) -> C,
        E: Copy + 'e,
    {
        // Either we are executing in a grouping way or we are not. These two modes cannot
        // be mixed. Also, when aggregate functions have been specified but no group-by clause -
        // the whole dataset effectively becomes a single group.
        if !self.group_by.is_empty() || !self.column_spec.agg_functions().is_empty() {
            self.execute_with_groups(items, context_fn)
        } else {
            self.execute_without_groups(items, context_fn, balance_updater)
        }
    }

    pub fn execute_to_groups<'e, E, F, C>(
        &self,
        items: impl Iterator<Item = E>,
        mut context_fn: F,
    ) -> JournResult<(BTreeMap<GroupKey<'h>, GroupState<'h>>, GroupState<'h>)>
    where
        C: IdentifierContext<'h> + 'e,
        F: FnMut(E) -> C,
        E: 'e,
    {
        // Aggregate events into groups
        let mut groups: BTreeMap<GroupKey, GroupState> = BTreeMap::new();
        let mut total_group = GroupState::try_from(self.column_spec.agg_functions())?;
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
                Entry::Vacant(e) => {
                    e.insert(GroupState::try_from(self.column_spec.agg_functions())?)
                }
            };
            group.add(&mut context)?;
            total_group.add(&mut context)?;
        }
        Ok((groups, total_group))
    }

    fn execute_with_groups<'e, 'j, E, F, C>(
        &self,
        items: impl Iterator<Item = E>,
        context_fn: F,
    ) -> JournResult<Vec<RowData<'h>>>
    where
        C: IdentifierContext<'h> + 'e,
        F: FnMut(E) -> C,
        E: Copy + 'e,
    {
        let (groups, total_group) = self.execute_to_groups(items, context_fn)?;

        let mut rows = Vec::new();
        'next_row: for (key, group) in groups {
            let mut context = LateContext::new(key.clone(), group.finalize());
            let mut row_data = RowData::default();

            self.eval_sort_exprs(&mut context, &mut row_data)?;

            for (additional_key, col) in self
                .column_spec()
                .exprs()
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

            for cond in self.where_conditions.iter() {
                match cond.eval(&mut context)? {
                    ColumnValue::Boolean(bool) => {
                        if !bool {
                            continue 'next_row;
                        }
                    }
                    val => {
                        return Err(err!("Unable to evaluate where condition: '{}'", cond)
                            .with_source(err!("Value is not a boolean: '{}'", val)));
                    }
                }
            }

            rows.insert(self.row_insert_pos(&rows, &row_data), row_data);
        }
        // Evaluate total row
        if self.show_total {
            let mut total_context = TotalContext::new(total_group.finalize());
            let mut row_data = RowData::default();
            for col in self.column_spec.exprs() {
                row_data.push_column_value(
                    col.eval(&mut total_context).unwrap_or_else(|_| ColumnValue::StringRef("")),
                );
            }
            rows.push(row_data);
        }
        Ok(rows)
    }

    fn execute_without_groups<'e, E, F, B, C>(
        &self,
        items: impl Iterator<Item = E>,
        mut context_fn: F,
        balance_updater: Option<B>,
    ) -> JournResult<Vec<RowData<'h>>>
    where
        C: IdentifierContext<'h> + 'e,
        F: FnMut(E) -> C,
        B: BalanceUpdater<'h, E>,
        E: Copy + 'e,
    {
        // First Pass - sort the data
        let mut sorted_items = Vec::new();
        'next_item: for item in items {
            let mut context = context_fn(item);
            let mut row_data = RowData::default();

            self.eval_sort_exprs(&mut context, &mut row_data)?;

            for cond in self.where_conditions.iter() {
                match cond.eval(&mut context)? {
                    ColumnValue::Boolean(bool) => {
                        if !bool {
                            continue 'next_item;
                        }
                    }
                    val => {
                        return Err(err!("Unable to evaluate where condition: '{}'", cond)
                            .with_source(err!("Value is not a boolean: '{}'", val)));
                    }
                }
            }

            let insert_pos = self.sorted_row_insert_pos(&sorted_items, &row_data);
            sorted_items.insert(insert_pos, (item, row_data));
        }

        // Second Pass - Update balances and evaluate columns. Column evaluations may depend
        // on balance identifier.
        let mut rows = Vec::new();
        for (item, mut row_data) in sorted_items {
            let mut context = context_fn(item);

            // Update any running balances
            if let Some(ref balance_updater) = balance_updater {
                balance_updater.update(&rows, &mut row_data, item);
                if let Some(running_bals) = &mut row_data.running_balances {
                    for (key, col) in running_bals.iter() {
                        context.set_identifier(key, col.clone());
                    }
                }
            }

            for (additional_key, col) in self
                .column_spec()
                .exprs()
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
            rows.push(row_data);
        }
        Ok(rows)
    }

    fn eval_sort_exprs<C: IdentifierContext<'h>>(
        &self,
        context: &mut C,
        row_data: &mut RowData<'h>,
    ) -> JournResult<()> {
        for sort_res in self.sort_exprs.iter().map(|k| {
            k.eval(context)
                .map_err(|e| err!(e; "Unable to evaluate sort key from context"))
                .and_then(|v| {
                    if v.is_undefined() {
                        Err(err!("Sort key cannot evaluate as undefined: {}", k))
                    } else {
                        Ok(v)
                    }
                })
        }) {
            row_data.push_sort_value(sort_res?);
        }
        Ok(())
    }

    fn sorted_row_insert_pos<E>(&self, rows: &[(E, RowData<'h>)], row: &RowData<'h>) -> usize {
        // No sort expression specified; insert at the end.
        if self.sort_exprs.is_empty() {
            return rows.len();
        }
        match rows.binary_search_by(|r: &(E, RowData<'h>)| {
            let cmp = r.1.sort_values.partial_cmp(&row.sort_values).expect(&format!(
                "{:?} and {:?} to be comparable",
                &r.1.sort_values, &row.sort_values
            ));
            if !self.sort_ascending { cmp.reverse() } else { cmp }
        }) {
            Ok(i) | Err(i) => i,
        }
    }

    fn row_insert_pos(&self, rows: &[RowData<'h>], row: &RowData<'h>) -> usize {
        // No sort expression specified; insert at the end.
        if self.sort_exprs.is_empty() {
            return rows.len();
        }
        match rows.binary_search_by(|r: &RowData<'h>| {
            let cmp = r.sort_values.partial_cmp(&row.sort_values).expect(&format!(
                "{:?} and {:?} to be comparable",
                &r.sort_values, &row.sort_values
            ));
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
    pub running_balances: Option<HashMap<&'static str, ColumnValue<'h>>>,
}
impl<'h> RowData<'h> {
    pub fn column_count(&self) -> usize {
        self.column_values.len()
    }

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

    pub fn running_balance(&self, key: &str) -> Option<&ColumnValue<'h>> {
        self.running_balances.as_ref().and_then(|rb| rb.get(key))
    }

    pub fn set_running_balance(&mut self, key: &'static str, value: ColumnValue<'h>) {
        if self.running_balances.is_none() {
            self.running_balances = Some(HashMap::with_capacity(1));
        }
        self.running_balances.as_mut().unwrap().insert(key, value);
    }
}

/*
trait DataList {
    fn insert_row<'h>(
        &mut self,
        row_data: RowData<'h>,
        ascending: bool,
    ) -> (usize, &mut RowData<'h>);
}

impl<'h> DataList for Vec<RowData<'h>> {
    fn insert_row(&mut self, row: RowData<'h>, ascending: bool) -> (usize, &mut RowData<'h>) {
        // No sort expression specified; insert at the end.
        if row.sort_values.is_empty() {
            self.push(row);
            return (self.len(), self.last_mut().unwrap());
        }

        match self.binary_search_by(|r: &RowData<'h>| {
            let cmp = r.sort_values.cmp(&row.sort_values);
            if !ascending { cmp.reverse() } else { cmp }
        }) {
            Ok(i) | Err(i) => {
                self.insert(i, row);
                (i, self.last_mut().unwrap())
            }
        }
    }
}*/
