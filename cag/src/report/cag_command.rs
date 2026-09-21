/*
 * Copyright (c) 2024-2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::computer::CapitalGainsComputer;
use crate::pool_event::PoolEvent;
use crate::report::cmd_line::CagArguments;
use crate::report::expr::context::CagContext;
use clap::Parser;
use journ_core::configuration::{AccountFilter, Filter, UnitFilter};
use journ_core::error::JournResult;
use journ_core::journal_context::JContext;
use journ_core::module::ModuleCommand;
use journ_core::report::command::arguments::{Arguments, Command, DateTimeFormatCommand};
use journ_core::report::command::chained_result::ChainingResult;
use journ_core::report::command::cmd_line::BeginAndEndCommand;
use journ_core::report::command::table_format_args::TableFormatCommand;
use journ_core::report::command::{ChainableCommand, ExecCommand, IntoExecCommand};
use journ_core::report::expr::parser::parse_plan;
use journ_core::report::expr::{ColumnValue, Expr, RowData, ScalarExpr};
use journ_core::report::table2::{PolicyWrappingCell, Row, RowKind, StyledCell, Table, WrapPolicy};
use journ_core::report::term_style::{Style, Weight};
use journ_core::unit::Unit;
use std::collections::HashMap;
use std::ops::Deref;
use yaml_rust2::{Yaml, yaml};

#[derive(Default, Debug, Clone)]
pub struct CagCommand {
    pub(super) datetime_fmt_cmd: DateTimeFormatCommand,
    pub(super) begin_and_end_cmd: BeginAndEndCommand,
    pub(super) table_fmt_cmd: TableFormatCommand,
    pub(super) account_filter: Vec<String>,
    pub(super) filter: Vec<ScalarExpr>,
    pub(super) unit_filter: Vec<String>,
    pub(super) head: Option<usize>,
    pub(super) tail: Option<usize>,
    pub(super) group_by: Option<String>,
    pub(super) order_by_spec: Option<String>,
    pub(super) order_descending: bool,
    pub(super) output_yaml: bool,
    pub(super) yaml_map_key: Option<String>,
    pub(super) title: Option<String>,
    pub(super) no_header: bool,
    pub(super) no_total: bool,
    pub(super) short_total: bool,
    pub(super) column_spec: Option<String>,
    pub(super) where_conditions: Option<String>,
    pub(super) chain: Option<Box<CagCommand>>,
}

impl CagCommand {
    pub fn begin_and_end_cmd(&self) -> &BeginAndEndCommand {
        &self.begin_and_end_cmd
    }

    pub fn account_filter(&self) -> AccountFilter {
        AccountFilter::new(self.account_filter.iter())
    }

    pub fn unit_filter(&self) -> impl for<'h> Filter<Unit<'h>> + '_ {
        UnitFilter::new(self.unit_filter.iter())
    }

    pub fn column_spec(&self) -> &str {
        self.column_spec.as_deref().unwrap_or(
            "DealDate.Start.Date as Date, Sum(PooledAmount) as Amount, Sum(netProceeds) as \"Net Proceeds\", Sum(Expenses) as \"Expenses\", sum(actualCost) as \"Actual Cost\", sum(match.gain) as Gain/-Loss")
    }

    pub fn create_table<'cell>(&self) -> Table<'cell> {
        Table::from(&self.table_fmt_cmd)
    }

    pub fn append_table<'a, 'h>(
        &'a self,
        mut table: Table<'a>,
        column_expressions: &[Expr],
        data_rows: Vec<RowData<'h>>,
        total_row: Option<RowData<'h>>,
    ) -> Table<'a>
    where
        'h: 'a,
    {
        let chain_pos = JContext::get().chain_position();
        let mut rows = vec![];

        if chain_pos > 0 {
            rows.push(table.create_chain_separator());
        }
        if let Some(title) = &self.title {
            let (title, title_sep) = table.create_title_row(
                title,
                data_rows.iter().map(|r| r.column_count()).max().unwrap_or(1),
            );
            rows.push(title);
            rows.push(title_sep);
        }
        // Heading Row
        if !self.no_header {
            let heading_style = Style::default().with_weight(Weight::Bold);
            let headings = column_expressions
                .iter()
                .map(|col| {
                    StyledCell::new(
                        PolicyWrappingCell::new(col.to_string(), WrapPolicy::Word),
                        heading_style,
                    )
                })
                .collect::<Vec<_>>();
            rows.push(table.create_heading_row(headings));
        }

        for row in data_rows {
            rows.push(Row::new(
                row.column_values.into_iter().map(|v| v.into_cell_ref(false, true)),
            ));
        }
        if let Some(total) = total_row {
            rows.push(table.create_separator_row(
                RowKind::TotalSeparator,
                table.total_separator(),
                column_expressions.len(),
            ));
            let mut total_row = Row::new(
                total.column_values.into_iter().map(|v| v.into_cell_ref(false, !self.short_total)),
            );
            total_row.set_kind(RowKind::Total);
            rows.push(total_row);
        }

        for row in rows {
            table.push_row(row);
        }
        table
    }

    pub fn create_yaml(&self) -> Yaml {
        // Have a single root node, as is conventional for JSON/YAML.
        let mut gains_map = yaml::Hash::new();
        let key = self
            .title
            .as_ref()
            .map(|title| Yaml::String(title.clone()))
            .unwrap_or(Yaml::String("capital_gains".to_string()));
        gains_map.insert(key, Yaml::Array(vec![]));
        Yaml::Hash(gains_map)
    }

    pub fn append_yaml<'h>(
        &self,
        yaml: Yaml,
        column_expressions: &[Expr],
        rows: Vec<RowData<'h>>,
        total: Option<RowData<'h>>,
    ) -> Yaml {
        let root_key = Yaml::String("capital_gains".to_string());

        let mut rows_list = yaml::Array::new();
        let mut rows_map = yaml::Hash::new();
        let mut is_using_map_keys = false;
        for row_data in rows.into_iter().chain(total) {
            let mut row_map = yaml::Hash::new();
            let mut additional = row_data.additional;

            // Keys get inserted in user supplied order.
            for (expr, col) in
                column_expressions.iter().map(ToString::to_string).zip(row_data.column_values)
            {
                let val = col.into_yaml(false);
                // No nulls
                if val != Yaml::Null {
                    Self::do_yaml_insert(&mut row_map, expr.to_string(), val);
                }
            }
            match additional.remove("yaml_map_key") {
                Some(yaml_map_key) => {
                    is_using_map_keys = true;
                    let yaml_key = yaml_map_key.into_yaml(false);
                    if !yaml_key.is_null() {
                        rows_map.insert(yaml_key, Yaml::Hash(row_map));
                    }
                }
                None => rows_list.push(Yaml::Hash(row_map)),
            }
        }
        let data_yaml =
            if is_using_map_keys { Yaml::Hash(rows_map) } else { Yaml::Array(rows_list) };

        let mut yaml_hash = yaml.into_hash().unwrap();
        let key = self
            .title
            .as_ref()
            .map(|title| Yaml::String(title.clone()))
            .unwrap_or(root_key.clone());

        let chain_pos = JContext::get().chain_position();
        if chain_pos == 0 {
            // If this is the first command, we just set it at the root key.
            *yaml_hash.get_mut(&key).unwrap() = data_yaml;
            Yaml::Hash(yaml_hash)
        } else {
            // Otherwise create a new sub_map for these results.
            let index = yaml_hash.len();
            let title_or_index =
                self.title.as_ref().cloned().unwrap_or(yaml_hash.len().to_string());

            // Previous root gets lifted to a sub_map
            if chain_pos == 1 {
                let mut sub_map = yaml::Hash::new();

                // Remove the previous key, value. Change the key if necessary
                let (mut prev_key, prev_value) = yaml_hash.into_iter().next().unwrap();
                if prev_key == root_key {
                    prev_key = Yaml::String((index - 1).to_string());
                }
                // Insert previous key/value into sub_map
                sub_map.insert(prev_key, prev_value);
                // Insert current key/value into sub_map
                sub_map.insert(Yaml::String(title_or_index), data_yaml);
                // Create a new root, having sub_map
                let mut new_root = yaml::Hash::new();
                new_root.insert(root_key.clone(), Yaml::Hash(sub_map));
                Yaml::Hash(new_root)
            } else {
                // Previous root has a map value. We just need to insert the current values
                yaml_hash
                    .get_mut(&root_key)
                    .unwrap()
                    .as_mut_hash()
                    .unwrap()
                    .insert(Yaml::String(title_or_index), data_yaml);
                Yaml::Hash(yaml_hash)
            }
        }
    }

    /// Insert `key_name` and `yaml_value` into `map`.
    ///
    /// If the `key_name` is in format "A.B", then it gets inserted at depth 1 in a map at key 'A'.
    /// # Example
    /// ```
    /// unit.name: Euro
    /// unit.format: #,##0.00
    /// ```
    /// becomes:
    /// ```
    /// unit:
    ///   name: Euro
    ///   format: #,##0.00
    ///
    /// ```
    ///
    /// However, this will only be performed if, in the example, there isn't already a key in the map called 'unit'.
    fn do_yaml_insert(map: &mut yaml::Hash, key_name: String, yaml_value: Yaml) {
        let mut key_split = key_name.splitn(2, '.');
        let prefix = key_split.next().unwrap().to_string();
        let suffix = key_split.next().map(|s| s.to_string());

        match suffix {
            Some(suffix) => match map.get_mut(&Yaml::String(prefix.clone())) {
                Some(value) => match value.as_mut_hash() {
                    Some(inner_map) => {
                        Self::do_yaml_insert(inner_map, suffix, yaml_value);
                    }
                    None => {
                        map.insert(Yaml::String(key_name), yaml_value);
                    }
                },
                None => {
                    let mut inner_map = yaml::Hash::new();
                    Self::do_yaml_insert(&mut inner_map, suffix, yaml_value);
                    map.insert(Yaml::String(prefix), Yaml::Hash(inner_map));
                }
            },
            None => {
                // Insert at key_name. If there was a map at this position, we will flatten.
                // Keys are reinserted in a way that tries to preserve user order.
                if let Some(val) = map.remove(&Yaml::String(key_name.clone()))
                    && let Some(old_map) = val.into_hash()
                {
                    for (key, value) in old_map {
                        map.insert(
                            Yaml::String(key_name.to_string() + "." + key.as_str().unwrap()),
                            value,
                        );
                    }
                }
                map.insert(Yaml::String(key_name.clone()), yaml_value);
            }
        }
    }

    pub fn merge_from(&self, other: &Self) -> Self {
        Self {
            title: self.title.clone().or(other.title.clone()),
            datetime_fmt_cmd: self.datetime_fmt_cmd.merge_from(&other.datetime_fmt_cmd),
            begin_and_end_cmd: self.begin_and_end_cmd.merge_from(&other.begin_and_end_cmd),
            table_fmt_cmd: self.table_fmt_cmd.merge_from(&other.table_fmt_cmd),
            account_filter: if !self.account_filter.is_empty() {
                self.account_filter.clone()
            } else {
                other.account_filter.clone()
            },
            unit_filter: if !self.unit_filter.is_empty() {
                self.unit_filter.clone()
            } else {
                other.unit_filter.clone()
            },
            filter: if !self.filter.is_empty() {
                self.filter.clone()
            } else {
                other.filter.clone()
            },
            group_by: match &self.group_by {
                Some(group_by) if !group_by.is_empty() => Some(group_by.clone()),
                Some(_) => None,
                None => other.group_by.clone(),
            },
            head: self.head.or(other.head),
            tail: self.tail.or(other.tail),
            order_descending: if self.order_descending {
                !other.order_descending
            } else {
                other.order_descending
            },
            no_header: if self.no_header { !other.no_header } else { other.no_header },
            no_total: if self.no_total { !other.no_total } else { other.no_total },
            short_total: if self.short_total { !other.short_total } else { other.short_total },
            yaml_map_key: match &self.yaml_map_key {
                Some(yaml_map_key) if !yaml_map_key.is_empty() => Some(yaml_map_key.clone()),
                Some(_) => None,
                None => other.yaml_map_key.clone(),
            },
            column_spec: self.column_spec.clone().or(other.column_spec.clone()),
            where_conditions: self.where_conditions.clone().or(other.where_conditions.clone()),
            order_by_spec: self.order_by_spec.clone().or(other.order_by_spec.clone()),
            output_yaml: other.output_yaml,
            chain: self.chain.clone(),
        }
    }
}

impl Command for CagCommand {
    fn datetime_fmt_cmd(&self) -> &DateTimeFormatCommand {
        &self.datetime_fmt_cmd
    }

    fn begin_and_end_cmd(&self) -> &BeginAndEndCommand {
        &self.begin_and_end_cmd
    }
}

impl ModuleCommand for CagCommand {
    fn name(&self) -> &'static str {
        "cag"
    }

    fn create(
        &self,
        args: &Arguments,
        command_args: &[String],
    ) -> JournResult<Box<dyn ExecCommand>> {
        CagArguments::parse_from(command_args)
            .into_exec_cmd(args)
            .map(Box::new)
            .map(|boxed| boxed as Box<dyn ExecCommand>)
    }
}

impl ChainableCommand for CagCommand {
    fn next_chain(&self) -> Option<&dyn ChainableCommand> {
        Some(self.chain.as_ref()?.deref())
    }
}

impl ExecCommand for CagCommand {
    fn execute<'h, 'a, 'cell>(
        &self,
        chained: Option<ChainingResult<'h, 'a, 'cell>>,
    ) -> JournResult<()>
    where
        'h: 'cell,
    {
        let mut computer = CapitalGainsComputer::default();
        let capital_gains = computer.compute_gains(&JContext::get().journal())?;

        let mut additional = HashMap::new();
        if let Some(yaml_map_key) = &self.yaml_map_key {
            additional.insert("yaml_map_key", yaml_map_key.as_str());
        }
        let plan = parse_plan(
            self.column_spec(),
            self.where_conditions.as_deref(),
            !self.no_total,
            self.group_by.as_deref(),
            additional,
            self.order_by_spec.as_deref(),
            !self.order_descending,
            None,
            None,
        )?;
        let balance_update_fn = |prev: &[RowData<'h>],
                                 row: &mut RowData<'h>,
                                 event: &PoolEvent<'h>| {
            let bal_diff = (event.balance_after().as_valued_amount()
                - event.balance_before().as_valued_amount())
            .unwrap();

            for prev_row in prev.iter().rev() {
                let bal = prev_row.running_balance("balance").unwrap().as_valued_amount().unwrap();
                if bal.unit() == bal_diff.amount().unit() {
                    let new_bal = (bal + &bal_diff).unwrap();
                    row.set_running_balance("balance", ColumnValue::ValuedAmount(new_bal));
                    return;
                }
            }
            row.set_running_balance("balance", ColumnValue::ValuedAmount(bal_diff));
        };
        let (data, total_row) = plan.execute(
            capital_gains.events().iter().filter_map(move |e| {
                let mut context = CagContext::new(e);
                for filter_expr in &self.filter {
                    match filter_expr.eval(&mut context) {
                        Ok(val) => match val.as_lenient_bool() {
                            false => return None,
                            true => continue,
                        },
                        Err(e) => return Some(Err(e)),
                    }
                }
                Some(Ok(e))
            }),
            None,
            CagContext::new,
            Some(balance_update_fn),
        )?;

        // The data rows are limited by --head and/or --tail.
        let data = if self.head.is_some() || self.tail.is_some() {
            let data_len = data.len();
            data.into_iter()
                .enumerate()
                .filter(|(i, _)| {
                    if let Some(head) = self.head
                        && i < &head
                    {
                        true
                    } else if let Some(tail) = self.tail
                        && &(data_len - tail) <= i
                    {
                        true
                    } else {
                        false
                    }
                })
                .map(|e| e.1)
                .collect()
        } else {
            data
        };

        let previously_chained = chained.is_some();
        let chaining_res = if chained
            .as_ref()
            .is_some_and(|c| matches!(c, ChainingResult::Table { .. }))
            || (chained.as_ref().is_none() && !self.output_yaml)
        {
            let (table, grand_total) = chained
                .and_then(ChainingResult::into_table)
                .unwrap_or_else(|| (self.create_table(), None));
            ChainingResult::Table {
                table: self.append_table(table, plan.column_spec().exprs(), data, total_row),
                grand_total,
            }
        } else {
            let yaml =
                chained.and_then(ChainingResult::into_yaml).unwrap_or_else(|| self.create_yaml());

            ChainingResult::Yaml(self.append_yaml(
                yaml,
                plan.column_spec().exprs(),
                data,
                total_row,
            ))
        };

        self.chain_or_print(chaining_res, previously_chained)?;

        // Always write the price databases.
        JContext::get()
            .journal()
            .config()
            .price_databases()
            .into_iter()
            .for_each(|db| db.write_file().unwrap());
        Ok(())
    }

    fn as_chainable(&self) -> Option<&dyn ChainableCommand> {
        Some(self)
    }
}
