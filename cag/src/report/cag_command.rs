/*
 * Copyright (c) 2024-2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::cgt_configuration::{EventFilter, EventPattern, PoolFilter};
use crate::computer::CapitalGainsComputer;
use crate::report::cmd_line::CagArguments;
use crate::report::expr::context::CagContext;
use clap::Parser;
use journ_core::account::Account;
use journ_core::configuration::{AccountFilter, Filter, UnitFilter};
use journ_core::error::JournResult;
use journ_core::journal::Journal;
use journ_core::module::ModuleCommand;
use journ_core::report::command::arguments::{Arguments, Cmd, Command, DateTimeFormatCommand};
use journ_core::report::command::chained_result::ChainingResult;
use journ_core::report::command::cmd_line::BeginAndEndCommand;
use journ_core::report::command::{ChainableCommand, ExecCommand, IntoExecCommand};
use journ_core::report::expr::parser::parse_plan;
use journ_core::report::expr::{Expr, RowData};
use journ_core::report::table2::{Row, StyledCell, Table};
use journ_core::report::term_style::{Style, Weight};
use journ_core::unit::Unit;
use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;
use yaml_rust2::{Yaml, YamlEmitter, yaml};

#[derive(Default, Debug)]
pub struct CagCommand {
    pub(super) datetime_fmt_cmd: DateTimeFormatCommand,
    pub(super) begin_and_end_cmd: BeginAndEndCommand,
    pub(super) account_filter: Vec<String>,
    pub(super) event_filter: Vec<EventPattern>,
    pub(super) unit_filter: Vec<String>,
    pub(super) pool_filter: Vec<String>,
    pub(super) head: Option<usize>,
    pub(super) tail: Option<usize>,
    pub(super) group_by: Option<String>,
    pub(super) group_deals_by_date: bool,
    pub(super) order_by_spec: Option<String>,
    pub(super) order_ascending: bool,
    pub(super) output_yaml: bool,
    pub(super) yaml_map_key: Option<String>,
    pub(super) title: Option<String>,
    pub(super) no_header: bool,
    pub(super) column_spec: String,
    pub(super) chain: Option<Box<CagCommand>>,
}

impl CagCommand {
    pub fn begin_and_end_cmd(&self) -> &BeginAndEndCommand {
        &self.begin_and_end_cmd
    }

    pub fn account_filter(&self) -> impl for<'h> Filter<Account<'h>> + '_ {
        AccountFilter::new(self.account_filter.iter())
    }

    pub fn unit_filter(&self) -> impl for<'h> Filter<Unit<'h>> + '_ {
        UnitFilter::new(self.unit_filter.iter())
    }

    pub fn event_filter(&self) -> EventFilter<'_> {
        EventFilter(&self.event_filter)
    }

    pub fn pool_filter(&self) -> PoolFilter {
        PoolFilter(self.pool_filter.iter().map(|s| s.as_str().into()).collect())
    }

    pub fn group_deals_by_date(&self) -> bool {
        self.group_deals_by_date
    }

    pub fn create_table(&self) -> Table<'_> {
        Table::default()
    }

    pub fn append_table<'a>(
        &'a self,
        mut table: Table<'a>,
        column_expressions: &[Expr],
        rows: Vec<RowData<'a>>,
    ) -> Table<'a> {
        let chain_pos = Cmd::chain_position();
        if chain_pos > 0 {
            table.append_chain_separator(self.title.as_ref());
        }
        // Heading Row
        if !self.no_header {
            let heading_style = Style::default().with_weight(Weight::Bold);
            let headings = column_expressions
                .iter()
                .map(|col| StyledCell::new(col.to_string(), heading_style))
                .collect::<Vec<_>>();
            table.append_heading_row(headings);
        }

        for row in rows {
            table.push_row(Row::new(row.column_values.into_iter().map(|v| v.into_cell_ref(false))));
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

    pub fn append_yaml(&self, yaml: Yaml, column_expressions: &[Expr], rows: Vec<RowData>) -> Yaml {
        let root_key = Yaml::String("capital_gains".to_string());

        let mut rows_list = yaml::Array::new();
        let mut rows_map = yaml::Hash::new();
        let mut is_using_map_keys = false;
        for row_data in rows {
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

        let chain_pos = Cmd::chain_position();
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
        journal: &Journal,
        args: &Arguments,
        command_args: &[String],
    ) -> JournResult<Box<dyn ExecCommand>> {
        CagArguments::parse_from(command_args)
            .into_exec_cmd(journal, args)
            .map(Box::new)
            .map(|boxed| boxed as Box<dyn ExecCommand>)
    }
}

impl ChainableCommand for CagCommand {
    fn next_chain(&'static self) -> Option<&'static dyn ChainableCommand> {
        Some(self.chain.as_ref()?.deref())
    }
}

impl ExecCommand for CagCommand {
    fn execute<'h>(
        &'h self,
        journ: &'h mut Journal<'h>,
        chained: Option<ChainingResult<'h>>,
    ) -> JournResult<()> {
        let mut computer = CapitalGainsComputer::new();
        let capital_gains = computer.compute_gains(journ)?;

        let mut additional = HashMap::new();
        if let Some(yaml_map_key) = &self.yaml_map_key {
            additional.insert("yaml_map_key", yaml_map_key.as_str());
        }
        let plan = parse_plan(
            &self.column_spec,
            self.group_by.as_deref(),
            additional,
            self.order_by_spec.as_ref().map(String::as_str),
            self.order_ascending,
        )?;
        let event_filter = self.event_filter();
        let pool_filter = self.pool_filter();
        let data = plan.execute(
            &*journ,
            capital_gains
                .events()
                .iter()
                .filter(|e| event_filter.is_included(e))
                .filter(|e| pool_filter.is_included(e)),
            |e| CagContext::new(journ, e),
        )?;

        // The data rows are limited by --head and/or --tail.
        let data = if self.head.is_some() || self.tail.is_some() {
            let data_len = data.len();
            data.into_iter()
                .enumerate()
                .filter(|(i, _)| {
                    return if let Some(head) = self.head
                        && i < &head
                    {
                        true
                    } else if let Some(tail) = self.tail
                        && &(data_len - tail) <= i
                    {
                        true
                    } else {
                        false
                    };
                })
                .map(|e| e.1)
                .collect()
        } else {
            data
        };

        let chaining_res = if chained
            .as_ref()
            .is_some_and(|c| matches!(c, ChainingResult::Table(_)))
            || (chained.as_ref().is_none() && !self.output_yaml)
        {
            let table =
                chained.and_then(ChainingResult::into_table).unwrap_or_else(|| self.create_table());
            ChainingResult::Table(self.append_table(table, plan.column_expressions(), data))
        } else {
            let yaml =
                chained.and_then(ChainingResult::into_yaml).unwrap_or_else(|| self.create_yaml());

            ChainingResult::Yaml(self.append_yaml(yaml, plan.column_expressions(), data))
        };

        // Move to next chain
        if let Some(next) = Cmd::advance_chain() {
            return next.execute(journ, Some(chaining_res));
        }

        // Otherwise print output
        match chaining_res {
            ChainingResult::Table(table) => {
                print!("{table}")
            }
            ChainingResult::Yaml(root) => {
                let mut string = String::new();
                let mut emitter = YamlEmitter::new(&mut string);
                emitter.dump(&root).map_err(|_| fmt::Error).unwrap();
                print!("{string}")
            }
        }

        // Always write the price databases.
        journ.config().price_databases().into_iter().for_each(|db| db.write_file().unwrap());
        Ok(())
    }

    fn as_chainable(&self) -> Option<&dyn ChainableCommand> {
        Some(self)
    }
}
