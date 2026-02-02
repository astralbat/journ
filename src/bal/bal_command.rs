/*
 * Copyright (c) 2024-2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::ExecCommand;
use journ_core::account::Account;
use journ_core::configuration::{AccountFilter, DescriptionFilter, FileFilter, Filter, UnitFilter};
use journ_core::err;
use journ_core::error::JournResult;
use journ_core::journal::Journal;
use journ_core::journal_entry::JournalEntry;
use journ_core::journal_node::JournalNode;
use journ_core::posting::Posting;
use journ_core::report::command::arguments::{Cmd, Command, DateTimeFormatCommand};
use journ_core::report::command::chained_result::ChainingResult;
use journ_core::report::command::cmd_line::BeginAndEndCommand;
use journ_core::report::expr::parser::parse_plan;
use journ_core::report::expr::{
    ColumnValue, GroupKey, GroupState, LateContext, PostingContext, TotalContext,
};
use journ_core::report::table2::{CellRef, StyledCell};
use journ_core::report::term_style::{Style, Weight};
use journ_core::unit::Unit;
use std::collections::{BTreeMap, HashMap};

#[derive(Default, Debug)]
pub struct BalCommand {
    pub(super) datetime_fmt_cmd: DateTimeFormatCommand,
    pub(super) begin_and_end_cmd: BeginAndEndCommand,
    pub(super) file_filter: Vec<String>,
    /// Each inner Vec is a partition
    pub(super) account_filter: Vec<Vec<String>>,
    pub(super) unit_filter: Vec<Vec<String>>,
    pub(super) description_filter: Vec<String>,
    pub(super) no_header: bool,
    pub(super) no_total: bool,
    pub(super) show_zeros: bool,
    pub(super) group_by: String,
    pub(super) column_spec: String,
}

impl BalCommand {
    pub fn account_filter<'h>(&self) -> impl Filter<Account<'h>> + Clone {
        AccountFilter::new(self.account_filter.iter().flatten())
    }

    pub fn unit_filter<'h>(&self) -> impl Filter<Unit<'h>> + Clone {
        UnitFilter::new(self.unit_filter.iter().flatten())
    }

    pub fn file_filter<'h>(&self) -> impl Filter<JournalNode<'h>> {
        FileFilter(&self.file_filter)
    }

    pub fn description_filter(&self) -> impl Filter<str> + Clone {
        DescriptionFilter(&self.description_filter)
    }

    pub fn column_spec(&self) -> &str {
        &self.column_spec
    }

    pub fn group_by(&self) -> &str {
        &self.group_by
    }

    pub fn filtered_postings<'h, 'j, 'a>(
        &'a self,
        journ: &'j Journal<'h>,
    ) -> impl Fn() -> Box<dyn Iterator<Item = (&'h JournalEntry<'h>, &'h Posting<'h>)> + 'a> + 'a
    where
        'h: 'j,
        'j: 'a,
    {
        move || {
            let cmd: &BalCommand = Cmd::cast();
            let description_filter = self.description_filter();
            let file_filter = self.file_filter();
            let unit_filter = self.unit_filter();
            let account_filter = self.account_filter();
            Box::new(
                journ
                    .entry_range(cmd.begin_and_end_cmd.begin_end_range())
                    .filter(move |e| description_filter.is_included(e.description()))
                    .filter(move |e| {
                        file_filter
                            .is_included(journ.root().find_by_node_id(e.id().node_id()).unwrap())
                    })
                    .flat_map(move |e| e.postings().map(move |p| (e, p)))
                    .filter(move |(_e, p)| account_filter.is_included(p.account()))
                    .filter(move |(_e, p)| unit_filter.is_included(p.unit())),
            )
        }
    }
}

impl Command for BalCommand {
    fn datetime_fmt_cmd(&self) -> &DateTimeFormatCommand {
        &self.datetime_fmt_cmd
    }

    fn begin_and_end_cmd(&self) -> &BeginAndEndCommand {
        &self.begin_and_end_cmd
    }
}

impl ExecCommand for BalCommand {
    fn execute<'h>(
        &'h self,
        journ: &'h mut Journal<'h>,
        _chained: Option<ChainingResult>,
    ) -> JournResult<()> {
        let config = journ.config();
        let plan = parse_plan(
            self.column_spec(),
            None,
            !self.no_total,
            Some(self.group_by()),
            HashMap::new(),
            None,
            true,
        )?;

        let mut table = journ_core::report::table2::Table::default();
        // Heading Row
        if !self.no_header {
            let heading_style = Style::default().with_weight(Weight::Bold);
            let headings = plan
                .column_spec()
                .exprs()
                .iter()
                .map(|col| StyledCell::new(col.to_string(), heading_style))
                .collect::<Vec<_>>();
            table.append_heading_row(headings);
        }

        let (groups, total_group) = plan
            .execute_to_groups(self.filtered_postings(journ)(), |(entry, pst)| {
                PostingContext::new(journ, entry, pst)
            })?;

        // Finalize groups and add to table
        let partitions = FilterPartitions::new(self, groups)?;
        let mut partition_count = 0;
        let mut partition_peek = partitions.into_iter().peekable();
        while let Some(partition) = partition_peek.next() {
            let mut partition_rows = 0;
            let mut partition_total = GroupState::try_from(plan.column_spec().agg_functions())?;
            for (key, group) in partition {
                let mut context = LateContext::new(journ, key.clone(), group.finalize());
                let mut row: Vec<CellRef> = vec![];
                let mut found_non_zero = false;
                for col in plan.column_spec().exprs().iter() {
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

                    found_non_zero |= value
                        .as_list()
                        .iter()
                        .any(|a| a.as_amount().map(|a| !a.is_zero()).unwrap_or(false));
                    row.push(value.into_cell_ref(self.show_zeros, true));
                }
                if found_non_zero || self.show_zeros {
                    // Section divider
                    if partition_count > 0 && partition_rows == 0 {
                        table.push_separator_row('-', plan.column_spec().exprs().len());
                    }
                    table.push_row(row);
                    partition_rows += 1;
                }
                partition_total.merge(&group)?;
            }
            // Partition subtotal row
            if !self.no_total && partition_rows > 1 {
                let mut context = TotalContext::new(journ, partition_total.finalize());
                let mut total_row: Vec<CellRef> = vec![];
                for col in plan.column_spec().exprs().iter() {
                    let value = col.eval(&mut context).unwrap_or(ColumnValue::StringRef(""));

                    total_row.push(value.into_cell_ref(self.show_zeros, true));
                }
                table.push_separator_row('-', plan.column_spec().exprs().len());
                table.push_row(total_row);
            }
            if partition_rows > 0 {
                partition_count += 1;
            }
        }

        // Total row
        if !self.no_total && partition_count > 1 {
            table.push_separator_row('=', plan.column_spec().exprs().len());
            let mut context = TotalContext::new(journ, total_group.finalize());
            let mut total_row: Vec<CellRef> = vec![];
            for col in plan.column_spec().exprs().iter() {
                let value = col.eval(&mut context).unwrap_or(ColumnValue::StringRef(""));

                total_row.push(value.into_cell_ref(self.show_zeros, true));
            }
            table.push_row(total_row);
            table.push_separator_row('=', plan.column_spec().exprs().len());
        }

        // Print the table
        let mut output = String::new();
        //if cmd.write_csv() {
        //    table.print_csv(&mut output).unwrap();
        //} else {
        table.print(&mut output).unwrap();
        //}
        print!("{output}");

        // We only need to write the price database.
        config.price_databases().into_iter().for_each(|db| db.write_file().unwrap());

        Ok(())
    }
}

/// A partitioning scheme for filters that have been specified multiple times.
/// For example, if the user specifies:
/// --account "Assets:Bank..,Assets:Savings.." --account "Assets:Property.." or
/// --unit "USD" --unit "EUR"
/// then we create and iterate over two partitions of the grouped data.
struct FilterPartitions<'h> {
    groups: HashMap<GroupKey<'h>, GroupState<'h>>,
    account_filters: Vec<AccountFilter>,
    unit_filters: Vec<UnitFilter>,
}
impl<'h> FilterPartitions<'h> {
    pub fn new(
        cmd: &BalCommand,
        groups: HashMap<GroupKey<'h>, GroupState<'h>>,
    ) -> JournResult<Self> {
        // All filter partitions must have the same number of partitions, or be empty
        let mut all_filters = vec![cmd.account_filter.clone(), cmd.unit_filter.clone()];
        let max_len = all_filters.iter().map(Vec::len).max().unwrap_or(0);
        all_filters.iter_mut().for_each(|f| {
            if f.len() != max_len {
                f.push(f.last().cloned().unwrap_or_default());
            }
        });

        Ok(Self {
            groups,
            account_filters: cmd
                .account_filter
                .iter()
                .rev()
                .map(|accounts| AccountFilter::new(accounts.iter()))
                .collect(),
            unit_filters: cmd
                .unit_filter
                .iter()
                .rev()
                .map(|units| UnitFilter::new(units.iter()))
                .collect(),
        })
    }
}
impl<'h> Iterator for FilterPartitions<'h> {
    type Item = BTreeMap<GroupKey<'h>, GroupState<'h>>;
    fn next(&mut self) -> Option<Self::Item> {
        // If there are no more filters, we are done, though the group may not be empty.
        if self.account_filters.is_empty() {
            return None;
        }
        let account_filter = self.account_filters.pop().unwrap_or_default();
        let unit_filter = self.unit_filters.pop().unwrap_or_default();

        let mut partitioned_groups: BTreeMap<GroupKey<'h>, GroupState<'h>> = BTreeMap::new();
        let mut new_groups = HashMap::new();
        for (key, state) in self.groups.drain() {
            let mut include = true;

            // Account filter include
            include &= key.values().iter().map(|v| &v.1).any(|v| {
                v.as_list().into_iter().flatten().any(|cv| {
                    cv.as_account().map(|a| account_filter.is_included(a)).unwrap_or(false)
                })
            });

            // Unit filter include
            let unit_in_key = key.values().iter().map(|v| &v.1).any(|v| {
                v.as_list()
                    .into_iter()
                    .flatten()
                    .any(|cv| cv.as_unit().map(|u| unit_filter.is_included(u)).unwrap_or(false))
            });
            let unit_in_state = state
                .aggs()
                .iter()
                .flat_map(|agg| agg.finalize().into_iter())
                .any(|cv| cv.as_unit().map(|u| unit_filter.is_included(u)).unwrap_or(false));
            include &= unit_in_key || unit_in_state;

            if include {
                partitioned_groups.insert(key, state);
            } else {
                new_groups.insert(key, state);
            }
        }
        self.groups = new_groups;

        if partitioned_groups.is_empty() { self.next() } else { Some(partitioned_groups) }
    }
}
