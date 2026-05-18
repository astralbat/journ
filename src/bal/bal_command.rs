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
use journ_core::error::JournResult;
use journ_core::journal::Journal;
use journ_core::journal_context::JournalContext;
use journ_core::journal_entry::JournalEntry;
use journ_core::journal_node::JournalNode;
use journ_core::posting::Posting;
use journ_core::report::command::ChainableCommand;
use journ_core::report::command::arguments::{Cmd, Command, DateTimeFormatCommand};
use journ_core::report::command::chained_result::ChainingResult;
use journ_core::report::command::cmd_line::BeginAndEndCommand;
use journ_core::report::expr::parser::parse_plan;
use journ_core::report::expr::{Expr, NullBalanceUpdater, PostingContext, RowData};
use journ_core::report::table2;
use journ_core::report::table2::{Row, StyledCell, Table};
use journ_core::report::term_style::{Style, Weight};
use journ_core::unit::Unit;
use std::collections::HashMap;
use std::ops::Deref;

#[derive(Default, Debug)]
pub struct BalCommand {
    pub(super) datetime_fmt_cmd: DateTimeFormatCommand,
    pub(super) begin_and_end_cmd: BeginAndEndCommand,
    pub(super) file_filter: Vec<String>,
    /// Each inner Vec is a partition
    pub(super) account_filter: Vec<Vec<String>>,
    pub(super) unit_filter: Vec<Vec<String>>,
    pub(super) description_filter: Vec<String>,
    pub(super) title: Option<String>,
    pub(super) no_header: bool,
    pub(super) no_total: bool,
    pub(super) show_zeros: bool,
    pub(super) group_by: Option<String>,
    pub(super) order_by_spec: Option<String>,
    pub(super) order_ascending: bool,
    pub(super) column_spec: String,
    pub(super) chain: Option<Box<BalCommand>>,
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

    pub fn group_by(&self) -> Option<&str> {
        self.group_by.as_deref()
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
                        file_filter.is_included(
                            journ.root().find_by_node_id(&e.id().parent().unwrap()).unwrap(),
                        )
                    })
                    .flat_map(move |e| e.postings().map(move |p| (e, p)))
                    .filter(move |(_e, p)| account_filter.is_included(p.account()))
                    .filter(move |(_e, p)| unit_filter.is_included(p.unit())),
            )
        }
    }

    fn create_table(&self) -> Table<'_> {
        Table::default()
    }

    fn append_table<'a>(
        &'a self,
        mut table: Table<'a>,
        column_expressions: &[Expr],
        mut rows: Vec<RowData<'a>>,
        total_row_index: Option<usize>,
        show_total: bool,
    ) -> Table<'a> {
        let chain_pos = Cmd::chain_position();

        if chain_pos > 0 {
            table.append_chain_separator();
        }
        if let Some(title) = &self.title {
            table.append_title_row(title, rows.iter().map(|r| r.column_count()).max().unwrap_or(1));
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

        for (i, row_data) in rows.into_iter().enumerate() {
            if Some(i) == total_row_index {
                if !show_total {
                    break;
                }
                table.push_separator_row('-', column_expressions.len())
            }
            table.push_row(Row::new(
                row_data.column_values.into_iter().map(|c| c.into_cell_ref(self.show_zeros, true)),
            ))
        }

        table
    }

    fn datetime_fmt_cmd(&self) -> &DateTimeFormatCommand {
        &self.datetime_fmt_cmd
    }

    fn begin_and_end_cmd(&self) -> &BeginAndEndCommand {
        &self.begin_and_end_cmd
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

impl ChainableCommand for BalCommand {
    fn next_chain(&'static self) -> Option<&'static dyn ChainableCommand> {
        Some(self.chain.as_ref()?.deref())
    }
}

impl ExecCommand for BalCommand {
    fn execute<'h>(&'h self, chained: Option<ChainingResult<'h>>) -> JournResult<()> {
        let journ = JournalContext::current().journal();
        let plan = parse_plan(
            self.column_spec(),
            None,
            !self.no_total,
            self.group_by(),
            HashMap::new(),
            self.order_by_spec.as_ref().map(String::as_str),
            self.order_ascending,
        )?;

        let mut data = plan.execute(
            self.filtered_postings(&journ)(),
            |(entry, pst)| PostingContext::new(entry, pst),
            None::<NullBalanceUpdater<'h, (&JournalEntry<'h>, &Posting<'h>)>>,
        )?;

        // Filter out zeros if not required
        data.retain(|row_data| {
            let all_zero = row_data.column_values.iter().all(|cv| {
                cv.as_list().iter().all(|cv| cv.as_amount().map(|a| a.is_zero()).unwrap_or(true))
            });
            // Don't show if all amounts are zero unless show_zeros option is used
            !all_zero || self.show_zeros
        });

        // Add to the table
        let row_count = data.len();
        let total_row_index = if self.no_total || plan.group_by().is_empty() {
            None
        } else if row_count > 0 {
            Some(row_count - 1)
        } else {
            None
        };

        // Show the total only if it is unique. We use a heuristical approach here by understanding
        // whether the total compresses any of the aggregate columns - that is, the number of values
        // in the total is less than non-total rows.
        let show_total = !self.no_total
            && 'is_total_unique: {
                for icol in plan
                    .column_spec()
                    .exprs()
                    .iter()
                    .enumerate()
                    .filter(|e| matches!(e.1, Expr::AggFunction(_, _, _)))
                    .map(|e| e.0)
                {
                    let mut num_vals_in_col = 0;
                    for (irow, row_data) in data.iter().enumerate() {
                        if Some(irow) == total_row_index {
                            let num_vals_in_total = row_data.column_values[icol].as_list().len();
                            if num_vals_in_total > 0 && num_vals_in_total < num_vals_in_col {
                                break 'is_total_unique true;
                            }
                        } else {
                            num_vals_in_col += row_data.column_values[icol].as_list().len();
                        }
                    }
                }
                false
            };

        /*
        for (i, row_data) in data.into_iter().enumerate() {
            if Some(i) == total_row_index {
                if !show_total {
                    break;
                }
                table.push_separator_row('-', plan.column_spec().exprs().len())
            }
            table.push_row(Row::new(
                row_data.column_values.into_iter().map(|c| c.into_cell_ref(self.show_zeros, true)),
            ))
        }*/

        let table = chained.map(|c| c.into_table().unwrap()).unwrap_or_else(|| self.create_table());
        let chained = ChainingResult::Table(self.append_table(
            table,
            plan.column_spec().exprs(),
            data,
            total_row_index,
            show_total,
        ));
        self.chain_or_print(chained)?;

        /*
        // Print the table
        let mut output = String::new();
        //if cmd.write_csv() {
        //    table.print_csv(&mut output).unwrap();
        //} else {
        table.print(&mut output).unwrap();
        //}
        // Using print! macro can cause panic when piping. Use write and ignore the result.
        let stdout = std::io::stdout();
        let _ = write!(&mut stdout.lock(), "{}", &output);*/

        // We only need to write the price database.
        journ.config().price_databases().into_iter().for_each(|db| db.write_file().unwrap());

        Ok(())
    }

    fn as_chainable(&self) -> Option<&dyn ChainableCommand> {
        Some(self)
    }
}
