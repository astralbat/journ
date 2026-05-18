/*
 * Copyright (c) 2025. Mark Barrett
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
use journ_core::report::command::arguments::{Cmd, Command, DateTimeFormatCommand};
use journ_core::report::command::chained_result::ChainingResult;
use journ_core::report::command::cmd_line::BeginAndEndCommand;
use journ_core::report::expr::parser::parse_plan;
use journ_core::report::expr::{ColumnValue, Expr, PostingContext, RowData};
use journ_core::report::table2::{Row, StyledCell};
use journ_core::report::term_style::{Style, Weight};
use journ_core::unit::Unit;
use std::collections::HashMap;
use std::io::Write;

#[derive(Default, Debug)]
pub struct RegCommand {
    pub(super) datetime_fmt_cmd: DateTimeFormatCommand,
    pub(super) begin_and_end_cmd: BeginAndEndCommand,
    pub(super) file_filter: Vec<String>,
    pub(super) account_filter: Vec<String>,
    pub(super) unit_filter: Vec<String>,
    pub(super) description_filter: Vec<String>,
    pub(super) column_spec: String,
    pub(super) order_by_spec: Option<String>,
    pub(super) order_ascending: bool,
    pub(super) where_conditions: Option<String>,
}

impl RegCommand {
    pub fn account_filter(&self) -> impl for<'t> Filter<Account<'t>> + '_ {
        AccountFilter::new(self.account_filter.iter())
    }

    pub fn unit_filter(&self) -> impl for<'t> Filter<Unit<'t>> + '_ {
        UnitFilter::new(self.unit_filter.iter())
    }

    pub fn file_filter(&self) -> impl for<'h> Filter<JournalNode<'h>> + '_ {
        FileFilter(&self.file_filter)
    }

    pub fn description_filter(&self) -> impl Filter<str> + '_ {
        DescriptionFilter(&self.description_filter)
    }

    pub fn filtered_postings<'h>(
        &self,
        journ: &Journal<'h>,
    ) -> impl Iterator<Item = (&'h JournalEntry<'h>, &'h Posting<'h>)> {
        let cmd = Cmd::cast::<RegCommand>();
        let description_filter = self.description_filter();
        let file_filter = self.file_filter();
        let account_filter = self.account_filter();
        let unit_filter = self.unit_filter();
        journ
            .entry_range(cmd.begin_and_end_cmd.begin_end_range())
            .filter(move |e| description_filter.is_included(e.description()))
            .filter(move |e| {
                file_filter
                    .is_included(journ.root().find_by_node_id(&e.id().parent().unwrap()).unwrap())
            })
            .flat_map(|e| e.postings().map(move |p| (e, p)))
            .filter(move |(_, p)| account_filter.is_included(p.account()))
            .filter(move |(_, p)| unit_filter.is_included(p.unit()))
    }
}

impl Command for RegCommand {
    fn datetime_fmt_cmd(&self) -> &DateTimeFormatCommand {
        &self.datetime_fmt_cmd
    }

    fn begin_and_end_cmd(&self) -> &BeginAndEndCommand {
        &self.begin_and_end_cmd
    }
}

impl ExecCommand for RegCommand {
    fn execute<'h>(&self, _chained: Option<ChainingResult>) -> JournResult<()> {
        let cmd: &RegCommand = Cmd::cast();
        let journ = JournalContext::current().journal();
        let config = journ.config();

        // Parse the column specification. We need a lower-case version for evaluation.
        let plan = parse_plan(
            &cmd.column_spec,
            cmd.where_conditions.as_deref(),
            false,
            None,
            HashMap::new(),
            cmd.order_by_spec.as_deref(),
            cmd.order_ascending,
        )?;

        let mut table = journ_core::report::table2::Table::default();
        // Heading Row
        let heading_style = Style::default().with_weight(Weight::Bold);
        let headings = plan
            .column_spec()
            .exprs()
            .iter()
            .map(|col| StyledCell::new(col.to_string(), heading_style))
            .collect::<Vec<_>>();
        table.append_heading_row(headings);

        // Allow these columns to expand. Look better.
        for (i, col) in plan.column_spec().exprs().iter().enumerate() {
            if let Expr::Identifier(s) = &col
                && (s.eq_ignore_ascii_case("description") || s.eq_ignore_ascii_case("account"))
            {
                table.expand_column(i);
            }
        }

        // Evaluate against all the filtered postings
        //let mut balance = vec![];
        let balance_update_fn =
            |prev: &[RowData<'h>],
             row: &mut RowData<'h>,
             (_entry, pst): (&JournalEntry<'h>, &Posting<'h>)| {
                for prev_row in prev.iter().rev() {
                    let amount = prev_row.running_balance("balance").unwrap().as_amount().unwrap();
                    if amount.unit() == pst.unit() {
                        row.set_running_balance(
                            "balance",
                            ColumnValue::Amount(amount + pst.amount()),
                        );
                        return;
                    }
                }
                row.set_running_balance("balance", ColumnValue::Amount(pst.amount()));
            };
        let data = plan.execute(
            self.filtered_postings(&journ),
            |(entry, pst)| PostingContext::new(entry, pst),
            Some(balance_update_fn),
        )?;

        // Add to the table
        for row_data in data {
            table.push_row(Row::new(
                row_data.column_values.into_iter().map(|c| c.into_cell_ref(false, true)),
            ))
        }

        // Print the table
        let mut output = String::new();
        table.print(&mut output).unwrap();
        // Using print! macro can cause panic when piping. Use write and ignore the result.
        let stdout = std::io::stdout();
        let _ = write!(&mut stdout.lock(), "{}", &output);

        // Now we only need to write the price database.
        config.price_databases().into_iter().for_each(|db| db.write_file().unwrap());
        Ok(())
    }
}
