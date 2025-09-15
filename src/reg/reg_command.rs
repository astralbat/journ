/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::ExecCommand;
use crate::column_expr::{ColumnExpr, ColumnValue, DataContext, EvalContext, PostingGroup};
use crate::grouping::GroupKey;
use crate::report::parse_columns;
use journ_core::account::Account;
use journ_core::amount::Amount;
use journ_core::arguments::{Arguments, Command};
use journ_core::configuration::{
    AccountFilter, DescriptionFilter, FileFilter, Filter, create_unit_filter,
};
use journ_core::err;
use journ_core::error::JournResult;
use journ_core::journal::Journal;
use journ_core::journal_entry::JournalEntry;
use journ_core::journal_node::JournalNode;
use journ_core::posting::Posting;
use journ_core::reporting::balance::Balance;
use journ_core::reporting::table2::{CellRef, StyledCell};
use journ_core::reporting::term_style::{Style, Weight};
use journ_core::unit::Unit;
use num_format::Locale::pa;
use std::io;

#[derive(Default, Debug)]
pub struct RegCommand {
    file_filter: Vec<String>,
    account_filter: Vec<String>,
    unit_filter: Vec<String>,
    description_filter: Vec<String>,
    group_postings_by: Option<String>,
    column_spec: String,
}

impl RegCommand {
    pub fn account_filter(&self) -> impl for<'t> Filter<Account<'t>> + '_ {
        AccountFilter::new(&self.account_filter)
    }

    pub fn set_account_filter(&mut self, accounts: Vec<String>) {
        self.account_filter = accounts;
    }

    pub fn unit_filter(&self) -> impl for<'t> Filter<Unit<'t>> + '_ {
        create_unit_filter(&self.unit_filter)
    }

    pub fn set_unit_filter(&mut self, units: Vec<String>) {
        self.unit_filter = units;
    }

    pub fn file_filter(&self) -> impl for<'h> Filter<JournalNode<'h>> + '_ {
        FileFilter(&self.file_filter)
    }

    pub fn set_file_filter(&mut self, files: Vec<String>) {
        self.file_filter = files;
    }

    pub fn description_filter(&self) -> impl Filter<str> + '_ {
        DescriptionFilter(&self.description_filter)
    }

    pub fn set_description_filter(&mut self, descriptions: Vec<String>) {
        self.description_filter = descriptions
    }

    pub fn set_column_spec(&mut self, spec: String) {
        self.column_spec = spec;
    }
}

impl Command for RegCommand {}

impl ExecCommand for RegCommand {
    fn execute<'h>(&self, mut journ: Journal<'h>) -> JournResult<()> {
        let args = Arguments::get();
        let cmd: &RegCommand = args.cast_cmd().unwrap();
        let config = journ.config();

        // Parse the column specification. We need a lower-case version for evaluation.
        let lowercase_spec = cmd.column_spec.to_ascii_lowercase();
        let lower_column_spec = parse_columns(&lowercase_spec)?;
        let column_spec = parse_columns(&cmd.column_spec)?;

        // Get all the accounts to balance for.
        let description_filter = cmd.description_filter();
        let file_filter = cmd.file_filter();
        let account_filter = cmd.account_filter();
        let unit_filter = cmd.unit_filter();
        let filtered_postings = Box::new(|| {
            Box::new(
                journ
                    .entry_range(args.begin_end_range())
                    .filter(|e| description_filter.is_included(e.description()))
                    .filter(|e| {
                        file_filter
                            .is_included(journ.root().find_by_node_id(e.id().node_id()).unwrap())
                    })
                    .flat_map(|e| e.postings().map(move |p| (e, p)))
                    .filter(|(_, p)| account_filter.is_included(p.account()))
                    .filter(|(_, p)| unit_filter.is_included(p.unit())),
            ) as Box<dyn Iterator<Item = (&'h JournalEntry, &'h Posting<'h>)>>
        });

        let mut table = journ_core::reporting::table2::Table::default();
        table.set_color(!args.no_color);
        // Heading Row
        let heading_style = Style::default().with_weight(Weight::Bold);
        let headings = column_spec
            .iter()
            .map(|col| StyledCell::new(col.to_string(), heading_style))
            .collect::<Vec<_>>();
        table.set_heading_row(headings);

        let mut balance = vec![];
        for (entry, pst) in filtered_postings() {
            let mut context = EvalContext::new(&journ, DataContext::Scalar(entry, pst));
            balance += pst.amount();
            context.add_variable("balance", ColumnValue::Amount(balance.balance(pst.unit())));
            let mut row: Vec<CellRef> = vec![];
            for (i, col) in lower_column_spec.iter().enumerate() {
                let value = col
                    .expr
                    .eval(&context)
                    .map_err(|e| err!("Unable to evaluate column: '{}'", col).with_source(e))?;

                // Allow these columns to expand. Look better.
                if let ColumnExpr::Column("description") | ColumnExpr::Column("account") = &col.expr
                {
                    table.expand_column(i);
                }

                row.push(value.into());
            }
            table.push_row(row);
        }
        let mut output = String::new();
        table.print(&mut output).unwrap();
        println!("{output}");

        // We only need to write the price database.
        config.price_databases().into_iter().for_each(|db| db.write_file().unwrap());
        Ok(())
    }
}
