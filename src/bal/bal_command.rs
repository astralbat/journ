/*
 * Copyright (c) 2024-2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::ExecCommand;
use crate::expr::parser::parse_plan;
use crate::expr::{ColumnValue, GroupKey, GroupState, LateContext, PostingContext, TotalContext};
use journ_core::account::Account;
use journ_core::arguments::{Arguments, Command};
use journ_core::configuration::{AccountFilter, DescriptionFilter, FileFilter, Filter, UnitFilter};
use journ_core::err;
use journ_core::error::JournResult;
use journ_core::journal::Journal;
use journ_core::journal_entry::JournalEntry;
use journ_core::journal_node::JournalNode;
use journ_core::posting::Posting;
use journ_core::reporting::table2::{CellRef, StyledCell};
use journ_core::reporting::term_style::{Style, Weight};
use journ_core::unit::Unit;
use std::collections::hash_map::Entry;
use std::collections::{BTreeMap, HashMap};

#[derive(Default, Debug)]
pub struct BalCommand {
    file_filter: Vec<String>,
    account_filter: Vec<String>,
    unit_filter: Vec<String>,
    description_filter: Vec<String>,
    write_csv: bool,
    no_header: bool,
    no_total: bool,
    show_zeros: bool,
    group_by: String,
    column_spec: String,
}

impl BalCommand {
    pub fn account_filter<'h>(&self) -> impl Filter<Account<'h>> + Clone {
        AccountFilter::new(&self.account_filter)
    }

    pub fn set_account_filter(&mut self, accounts: Vec<String>) {
        self.account_filter = accounts;
    }

    pub fn unit_filter<'h>(&self) -> impl Filter<Unit<'h>> + Clone {
        UnitFilter::new(&self.unit_filter)
    }

    pub fn set_unit_filter(&mut self, units: Vec<String>) {
        self.unit_filter = units;
    }

    pub fn write_csv(&self) -> bool {
        self.write_csv
    }

    pub fn set_write_csv(&mut self, write_csv: bool) {
        self.write_csv = write_csv;
    }

    pub fn file_filter<'h>(&self) -> impl Filter<JournalNode<'h>> {
        FileFilter(&self.file_filter)
    }

    pub fn set_file_filter(&mut self, files: Vec<String>) {
        self.file_filter = files;
    }

    pub fn description_filter(&self) -> impl Filter<str> + Clone {
        DescriptionFilter(&self.description_filter)
    }

    pub fn set_description_filter(&mut self, descriptions: Vec<String>) {
        self.description_filter = descriptions
    }

    pub fn column_spec(&self) -> &str {
        &self.column_spec
    }

    pub fn set_column_spec(&mut self, column_spec: String) {
        self.column_spec = column_spec;
    }

    pub fn group_by(&self) -> &str {
        &self.group_by
    }

    pub fn set_no_header(&mut self, no_header: bool) {
        self.no_header = no_header;
    }

    pub fn set_no_total(&mut self, no_total: bool) {
        self.no_total = no_total;
    }

    pub fn set_show_zeros(&mut self, show_zeros: bool) {
        self.show_zeros = show_zeros;
    }

    pub fn set_group_by(&mut self, group_by: String) {
        self.group_by = group_by;
    }

    pub fn filtered_entries<'h, 'a, A>(
        &'a self,
        journ: &'a Journal<'h>,
        account_filter: A,
    ) -> impl Fn() -> Box<dyn Iterator<Item = &'h JournalEntry<'h>> + 'a> + 'a
    where
        A: Filter<Account<'h>> + Clone + 'a,
    {
        let args = Arguments::get();
        move || {
            let description_filter = self.description_filter();
            let file_filter = self.file_filter();
            let unit_filter = self.unit_filter();
            let account_filter = account_filter.clone();
            Box::new(
                journ
                    .entry_range(args.begin_end_range())
                    .filter(move |e| description_filter.is_included(e.description()))
                    .filter(move |e| {
                        file_filter
                            .is_included(journ.root().find_by_node_id(e.id().node_id()).unwrap())
                    })
                    .filter(move |e| e.postings().any(|p| account_filter.is_included(p.account())))
                    .filter(move |e| e.postings().any(|p| unit_filter.is_included(p.unit()))),
            )
        }
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
            let args = Arguments::get();
            let description_filter = self.description_filter();
            let file_filter = self.file_filter();
            let unit_filter = self.unit_filter();
            let account_filter = self.account_filter();
            Box::new(
                journ
                    .entry_range(args.begin_end_range())
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

impl Command for BalCommand {}

impl ExecCommand for BalCommand {
    fn execute<'h>(&'h self, journ: &'h mut Journal<'h>) -> JournResult<()> {
        let args = Arguments::get();

        let config = journ.config();
        let plan = parse_plan(self.column_spec(), self.group_by())?;

        let mut table = journ_core::reporting::table2::Table::default();
        table.set_color(table.color() && !args.no_color);
        // Heading Row
        if !self.no_header {
            let heading_style = Style::default().with_weight(Weight::Bold);
            let headings = plan
                .column_spec()
                .iter()
                .map(|col| StyledCell::new(col.to_string(), heading_style))
                .collect::<Vec<_>>();
            table.set_heading_row(headings);
        }

        // Aggregate postings into groups
        let mut groups: HashMap<GroupKey, GroupState> = HashMap::new();
        let mut total_group = GroupState::new(&plan)?;
        for (entry, pst) in self.filtered_postings(&journ)() {
            let mut context = PostingContext::new(journ, entry, pst);

            let key = GroupKey::new(
                plan.group_by()
                    .iter()
                    .map(|e| e.eval(&mut context).map(|v| (e.clone(), v)))
                    .collect::<JournResult<Vec<(_)>>>()?,
            );
            let group = match groups.entry(key) {
                Entry::Occupied(e) => e.into_mut(),
                Entry::Vacant(e) => e.insert(GroupState::new(&plan)?),
            };
            group.add(&mut context)?;
            total_group.add(&mut context)?;
        }

        // Finalize groups and add to table
        let mut sorted_groups = groups.into_iter().collect::<BTreeMap<_, _>>();
        for (key, group) in sorted_groups.iter() {
            let mut context = LateContext::new(journ, key.clone(), group.finalize());
            let mut row: Vec<CellRef> = vec![];
            let mut found_non_zero = false;
            for col in plan.column_spec().iter() {
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

                match &value {
                    ColumnValue::List(list) => {
                        if list.iter().any(|a| a.as_amount().map(|a| !a.is_zero()).unwrap_or(false))
                        {
                            found_non_zero = true;
                        }
                    }
                    ColumnValue::Amount(amount) => {
                        if !amount.is_zero() {
                            found_non_zero = true;
                        }
                    }
                    _ => {}
                }
                row.push(value.into());
            }
            if found_non_zero || self.show_zeros {
                table.push_row(row);
            }
        }

        // Total row
        if !self.no_total && table.rows()[1..].len() > 1 {
            table.push_separator_row('-', plan.column_spec().len());
            let mut context = TotalContext::new(journ, total_group.finalize());
            let mut total_row: Vec<CellRef> = vec![];
            for col in plan.column_spec().iter() {
                let value = col.eval(&mut context).unwrap_or(ColumnValue::StringRef(""));

                total_row.push(value.into());
            }
            table.push_row(total_row);
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
