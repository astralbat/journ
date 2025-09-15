/*
 * Copyright (c) 2024-2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::ExecCommand;
use crate::column_expr::{ColumnValue, DataContext, EvalContext, PostingGroup};
use crate::grouping::GroupKey;
use crate::report::parse_columns;
use journ_core::account::Account;
use journ_core::arguments::{Arguments, Command};
use journ_core::configuration::{
    AccountFilter, DescriptionFilter, FileFilter, Filter, create_unit_filter,
};
use journ_core::err;
use journ_core::error::JournResult;
use journ_core::journal::Journal;
use journ_core::journal_entry::JournalEntry;
use journ_core::journal_node::JournalNode;
use journ_core::reporting::table2::{CellRef, StyledCell};
use journ_core::reporting::term_style::{Style, Weight};
use journ_core::unit::Unit;

#[derive(Default, Debug)]
pub struct BalCommand {
    file_filter: Vec<String>,
    account_filter: Vec<String>,
    unit_filter: Vec<String>,
    description_filter: Vec<String>,
    write_csv: bool,
    write_file: bool,
    column_spec: String,
}

impl BalCommand {
    pub fn account_filter<'a>(&'a self) -> impl for<'h> Filter<'a, Account<'h>> + Clone {
        AccountFilter::new(&self.account_filter)
    }

    pub fn set_account_filter(&mut self, accounts: Vec<String>) {
        self.account_filter = accounts;
    }

    pub fn unit_filter<'a>(&'a self) -> impl for<'h> Filter<'a, Unit<'h>> + Clone {
        create_unit_filter(&self.unit_filter)
    }

    pub fn set_unit_filter(&mut self, units: Vec<String>) {
        self.unit_filter = units;
    }

    /*
    pub fn posting_filter(&self) -> impl for<'h> Filter<Posting<'h>> + '_ {
        PostingFilter::new(self.unit_filter(), self.account_filter())
    }*/

    pub fn write_csv(&self) -> bool {
        self.write_csv
    }

    pub fn set_write_csv(&mut self, write_csv: bool) {
        self.write_csv = write_csv;
    }

    pub fn file_filter(&self) -> impl for<'h> Filter<'_, JournalNode<'h>> + '_ {
        FileFilter(&self.file_filter)
    }

    pub fn set_file_filter(&mut self, files: Vec<String>) {
        self.file_filter = files;
    }

    pub fn description_filter(&self) -> impl Filter<'_, str> + '_ {
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

    pub fn write_file(&self) -> bool {
        self.write_file
    }

    pub fn set_write_file(&mut self, write_file: bool) {
        self.write_file = write_file;
    }
}

impl Command for BalCommand {}

impl ExecCommand for BalCommand {
    fn execute<'h>(&self, journ: Journal<'h>) -> JournResult<()> {
        let args = Arguments::get();

        let cmd: &BalCommand = args.cast_cmd().unwrap();
        let config = journ.config();

        // Parse the column specification. We need a lower-case version for evaluation.
        let lowercase_spec = cmd.column_spec.to_ascii_lowercase();
        let lower_column_spec = parse_columns(&lowercase_spec)?;
        let column_spec = parse_columns(cmd.column_spec())?;

        // Get all the accounts to balance for.
        let description_filter = cmd.description_filter();
        let file_filter = cmd.file_filter();
        let account_filter = cmd.account_filter();
        let unit_filter = cmd.unit_filter();
        let filtered_entries = Box::new(|| {
            Box::new(
                journ
                    .entry_range(args.begin_end_range())
                    .filter(|e| description_filter.is_included(e.description()))
                    .filter(|e| {
                        file_filter
                            .is_included(journ.root().find_by_node_id(e.id().node_id()).unwrap())
                    }),
            ) as Box<dyn Iterator<Item = &'h JournalEntry<'h>>>
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

        let mut accounts_to_bal = vec![];
        for acc in config.accounts() {
            if account_filter.is_included(&**acc) {
                accounts_to_bal.push(acc);
            }
        }
        accounts_to_bal.sort();

        for account in accounts_to_bal.into_iter() {
            let account_filter_with_group_acc =
                account_filter.clone().and(|a: &Account| a == &**account);
            let context = EvalContext::new(
                &journ,
                DataContext::Group(PostingGroup::new(
                    filtered_entries.clone(),
                    Box::new(unit_filter.clone()),
                    Box::new(account_filter_with_group_acc),
                    Some(GroupKey::Account(account)),
                )),
            );

            let mut row: Vec<CellRef> = vec![];
            let mut found_non_zero = false;
            for col in lower_column_spec.iter() {
                let value = col
                    .expr
                    .eval(&context)
                    .map_err(|e| err!("Unable to evaluate column: '{}'", col).with_source(e))?;

                if let Some(amounts) = value.as_amounts() {
                    if !amounts.iter().all(|a| a.is_zero()) {
                        found_non_zero = true;
                    }
                }
                row.push(value.into());
            }
            if found_non_zero {
                table.push_row(row);
            }
        }

        // Total row
        if table.rows()[1..].len() > 1 {
            table.push_separator_row('-', lower_column_spec.len());
            let mut total_row: Vec<CellRef> = vec![];
            let context = EvalContext::new(
                &journ,
                DataContext::Group(PostingGroup::new(
                    filtered_entries.clone(),
                    Box::new(unit_filter.clone()),
                    Box::new(account_filter),
                    None,
                )),
            );
            for col in lower_column_spec.iter() {
                let value = col.expr.eval(&context).unwrap_or(ColumnValue::StringRef(""));

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
        println!("{output}");

        // We only need to write the price database.
        config.price_databases().into_iter().for_each(|db| db.write_file().unwrap());

        /*
        // Get the value units. The unit to value in might not exist in the config,
        // and in this case, we'll just create a new object.
        let mut value_units = vec![];
        for col in cmd.columns().iter() {
            if let BalColumn::Value(unit_code) = col {
                value_units.push(
                    config
                        .get_unit(unit_code)
                        .unwrap_or_else(|| config.allocator().alloc(Unit::new(unit_code))),
                );
            }
        }
        let with_amount = columns.contains(&BalColumn::Amount) || value_units.is_empty();
        let mut bals = AccountBalances::new(with_amount, value_units.clone());

        let account_filter = cmd.account_filter();
        let unit_filter = cmd.unit_filter();
        let description_filter = cmd.description_filter();
        let file_filter = cmd.file_filter();
        let mut modified_entries = vec![];
        for entry in journ.entry_range(args.begin_end_range()) {
            let mut cow_entry = Cow::Borrowed(entry);
            if !file_filter.is_included(journ.root().find_by_node_id(entry.id().node_id()).unwrap())
            {
                continue;
            }

            // First pass: Ensure postings can be valued in the desired units.
            valuer::exec_optimistic(&mut cow_entry, |entry| {
                if !description_filter.is_included(entry.description()) {
                    return ValueResult::Ok(());
                }
                for pst in entry.postings() {
                    if !account_filter.is_included(pst.account()) {
                        continue;
                    }
                    if !unit_filter.is_included(pst.unit()) {
                        continue;
                    }
                    if args.real_postings() && pst.account().is_virtual() {
                        continue;
                    }

                    for val_unit in value_units.iter().copied() {
                        if pst.valued_amount().value_in(val_unit).is_none() {
                            return ValueResult::ValuationNeeded(pst.unit(), val_unit);
                        }
                    }
                }
                return ValueResult::Ok(());
            })?;

            // Second pass: Update balances
            for pst in cow_entry.postings() {
                if !account_filter.is_included(pst.account()) {
                    continue;
                }
                if !unit_filter.is_included(pst.unit()) {
                    continue;
                }
                if args.real_postings() && pst.account().is_virtual() {
                    continue;
                }

                bals.update_balance(pst.account(), pst.valued_amount(), false);
            }

            // Add to the list of entries to write back.
            if !cow_entry.is_borrowed() {
                modified_entries.push(cow_entry);
            }
        }

        if cmd.write_file() {
            let mut node_ids = HashSet::new();
            for entry in modified_entries.iter() {
                node_ids.insert(entry.id().node_id());
            }
            journ.replace_entries(
                modified_entries.into_iter().map(|e| e.into_owned()).collect(),
                journ.allocator(),
            )?;
            for node_id in node_ids {
                journ.node(node_id).write_nearest_file()?;
            }
        }

        let mut output = String::new();
        let table = create_table(&bals);
        if cmd.write_csv() {
            table.print_csv(&mut output).unwrap();
        } else {
            table.print(&mut output).unwrap();
        }
        println!("{output}");

        // We only need to write the price database.
        config.price_databases().into_iter().for_each(|db| db.write_file().unwrap());

         */
        Ok(())
    }
}

/*
fn create_table<'a>(bals: &'a AccountBalances) -> Table<'a> {
    let cmd = Arguments::get().cast_cmd::<BalCommand>().unwrap();
    let mut table = Table::default();

    // Heading Row
    if !bals.is_empty() {
        let headings = cmd.columns.iter().map(|col| Cell::from(col.to_string())).collect();
        table.set_heading_row(headings);
    }

    // Account Rows
    for line in bals.iter().filter(|l| !l.valued_amount().is_zero()) {
        let mut row = vec![];
        for col in cmd.columns.iter() {
            match col {
                BalColumn::Account => row.push(
                    Cell::from(line.account().to_string()).with_foreground(Some(Colour::Blue)),
                ),
                BalColumn::Amount => row.push(Cell::from(line.valued_amount().amount())),
                BalColumn::Value(unit) => {
                    if let Some(unit) = bals.value_units().iter().find(|u| u.code() == unit) {
                        row.push(Cell::from(line.valued_amount().value_in(unit).unwrap()));
                    } else {
                        // The unit could be invalid, or we just never encountered it.
                        row.push(Cell::from(&""));
                    }
                }
            }
        }
        table.add_row(row);
    }

    /*
    // Total rows for each unit
    if num_rows > 1 {
        table.add_separator_row('-', (2 + bals.value_units().len()) as u16);
        let mut num_totals = 0;
        for unit in bals.units() {
            let mut total = unit.with_quantity(0);
            let mut val_totals = vec![];
            for vu in bals.value_units().iter() {
                val_totals.push(vu.with_quantity(0));
            }
            for bl in bals
                .iter()
                .filter(|l| !l.valued_amount().is_zero())
                .filter(|l| l.valued_amount().unit() == unit)
            {
                total += bl.valued_amount().amount();
                for (i, val_curr) in bals.value_units().iter().copied().enumerate() {
                    val_totals[i] += bl.valued_amount().value_in(val_curr).unwrap();
                }
            }
            let mut row = vec![Cell::from(&"")];
            row.push(Cell::from(total));
            for val in val_totals {
                row.push(Cell::from(val));
            }
            table.add_row(row);
            num_totals += 1;
        }

        // Grand total row
        if num_totals > 1 && !bals.value_units().is_empty() {
            table.add_separator_row('-', (2 + bals.value_units().len()) as u16);
            let mut val_totals = vec![];
            for vu in bals.value_units().iter() {
                val_totals.push(vu.with_quantity(0));
            }
            for bl in bals.iter().filter(|l| !l.valued_amount().is_zero()) {
                for (i, val_curr) in bals.value_units().iter().copied().enumerate() {
                    val_totals[i] += bl.valued_amount().value_in(val_curr).unwrap();
                }
            }
            let mut row = vec![Cell::from(&""), Cell::from(&"")];
            for val in val_totals {
                row.push(Cell::from(val));
            }
            table.add_row(row);
        }
    }*/

    table
}*/
