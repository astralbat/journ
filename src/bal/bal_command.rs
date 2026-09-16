/*
 * Copyright (c) 2024-2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::ExecCommand;
use journ_core::configuration::{AccountFilter, DescriptionFilter, UnitFilter};
use journ_core::err;
use journ_core::error::JournResult;
use journ_core::journal::Journal;
use journ_core::journal_context::JContext;
use journ_core::journal_entry::JournalEntry;
use journ_core::journal_entry_query::PostingQuery;
use journ_core::posting::Posting;
use journ_core::report::command::ChainableCommand;
use journ_core::report::command::arguments::{Command, DateTimeFormatCommand};
use journ_core::report::command::chained_result::ChainingResult;
use journ_core::report::command::cmd_line::BeginAndEndCommand;
use journ_core::report::command::table_format_args::TableFormatCommand;
use journ_core::report::expr::parser::parse_plan;
use journ_core::report::expr::{
    Expr, GroupState, NullBalanceUpdater, PostingContext, RowData, ScalarExpr, parser,
};
use journ_core::report::table2::{
    PolicyWrappingCell, Row, RowKind, Rows, SpannedCell, StyledCell, Table, WrapPolicy,
};
use journ_core::report::term_style::{Style, Weight};
use std::collections::HashMap;
use std::ops::Deref;

#[derive(Default, Debug, Clone)]
pub struct BalCommand {
    pub(super) datetime_fmt_cmd: DateTimeFormatCommand,
    pub(super) begin_and_end_cmd: BeginAndEndCommand,
    pub(super) table_fmt_cmd: TableFormatCommand,
    pub(super) filter: Vec<String>,
    /// Each inner Vec is a partition
    pub(super) account_filter: Vec<Vec<String>>,
    pub(super) unit_filter: Vec<Vec<String>>,
    pub(super) description_filter: Vec<String>,
    pub(super) title: Option<String>,
    pub(super) header: Option<String>,
    pub(super) footer: Option<String>,
    pub(super) no_heading: bool,
    pub(super) no_total: bool,
    pub(super) short_total: bool,
    pub(super) total_as_spec: Option<String>,
    pub(super) grand_total_as_spec: Option<String>,
    pub(super) show_zeros: bool,
    pub(super) group_by: Option<String>,
    pub(super) order_by_spec: Option<String>,
    pub(super) order_ascending: bool,
    pub(super) column_spec: Option<String>,
    pub(super) where_conditions: Option<String>,
    pub(super) chain: Option<Box<BalCommand>>,
}

impl BalCommand {
    pub fn account_filter(&self) -> AccountFilter {
        AccountFilter::new(self.account_filter.iter().flatten())
    }

    pub fn unit_filter(&self) -> UnitFilter {
        UnitFilter::new(self.unit_filter.iter().flatten())
    }

    pub fn description_filter(&self) -> DescriptionFilter {
        DescriptionFilter::new(self.description_filter.iter())
    }

    pub fn expr_filter(&self) -> JournResult<Vec<ScalarExpr>> {
        Ok(self
            .filter
            .iter()
            .map(|expr| {
                parser::parse_non_aggregate(expr).map_err(|e| {
                    err!(format!("Error parsing filter expression '{}'", expr)).with_source(e)
                })
            })
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect())
    }

    pub fn column_spec(&self) -> &str {
        self.column_spec.as_deref().unwrap_or("Account, Sum(Amount)")
    }

    pub fn group_by(&self) -> Option<&str> {
        self.group_by.as_deref()
    }

    pub fn filtered_postings<'a, 'h, 'j>(
        &'a self,
        journ: &'j Journal<'h>,
    ) -> impl Fn() -> JournResult<
        Box<dyn Iterator<Item = JournResult<(&'h JournalEntry<'h>, &'h Posting<'h>)>> + 'a>,
    > + 'a
    where
        'h: 'a,
        'j: 'a,
    {
        move || {
            let cmd = JContext::get().cast_cmd::<BalCommand>();

            let query = PostingQuery::between(cmd.begin_and_end_cmd.begin_end_range())
                .with_description_filter(self.description_filter())
                .with_unit_filter(self.unit_filter())
                .with_account_filter(self.account_filter())
                .with_expr_filter(self.expr_filter()?);
            Ok(Box::new(journ.postings(query)))
            /*
            Ok(Box::new(
                journ
                    .entry_range(cmd.begin_and_end_cmd.begin_end_range())
                    .filter(move |e| description_filter.is_included(e.description()))
                    .flat_map(move |e| e.postings().map(move |p| (e, p)))
                    .filter(move |(_e, p)| account_filter.is_included(p.account()))
                    .filter(move |(_e, p)| unit_filter.is_included(p.unit()))
                    .filter_map(move |(e, p)| {
                        let mut context = PostingContext::new(e, p);
                        for filter_expr in &filter_exprs {
                            match filter_expr.eval(&mut context) {
                                Ok(val) => match val.as_bool() {
                                    Some(true) => continue,
                                    Some(false) => return None,
                                    None => {
                                        return Some(Err(err!(format!(
                                            "Filter expression must evaluate to true of false: {}",
                                            filter_expr
                                        ))));
                                    }
                                },
                                Err(e) => return Some(Err(e)),
                            }
                        }
                        Some(Ok((e, p)))
                    }),
            ))*/
        }
    }

    fn create_table<'cell>(&self) -> Table<'cell> {
        Table::from(&self.table_fmt_cmd)
    }

    fn append_table<'h, 'cell>(
        &self,
        mut table: Table<'cell>,
        column_expressions: &[Expr],
        data_rows: Vec<RowData<'h>>,
        total_row: Option<RowData<'h>>,
    ) -> Table<'cell>
    where
        'h: 'cell,
    {
        let chain_pos = JContext::get().chain_position();
        let mut rows = vec![];

        if chain_pos > 0 {
            rows.push(table.create_chain_separator());
        }
        if let Some(title) = &self.title {
            let (title, title_sep) = table.create_title_row(
                title.clone(),
                data_rows.iter().map(|r| r.column_count()).max().unwrap_or(1),
            );
            rows.push(title);
            rows.push(title_sep);
        }
        // Heading Row
        if !self.no_heading {
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

        for row_data in data_rows.into_iter() {
            let row = Row::new(
                row_data.column_values.into_iter().map(|c| c.into_cell_ref(self.show_zeros, true)),
            );
            rows.push(row);
        }

        // Add total row
        if let Some(total) = total_row {
            rows.push(table.create_separator_row(
                RowKind::TotalSeparator,
                table.total_separator(),
                total.column_values.len(),
            ));
            let mut row = Row::new(
                total
                    .column_values
                    .into_iter()
                    .map(|c| c.into_cell_ref(self.show_zeros, !self.short_total)),
            );
            row.set_kind(RowKind::Total);
            rows.push(row);
        }

        let max_column_count = table.rows().column_count().max(rows.column_count());

        // Insert header
        if let Some(header) = &self.header
            && !header.is_empty()
        {
            let mut row: Row = Row::default();
            row.set_kind(RowKind::Header);
            let spanned = SpannedCell::new(header.clone(), max_column_count);
            row.append(spanned);
            let insert_pos =
                rows.iter().position(|r| r.kind() == RowKind::TitleSeparator).unwrap_or(0);
            rows.insert(insert_pos, row);
        }
        // Insert footer
        if let Some(footer) = &self.footer
            && !footer.is_empty()
        {
            let mut row: Row = Row::default();
            row.set_kind(RowKind::Footer);
            let spanned = SpannedCell::new(footer.clone(), max_column_count);
            row.append(spanned);
            rows.push(row);
        }

        if self.show_zeros || rows.iter().any(|r| r.kind() == RowKind::Data) {
            for row in rows {
                table.push_row(row);
            }
        }

        table
    }

    pub fn merge_from(&self, other: &Self) -> Self {
        let cmd = Self {
            datetime_fmt_cmd: self.datetime_fmt_cmd.merge_from(&other.datetime_fmt_cmd),
            begin_and_end_cmd: self.begin_and_end_cmd.merge_from(&other.begin_and_end_cmd),
            table_fmt_cmd: self.table_fmt_cmd.merge_from(&other.table_fmt_cmd),
            title: self.title.clone().or(other.title.clone()),
            filter: if !self.filter.is_empty() {
                self.filter.clone()
            } else {
                other.filter.clone()
            },
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
            description_filter: if !self.description_filter.is_empty() {
                self.description_filter.clone()
            } else {
                other.description_filter.clone()
            },
            no_heading: if self.no_heading { !other.no_heading } else { other.no_heading },
            header: self.header.clone().or(other.header.clone()),
            footer: self.footer.clone().or(other.footer.clone()),
            no_total: if self.no_total { !other.no_total } else { other.no_total },
            short_total: if self.short_total { !other.short_total } else { other.short_total },
            show_zeros: if self.show_zeros { !self.show_zeros } else { other.show_zeros },
            total_as_spec: self.total_as_spec.clone().or(other.total_as_spec.clone()),
            grand_total_as_spec: self
                .grand_total_as_spec
                .clone()
                .or(other.grand_total_as_spec.clone()),
            group_by: self.group_by.clone().or(other.group_by.clone()),
            order_by_spec: self.order_by_spec.clone().or(other.order_by_spec.clone()),
            order_ascending: if self.order_ascending {
                !other.order_ascending
            } else {
                other.order_ascending
            },
            column_spec: self.column_spec.clone().or(other.column_spec.clone()),
            where_conditions: match &self.where_conditions {
                Some(conds) if !conds.is_empty() => Some(conds.clone()),
                Some(_) => None,
                None => other.where_conditions.clone(),
            },
            chain: self.chain.clone(),
        };
        cmd
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
    fn next_chain(&self) -> Option<&dyn ChainableCommand> {
        Some(self.chain.as_ref()?.deref())
    }
}

impl ExecCommand for BalCommand {
    fn execute<'h, 'a, 'cell>(
        &self,
        chained: Option<ChainingResult<'h, 'a, 'cell>>,
    ) -> JournResult<()>
    where
        'h: 'cell,
    {
        let journ = JContext::get().journal();
        let plan = parse_plan(
            self.column_spec(),
            self.where_conditions.as_deref(),
            !self.no_total,
            self.group_by(),
            HashMap::new(),
            self.order_by_spec.as_deref(),
            self.order_ascending,
            self.total_as_spec.as_deref(),
            self.grand_total_as_spec.as_deref(),
        )?;

        let previously_chained = chained.is_some();
        let init_grand_total = if !self.no_total {
            Some(GroupState::try_from(plan.grand_total_spec().clone())?)
        } else {
            None
        };
        let (table, mut grand_total) = chained
            .map(|c| c.into_table().unwrap())
            .unwrap_or_else(|| (self.create_table(), init_grand_total));

        let (mut data, total) = plan.execute(
            self.filtered_postings(&journ)()?,
            grand_total.as_mut(),
            |(entry, pst)| PostingContext::new(entry, pst),
            None::<NullBalanceUpdater<'h, (&'h JournalEntry<'h>, &'h Posting<'h>)>>,
        )?;

        // Filter out zeros if not required
        data.retain(|row_data| {
            let any_non_zero = if row_data
                .column_values
                .iter()
                .flat_map(|cv| cv.as_list())
                .any(|cv| cv.as_amount().is_some())
            {
                row_data
                    .column_values
                    .iter()
                    .flat_map(|cv| cv.as_list())
                    .filter_map(|cv| cv.as_amount())
                    .any(|(amount, _)| !amount.is_zero())
            } else {
                true
            };
            // Don't show if all amounts are zero unless show_zeros option is used
            any_non_zero || self.show_zeros
        });

        // Show the total only if it has been specified _or_ if it is unique. We use a heuristical approach here by understanding
        // whether the total compresses any of the aggregate columns - that is, the number of values
        // in the total is less than non-total rows.
        let show_total = match &total {
            None => false,
            Some(_) if self.total_as_spec.is_some() => true,
            Some(total) => 'is_total_unique: {
                for icol in plan
                    .column_spec()
                    .exprs()
                    .iter()
                    .enumerate()
                    .filter(|e| e.1.is_aggregate())
                    .map(|e| e.0)
                {
                    let mut num_vals_in_col = 0;
                    for row_data in data.iter() {
                        num_vals_in_col += row_data.column_values[icol].as_list().len();
                    }
                    let num_vals_in_total = total.column_values[icol].as_list().len();
                    if num_vals_in_total > 0 && num_vals_in_total < num_vals_in_col {
                        break 'is_total_unique true;
                    }
                }
                false
            }
        };

        let chain_result = ChainingResult::Table {
            table: self.append_table(
                table,
                plan.column_spec().exprs(),
                data,
                if show_total { total } else { None },
            ),
            grand_total,
        };
        self.chain_or_print(chain_result, previously_chained)?;

        // We only need to write the price database.
        journ.config().price_databases().into_iter().for_each(|db| db.write_file().unwrap());

        Ok(())
    }

    fn as_chainable(&self) -> Option<&dyn ChainableCommand> {
        Some(self)
    }
}
