/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::bal::bal_command::BalCommand;
use clap::Parser;
use journ_core::error::JournResult;
use journ_core::journal_context::JContext;
use journ_core::report::command::IntoExecCommand;
use journ_core::report::command::arguments::{Arguments, DateTimeFormatCommand};
use journ_core::report::command::cmd_line::BeginAndEndArguments;
use journ_core::report::command::table_format_args::TableFormatArguments;

#[derive(Parser, Debug)]
#[command(name = "bal", about = "Print balances of accounts")]
pub struct BalArguments {
    #[command(flatten)]
    begin_and_end: BeginAndEndArguments,
    #[command(flatten)]
    table_format_arguments: TableFormatArguments,
    #[arg(value_name = "ACCOUNT_FILTER", help = "Filter accounts by pattern. E.g. Expenses..Food")]
    account_filter: Vec<String>,
    #[arg(short = 'u', long = "unit", value_name = "UNIT", help = "Filter by unit expression")]
    unit_filter: Vec<String>,
    #[arg(
        short = 'd',
        long = "description",
        value_name = "DESCRIPTION",
        help = "Filter by description"
    )]
    description_filter: Vec<String>,
    #[arg(
        short = 'f',
        long,
        value_name = "FILTER_EXPR",
        help = "A filter expression applied to postings/entries. Must evaluate to a boolean. E.g. startsWith(file, \"savings\")"
    )]
    filter: Vec<String>,
    #[arg(
        short = 'o',
        help = "A comma separated list of columns to print. This can be: \
        account: the account matched by the account filter, \
        amount: the summed amount for each account matched, \
        value(<unit>): the value of the amount in a particular unit. Ay valuation will be looked up on the entry or evaluated on the date/time of the entry, \
        cosum(<account>): an additional sum for the accounts matched on matched entries"
    )]
    column_spec: Option<String>,
    #[arg(long = "where", help = "Filter the results on conditions")]
    where_conditions: Option<String>,
    #[arg(short = 'H', long = "no-heading", help = "do not print column headings")]
    no_heading: bool,
    #[arg(long, help = "A header note for the table")]
    header: Option<String>,
    #[arg(long, help = "A footer note for the table")]
    footer: Option<String>,
    #[arg(short = 'T', long = "no-total", help = "do not print total row")]
    no_total: bool,
    #[arg(long, help = "Keep totals to a single row; do not show lists")]
    short_total: bool,
    #[arg(
        long = "total-as",
        help = "Use an alternative expression specification for the total rows"
    )]
    total_as: Option<String>,
    #[arg(
        long = "grand-total-as",
        help = "Use an alternative expression specification for the grand total row (must be first in chain)."
    )]
    grand_total_as: Option<String>,
    #[arg(short = 'z', long = "zero", help = "show groups that have zero amounts")]
    show_zeros: bool,
    #[arg(long, help = "Title to display for the table")]
    title: Option<String>,
    #[arg(
        short = 'g',
        long = "group-by",
        help = "group by a column, e.g. unit, account, date. Default is account"
    )]
    group_by: Option<String>,
    #[arg(
        long = "order-by",
        value_delimiter = ',',
        value_name = "ORDER_BY",
        help = "Order the output by the specified column(s)"
    )]
    order_by: Option<String>,
    #[arg(long = "descending")]
    order_descending: bool,
    #[arg(long, allow_hyphen_values = true, num_args = 0..)]
    chain: Vec<String>,
}

impl BalArguments {
    fn into_exec_cmd_inner(self, args: &Arguments) -> JournResult<BalCommand> {
        let datetime_fmt_cmd =
            DateTimeFormatCommand::from_args_or_config(args, JContext::get().journal().config());
        let bal_cmd = BalCommand {
            begin_and_end_cmd: self.begin_and_end.into_cmd(&datetime_fmt_cmd),
            table_fmt_cmd: self.table_format_arguments.into_cmd(),
            datetime_fmt_cmd,
            account_filter: self
                .account_filter
                .into_iter()
                .map(|accounts| accounts.split(",").map(String::from).collect())
                .collect(),
            unit_filter: self
                .unit_filter
                .into_iter()
                .map(|units| units.split(",").map(String::from).collect())
                .collect(),
            description_filter: self.description_filter,
            filter: self.filter,
            column_spec: self.column_spec,
            where_conditions: self.where_conditions,
            no_heading: self.no_heading,
            header: self.header,
            footer: self.footer,
            title: self.title,
            no_total: self.no_total,
            short_total: self.short_total,
            total_as_spec: self.total_as,
            grand_total_as_spec: self.grand_total_as,
            show_zeros: self.show_zeros,
            order_by_spec: self.order_by,
            order_ascending: !self.order_descending,
            group_by: self.group_by,
            chain: if self.chain.is_empty() {
                None
            } else {
                // Clap ignores first argument
                let mut chain_args = vec!["bal".to_string()];
                chain_args.append(&mut self.chain.clone());
                Some(Box::new(BalArguments::parse_from(chain_args).into_exec_cmd_inner(args)?))
            },
        };
        Ok(bal_cmd)
    }
}

impl IntoExecCommand for BalArguments {
    type Command = BalCommand;
    fn into_exec_cmd(self, args: &Arguments) -> JournResult<Self::Command> {
        let mut bal_cmd = self.into_exec_cmd_inner(args)?;
        // The chained command is merged, with default options set by the previous command.
        let mut cmd_opt = Some(&mut bal_cmd);
        while let Some(cmd) = cmd_opt {
            cmd.chain = cmd.chain.as_ref().map(|c| Box::new(c.merge_from(&cmd)));
            cmd_opt = cmd.chain.as_deref_mut();
        }
        Ok(bal_cmd)
    }
}
