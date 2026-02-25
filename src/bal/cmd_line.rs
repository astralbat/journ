/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::bal::bal_command::BalCommand;
use journ_core::error::JournResult;
use journ_core::journal::Journal;
use journ_core::report::command::IntoExecCommand;
use journ_core::report::command::arguments::{Arguments, DateTimeFormatCommand};
use journ_core::report::command::cmd_line::BeginAndEndArguments;

#[derive(clap::Args, Debug)]
#[command(name = "bal", about = "Print balances of accounts")]
pub struct BalArguments {
    #[command(flatten)]
    begin_and_end: BeginAndEndArguments,
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
    #[arg(short = 'f', long = "file", value_name = "FILE", help = "Filter by file")]
    file_filter: Vec<String>,
    #[arg(
        short = 'o',
        help = "A comma separated list of columns to print. This can be: \
        account: the account matched by the account filter, \
        amount: the summed amount for each account matched, \
        value(<unit>): the value of the amount in a particular unit. Ay valuation will be looked up on the entry or evaluated on the date/time of the entry, \
        cosum(<account>): an additional sum for the accounts matched on matched entries",
        default_value = "Account,Sum(amount)"
    )]
    column_spec: String,
    #[arg(short = 'H', long = "no-header", help = "do not print header row")]
    no_header: bool,
    #[arg(short = 'T', long = "no-total", help = "do not print total row")]
    no_total: bool,
    #[arg(short = 'z', long = "zero", help = "show groups that have zero amounts")]
    show_zeros: bool,
    #[arg(
        short = 'g',
        long = "group-by",
        help = "group by a column, e.g. unit, account, date. Default is account",
        default_value = "account"
    )]
    group_by: String,
    #[arg(
        long = "order-by",
        value_delimiter = ',',
        value_name = "ORDER_BY",
        help = "Order the output by the specified column(s)"
    )]
    order_by: Option<String>,
    #[arg(long = "descending")]
    order_descending: bool,
}

impl IntoExecCommand for BalArguments {
    type Command = BalCommand;
    fn into_exec_cmd(self, journ: &Journal, args: &Arguments) -> JournResult<Self::Command> {
        let datetime_fmt_cmd = DateTimeFormatCommand::from_args_or_config(args, journ.config());
        let bal_cmd = BalCommand {
            begin_and_end_cmd: self.begin_and_end.into_cmd(&datetime_fmt_cmd),
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
            file_filter: self.file_filter,
            column_spec: self.column_spec,
            no_header: self.no_header,
            no_total: self.no_total,
            show_zeros: self.show_zeros,
            order_by_spec: self.order_by,
            order_ascending: !self.order_descending,
            group_by: self.group_by,
        };
        Ok(bal_cmd)
    }
}
