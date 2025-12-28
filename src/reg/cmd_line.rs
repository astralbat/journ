/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reg::reg_command::RegCommand;
use journ_core::error::JournResult;
use journ_core::journal::Journal;
use journ_core::reporting::command::IntoExecCommand;
use journ_core::reporting::command::arguments::{Arguments, DateTimeFormatCommand};
use journ_core::reporting::command::cmd_line::BeginAndEndArguments;

#[derive(clap::Args, Debug)]
#[command(name = "reg", about = "Register of accounts")]
pub struct RegArguments {
    #[command(flatten)]
    begin_and_end: BeginAndEndArguments,
    #[arg(
        value_name = "ACCOUNT_FILTER",
        help = "Filter accounts by regexp pattern",
        use_value_delimiter = true
    )]
    account_filter: Vec<String>,
    #[arg(
        short = 'u',
        long = "unit",
        value_name = "UNIT",
        help = "Filter by unit expression",
        use_value_delimiter = true
    )]
    unit_filter: Vec<String>,
    #[arg(
        short = 'd',
        value_name = "DESCRIPTION_FILTER",
        help = "Filter descriptions by regexp pattern",
        use_value_delimiter = true
    )]
    description_filter: Vec<String>,
    #[arg(
        short = 'f',
        long = "file",
        value_name = "FILE",
        help = "Filter by file",
        use_value_delimiter = true
    )]
    file_filter: Vec<String>,
    #[arg(
        short = 'o',
        help = "A comma separated list of columns to print. This can be: \
        date: the date of the entry, \
        account: the account matched by the account filter, \
        description: the description of the entry, \
        amount: the summed amount for each account matched, \
        balance: the running balance for the register",
        default_value = "Date,Description,Account,Amount,Balance"
    )]
    column_spec: String,
}

impl IntoExecCommand for RegArguments {
    type Command = RegCommand;
    fn into_exec_cmd(self, journ: &Journal, args: &Arguments) -> JournResult<Self::Command> {
        let datetime_fmt_cmd = DateTimeFormatCommand::from_args_or_config(args, journ.config());
        let cmd = RegCommand {
            begin_and_end_cmd: self.begin_and_end.into_cmd(&datetime_fmt_cmd),
            datetime_fmt_cmd,
            unit_filter: self.unit_filter,
            account_filter: self.account_filter,
            description_filter: self.description_filter,
            file_filter: self.file_filter,
            column_spec: self.column_spec,
        };
        Ok(cmd)
    }
}
