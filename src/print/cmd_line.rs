/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::print::print_command::PrintCommand;
use journ_core::error::JournResult;
use journ_core::journal::Journal;
use journ_core::report::command::IntoExecCommand;
use journ_core::report::command::arguments::{Arguments, DateTimeFormatCommand};
use journ_core::report::command::cmd_line::BeginAndEndArguments;

#[derive(clap::Args, Debug)]
#[command(name = "print", about = "Prints the journal file")]
pub struct PrintArguments {
    #[command(flatten)]
    begin_and_end: BeginAndEndArguments,
    #[arg(
        value_name = "ACCOUNT_FILTER",
        value_delimiter = ',',
        help = "Filters the print output to just the matching accounts"
    )]
    account_filter: Vec<String>,
    #[arg(short, value_name = "JOURNAL_FILE", help = "The file to print")]
    print_file: Option<String>,
}

impl IntoExecCommand for PrintArguments {
    type Command = PrintCommand;

    fn into_exec_cmd(self, journ: &Journal, args: &Arguments) -> JournResult<Self::Command> {
        let datetime_fmt_cmd = DateTimeFormatCommand::from_args_or_config(args, journ.config());
        let cmd = PrintCommand {
            begin_and_end_cmd: self.begin_and_end.into_cmd(&datetime_fmt_cmd),
            datetime_fmt_cmd,
            accounts: self.account_filter.clone(),
            print_file: None,
        };
        Ok(cmd)
    }
}
