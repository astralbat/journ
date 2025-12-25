/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
mod print_command;

use crate::IntoExecCommand;
use crate::print::print_command::PrintCommand;
use journ_core::error::JournResult;
use journ_core::journal::Journal;

#[derive(clap::Args, Debug)]
#[command(name = "print", about = "Prints the journal file")]
pub struct PrintArguments {
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

    fn into_exec_cmd(self, _journ: &Journal) -> JournResult<Self::Command> {
        let mut cmd = PrintCommand::default();
        cmd.set_accounts(self.account_filter.clone());
        Ok(cmd)
    }
}
