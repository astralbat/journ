/*
 * Copyright (c) 2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::{read_date_time_args, ExecCommand, IntoExecCommand};
use journ_core::arguments::{Arguments, CsvCommand};
use journ_core::error::JournResult;
use journ_core::journal::Journal;

#[derive(clap::Args, Debug)]
#[command(name = "csv", about = "Comma separated value report")]
pub struct CsvArguments {
    #[arg(value_name = "ACCOUNT_FILTER", help = "Filter accounts by regexp pattern")]
    account_filter: Vec<String>,
    #[arg(
        short = 'd',
        value_name = "DESCRIPTION_FILTER",
        help = "Filter descriptions by regexp pattern"
    )]
    description_filter: Vec<String>,
    group_by: Option<String>,
}

impl IntoExecCommand for CsvArguments {
    type Command = CsvCommand;
    #[allow(clippy::field_reassign_with_default)]
    fn into_exec_cmd(self, journ: &Journal) -> JournResult<Self::Command> {
        let mut cmd = CsvCommand::default();
        cmd.datetime_args = read_date_time_args(journ)?;
        cmd.set_account_filter(self.account_filter);
        cmd.set_description_filter(self.description_filter);
        cmd.set_group_postings_by(self.group_by)?;
        Ok(cmd)
    }
}

impl ExecCommand for CsvCommand {
    fn execute(&self, mut journ: Journal) -> JournResult<()> {
        let args = Arguments::get();
        journ.csv(args)
    }
}
