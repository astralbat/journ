/*
 * Copyright (c) 2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::{ExecCommand, IntoExecCommand};
use journ_core::arguments::PrintCommand;
use journ_core::error::JournResult;
use journ_core::journal::Journal;
use journ_core::journal_node::JournalNode;
use std::path::Path;
use std::process::exit;

#[derive(clap::Args, Debug)]
#[command(name = "print", about = "Prints the journal file")]
pub struct PrintArguments {
    #[arg(
        short,
        value_name = "FILTER",
        help = "Filters the print output to just the matching accounts"
    )]
    account_filter: Option<String>,
    #[arg(short, value_name = "JOURNAL_FILE", help = "The file to print after computing tax")]
    print_file: Option<String>,
}

impl IntoExecCommand for PrintArguments {
    type Command = PrintCommand;

    fn into_exec_cmd(self, _journ: &Journal) -> JournResult<Self::Command> {
        let cmd = PrintCommand { account_filter: self.account_filter, print_file: self.print_file };
        Ok(cmd)
    }
}

impl ExecCommand for PrintCommand {
    fn execute(&self, journ: Journal) -> JournResult<()> {
        let account_filter = &self.account_filter;
        match self.print_file.as_ref().map(|s| s.to_string()) {
            Some(pf) => match journ.find_node_by_filename(Path::new(&pf)) {
                Some(jf) => jf.print(account_filter.as_deref()).unwrap(),
                None => {
                    eprintln!("No file matches: {}", pf);
                    exit(1)
                }
            },
            None => JournalNode::print_all(journ.root(), account_filter.as_deref()).unwrap(),
        }
        Ok(())
    }
}
