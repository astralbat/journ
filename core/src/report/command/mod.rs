/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::error::{BlockContextError, JournError, JournResult};
use crate::journal::Journal;
use crate::report::command::arguments::{Arguments, Command};
use crate::report::command::chained_result::ChainingResult;
use std::env;

pub mod arguments;
pub mod chained_result;
pub mod cmd_line;

pub fn print_jerror(mut e: JournError) {
    e.prune_except_last::<BlockContextError>();

    eprint!("{}:", env::args().next().unwrap(),);
    eprintln!(" {}", e);
}

pub trait ExecCommand: Command {
    fn execute<'h>(
        &'h self,
        journ: &'h mut Journal<'h>,
        chained: Option<ChainingResult<'h>>,
    ) -> JournResult<()>;

    fn as_chainable(&self) -> Option<&dyn ChainableCommand> {
        None
    }
}

/// Used to create a command from command arguments.
pub trait IntoExecCommand {
    type Command: ExecCommand;

    fn into_exec_cmd(self, journ: &Journal, args: &Arguments) -> JournResult<Self::Command>;
}

pub trait ChainableCommand: ExecCommand {
    fn next_chain(&'static self) -> Option<&'static dyn ChainableCommand>;
}
