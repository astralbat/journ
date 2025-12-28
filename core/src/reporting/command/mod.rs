/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::error::{BlockContextError, JournError, JournResult};
use crate::journal::Journal;
use crate::reporting::command::arguments::{Arguments, Command};
use std::env;

pub mod arguments;
pub mod cmd_line;

pub fn print_jerror(mut e: JournError) {
    e.prune_except_last::<BlockContextError>();

    eprint!("{}:", env::args().next().unwrap(),);
    eprintln!(" {}", e);
}

pub trait ExecCommand: Command {
    fn execute<'j, 'h: 'j>(&'h self, journ: &'j mut Journal<'j>) -> JournResult<()>;
}

/// Used to create a command from command arguments.
pub trait IntoExecCommand {
    type Command: ExecCommand;

    fn into_exec_cmd(self, journ: &Journal, args: &Arguments) -> JournResult<Self::Command>;
}
