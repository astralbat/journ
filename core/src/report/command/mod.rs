/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::error::{BlockContextError, JournError, JournResult};
use crate::report::command::arguments::{Arguments, Cmd, Command};
use crate::report::command::chained_result::ChainingResult;
use crate::report::expr::RowData;
use std::any::Any;
use std::io::Write;
use std::{env, fmt};
use yaml_rust2::YamlEmitter;

pub mod arguments;
pub mod chained_result;
pub mod cmd_line;

pub fn print_jerror(mut e: JournError) {
    e.prune_except_last::<BlockContextError>();

    eprint!("{}:", env::args().next().unwrap(),);
    eprintln!(" {}", e);
}

pub trait ExecCommand: Command {
    fn execute<'h>(&'h self, chained: Option<ChainingResult<'h>>) -> JournResult<()>;

    fn as_chainable(&self) -> Option<&dyn ChainableCommand> {
        None
    }
}

/// Used to create a command from command arguments.
pub trait IntoExecCommand {
    type Command: ExecCommand;

    fn into_exec_cmd(self, args: &Arguments) -> JournResult<Self::Command>;
}

pub trait ChainableCommand: ExecCommand {
    fn next_chain(&'static self) -> Option<&'static dyn ChainableCommand>;

    fn chain_or_print(&self, chaining_res: ChainingResult) -> JournResult<()> {
        // Move to next chain
        if let Some(next) = Cmd::advance_chain() {
            return next.execute(Some(chaining_res));
        }

        // Otherwise print output
        match chaining_res {
            ChainingResult::Table(table) => {
                // Using print! macro can cause panic when piping. Use write and ignore the result.
                let stdout = std::io::stdout();
                let _ = write!(&mut stdout.lock(), "{}", table);
            }
            ChainingResult::Yaml(root) => {
                let mut string = String::new();
                let mut emitter = YamlEmitter::new(&mut string);
                emitter.dump(&root).map_err(|_| fmt::Error).unwrap();
                // Using print! macro can cause panic when piping. Use write and ignore the result.
                let stdout = std::io::stdout();
                let _ = write!(&mut stdout.lock(), "{}", string);
            }
        }

        Ok(())
    }
}
