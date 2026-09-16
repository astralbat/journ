/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::error::{BlockContextError, JournError, JournResult};
use crate::journal_context::JContext;
use crate::report::command::arguments::{Arguments, Command};
use crate::report::command::chained_result::ChainingResult;
use crate::report::expr::{ColumnValue, RowData, TotalContext};
use crate::report::table2::{Row, RowKind, StyledCell};
use crate::report::term_style::{Style, Weight};
use std::io::Write;
use std::{env, fmt};
use yaml_rust2::YamlEmitter;

pub mod arguments;
pub mod chained_result;
pub mod cmd_line;
pub mod table_format_args;

pub fn print_jerror(mut e: JournError) {
    e.prune_except_last::<BlockContextError>();

    eprint!("{}:", env::args().next().unwrap(),);
    eprintln!(" {}", e);
}

pub trait ExecCommand: Command {
    fn execute<'h, 'a, 'cell>(
        &self,
        chained: Option<ChainingResult<'h, 'a, 'cell>>,
    ) -> JournResult<()>
    where
        'h: 'cell;

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
    fn next_chain(&self) -> Option<&dyn ChainableCommand>;

    fn chain_or_print(
        &self,
        chaining_res: ChainingResult,
        previously_chained: bool,
    ) -> JournResult<()> {
        // Move to next chain
        if let Some(next) = JContext::get().advance_chain() {
            return next.execute(Some(chaining_res));
        }

        // Otherwise print output
        match chaining_res {
            ChainingResult::Table { mut table, grand_total } => {
                if let Some(grand_total) = grand_total
                    && previously_chained
                {
                    let mut total_context = TotalContext::new(grand_total.finalize());
                    let mut row_data = RowData::default();
                    for col in grand_total.exprs() {
                        row_data.push_column_value(
                            col.eval(&mut total_context)
                                .unwrap_or_else(|_| ColumnValue::StringRef("")),
                        );
                    }
                    let gt_sep1 = table.create_separator_row(
                        RowKind::GrandTotalSeparator,
                        ' ',
                        grand_total.exprs().len(),
                    );
                    let gt_sep2 = table.create_grand_total_separator();
                    table.push_row(gt_sep1);
                    table.push_row(gt_sep2);
                    let row = Row::new(row_data.column_values.into_iter().map(|c| {
                        StyledCell::new(
                            c.into_cell_ref(true, true),
                            Style::default().with_weight(Weight::Bold),
                        )
                    }));
                    table.push_row(row);
                }
                // Using print! macro can cause panic when piping. Use write and ignore the result.
                let stdout = std::io::stdout();
                let _ = writeln!(&mut stdout.lock(), "{}", table);
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
