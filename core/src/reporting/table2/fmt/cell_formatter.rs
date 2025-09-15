/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table2::Cell;
use crate::reporting::table2::cell::blank::BLANK_CELL;
use crate::reporting::table2::cell_width::CellWidth;
use crate::reporting::table2::column::ColumnsVec;
use crate::reporting::table2::column::TableColumn;
use crate::reporting::table2::row::Row;
use crate::reporting::table2::table::ColumnPreferences;
use std::fmt;
use std::fmt::Write;

/// A cell formatter is a kind of cell visitor, operating via double dispatch to print the cell's content with `Cell::print(CellFormatter)`.
pub trait CellFormatter<'format>: Write {
    fn format_cell(
        &mut self,
        cell: &dyn Cell,
        row: &Row,
        line: usize,
        columns: &[TableColumn],
    ) -> fmt::Result;

    fn format_row_start(&mut self, _row: &Row, _row_num: usize, _line: usize) -> fmt::Result {
        Ok(())
    }

    fn format_row_end(&mut self, _row: &Row, _row_num: usize, _line: usize) -> fmt::Result {
        Ok(())
    }

    fn format_line_start(&mut self, _row: &Row, _row_num: usize, _line: usize) -> fmt::Result {
        Ok(())
    }

    fn format_line_end(&mut self, _row: &Row, _row_num: usize, _line: usize) -> fmt::Result {
        Ok(())
    }

    fn prepare_columns(&mut self, _columns: &mut Vec<TableColumn>) {}

    fn width(&self) -> Option<&CellWidth>;

    fn set_width(&mut self, width: Option<CellWidth>);

    /// Allow printing of escaped terminal color codes
    fn color(&self) -> bool {
        false
    }

    /// Pushes a transformer onto the stack. The last one pushed is the first one applied.
    /// Cells using transformers should ensure that they pop them off again before returning.
    fn push_transformer(&mut self, f: Box<dyn Transformer + 'format>);

    /// Pops a transformer off the stack, returning it if there was one.
    fn pop_transformer(&mut self) -> Option<Box<dyn Transformer + 'format>>;

    /// Current column position
    fn cursor_col(&self) -> usize;

    fn print<'cell>(
        &mut self,
        rows: &[Row<'cell>],
        column_preferences: &[ColumnPreferences],
    ) -> fmt::Result {
        let mut columns = create_columns(rows, column_preferences);
        self.prepare_columns(&mut columns);

        let mut iline = 0;
        for (row_num, row) in rows.iter().enumerate() {
            self.format_row_start(row, row_num, iline)?;
            let max_height = row.iter().map(|c| c.height()).max().unwrap_or(0);
            for cell_line in 0..max_height {
                self.format_line_start(row, row_num, iline)?;

                let mut icol = 0;
                while icol < row.len() {
                    let cell = row.cell(icol).unwrap();
                    // Some cells will not print anything on this cell_line so ignore the result.
                    let _ =
                        self.format_cell(cell, row, cell_line, &columns[icol..icol + cell.hspan()]);
                    icol += cell.hspan();
                }
                self.format_line_end(row, row_num, iline)?;
                iline += 1;
            }
            self.format_row_end(row, row_num, iline)?;
        }

        Ok(())
    }
}

/// Creates columns from the rows' cells, handling spanning in such a way that each column
/// has exactly the same number of cells as there are rows, with blank cells inserted as needed.
fn create_columns<'r>(
    rows: &'r [Row],
    column_preferences: &[ColumnPreferences],
) -> Vec<TableColumn<'r>> {
    let mut columns: Vec<TableColumn> = Vec::new();
    for (irow, row) in rows.iter().enumerate() {
        let mut col = 0;

        // Create columns if not present and append the row's cells to them.
        for cell in row.iter() {
            // Skip columns that already have a cell at this row (due to vertical spanning).
            while col < columns.len() && columns[col].cell_at(irow).is_some() {
                col += 1;
            }

            if col >= columns.len() {
                columns.push(TableColumn::new(
                    col,
                    column_preferences.get(col).cloned().unwrap_or_default(),
                ));
            }

            // Append the cell to the current column.
            columns[col].append(cell);

            // Append further blank cells for vertical spanning.
            for _ in 1..cell.vspan() {
                columns[col].append(&BLANK_CELL);
            }

            // Append further blank cells to subsequent columns for horizontal spanning.
            for c in 1..cell.hspan() {
                if col + c >= columns.len() {
                    columns.push(TableColumn::new(
                        col,
                        column_preferences.get(col).cloned().unwrap_or_default(),
                    ));
                }
                columns[col + c].append(&BLANK_CELL);
            }
            col += cell.hspan();
        }
    }
    columns.as_mut_slice().finalise_spanned();
    columns
}

pub(super) fn lines_and_cols(s: &str) -> (usize, usize) {
    // Ignore ANSI escape sequences.
    if s.starts_with("\x1b[") {
        return (0, 0);
    }
    if !s.is_empty() {
        let lines = s.lines().count() - 1;
        let cols = s.lines().last().unwrap().chars().count();
        (lines, cols)
    } else {
        (0, 0)
    }
}

pub trait Transformer {
    fn transform(&mut self, s: &str, writer: &mut dyn Write) -> fmt::Result;
}

pub(super) fn apply_transformers<'a, 't>(
    initial: &str,
    transformers: &'a mut [Box<dyn Transformer + 't>],
    output: &'a mut String,
) {
    thread_local! {
        static BUF: std::cell::RefCell<String> = const { std::cell::RefCell::new(String::new()) };
    }
    BUF.with_borrow_mut(move |buf| {
        output.clear();
        write!(output, "{initial}").unwrap();
        for transformer in transformers.iter_mut().rev() {
            buf.clear();
            transformer.transform(&output, buf).unwrap();
            output.clear();
            write!(output, "{buf}").unwrap();
        }
    })
}
