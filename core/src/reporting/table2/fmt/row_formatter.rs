/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table2::cell::blank::BLANK_CELL;
use crate::reporting::table2::column::ColumnsVec;
use crate::reporting::table2::fmt::CellFormatter;
use crate::reporting::table2::table::ColumnPreferences;
use crate::reporting::table2::{ColumnWidth, Row, TableColumn};
use std::fmt;

pub trait RowFormatter<'format>: CellFormatter {
    fn format_row_start(
        &mut self,
        _row: &'format Row<'format>,
        _row_num: usize,
        _line: usize,
    ) -> fmt::Result {
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

    fn print<'cell>(
        &mut self,
        rows: &'format [Row<'cell>],
        column_preferences: &[ColumnPreferences],
    ) -> fmt::Result
    where
        'cell: 'format,
    {
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
                    let width: ColumnWidth =
                        columns[icol..icol + cell.hspan()].iter().map(|col| col.width()).sum();
                    let _ = self.format_cell(cell, cell_line, width);
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
