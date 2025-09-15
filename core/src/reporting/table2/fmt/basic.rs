/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table2::fmt::cell_formatter::{CellFormatter, Transformer, lines_and_cols};
use crate::reporting::table2::{Cell, CellWidth, Row, TableColumn};
use std::fmt;
use std::fmt::Write;

/// Cell formatter that prints cells to a string
pub struct BasicCellFormatter<'w, 't> {
    writer: &'w mut dyn Write,
    line: usize,
    col: usize,
    width: Option<CellWidth>,
    transformers: Vec<Box<dyn Transformer + 't>>,
}
impl<'w, 'format> BasicCellFormatter<'w, 'format> {
    pub fn new(writer: &'w mut dyn Write) -> Self {
        Self { writer, width: None, transformers: Vec::new(), line: 0, col: 0 }
    }
}

impl<'w, 'format> CellFormatter<'format> for BasicCellFormatter<'w, 'format> {
    fn format_cell(
        &mut self,
        cell: &dyn Cell,
        _row: &Row,
        line: usize,
        _col: &[TableColumn],
    ) -> fmt::Result {
        cell.print(self, line)
    }

    fn format_line_start(&mut self, _row: &Row, _row_num: usize, _line: usize) -> fmt::Result {
        Ok(())
    }

    fn format_line_end(&mut self, _row: &Row, _row_num: usize, _line: usize) -> fmt::Result {
        Ok(())
    }

    fn width(&self) -> Option<&CellWidth> {
        self.width.as_ref()
    }

    fn set_width(&mut self, width: Option<CellWidth>) {
        self.width = width;
    }

    fn push_transformer(&mut self, f: Box<dyn Transformer + 'format>) {
        self.transformers.push(f);
    }

    fn pop_transformer(&mut self) -> Option<Box<dyn Transformer + 'format>> {
        self.transformers.pop()
    }

    fn cursor_col(&self) -> usize {
        self.col
    }
}

impl Write for BasicCellFormatter<'_, '_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.writer.write_str(s)?;

        // Keep track of self.line and self.col
        let (lines, cols) = lines_and_cols(s);
        self.line += lines;
        if lines == 0 {
            self.col += cols;
        } else {
            self.col = cols;
        }

        Ok(())
    }
}
