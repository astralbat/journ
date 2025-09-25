/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table2::fmt::cell_formatter::{CellFormatter, lines_and_cols};
use crate::reporting::table2::{Cell, CellWidth, ColumnWidth};
use std::fmt;
use std::fmt::Write;

/// Cell formatter that prints cells to a string
pub struct BasicCellFormatter<'w> {
    writer: &'w mut dyn Write,
    line: usize,
    col: usize,
    width: Option<CellWidth>,
}
impl<'w> BasicCellFormatter<'w> {
    pub fn new(writer: &'w mut dyn Write) -> Self {
        Self { writer, width: None, line: 0, col: 0 }
    }
}

impl<'w> CellFormatter for BasicCellFormatter<'w> {
    fn format_cell(&mut self, cell: &dyn Cell, line: usize, _width: ColumnWidth) -> fmt::Result {
        cell.print(self, line, None)
    }
}

impl Write for BasicCellFormatter<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        write!(self.writer, "{}", s)
    }
}

#[derive(Default)]
pub struct StringCellFormatter {
    buf: String,
}
impl StringCellFormatter {
    pub fn buffer(&self) -> &str {
        &self.buf
    }

    pub fn clear(&mut self) {
        self.buf.clear();
    }
}

impl CellFormatter for StringCellFormatter {
    fn format_cell(&mut self, cell: &dyn Cell, line: usize, _width: ColumnWidth) -> fmt::Result {
        cell.print(self, line, None)
    }
}
impl Write for StringCellFormatter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.buf.push_str(s);
        Ok(())
    }
}
