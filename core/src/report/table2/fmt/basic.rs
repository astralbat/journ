/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::report::table2::fmt::cell_formatter::{CellFormatter, lines_and_cols};
use crate::report::table2::{Cell, ColumnWidth};
use std::fmt;
use std::fmt::Write;

/// Cell formatter that prints cells to a string
pub struct BasicCellFormatter<'w> {
    writer: &'w mut dyn Write,
}
impl<'w> BasicCellFormatter<'w> {
    pub fn new(writer: &'w mut dyn Write) -> Self {
        Self { writer }
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

/// A formatter that can be reused for writing strings in a buffered way.
#[derive(Default)]
pub struct StringCellFormatter {
    // An array of buffers rather than one String buffer means we make separate
    // writes out in the same way as in for fewer side effects as when compared
    // to not buffering (TableCellFormatter). The main side effect is the detection of escape
    // chars when styling. Separate writes retains this detection.
    bufs: Vec<String>,
    // A buffer pointing to bufs next write position.
    buf_ptr: usize,
    width: usize,
}
impl StringCellFormatter {
    /// Gets the number of chars that have been written to the buffer.
    pub fn count(&self) -> usize {
        self.width
    }

    pub fn clear(&mut self) {
        self.buf_ptr = 0;
        self.width = 0;
    }
}

impl CellFormatter for StringCellFormatter {
    fn format_cell(&mut self, cell: &dyn Cell, line: usize, _width: ColumnWidth) -> fmt::Result {
        cell.print(self, line, None)
    }
}
impl Write for StringCellFormatter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let (_, cols) = lines_and_cols(s);
        self.width += cols;

        let buf = if self.buf_ptr == self.bufs.len() {
            self.bufs.push(String::new());
            &mut self.bufs[self.buf_ptr]
        } else {
            &mut self.bufs[self.buf_ptr]
        };
        buf.clear();
        buf.push_str(s);
        self.buf_ptr += 1;
        Ok(())
    }
}

impl fmt::Display for StringCellFormatter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for i in 0..self.buf_ptr {
            self.bufs[i].fmt(f)?;
        }
        Ok(())
    }
}
