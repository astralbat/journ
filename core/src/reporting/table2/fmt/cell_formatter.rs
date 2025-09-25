/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table2::{Cell, ColumnWidth};
use std::fmt;
use std::fmt::Write;

/// A cell formatter is a kind of cell visitor, operating via double dispatch to print the cell's content with `Cell::print(CellFormatter)`.
pub trait CellFormatter: Write {
    fn format_cell(&mut self, cell: &dyn Cell, line: usize, width: ColumnWidth) -> fmt::Result;

    /// Allow printing of escaped terminal color codes
    fn color(&self) -> bool {
        false
    }
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
