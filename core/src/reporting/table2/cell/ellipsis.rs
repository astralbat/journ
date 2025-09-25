/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table2::cell::ModifiableCell;
use crate::reporting::table2::fmt::CellFormatter;
use crate::reporting::table2::{Cell, CellRef, CellWidth, ColumnWidth, ShrinkableCell};

pub struct EllipsisCell<'c> {
    modifiable_cell: ModifiableCell<'c>,
}

impl<'c> EllipsisCell<'c> {
    pub fn new<C: Into<CellRef<'c>>>(cell: C) -> Self {
        Self { modifiable_cell: ModifiableCell::new(cell) }
    }
}

impl Cell for EllipsisCell<'_> {
    fn print<'format>(
        &self,
        f: &mut dyn CellFormatter,
        line: usize,
        width: Option<ColumnWidth>,
    ) -> std::fmt::Result {
        self.modifiable_cell.print(f, line, width)
    }

    fn width(&self) -> CellWidth {
        self.modifiable_cell.width()
    }

    fn height(&self) -> usize {
        self.modifiable_cell.height()
    }

    fn hspan(&self) -> usize {
        self.modifiable_cell.hspan()
    }

    fn vspan(&self) -> usize {
        self.modifiable_cell.vspan()
    }

    fn padding_char(&self) -> char {
        self.modifiable_cell.padding_char()
    }

    fn as_shrinkable(&self) -> Option<&dyn ShrinkableCell> {
        Some(self)
    }
}

impl ShrinkableCell for EllipsisCell<'_> {
    fn try_shrink(&self, max_width: usize) -> bool {
        for mut line in self.modifiable_cell.longest_lines_mut() {
            // Don't shrink unless we have to (reluctant)
            if line.chars().count() < max_width {
                return false;
            }

            let trim_len = if line.ends_with("...") { 4 } else { 1 };
            let i = line.char_indices().rev().nth(trim_len).map(|(i, _)| i).unwrap_or(0);
            if i <= 3 {
                return false;
            }
            line.truncate(i);
            while line.ends_with(' ') {
                line.pop();
            }
            line.push_str("...");
        }
        true
    }
}
