/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::report::table2::cell::ModifiableCell;
use crate::report::table2::fmt::CellFormatter;
use crate::report::table2::{Cell, CellRef, CellWidth, ColumnWidth, ShrinkableCell};
use std::fmt;
use std::fmt::Formatter;

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
    ) -> fmt::Result {
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
    fn try_shrink(&self, target_width: &CellWidth) -> usize {
        let mut success_len = 0;
        for mut line in self.modifiable_cell.longest_lines_mut() {
            debug_assert!(
                line.width().sum() >= target_width.sum(),
                "{} < {}",
                line.width().sum(),
                target_width.sum()
            );

            let width_diff = line.width().sum() - target_width.sum();
            if width_diff > 0 {
                let trim_len = width_diff + 2;
                let i = line.char_indices().rev().nth(trim_len - 1).map(|(i, _)| i).unwrap_or(0);
                let trunc_len = line.len() - (line.len() - i);
                // Keep at least two chars
                if trunc_len < 2 {
                    return 0;
                }
                line.truncate(trunc_len);
                while line.ends_with(' ') {
                    line.pop();
                }
                line.push_str("..");
                success_len = trim_len;
            }
        }
        success_len
    }

    fn is_lossy(&self) -> bool {
        true
    }

    fn min_width(&self) -> usize {
        // Two chars plus "..". Must agree with trunc_len above.
        4
    }
}

impl fmt::Debug for EllipsisCell<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Ellipsis({:?})", self.modifiable_cell)
    }
}
