/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table2::cell_width::CellWidth;
use crate::reporting::table2::fmt::CellFormatter;
use crate::reporting::table2::{Cell, CellRef};
use smallvec::SmallVec;

/// A cell of other cells arranged vertically. Each cell is printed on its own
/// line (or lines if the inner is itself multi-line).
pub struct MultiLineCell<'c> {
    cells: SmallVec<[CellRef<'c>; 2]>,
}

impl<'c> MultiLineCell<'c> {
    /// Create a new multi-line cell from the provided cells.
    pub fn new(cells: impl IntoIterator<Item = CellRef<'c>>) -> Self {
        Self { cells: cells.into_iter().collect() }
    }
}

impl<'c> Cell for MultiLineCell<'c> {
    fn print<'format>(&self, f: &mut dyn CellFormatter<'format>, line: usize) -> std::fmt::Result {
        let mut lines_to_go = line;
        for (i, cell) in self.cells.iter().enumerate() {
            if cell.height() > lines_to_go {
                return cell.print(f, lines_to_go);
            } else {
                lines_to_go -= cell.height();
            }
        }
        Ok(())
    }

    fn width(&self) -> CellWidth {
        let mut max = CellWidth::Unary(0);
        for cell in self.cells.iter() {
            max = max.distributed_max(&cell.width());
        }
        max
    }

    fn height(&self) -> usize {
        self.cells.iter().map(|c| c.height()).sum()
    }
}
