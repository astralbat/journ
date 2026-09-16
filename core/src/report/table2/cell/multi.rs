/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::report::table2::cell_width::CellWidth;
use crate::report::table2::fmt::CellFormatter;
use crate::report::table2::{Cell, CellRef, ColumnWidth, SpaceDistribution, distributed_max};
use smallvec::SmallVec;
use std::fmt;
use std::fmt::Formatter;

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
    fn print<'format>(
        &self,
        f: &mut dyn CellFormatter,
        line: usize,
        width: Option<ColumnWidth>,
    ) -> std::fmt::Result {
        let mut lines_to_go = line;
        for cell in self.cells.iter() {
            if cell.height() > lines_to_go {
                return cell.print(f, lines_to_go, width);
            } else {
                lines_to_go -= cell.height();
            }
        }
        Ok(())
    }

    fn width(&self) -> CellWidth {
        let mut max = CellWidth::Leaf(0);
        for cell in self.cells.iter() {
            max = distributed_max(&max, &cell.width());
        }
        max
    }

    fn height(&self) -> usize {
        self.cells.iter().map(|c| c.height()).sum()
    }
}

impl fmt::Debug for MultiLineCell<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "MultiLineCell(")?;
        for (i, cell) in self.cells.iter().enumerate() {
            write!(f, "{:?},", cell)?;
            if i < self.cells.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

/// A cell that arranges its constituents horizontally. This differs from `BinaryCell`, in that
/// it does not adhere to a binary width and therefore does not create an effect of distinct columns.
pub struct MultiCell<'c> {
    cells: SmallVec<[CellRef<'c>; 2]>,
}

impl<'c> MultiCell<'c> {
    pub fn new(cells: impl IntoIterator<Item = CellRef<'c>>) -> Self {
        // Only use cells that have a width, otherwise printing them will always return std::fmt::Error.
        Self { cells: cells.into_iter().filter(|c| c.width() > 0).collect() }
    }
}

impl<'c> Cell for MultiCell<'c> {
    fn print<'format>(
        &self,
        f: &mut dyn CellFormatter,
        line: usize,
        _width: Option<ColumnWidth>,
    ) -> std::fmt::Result {
        // We just print the actual width, leaving it the renderer to right-pad.
        let mut width_to_go = Some(self.width());
        for cell in self.cells.iter() {
            let width = width_to_go.as_mut().and_then(|w| w.pop_right());
            cell.print(f, line, width)?;
        }
        Ok(())
    }

    fn width(&self) -> CellWidth {
        let mut total = CellWidth::Leaf(0);
        // Arrange tree with the first cell on the right, ready for popping in sequence
        // during print.
        for cell in self.cells.iter().rev() {
            total.push_right(cell.width(), SpaceDistribution::default());
        }
        total
    }

    fn height(&self) -> usize {
        self.cells.iter().map(|c| c.height()).max().unwrap_or(0)
    }
}

impl fmt::Debug for MultiCell<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Multi(")?;
        for (i, cell) in self.cells.iter().enumerate() {
            write!(f, "{:?}", cell)?;
            if i < self.cells.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}
