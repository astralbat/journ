/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table2::fmt::CellFormatter;
use crate::reporting::table2::{Cell, CellWidth, ColumnWidth};
use std::fmt;

// public to crate only so that we can ensure it is not used outside. We use this to compare for filler cells
// on multi-spanned cells.
pub(crate) static BLANK_CELL: BlankCell = BlankCell;

/// A special cell that does not write anything; its width is 0.
pub struct BlankCell;
impl Cell for BlankCell {
    fn print<'format>(
        &self,
        _f: &mut dyn CellFormatter,
        _line: usize,
        _: Option<ColumnWidth>,
    ) -> fmt::Result {
        Err(fmt::Error)
    }

    fn width(&self) -> CellWidth {
        CellWidth::Unary(0)
    }
}
