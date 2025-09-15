/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::arguments::Arguments;
use crate::reporting::table2::fmt::CellFormatter;
use crate::reporting::table2::{Cell, CellRef, ShrinkableCell};
use crate::reporting::term_style::Style;

pub struct StyledCell<'c> {
    inner: CellRef<'c>,
    style: Style,
}
impl<'c> StyledCell<'c> {
    pub fn new<C: Into<CellRef<'c>>>(inner: C, style: Style) -> Self {
        Self { inner: inner.into(), style }
    }
}

impl Cell for StyledCell<'_> {
    fn print<'format>(&self, f: &mut dyn CellFormatter<'format>, line: usize) -> std::fmt::Result {
        if self.inner.height() <= line {
            return Err(std::fmt::Error);
        }

        if f.color() {
            self.style.start(f)?;
            let start_cursor_col = f.cursor_col();

            let res = self.inner.print(f, line);

            // It's a good idea to pad here (otherwise the formatter will do it) to keep a consistent style
            if let Some(width) = f.width() {
                for _ in f.cursor_col()..(start_cursor_col + width.width()) {
                    write!(f, "{}", self.padding_char())?;
                }
            }

            self.style.end(f)?;
            res
        } else {
            self.inner.print(f, line)
        }
    }

    fn width(&self) -> crate::reporting::table2::cell_width::CellWidth {
        self.inner.width()
    }

    fn height(&self) -> usize {
        self.inner.height()
    }

    fn as_shrinkable(&self) -> Option<&dyn ShrinkableCell> {
        self.inner.as_shrinkable()
    }
}
