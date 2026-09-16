/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::report::table2::fmt::CellFormatter;
use crate::report::table2::{BinaryCell, Cell, CellRef, CellWidth, ColumnWidth, ShrinkableCell};
use std::fmt;
use std::fmt::Formatter;

pub struct SpannedCell<'c> {
    inner: CellRef<'c>,
    hspan: usize,
}
impl<'c> SpannedCell<'c> {
    pub fn new<C: Into<CellRef<'c>>>(inner: C, hspan: usize) -> Self {
        Self { inner: inner.into(), hspan }
    }
}
impl Cell for SpannedCell<'_> {
    fn print<'format>(
        &self,
        f: &mut dyn CellFormatter,
        line: usize,
        width: Option<ColumnWidth>,
    ) -> fmt::Result {
        self.inner.print(f, line, width)
    }

    fn width(&self) -> CellWidth {
        self.inner.width()
    }

    fn height(&self) -> usize {
        self.inner.height()
    }

    fn hspan(&self) -> usize {
        self.hspan
    }

    fn as_binary(&self) -> Option<&BinaryCell> {
        self.inner.as_binary()
    }

    fn as_shrinkable(&self) -> Option<&dyn ShrinkableCell> {
        self.inner.as_shrinkable()
    }
}

impl<'c> From<SpannedCell<'c>> for CellRef<'c> {
    fn from(s: SpannedCell<'c>) -> Self {
        CellRef::Owned(Box::new(s))
    }
}

impl fmt::Debug for SpannedCell<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Spanned({:?})", self.inner)
    }
}
