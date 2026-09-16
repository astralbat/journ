/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::report::table2::{Cell, CellRef};
use std::ops::Deref;

#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub enum RowKind {
    Heading,
    Header,
    Footer,
    Title,
    ChainSeparator,
    TitleSeparator,
    TotalSeparator,
    GrandTotalSeparator,
    #[default]
    Data,
    Total,
}

#[derive(Default)]
pub struct Row<'c> {
    pub cells: Vec<CellRef<'c>>,
    row_kind: RowKind,
}
impl<'c> Row<'c> {
    pub fn new<I, C>(cells: I) -> Self
    where
        I: IntoIterator<Item = C>,
        C: Into<CellRef<'c>>,
    {
        let cells = cells.into_iter().map(Into::into).collect();
        Self { cells, ..Default::default() }
    }

    pub fn column_count(&self) -> usize {
        self.cells.iter().map(|c| c.hspan()).sum()
    }

    pub fn kind(&self) -> RowKind {
        self.row_kind
    }

    pub fn set_kind(&mut self, row_kind: RowKind) {
        self.row_kind = row_kind;
    }

    pub fn is_striped(&self) -> bool {
        self.row_kind == RowKind::Data
    }

    pub fn width(&self) -> usize {
        self.cells.iter().map(|c| c.width().sum()).sum()
    }

    pub fn append_borrowed<'a>(&mut self, content: &'a dyn Cell)
    where
        'a: 'c,
    {
        self.cells.push(CellRef::Borrowed(content));
    }

    pub fn append<C: Into<CellRef<'c>>>(&mut self, cell: C) {
        self.cells.push(cell.into());
    }

    pub fn cell(&self, index: usize) -> Option<&dyn Cell> {
        self.cells.get(index).map(|c| c.deref())
    }

    pub fn iter(&self) -> impl Iterator<Item = &dyn Cell> {
        self.cells.iter().map(|c| c.deref())
    }

    pub fn len(&self) -> usize {
        self.cells.len()
    }

    pub fn is_empty(&self) -> bool {
        self.cells.is_empty()
    }
}

impl<'a> FromIterator<CellRef<'a>> for Row<'a> {
    fn from_iter<T: IntoIterator<Item = CellRef<'a>>>(iter: T) -> Self {
        Self::new(iter)
    }
}

impl<'a, C: Into<CellRef<'a>>> From<Vec<C>> for Row<'a> {
    fn from(cells: Vec<C>) -> Self {
        Self::new(cells)
    }
}

pub trait Rows {
    fn column_count(&self) -> usize;
}

macro_rules! impl_rows {
    ($t:ty) => {
        impl<'cell> Rows for $t {
            fn column_count(&self) -> usize {
                self.iter().map(|r| r.column_count()).max().unwrap_or(0)
            }
        }
    };
}
impl_rows!(Vec<Row<'cell>>);
impl_rows!([Row<'cell>]);
