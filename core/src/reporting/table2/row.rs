/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table2::{Cell, CellRef};
use std::ops::Deref;

#[derive(Default)]
pub struct Row<'c> {
    pub cells: Vec<CellRef<'c>>,
    header: bool,
}
impl<'c> Row<'c> {
    pub fn new<I, C>(cells: I) -> Self
    where
        I: IntoIterator<Item = C>,
        C: Into<CellRef<'c>>,
    {
        let cells = cells.into_iter().map(Into::into).collect();
        Self { cells, header: false }
    }

    pub fn is_header(&self) -> bool {
        self.header
    }

    pub fn set_header(&mut self, is_header: bool) {
        self.header = is_header;
    }

    pub fn width(&self) -> usize {
        self.cells.iter().map(|c| c.width().width()).sum()
    }

    pub fn append_borrowed<'a>(&mut self, content: &'a dyn Cell)
    where
        'a: 'c,
    {
        self.cells.push(CellRef::Borrowed(content));
    }

    pub fn append(&mut self, cell: CellRef<'c>) {
        self.cells.push(cell);
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
