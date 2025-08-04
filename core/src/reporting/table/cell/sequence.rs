/*
 * Copyright (c) 2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table::cell::node::Cell;
use crate::reporting::term_style::Style;
use smallvec::SmallVec;
use std::cell::{Ref, RefCell, RefMut};
use std::fmt;
use std::iter::FromIterator;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

#[derive(Clone, Default)]
pub struct CellSequence<'cell> {
    pub cells: SmallVec<[Rc<RefCell<Cell<'cell>>>; 8]>,
    pub separator: &'static str,
}

impl<'cell> CellSequence<'cell> {
    pub fn new<I>(cells: I, separator: &'static str) -> Self
    where
        I: IntoIterator<Item = Rc<RefCell<Cell<'cell>>>>,
    {
        let cells: SmallVec<[Rc<RefCell<Cell<'cell>>>; 8]> = cells.into_iter().collect();
        assert!(!cells.is_empty(), "Cell sequence must have at least one cell");
        Self { cells, separator }
    }

    pub fn len(&self) -> usize {
        self.cells.len()
    }

    pub fn set_separator(&mut self, separator: &'static str) {
        self.separator = separator;
    }

    pub fn cells<'a>(&'a self) -> &'a [Rc<RefCell<Cell<'cell>>>] {
        &self.cells
    }

    pub fn iter(&self) -> impl Iterator<Item = Ref<Cell<'cell>>> {
        self.cells.iter().map(|c| c.borrow())
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = RefMut<Cell<'cell>>> {
        self.cells.iter().map(|c| c.borrow_mut())
    }

    pub fn content_width(&self) -> usize {
        self.cells.iter().map(|c| c.borrow().content_width() + self.separator.len()).sum::<usize>()
            - self.separator.len()
    }

    pub fn actual_content_width(&self) -> usize {
        self.cells
            .iter()
            .map(|c| c.borrow().actual_content_width() + self.separator.len())
            .sum::<usize>()
            - self.separator.len()
    }

    pub fn padded_width(&self) -> usize {
        self.cells.iter().map(|c| c.borrow().padded_width() + self.separator.len()).sum::<usize>()
            - self.separator.len()
    }

    pub fn into_owned<'a>(self) -> CellSequence<'a> {
        CellSequence::new(
            self.cells.into_iter().map(|c| c.take().into_owned()).map(RefCell::new).map(Rc::new),
            self.separator,
        )
    }

    /// Attempts to unwrap the sequence as a single cell.
    pub fn try_into_cell(mut self) -> Result<Rc<RefCell<Cell<'cell>>>, Self> {
        if self.cells.len() == 1 { Ok(self.cells.pop().unwrap()) } else { Err(self) }
    }

    pub fn insert(&mut self, index: usize, cell: Rc<RefCell<Cell<'cell>>>) {
        self.cells.insert(index, cell);
    }

    pub fn push(&mut self, cell: Rc<RefCell<Cell<'cell>>>) {
        self.cells.push(cell);
    }

    pub fn extend<I: IntoIterator<Item = Rc<RefCell<Cell<'cell>>>>>(&mut self, iter: I) {
        self.cells.extend(iter);
    }

    pub fn wrap_text_eager(&mut self) -> Option<CellSequence<'cell>> {
        self.cells.last_mut().unwrap().borrow_mut().wrap_text_eager().1
    }

    /// Wraps the text based on the `position_advice` if given; otherwise defaulting to
    /// the approximate middle of the sequence.
    pub fn wrap_text(&mut self, position_advice: Option<usize>) -> Option<CellSequence<'cell>> {
        // It is important to make sure the position_advice is in bounds. We certainly don't
        // want to allow 0 or the length of the sequence as this would produce an empty segment.
        let mid = match position_advice {
            Some(p) if p > 0 && p < self.cells.len() => p,
            _ => self.cells.len() / 2,
        };
        if mid > 0 {
            Some(CellSequence::new(self.cells.drain(mid..), self.separator))
        } else {
            debug_assert!(
                self.cells.len() == 1,
                "Illegal wrapping of first cell in multi-cell sequence"
            );
            self.cells.first_mut().unwrap().borrow_mut().wrap_text().1
        }
    }

    pub fn print<W: fmt::Write>(&self, w: &mut W, style: Style) -> fmt::Result {
        for (col, cell) in self.cells.iter().enumerate() {
            let cell_borrow = cell.borrow();
            cell_borrow.print(w, style.overlay(cell_borrow.style()))?;
            if col < self.cells.len() - 1 {
                Style::default().fmt(w, self.separator)?;
            }
        }
        Ok(())
    }
}

impl fmt::Debug for CellSequence<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, c) in self.cells.iter().map(|c| c.borrow()).enumerate() {
            write!(f, "{:?}", c)?;
            if i < self.cells.len() - 1 {
                write!(f, ", ")?;
            }
        }
        Ok(())
    }
}

impl<'a> Deref for CellSequence<'a> {
    type Target = [Rc<RefCell<Cell<'a>>>];

    fn deref(&self) -> &Self::Target {
        &self.cells
    }
}

impl<'a> DerefMut for CellSequence<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.cells
    }
}

impl<'cell, C> FromIterator<C> for CellSequence<'cell>
where
    C: Into<Cell<'cell>>,
{
    fn from_iter<T: IntoIterator<Item = C>>(iter: T) -> Self {
        CellSequence::new(iter.into_iter().map(C::into).map(RefCell::new).map(Rc::new), "")
    }
}
