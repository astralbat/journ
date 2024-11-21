/*
 * Copyright (c) 2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table::cell::node::{Cell, CellNodeKind};
use std::cell::{Ref, RefCell, RefMut};
use std::iter::Map;
use std::rc::Rc;
use std::slice::Iter;

/// An iterator that iterates over the cells in all lines of a cell. This will only
/// return cells if the cell being iterated is a branch, and then only those cells that
/// are the immediate children.
#[allow(clippy::type_complexity)]
pub struct CellIterator<'a, 'cell> {
    iter: Option<
        Map<
            Iter<'a, Rc<RefCell<Cell<'cell>>>>,
            fn(&'a Rc<RefCell<Cell<'cell>>>) -> Ref<'a, Cell<'cell>>,
        >,
    >,
}

fn cell_to_ref<'a, 'cell>(cell: &'a Rc<RefCell<Cell<'cell>>>) -> Ref<'a, Cell<'cell>> {
    cell.borrow()
}

impl<'a, 'cell> CellIterator<'a, 'cell> {
    // Creates a new iterator. This has to take an outside reference with lifetime 'a
    // to the cell. Otherwise we need to create a Ref, store it and borrow from it
    // which essentially means borrowing from `Self` which we want to avoid.
    pub fn new(cell: &'a Cell<'cell>) -> Self {
        let mut iter = Self { iter: None };
        match cell.kind() {
            CellNodeKind::Branch(seq) | CellNodeKind::ColRoot(seq) => {
                iter.iter = Some(seq.cells.iter().map(cell_to_ref));
            }
            CellNodeKind::Leaf(_) => {}
        }
        iter
    }
}

impl<'a, 'cell> Iterator for CellIterator<'a, 'cell> {
    type Item = Ref<'a, Cell<'cell>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.as_mut().and_then(Iterator::next)
    }
}

impl DoubleEndedIterator for CellIterator<'_, '_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iter.as_mut().and_then(DoubleEndedIterator::next_back)
    }
}

fn cell_to_ref_mut<'a, 'cell>(cell: &'a Rc<RefCell<Cell<'cell>>>) -> RefMut<'a, Cell<'cell>> {
    cell.borrow_mut()
}

/// An iterator that iterates over the cells in all lines of a cell. This will only
/// return cells if the cell being iterated is a branch, and then only those cells that
/// are the immediate children.
#[allow(clippy::type_complexity)]
pub struct CellIteratorMut<'a, 'cell> {
    iter: Option<
        Map<
            Iter<'a, Rc<RefCell<Cell<'cell>>>>,
            fn(&'a Rc<RefCell<Cell<'cell>>>) -> RefMut<'a, Cell<'cell>>,
        >,
    >,
}

impl<'a, 'cell> CellIteratorMut<'a, 'cell> {
    pub fn new(cell: &'a mut Cell<'cell>) -> Self {
        let mut iter = Self { iter: None };
        if let CellNodeKind::Branch(seq) = cell.kind() {
            iter.iter = Some(seq.cells.iter().map(cell_to_ref_mut));
        }
        iter
    }
}

impl<'a, 'cell> Iterator for CellIteratorMut<'a, 'cell> {
    type Item = RefMut<'a, Cell<'cell>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.as_mut().and_then(Iterator::next)
    }
}

impl DoubleEndedIterator for CellIteratorMut<'_, '_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iter.as_mut().and_then(DoubleEndedIterator::next_back)
    }
}
