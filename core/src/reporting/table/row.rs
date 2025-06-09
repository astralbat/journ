/*
 * Copyright (c) 2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table::cell::sequence::CellSequence;
use crate::reporting::table::cell::{Cell, CellNodeKind};
use crate::reporting::term_style::Style;
use std::cell::{Ref, RefCell, RefMut};
use std::iter::FromIterator;
use std::rc::Rc;
use std::{fmt, io};

/// A logical table row containing a number of columns, each having a number of lines.
pub struct Row<'cell> {
    pub(super) root: Rc<RefCell<Cell<'cell>>>,
}

impl<'cell> Row<'cell> {
    pub fn new<C: Into<Cell<'cell>>, I: IntoIterator<Item = C>>(cells: I) -> Self {
        let iter = cells.into_iter().map(|c| {
            let c = c.into();
            let c_span = c.span();
            // Let the span of the ColRoot be the same as the span of the cell
            Cell::new_root(CellSequence::new([Rc::new(RefCell::new(c))], "")).with_span(c_span)
        });

        let row = Self { root: Rc::new(RefCell::new(iter.collect())) };
        assert!(row.root.borrow().iter().next().is_some(), "Row must contain at least one cell");
        row
    }

    pub fn padded_width(&self) -> usize {
        self.root.borrow().padded_width()
    }

    pub fn num_lines(&self) -> usize {
        self.root
            .borrow()
            .iter()
            .map(|c| match c.kind() {
                CellNodeKind::Leaf(_) | CellNodeKind::Branch(_) => 1,
                CellNodeKind::ColRoot(seq) => seq.len(),
            })
            .max()
            .unwrap_or(0)
    }

    pub fn set_separator(&mut self, separator: &'static str) {
        self.root.borrow_mut().as_branch_mut().unwrap().set_separator(separator)
    }

    pub fn root(&self) -> Ref<Cell<'cell>> {
        self.root.borrow()
    }

    pub fn root_mut(&self) -> RefMut<Cell<'cell>> {
        self.root.borrow_mut()
    }

    pub fn into_owned<'b>(self) -> Row<'b> {
        Row { root: Rc::new(RefCell::new(self.root.take().into_owned())) }
    }

    pub fn set_faint(&mut self, faint: bool) {
        self.root.borrow_mut().set_faint(faint);
    }

    pub(super) fn wrap_text_eager(&mut self) -> bool {
        let mut wrapped_any = false;
        for mut cell in self.root.borrow_mut().iter_mut() {
            let res = cell.wrap_text_eager();
            assert!(res.1.is_none(), "Row child did not handle the wrapping");
            wrapped_any |= res.0;
        }
        wrapped_any
    }

    pub fn print<W: fmt::Write>(&self, w: &mut W) -> fmt::Result {
        let num_lines = self.num_lines();
        for line_num in 0..num_lines {
            let row_root_borrow = self.root.borrow();
            let seq = row_root_borrow.as_branch().unwrap();
            for (col, col_root) in seq.iter().enumerate() {
                let lines = col_root.as_root().unwrap();
                if line_num < lines.len() {
                    for _ in 0..col_root.left_padding() {
                        write!(w, "{}", col_root.padding_char())?;
                    }

                    let line = lines[line_num].borrow();
                    line.print(
                        w,
                        row_root_borrow.style().overlay(col_root.style().overlay(line.style())),
                    )?;

                    for _ in 0..col_root.right_padding() {
                        write!(w, "{}", col_root.padding_char())?;
                    }
                } else {
                    for _ in 0..col_root.padded_width() {
                        write!(w, " ")?;
                    }
                }
                if col < seq.len() - 1 {
                    Style::default().fmt(w, seq.separator)?;
                }
            }
            if line_num < num_lines - 1 {
                writeln!(w)?;
            }
        }
        Ok(())
    }

    pub fn print_csv<W: fmt::Write>(&self, w: &mut W) -> fmt::Result {
        let num_lines = self.num_lines();
        for line_num in 0..num_lines {
            let row_root_borrow = self.root.borrow();
            let seq = row_root_borrow.as_branch().unwrap();
            for (col, col_root) in seq.iter().enumerate() {
                let lines = col_root.as_root().unwrap();
                if line_num < lines.len() {
                    let line = lines[line_num].borrow();
                    line.print(w, line.style())?;
                }
                if col < seq.len() - 1 {
                    write!(w, ",")?;
                }
            }
            if line_num < num_lines - 1 {
                writeln!(w)?;
            }
        }
        Ok(())
    }
}

impl fmt::Debug for Row<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.root)
    }
}

impl<'a> FromIterator<Cell<'a>> for Row<'a> {
    fn from_iter<T: IntoIterator<Item = Cell<'a>>>(iter: T) -> Self {
        Self::new(iter)
    }
}

impl<'a, C: Into<Cell<'a>>> From<Vec<C>> for Row<'a> {
    fn from(cells: Vec<C>) -> Self {
        Self::new(cells)
    }
}
