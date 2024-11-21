/*
 * Copyright (c) 2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table::cell::node::Cell;
use rust_decimal::prelude::{FromPrimitive, Zero};
use rust_decimal::Decimal;
use smallvec::SmallVec;
use std::cell::RefCell;
use std::cmp;
use std::rc::Rc;

#[derive(Clone)]
pub struct CellColumnInfo<'cell> {
    pub(crate) cell: Rc<RefCell<Cell<'cell>>>,
    pub(crate) column: u16,
    pub(crate) parent: Option<Box<CellColumnInfo<'cell>>>,
}

impl<'cell> CellColumnInfo<'cell> {
    pub fn new(cell: Rc<RefCell<Cell<'cell>>>, column: u16, parent: Option<Box<Self>>) -> Self {
        Self { cell, column, parent }
    }

    pub fn depth(&self) -> usize {
        self.parent.as_ref().map(|p| p.depth() + 1).unwrap_or(0)
    }

    /// Find the first common parent and determine ordering based on the column index.
    pub fn parent_compare(&self, mut other: &Self) -> cmp::Ordering {
        let mut _self = self;
        let self_depth = self.depth();
        let other_depth = other.depth();

        // Bring _self and other up to the same depth
        for _ in self_depth..other_depth {
            other = other.parent.as_ref().unwrap();
        }
        for _ in other_depth..self_depth {
            _self = _self.parent.as_ref().unwrap();
        }

        // Work out the closest parent
        while _self.parent != other.parent {
            _self = _self.parent.as_ref().unwrap();
            other = other.parent.as_ref().unwrap();
        }

        // Compare the column index
        _self.column.cmp(&other.column)
    }
}

impl PartialEq for CellColumnInfo<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.parent == other.parent && self.column == other.column
    }
}

impl Eq for CellColumnInfo<'_> {}

impl PartialOrd<Self> for CellColumnInfo<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CellColumnInfo<'_> {
    /// Order depth first, starting at the root.
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.parent_compare(other).then_with(|| self.depth().cmp(&other.depth()))
    }
}

pub struct CellColumn<'cell> {
    pub cells: Vec<Rc<RefCell<Cell<'cell>>>>,
    pub depth: usize,
}

impl<'cell> CellColumn<'cell> {
    /// Align all cells in this column to have the same width.
    pub(super) fn align(&self) {
        self.pad_to_width(self.content_width())
    }

    /// The content width is the width of the content of the widest cell in the column,
    /// excluding its padding. Any multi-spanning cells are ignored.
    pub(super) fn content_width(&self) -> usize {
        // The width of a column is the width of the widest non-spanning cell in the column.
        let (_i, w) = self
            .cells
            .iter()
            .enumerate()
            .filter(|(_i, cell)| cell.borrow().span() == 1)
            .map(|(i, c)| (i, c.borrow().content_width()))
            .max_by_key(|(_, w)| *w)
            .unwrap_or((0, 0));
        w
    }

    /// The printed width of a column is its content_width plus the widest padding.
    pub(super) fn padded_width(&self) -> usize {
        // The width of a column is the width of the widest non-spanning cell in the column.
        self.cells
            .iter()
            .filter(|cell| cell.borrow().span() == 1)
            .map(|c| c.borrow().padded_width())
            .max()
            .unwrap_or(0)
    }

    pub(super) fn avg_actual_content_width(&self) -> f32 {
        let mut total = 0f32;
        for cell in self.cells.iter() {
            if cell.borrow().span() == 1 {
                total += cell.borrow().actual_content_width() as f32;
            }
        }
        if self.cells.is_empty() {
            0f32
        } else {
            total / self.cells.len() as f32
        }
    }

    /// Gets the standard deviation of the actual content width of the cells in this column.
    pub(super) fn std_dev_actual_content_width(&self) -> f32 {
        let avg = self.avg_actual_content_width();
        let mut total = 0f32;
        let mut count = 0;
        for cell in self.cells.iter() {
            if cell.borrow().span() == 1 {
                let diff = cell.borrow().actual_content_width() as f32 - avg;
                total += diff * diff;
                count += 1;
            }
        }
        if count == 0 {
            0f32
        } else {
            (total / count as f32).sqrt()
        }
    }

    pub(super) fn pad_to_width(&self, width: usize) {
        self.cells
            .iter()
            .filter(|cell| cell.borrow().span() == 1)
            .for_each(|cell| cell.borrow_mut().pad_to_width(width))
    }

    pub fn depth(&self) -> usize {
        self.depth
    }

    /// Attempts to wrap text. This only works for root columns where the depth is 0.
    pub fn wrap_text(&self) -> bool {
        let max_width = self.padded_width();
        let mut wrapped_any = false;
        self.cells.iter().for_each(|cell| {
            let mut cell_borrow = cell.borrow_mut();
            assert!(cell_borrow.as_root().is_some(), "Can only wrap text in root cells");

            let max_line_content_width = cell_borrow
                .as_root()
                .unwrap()
                .cells
                .iter()
                .map(|c| c.borrow().content_width())
                .max()
                .unwrap();
            if max_line_content_width == max_width {
                wrapped_any |= cell_borrow.wrap_text().0;
            }
        });
        wrapped_any
    }
}

/// A tree of columns.
/// Since table cells can be nested forming a tree structure within a logical row, it
/// follows that columns also form a tree.
pub struct ColumnTreeNode<'cell> {
    column: CellColumn<'cell>,
    children: Vec<ColumnTreeNode<'cell>>,
}

impl<'cell> ColumnTreeNode<'cell> {
    pub fn new(column: CellColumn<'cell>) -> Self {
        Self { column, children: vec![] }
    }

    /// Grows the tree right-most at the desired depth.
    pub fn append_column(&mut self, column: CellColumn<'cell>, depth: usize) {
        if depth == self.column.depth() + 1 {
            self.children.push(ColumnTreeNode::new(column));
        } else {
            self.children.last_mut().unwrap().append_column(column, depth);
        }
    }

    /// Get all the cells in the column to line up neatly.
    pub fn align_recursive(&self) {
        // Align all children first, then align this column.
        // Work in reverse order so that the right-most column is aligned first, this will
        // ensure more compactness in the case where there are overlapping multi-spanned cells.
        for child in self.children.iter().rev() {
            child.align_recursive();
        }

        // For all cells that span multiple columns, ensure that their content width can fit within
        // the spanned columns. Otherwise, pad the first spanned column with the extra width needed.
        for c1 in 0..self.children.len() {
            for c2 in 0..self.children[c1].children.len() {
                let col = &self.children[c1].children[c2].column;
                for i in 0..col.cells.len() {
                    let mut cell = col.cells[i].borrow_mut();
                    if cell.span() > 1 {
                        // The span goes rightward.
                        // We don't trust that the span is exact. It could be over-specified.
                        let to_col = (c1 + cell.span() as usize).min(self.children.len());
                        let mut total_col_width = 0;
                        let sep_width = self
                            .column
                            .cells
                            .first()
                            .unwrap()
                            .borrow()
                            .as_branch()
                            .unwrap()
                            .separator
                            .len();
                        for j in c1..to_col {
                            total_col_width += self.children[j].column.padded_width() + sep_width;
                        }
                        total_col_width -= sep_width;

                        if cell.content_width() > total_col_width {
                            let mut extra = cell.content_width() - total_col_width;

                            // Distribute the extra width proportionally amongst the spanned columns.
                            for j in c1..to_col {
                                let col = &self.children[j].column;
                                let extra_for_col = if j == to_col - 1 {
                                    extra
                                } else {
                                    ((col.padded_width() as f64 / total_col_width as f64)
                                        * extra as f64)
                                        .round() as usize
                                };
                                extra -= extra_for_col;
                                col.pad_to_width(col.padded_width() + extra_for_col);
                            }
                        } else {
                            // The cell's content is smaller than the spanned columns. We need to pad it out
                            // as spanned cells are ordinarily ignored during align().
                            cell.pad_to_width(total_col_width);
                            // And ensure that the cell's parent is at least as wide as its child
                            //let existing_width =
                            //    self.children[c1].column.cells[i].borrow().padded_width();
                            //self.children[c1].column.cells[i]
                            //   .borrow_mut()
                            //   .pad_to_width(total_col_width.max(existing_width));
                        }
                    }
                }
            }
        }

        // Lastly, align this column.
        self.column.align();
    }

    /// As [Self::align_recursive()], but tries to shrink some child columns if the total
    /// width is over `max_width`.
    pub fn try_align_to_max_width(&self, max_width: usize) -> bool {
        self.align_recursive();
        let curr_width = self.column.padded_width();

        let diff = curr_width as isize - max_width as isize;
        if diff > 0 {
            // Decide which columns to wrap first in such a way that the number of lines printed is minimized.
            // Score each column on its (max_width - avg_width) / std_dev. A higher score indicates that the column
            // has more outliers that we can wrap without producing too many more lines.
            let mut max_to_avg_scores = self
                .children
                .iter()
                .enumerate()
                .map(|(i, c)| match c.column.std_dev_actual_content_width() {
                    0f32 => (i, Decimal::zero()),
                    dev => (
                        i,
                        (Decimal::from(c.column.padded_width())
                            - Decimal::from_f32(c.column.avg_actual_content_width()).unwrap())
                            / Decimal::from_f32(dev).unwrap(),
                    ),
                })
                .collect::<SmallVec<[_; 16]>>();
            max_to_avg_scores.sort_by_key(|(_i, s)| *s);

            for (i, _s) in max_to_avg_scores.iter().rev() {
                let child = &self.children[*i];
                if child.column.wrap_text() {
                    return true;
                }
            }
        }
        false
    }
}
