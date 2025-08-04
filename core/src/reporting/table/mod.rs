/*
 * Copyright (c) 2023-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
mod cell;
pub mod row;

pub use cell::Cell;
pub use cell::WrapPolicy;
pub use cell::content::Alignment;

use crate::reporting::table::cell::column::{CellColumn, ColumnTreeNode};
//use crate::reporting::table::Cell;
use crate::reporting::term_style::Style;
pub use row::Row;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

pub struct Table<'cell> {
    heading_row: Option<RefCell<Row<'cell>>>,
    rows: Vec<RefCell<Row<'cell>>>,
    min_width: Option<usize>,
    max_width: Option<usize>,
    indent: usize,
    column_separator: &'static str,
}

macro_rules! all_rows {
    ($self:ident) => {
        $self
            .heading_row
            .as_ref()
            .map(|hr| hr.borrow())
            .into_iter()
            .chain($self.rows.iter().map(|r| r.borrow()))
    };
}
macro_rules! all_rows_mut {
    ($self:ident) => {
        $self
            .heading_row
            .as_ref()
            .map(|hr| hr.borrow_mut())
            .into_iter()
            .chain($self.rows.iter().map(|r| r.borrow_mut()))
    };
}

impl<'cell> Table<'cell> {
    pub fn add_row<R: Into<Row<'cell>>>(&mut self, row: R) {
        self.rows.push(RefCell::new(row.into()));
    }

    pub fn add_rows<I: IntoIterator<Item = Row<'cell>>>(&mut self, iter: I) {
        self.rows.extend(iter.into_iter().map(RefCell::new));
    }

    pub fn add_separator_row(&mut self, separator: char, span: u16) {
        self.add_row(vec![Cell::from(&"").with_padding_char(separator).with_span(span)]);
    }

    pub fn set_heading_row<C: Into<Cell<'cell>>>(&mut self, cells: Vec<C>) {
        let row = Row::new(cells);
        row.root_mut().iter_mut().for_each(|mut cell| {
            cell.set_bold(true);
        });
        self.heading_row = Some(RefCell::new(row));
    }

    fn align_columns<'a>(&'a self) {
        // Wrap newlines
        loop {
            let mut wrapped_any = false;
            for row in self.rows.iter().chain(self.heading_row.iter()) {
                wrapped_any |= row.borrow_mut().wrap_text_eager();
            }
            if !wrapped_any {
                break;
            }
        }

        // Temporarily lock all rows so the Refs remain valid for the duration of this function.
        let rows: Vec<_> =
            self.rows.iter().chain(self.heading_row.iter()).map(RefCell::borrow).collect();

        let mut align_all = true;
        while align_all {
            align_all = false;

            // Create a list of cell information for every cell in the table. Once we have processed
            // this list, we'll be sure that we have processed the needs of every cell in the table.
            let mut cell_info =
                rows.iter().flat_map(|r| Cell::all_cell_info(&r.root)).collect::<Vec<_>>();

            // Sort depth-first. This is crucial to ensure we append columns in the right order.
            cell_info.sort();
            cell_info.reverse();

            // A chain of columns where each entry represents a chain of columns at the same depth.
            let mut root_column_node: Option<ColumnTreeNode<'cell>> = None;
            while let Some(head) = cell_info.pop() {
                let mut col_cells = vec![Rc::clone(&head.cell)];
                // Remove all similar cells in the same column. We'll address them all at once.
                while let Some(next) = cell_info.last() {
                    if &head == next {
                        let next = cell_info.pop().unwrap();
                        col_cells.push(next.cell);
                    } else {
                        break;
                    }
                }
                let col = CellColumn { cells: col_cells, depth: head.depth() };
                match root_column_node.as_mut() {
                    Some(node) => {
                        node.append_column(col, head.depth());
                    }
                    None => {
                        root_column_node = Some(ColumnTreeNode::new(col));
                    }
                }
            }
            if let Some(root) = root_column_node {
                if root.try_align_to_max_width(
                    self.max_width
                        .unwrap_or(term_size::dimensions().map(|(w, _)| w).unwrap_or(usize::MAX)),
                ) {
                    align_all = true;
                }
            }
        }
    }

    pub fn set_min_width(&mut self, width: Option<usize>) {
        assert!(
            width.is_none()
                || self.max_width.is_none()
                || width.unwrap() <= self.max_width.unwrap()
        );

        self.min_width = width
    }

    pub fn set_max_width(&mut self, width: Option<usize>) {
        assert!(
            width.is_none()
                || self.min_width.is_none()
                || width.unwrap() >= self.min_width.unwrap()
        );

        self.max_width = width
    }

    pub fn set_column_separator(&mut self, separator: &'static str) {
        self.column_separator = separator;
    }

    pub fn set_indent(&mut self, indent: usize) {
        self.indent = indent;
    }

    fn pad_to_width(&self) {
        if let Some(width) = self.min_width {
            for row in all_rows_mut!(self) {
                let row_width = row.padded_width();
                if row_width < width {
                    // Pad cells proportionally with extra padding.
                    let mut extra = width - row_width;
                    let mut row_root = row.root_mut();
                    let mut row_cells = row_root.iter_mut().peekable();
                    while let Some(mut cell) = row_cells.next() {
                        let cell_width = cell.padded_width();
                        let cell_extra = if row_cells.peek().is_some() {
                            ((cell_width as f64 / row_width as f64) * extra as f64).round() as usize
                        } else {
                            extra
                        };
                        cell.pad_to_width(cell_width + cell_extra);
                        extra -= cell_extra;
                    }
                }
            }
        }
    }

    pub fn print<W: fmt::Write>(&self, w: &mut W) -> fmt::Result {
        for mut row in all_rows_mut!(self) {
            row.set_separator(self.column_separator);
        }
        self.align_columns();
        self.pad_to_width();
        trace!("Table: {:?}", self);

        let mut row_iter = all_rows!(self).peekable();
        while let Some(row) = row_iter.next() {
            write!(w, "{:indent$}", "", indent = self.indent)?;
            row.print(w)?;
            if row_iter.peek().is_some() {
                writeln!(w)?;
            }
        }
        Style::fmt_end(w)?;
        Ok(())
    }

    pub fn print_csv<W: fmt::Write>(&self, w: &mut W) -> fmt::Result {
        for mut row in all_rows_mut!(self) {
            row.set_separator(",");
        }
        trace!("Table: {:?}", self);

        let mut row_iter = all_rows!(self).peekable();
        while let Some(row) = row_iter.next() {
            row.print_csv(w)?;
            if row_iter.peek().is_some() {
                writeln!(w)?;
            }
        }
        Style::fmt_end(w)?;
        Ok(())
    }
}

impl Default for Table<'_> {
    fn default() -> Self {
        Self {
            column_separator: "  ",
            heading_row: None,
            rows: Vec::new(),
            min_width: None,
            max_width: term_size::dimensions().map(|(w, _)| w),
            indent: 0,
        }
    }
}

impl fmt::Debug for Table<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.align_columns();
        self.pad_to_width();

        let mut row_iter = all_rows!(self).peekable();
        while let Some(row) = row_iter.next() {
            write!(f, "{:indent$}", "", indent = self.indent)?;
            write!(f, "{:?}", row)?;
            if row_iter.peek().is_some() {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}
