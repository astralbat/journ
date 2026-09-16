/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
pub mod aligned;
pub mod binary;
pub mod blank;
pub mod decimal;
pub mod ellipsis;
pub mod multi;
pub mod separator;
pub mod spanned;
pub mod str;
pub mod styled;
pub mod wrapped;

use crate::report::table2::cell_width::CellWidth;
use crate::report::table2::fmt::{BasicCellFormatter, CellFormatter, StringCellFormatter};
use crate::report::table2::{BinaryCell, ColumnWidth};
use smallvec::SmallVec;
use smartstring::alias::String as SS;
use smartstring::{SmartString, SmartStringMode};
use std::cell::RefCell;
use std::{fmt, iter};

thread_local! {
    static TMP_FORMATTERS: RefCell<Vec<StringCellFormatter>> = const { RefCell::new(Vec::new()) };
}

/// Gets a temporary StringCellFormatter from the thread-local pool, or creates a new one if none are available.
fn lease_formatter() -> StringCellFormatter {
    TMP_FORMATTERS.with(|buffers| {
        let mut buffers = buffers.borrow_mut();
        let fmt = buffers.pop().unwrap_or_default();
        fmt
    })
}

/// Returns a temporary StringCellFormatter to the thread-local pool.
fn return_formatter(mut buffer: StringCellFormatter) {
    TMP_FORMATTERS.with(|buffers| {
        let mut buffers = buffers.borrow_mut();
        buffer.clear();
        buffers.push(buffer);
    });
}

pub enum CellRef<'c> {
    Owned(Box<dyn Cell + 'c>),
    Borrowed(&'c dyn Cell),
}
impl<'c> std::ops::Deref for CellRef<'c> {
    type Target = dyn Cell + 'c;

    fn deref(&self) -> &Self::Target {
        match self {
            CellRef::Owned(b) => b.as_ref(),
            CellRef::Borrowed(b) => *b,
        }
    }
}
impl<'c, C> From<Box<C>> for CellRef<'c>
where
    C: Cell + Sized + 'c,
{
    fn from(c: Box<C>) -> Self {
        CellRef::Owned(c)
    }
}

impl<'c, M: SmartStringMode + 'static> From<SmartString<M>> for CellRef<'c> {
    fn from(s: SmartString<M>) -> Self {
        CellRef::Owned(Box::new(s))
    }
}

impl From<String> for CellRef<'_> {
    fn from(s: String) -> Self {
        CellRef::Owned(Box::new(s))
    }
}

impl<'c, C> From<&'c C> for CellRef<'c>
where
    C: Cell + Sized + 'c,
{
    fn from(c: &'c C) -> Self {
        CellRef::Borrowed(c)
    }
}

impl fmt::Debug for CellRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Maybe we'll get rid of cell ref in the future. As they are usually
        // always owned, the variant debug is considered clutter.
        match self {
            CellRef::Owned(o) => write!(f, "{:?}", o),
            CellRef::Borrowed(b) => write!(f, "{:?}", b),
        }
    }
}

pub trait Cell: fmt::Debug {
    /// Writes the contents of the cell to the formatter, optionally padding its content to fill it.
    /// Returns Ok(()) if the line was printed, or Err(fmt::Error) if the line does not exist.
    fn print(
        &self,
        f: &mut dyn CellFormatter,
        line: usize,
        width: Option<ColumnWidth>,
    ) -> fmt::Result;

    /// The width of the cell in characters.
    fn width(&self) -> CellWidth;

    fn height(&self) -> usize;

    /// Returns the number of horizontal cells this cell spans. The default implementation returns 1.
    /// This must return >= 1.
    fn hspan(&self) -> usize {
        1
    }

    /// Returns the number of vertical cells this cell spans. The default implementation returns 1.
    /// This must return >= 1.
    fn vspan(&self) -> usize {
        1
    }

    fn padding_char(&self) -> char {
        ' '
    }

    fn as_shrinkable(&self) -> Option<&dyn ShrinkableCell> {
        None
    }

    fn as_binary(&self) -> Option<&BinaryCell> {
        None
    }
}

pub trait ShrinkableCell: Cell {
    /// Try to shrink the cell's width.
    ///
    /// The `target_width` provides a hint on the ideal maximum the cell should shrink to. This _must_
    /// be of magnitude less than or equal to the current width of the cell.
    ///
    /// Additionally, the `target_width` _must_ also be of the same shape as the cell's width.
    ///
    /// Returns how many chars the cell was shrunk by.
    fn try_shrink(&self, target_width: &CellWidth) -> usize;

    /// Gets whether shrinking this cell would result in the loss of information.
    fn is_lossy(&self) -> bool;

    fn min_width(&self) -> usize {
        1
    }
}

pub struct ModifiableCell<'c> {
    inner: CellRef<'c>,
    lines: RefCell<SmallVec<[SS; 2]>>,
}

impl<'c> ModifiableCell<'c> {
    pub fn new<C: Into<CellRef<'c>>>(cell: C) -> Self {
        let cell = cell.into();
        let mc = Self { inner: cell, lines: RefCell::new(SmallVec::new()) };
        mc.init_lines();
        mc
    }

    /// Initialize lines based on the inner cell's current content.
    fn init_lines(&self) {
        self.lines.borrow_mut().clear();
        let height = self.inner.height();
        for i in 0..height {
            let mut buf = SS::new();
            let mut cf = BasicCellFormatter::new(&mut buf);
            self.inner.print(&mut cf, i, None).unwrap();
            self.lines.borrow_mut().push(buf);
        }
    }

    pub fn lines_mut(&self) -> std::cell::RefMut<'_, SmallVec<[SS; 2]>> {
        self.lines.borrow_mut()
    }

    /// Gets all lines that are equal to the max width
    pub fn longest_lines_mut(&self) -> impl Iterator<Item = std::cell::RefMut<'_, SS>> {
        let max_width = self.width().sum();
        let mut index = 0;
        iter::from_fn(move || {
            let lines = self.lines.borrow_mut();
            while index < lines.len() {
                if lines[index].chars().count() == max_width {
                    let line_index = index;
                    index += 1;
                    return Some(std::cell::RefMut::map(lines, |l| &mut l[line_index]));
                }
                index += 1;
            }
            None
        })
    }
}

impl Cell for ModifiableCell<'_> {
    fn print<'format>(
        &self,
        f: &mut dyn CellFormatter,
        line: usize,
        width: Option<ColumnWidth>,
    ) -> fmt::Result {
        self.lines.borrow().get(line).map(|l| l.print(f, line, width)).unwrap_or(Err(fmt::Error))
    }

    fn width(&self) -> CellWidth {
        self.lines.borrow().iter().map(|s| s.width()).max().unwrap_or_default()
    }

    fn height(&self) -> usize {
        self.lines.borrow().len()
    }

    fn hspan(&self) -> usize {
        self.inner.hspan()
    }

    fn vspan(&self) -> usize {
        self.inner.vspan()
    }

    fn padding_char(&self) -> char {
        self.inner.padding_char()
    }

    fn as_shrinkable(&self) -> Option<&dyn ShrinkableCell> {
        self.inner.as_shrinkable()
    }

    fn as_binary(&self) -> Option<&BinaryCell> {
        self.inner.as_binary()
    }
}

impl fmt::Debug for ModifiableCell<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let len = self.lines.borrow().len();
        for (i, line) in self.lines.borrow().iter().enumerate() {
            write!(f, "{:?}", line)?;
            if i < len - 1 {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}
