/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::report::command::arguments::Cmd;
use crate::report::table2::cell::{lease_formatter, return_formatter};
use crate::report::table2::fmt::{BasicCellFormatter, CellFormatter};
use crate::report::table2::{Cell, CellRef, CellWidth, ColumnWidth, ShrinkableCell};
use crate::report::term_style;
use crate::report::term_style::Style;
use std::sync::LazyLock;

pub static IS_STYLED: LazyLock<bool> = LazyLock::new(|| {
    Cmd::args().color || (!Cmd::args().no_color && atty::is(atty::Stream::Stdout))
});

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
    fn print<'format>(
        &self,
        f: &mut dyn CellFormatter,
        line: usize,
        width: Option<ColumnWidth>,
    ) -> std::fmt::Result {
        if self.inner.height() <= line {
            return Err(std::fmt::Error);
        }

        if *IS_STYLED {
            let mut start_and_inner_fmt = lease_formatter();
            let mut end_fmt = lease_formatter();
            self.style.start(&mut start_and_inner_fmt)?;
            let res = self.inner.print(&mut start_and_inner_fmt, line, width.clone());
            self.style.end(&mut end_fmt)?;

            write!(f, "{}", start_and_inner_fmt)?;
            // It's a good idea to pad here (otherwise the formatter will do it) to keep a consistent style
            if let Some(width) = width {
                for _ in start_and_inner_fmt.count() + end_fmt.count()..width.width() {
                    write!(f, "{}", self.padding_char())?;
                }
            }
            write!(f, "{}", end_fmt)?;

            return_formatter(end_fmt);
            return_formatter(start_and_inner_fmt);
            res
        } else {
            self.inner.print(f, line, width)
        }
    }

    fn width(&self) -> CellWidth {
        if term_style::DEBUG_MODE {
            let mut buf = String::new();
            let mut fmt = BasicCellFormatter::new(&mut buf);
            self.style.start(&mut fmt).unwrap();
            self.style.end(&mut fmt).unwrap();
            CellWidth::Unary(buf.chars().count() + self.inner.width().width())
        } else {
            self.inner.width()
        }
    }

    fn height(&self) -> usize {
        self.inner.height()
    }

    fn as_shrinkable(&self) -> Option<&dyn ShrinkableCell> {
        self.inner.as_shrinkable()
    }
}

impl<'c> From<StyledCell<'c>> for CellRef<'c> {
    fn from(s: StyledCell<'c>) -> Self {
        CellRef::Owned(Box::new(s))
    }
}
