/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table2::cell::{Cell, ModifiableCell, ShrinkableCell};
use crate::reporting::table2::cell_width::CellWidth;
use crate::reporting::table2::fmt::CellFormatter;
use crate::reporting::table2::{CellRef, ColumnWidth};
use std::fmt;
use std::str::pattern::{Pattern, ReverseSearcher};

#[derive(Default, Copy, Clone, PartialEq, Eq)]
pub enum WrapPolicy {
    AfterStr(&'static str),
    #[default]
    Word,
}

#[derive(Default, PartialEq, Eq, Clone, Copy)]
pub enum WrapEase {
    /// Always wrap those lines that are the widest in the cell.
    Eager,
    #[default]
    /// Only wrap those lines that are the widest in the column.
    Reluctant,
}

pub struct PolicyWrappingCell<'c> {
    modifiable_cell: ModifiableCell<'c>,
    wrapping_policy: WrapPolicy,
    ease: WrapEase,
}
impl<'c> PolicyWrappingCell<'c> {
    pub fn new<C: Into<CellRef<'c>>>(inner: C, wrapping_policy: WrapPolicy) -> Self {
        Self {
            modifiable_cell: ModifiableCell::new(inner),
            wrapping_policy,
            ease: WrapEase::default(),
        }
    }

    pub fn set_wrap_ease(&mut self, ease: WrapEase) {
        self.ease = ease;
    }

    /// Finds the `pattern` that resides closest to the middle of `haystack`.
    fn find_closest_to_middle<P: Pattern + Copy>(haystack: &str, pattern: P) -> Option<usize>
    where
        for<'a> P::Searcher<'a>: ReverseSearcher<'a>,
    {
        let mut best_pos = haystack.len();
        while let Some(pos) = haystack[..best_pos].rfind(pattern) {
            let mid = haystack.len() / 2;
            // Minimise distance from middle
            if (pos as isize - mid as isize).abs() > (best_pos as isize - mid as isize).abs() {
                return Some(best_pos);
            }
            best_pos = pos;
        }
        if best_pos == haystack.len() { None } else { Some(best_pos) }
    }

    fn try_wrap_word(this: &PolicyWrappingCell, current_column_width: usize) -> bool {
        Self::try_wrap(this, current_column_width, |c| c == ' ' || c == '\t', 1, true)
    }

    fn try_wrap_after_str(this: &PolicyWrappingCell, current_column_width: usize, s: &str) -> bool {
        Self::try_wrap(this, current_column_width, s, s.len(), false)
    }

    /// Tries to wrap by splitting the relevant line(s) nearest the middle at (or after, using `split_offset`) the given `pattern`.
    fn try_wrap<P: Pattern + Copy>(
        this: &PolicyWrappingCell,
        current_column_width: usize,
        pattern: P,
        split_offset: usize,
        truncate: bool,
    ) -> bool
    where
        for<'a> P::Searcher<'a>: ReverseSearcher<'a>,
    {
        let max_line_width =
            this.modifiable_cell.lines.borrow().iter().map(|l| l.chars().count()).max();
        match max_line_width {
            Some(max_line_width) => {
                let mut lines = this.modifiable_cell.lines_mut();
                for (line_num, line) in
                    lines.iter_mut().enumerate().filter(|(_, l)| match this.ease {
                        WrapEase::Eager => l.chars().count() == max_line_width,
                        WrapEase::Reluctant => l.chars().count() == current_column_width,
                    })
                {
                    let found_pos = Self::find_closest_to_middle(line, pattern);
                    if let Some(pos) = found_pos
                        && pos + split_offset < line.len()
                    {
                        let new_line = line.split_off(pos + split_offset);
                        if truncate {
                            line.truncate(line.len() - split_offset);
                        }
                        lines.insert(line_num + 1, new_line);
                        return true;
                    }
                }
                false
            }
            None => false,
        }
    }
}
impl Cell for PolicyWrappingCell<'_> {
    fn print<'format>(
        &self,
        f: &mut dyn CellFormatter,
        line: usize,
        _width: Option<ColumnWidth>,
    ) -> fmt::Result {
        let lines = self.modifiable_cell.lines.borrow();
        let line = lines.get(line).ok_or(fmt::Error)?;
        write!(f, "{}", line)
    }

    fn width(&self) -> CellWidth {
        self.modifiable_cell.lines.borrow().iter().map(|l| l.width()).max().unwrap_or_default()
    }

    fn height(&self) -> usize {
        self.modifiable_cell.lines.borrow().len()
    }

    fn hspan(&self) -> usize {
        self.modifiable_cell.hspan()
    }

    fn vspan(&self) -> usize {
        self.modifiable_cell.vspan()
    }

    fn padding_char(&self) -> char {
        self.modifiable_cell.padding_char()
    }

    fn as_shrinkable(&self) -> Option<&dyn ShrinkableCell> {
        Some(self)
    }
}
impl ShrinkableCell for PolicyWrappingCell<'_> {
    fn try_shrink(&self, max_width: usize) -> bool {
        if self.ease == WrapEase::Reluctant && self.width() < max_width {
            return false;
        }
        match self.wrapping_policy {
            WrapPolicy::Word => Self::try_wrap_word(self, max_width),
            WrapPolicy::AfterStr(s) => Self::try_wrap_after_str(self, max_width, s),
        }
    }
}
