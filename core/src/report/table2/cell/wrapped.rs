/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::report::table2::cell::{Cell, ModifiableCell, ShrinkableCell};
use crate::report::table2::cell_width::CellWidth;
use crate::report::table2::fmt::CellFormatter;
use crate::report::table2::{CellRef, ColumnWidth};
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
    const fn wrap_word_pattern(c: char) -> bool {
        c == ' ' || c == '\t'
    }

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

    fn try_wrap_word(this: &PolicyWrappingCell) -> usize {
        Self::try_wrap(this, Self::wrap_word_pattern, 1, true)
    }

    fn try_wrap_after_str(this: &PolicyWrappingCell, s: &str) -> usize {
        Self::try_wrap(this, s, s.len(), false)
    }

    fn wrap_pos<'s, P: Pattern + Copy>(
        this: &PolicyWrappingCell,
        pattern: P,
        split_offset: usize,
    ) -> Option<(usize, usize)>
    where
        for<'a> P::Searcher<'a>: ReverseSearcher<'a>,
    {
        let max_line_width =
            this.modifiable_cell.lines.borrow().iter().map(|l| l.chars().count()).max();
        match max_line_width {
            Some(max_line_width) => {
                let mut lines = this.modifiable_cell.lines_mut();
                for (line_num, line) in lines
                    .iter_mut()
                    .enumerate()
                    .filter(|(_, l)| l.chars().count() == max_line_width)
                {
                    if let Some(pos) = Self::find_closest_to_middle(line, pattern)
                        && pos + split_offset < line.len()
                    {
                        return Some((line_num, pos));
                    }
                }
                None
            }
            None => None,
        }
    }

    /// Tries to wrap by splitting the relevant line(s) nearest the middle at (or after, using `split_offset`) the given `pattern`.
    fn try_wrap<P: Pattern + Copy>(
        this: &PolicyWrappingCell,
        pattern: P,
        split_offset: usize,
        truncate: bool,
    ) -> usize
    where
        for<'a> P::Searcher<'a>: ReverseSearcher<'a>,
    {
        if let Some((line_num, pos)) = Self::wrap_pos(this, pattern, split_offset) {
            let width_before = this.width().sum();
            {
                let mut lines = this.modifiable_cell.lines_mut();
                let line = lines.get_mut(line_num).unwrap();
                let new_line = line.split_off(pos + split_offset);
                if truncate {
                    line.truncate(line.len() - split_offset);
                }
                lines.insert(line_num + 1, new_line);
            }
            return width_before - this.width().sum();
        }
        0
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
        match self.wrapping_policy {
            WrapPolicy::Word => Self::wrap_pos(self, Self::wrap_word_pattern, 1),
            WrapPolicy::AfterStr(s) => Self::wrap_pos(self, s, s.len()),
        }?;
        Some(self)
    }
}
impl ShrinkableCell for PolicyWrappingCell<'_> {
    fn try_shrink(&self, _target_width: &CellWidth) -> usize {
        match self.wrapping_policy {
            WrapPolicy::Word => Self::try_wrap_word(self),
            WrapPolicy::AfterStr(s) => Self::try_wrap_after_str(self, s),
        }
    }

    fn is_lossy(&self) -> bool {
        false
    }
}
impl<'c> From<PolicyWrappingCell<'c>> for CellRef<'c> {
    fn from(pwc: PolicyWrappingCell<'c>) -> Self {
        CellRef::Owned(Box::new(pwc))
    }
}

impl fmt::Debug for PolicyWrappingCell<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Wrapping({:?})", self.modifiable_cell)
    }
}
