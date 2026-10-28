/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::ext::StrExt;
use crate::report::table2::cell::ModifiableCell;
use crate::report::table2::fmt::CellFormatter;
use crate::report::table2::{Cell, CellRef, CellWidth, ColumnWidth, ShrinkableCell};
use std::fmt;
use std::fmt::Formatter;

#[derive(Default, Copy, Clone, PartialEq, Eq)]
pub enum EllipsisPolicy {
    AfterMidSep(char),
    #[default]
    End,
}

pub struct EllipsisCell<'c> {
    modifiable_cell: ModifiableCell<'c>,
    policy: EllipsisPolicy,
}

impl<'c> EllipsisCell<'c> {
    pub fn new<C: Into<CellRef<'c>>>(cell: C, policy: EllipsisPolicy) -> Self {
        Self { modifiable_cell: ModifiableCell::new(cell), policy }
    }
}

impl Cell for EllipsisCell<'_> {
    fn print<'format>(
        &self,
        f: &mut dyn CellFormatter,
        line: usize,
        width: Option<ColumnWidth>,
    ) -> fmt::Result {
        self.modifiable_cell.print(f, line, width)
    }

    fn width(&self) -> CellWidth {
        self.modifiable_cell.width()
    }

    fn height(&self) -> usize {
        self.modifiable_cell.height()
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

impl ShrinkableCell for EllipsisCell<'_> {
    fn try_shrink(&self, target_width: &CellWidth) -> usize {
        let mut success_len = 0;
        for mut line in self.modifiable_cell.longest_lines_mut() {
            debug_assert!(
                line.width().sum() >= target_width.sum(),
                "{} < {}",
                line.width().sum(),
                target_width.sum()
            );

            let width_diff = line.width().sum() - target_width.sum();
            let trim_len = width_diff + 2;
            if width_diff > 0 {
                let (start_pos, end_pos) = match self.policy {
                    EllipsisPolicy::AfterMidSep(sep) => {
                        match line.find_closest_to_mid("..").or(line.find_closest_to_mid(sep)) {
                            Some(pos) => {
                                let (mut start_pos, mut end_pos) = (
                                    pos,
                                    if line[pos..].starts_with("..") { pos + 2 } else { pos + 1 },
                                );

                                while (start_pos..end_pos).len() < trim_len {
                                    let left = &line[..start_pos];
                                    let right = &line[end_pos..];
                                    if left.len() > right.len() {
                                        match left.chars().rev().position(|c| c == sep) {
                                            Some(i) => start_pos = left.len() - i - 1,
                                            // Retain first component
                                            None => break,
                                        }
                                    } else {
                                        match right.chars().position(|c| c == sep) {
                                            Some(i) => end_pos += i + 1,
                                            // Retain last component
                                            None => break,
                                        }
                                    }
                                }
                                (start_pos, end_pos)
                            }
                            None => return 0,
                        }
                    }
                    EllipsisPolicy::End => {
                        let mut start_pos =
                            line.char_indices().rev().nth(trim_len).map(|(i, _)| i).unwrap_or(0);
                        while line[..start_pos].ends_with(' ') {
                            start_pos -= 1;
                        }
                        let end_pos = line.len();
                        // Keep at least two chars
                        if (start_pos..end_pos).len() < 2 {
                            return 0;
                        }
                        (start_pos, end_pos)
                    }
                };
                line.replace_range(start_pos..end_pos, "..");
                success_len = (start_pos..end_pos).len() - 2;
            }
        }
        success_len
    }

    fn is_lossy(&self) -> bool {
        true
    }

    fn min_width(&self) -> usize {
        // Two chars plus "..". Must agree with trunc_len above.
        4
    }
}

impl fmt::Debug for EllipsisCell<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Ellipsis({:?})", self.modifiable_cell)
    }
}
