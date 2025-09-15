/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table2::cell::Cell;
use crate::reporting::table2::cell_width::{CellWidth, SpaceDistribution};
use crate::reporting::table2::fmt::CellFormatter;
use std::fmt;

/// A cell composed of two other cells.
pub struct BinaryCell {
    left: Box<dyn Cell>,
    right: Box<dyn Cell>,
    space_distribution: SpaceDistribution,
}
impl BinaryCell {
    pub fn new(left: Box<dyn Cell>, right: Box<dyn Cell>) -> Self {
        Self { left, right, space_distribution: SpaceDistribution::default() }
    }
    pub fn left(&self) -> &dyn Cell {
        self.left.as_ref()
    }
    pub fn right(&self) -> &dyn Cell {
        self.right.as_ref()
    }

    pub fn space_distribution(&self) -> SpaceDistribution {
        self.space_distribution
    }

    pub fn set_space_distribution(&mut self, space_distribution: SpaceDistribution) {
        self.space_distribution = space_distribution;
    }
}
impl Cell for BinaryCell {
    fn print<'format>(&self, f: &mut dyn CellFormatter<'format>, line: usize) -> fmt::Result {
        assert!(
            matches!(f.width(), Some(CellWidth::Binary(_, _, _)) | None),
            "BinaryCell can only be written to a CellFormatter with a Binary width"
        );
        let padded_l_width = f.width().and_then(|pad_w| pad_w.left().cloned());
        let padded_r_width = f.width().and_then(|pad_w| pad_w.right().cloned());

        let start_col = f.cursor_col();

        f.set_width(padded_l_width.clone());
        let left_res = self.left.print(f, line);

        // Print padding between left and right cells
        if let Some(padded_l_width) = padded_l_width {
            debug_assert!(
                f.cursor_col() - start_col <= padded_l_width.width(),
                "Left cell wrote more than its allocated width ({} > {})",
                f.cursor_col() - start_col,
                padded_l_width.width()
            );
            for _ in 0..(padded_l_width.width() - (f.cursor_col() - start_col)) {
                write!(f, "{}", self.padding_char())?;
            }
        }

        f.set_width(padded_r_width);
        let right_res = self.right.print(f, line);
        left_res.or(right_res)
    }

    fn width(&self) -> CellWidth {
        CellWidth::Binary(
            Box::new(self.left.width()),
            Box::new(self.right.width()),
            self.space_distribution,
        )
    }

    fn hspan(&self) -> usize {
        self.left.hspan() + self.right.hspan() - 1
    }

    fn vspan(&self) -> usize {
        self.left.vspan().max(self.right.vspan())
    }

    fn padding_char(&self) -> char {
        self.left.padding_char()
    }
}
