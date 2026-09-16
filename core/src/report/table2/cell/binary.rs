/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::report::table2::cell::{Cell, lease_formatter, return_formatter};
use crate::report::table2::cell_width::{CellWidth, SpaceDistribution};
use crate::report::table2::fmt::CellFormatter;
use crate::report::table2::{CellRef, ColumnWidth, ShrinkableCell};
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
    fn print<'format>(
        &self,
        f: &mut dyn CellFormatter,
        line: usize,
        width: Option<ColumnWidth>,
    ) -> fmt::Result {
        assert!(
            matches!(width, Some(CellWidth::Branch(_, _, _)) | None),
            "BinaryCell can only be written to a CellFormatter with a Binary width"
        );
        let padded_l_width = width.as_ref().and_then(|pad_w| pad_w.left().cloned());
        let padded_r_width = width.and_then(|pad_w| pad_w.right().cloned());

        let mut left_buffer = lease_formatter();
        let left_res = self.left.print(&mut left_buffer, line, padded_l_width.clone());
        write!(f, "{}", left_buffer)?;

        // Print padding between left and right cells
        if let Some(padded_l_width) = padded_l_width {
            debug_assert!(
                left_buffer.count() <= padded_l_width.sum(),
                "Left cell wrote more than its allocated width ({} > {})",
                left_buffer.count(),
                padded_l_width.sum()
            );
            for _ in 0..(padded_l_width.sum() - left_buffer.count()) {
                write!(f, "{}", self.padding_char())?;
            }
        }

        return_formatter(left_buffer);

        let right_res = self.right.print(f, line, padded_r_width);
        left_res.or(right_res)
    }

    fn width(&self) -> CellWidth {
        CellWidth::Branch(
            Box::new(self.left.width()),
            Box::new(self.right.width()),
            self.space_distribution,
        )
    }

    fn height(&self) -> usize {
        self.left.height().max(self.right.height())
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

    fn as_shrinkable(&self) -> Option<&dyn ShrinkableCell> {
        // It should not matter that we bias the left side here.
        // The column shrinking logic uses as_binary() to distribute
        // the shrinkage.
        self.left.as_shrinkable().or(self.right.as_shrinkable())
    }

    fn as_binary(&self) -> Option<&BinaryCell> {
        Some(self)
    }
}

/*
impl ShrinkableCell for BinaryCell {
    fn try_shrink(&self, target_width: &CellWidth) -> bool {
        debug_assert!(
            target_width.sum() <= self.width().sum(),
            "Target width ({}) must be <= cell width ({})",
            target_width.sum(),
            self.width().sum()
        );
        debug_assert_matches!(target_width.left(), Some(left_w) if left_w.sum() <= self.left.width().sum());
        debug_assert_matches!(target_width.right(), Some(right_w) if right_w.sum() <= self.right.width().sum());

        match (self.left.as_shrinkable(), self.right.as_shrinkable()) {
            // Both are shrinkable: shrink both sides according to target parts
            (Some(left), Some(right)) => {
                let mut shrunk_any = false;
                shrunk_any |= left.try_shrink(target_width.left().unwrap());
                shrunk_any |= right.try_shrink(target_width.right().unwrap());
                shrunk_any
            }
            // Only left is shrinkable: distribute the right side's target width diff on to the left side
            (Some(left), None) => {
                let right_diff = self.right.width().sum() as isize
                    - target_width.right().unwrap().sum() as isize;
                let dist_amount = right_diff
                    //.min((left.width().sum() - target_width.left().unwrap().sum()) as isize)
                    // Can't reduce target below 0
                    .min(target_width.left().unwrap().sum() as isize);
                left.try_shrink(&distribute(target_width.left().unwrap(), -dist_amount))
            }
            // Only right is shrinkable: distribute the left side's target width diff on to the right side
            (None, Some(right)) => {
                let left_diff =
                    self.left.width().sum() as isize - target_width.left().unwrap().sum() as isize;
                let dist_amount = left_diff
                    //.min((right.width().sum() - target_width.right().unwrap().sum()) as isize)
                    // Can't reduce target below 0
                    .min(target_width.right().unwrap().sum() as isize);
                right.try_shrink(&distribute(target_width.right().unwrap(), -dist_amount))
            }
            (None, None) => false,
        }
    }

    fn is_lossy(&self) -> bool {
        if let Some(left) = self.left.as_shrinkable()
            && left.is_lossy()
        {
            return true;
        }
        if let Some(right) = self.right.as_shrinkable()
            && right.is_lossy()
        {
            return true;
        }
        false
    }
}*/

impl<'c> From<BinaryCell> for CellRef<'c> {
    fn from(s: BinaryCell) -> Self {
        CellRef::Owned(Box::new(s))
    }
}

impl fmt::Debug for BinaryCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Binary({:?}, {:?})", &self.left, &self.right)
    }
}
