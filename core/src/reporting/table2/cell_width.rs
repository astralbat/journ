/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use std::iter::Sum;
use std::mem;
use std::ops::Add;

#[derive(Clone, Copy, Debug, Default)]
pub enum SpaceDistribution {
    Left,
    Right,
    Even,
    #[default]
    Proportional,
}

/// Represents the content width of a cell.
#[derive(Clone, Debug)]
pub enum CellWidth {
    Unary(usize),
    Binary(Box<CellWidth>, Box<CellWidth>, SpaceDistribution),
}

impl CellWidth {
    pub fn width(&self) -> usize {
        match self {
            CellWidth::Unary(width) => *width,
            CellWidth::Binary(left, right, _) => left.width() + right.width(),
        }
    }

    pub fn left(&self) -> Option<&CellWidth> {
        match self {
            CellWidth::Unary(_) => None,
            CellWidth::Binary(left, _, _) => Some(left),
        }
    }

    pub fn right(&self) -> Option<&CellWidth> {
        match self {
            CellWidth::Unary(_) => None,
            CellWidth::Binary(_, right, _) => Some(right),
        }
    }

    pub fn push_right(&mut self, rhs: CellWidth) {
        let left = mem::replace(self, CellWidth::Unary(0));
        *self = CellWidth::Binary(Box::new(left), Box::new(rhs), SpaceDistribution::default());
    }

    /// This is the reverse of the last [Self::push_right(CellWidth)] operation, returning the rhs of the binary width if it exists.
    pub fn pop_right(&mut self) -> Option<CellWidth> {
        if matches!(self, CellWidth::Unary(_)) {
            return None;
        }

        let _self = mem::replace(self, CellWidth::Unary(0));
        match _self {
            CellWidth::Unary(_) => unreachable!(),
            CellWidth::Binary(left, right, _) => {
                *self = *left;
                Some(*right)
            }
        }
    }

    /// Returns a width that is at same total width as the wider of the two. If one of them is binary,
    /// both sides of the binary will be increased proportionally to this width. This is repeated
    /// recursively for all descendants. In this way, no part of the width tree is ever reduced.
    pub fn distributed_max(&self, other: &CellWidth) -> CellWidth {
        match (self, other) {
            (CellWidth::Unary(l), CellWidth::Unary(r)) => CellWidth::Unary(*l.max(r)),
            (CellWidth::Binary(l1, r1, d1), CellWidth::Binary(l2, r2, _d2)) => CellWidth::Binary(
                Box::new(l1.distributed_max(l2)),
                Box::new(r1.distributed_max(r2)),
                *d1,
            ),
            // The unary width is wider than the binary width, so we distribute the extra space to the binary width.
            (binary, CellWidth::Unary(u2)) | (CellWidth::Unary(u2), binary)
                if u2 > &binary.width() =>
            {
                Self::distribute(binary, u2 - binary.width())
            }
            // The binary width is wider than the unary width, so we can just return the binary width.
            (binary, CellWidth::Unary(_)) | (CellWidth::Unary(_), binary) => binary.clone(),
        }
    }

    /// Distributes extra space proportionally.
    pub fn distribute(&self, extra_width: usize) -> CellWidth {
        if extra_width == 0 {
            return self.clone();
        }

        match self {
            CellWidth::Binary(left, right, strategy) => {
                let (left_increase, right_increase) = match strategy {
                    SpaceDistribution::Proportional => {
                        let left_width = left.width();
                        let right_width = right.width();

                        let left_pc_increase: f64 =
                            left_width as f64 / (left_width + right_width) as f64;
                        let left_increase =
                            (extra_width as f64 * left_pc_increase).round() as usize;
                        let right_increase = extra_width - left_increase;
                        (left_increase, right_increase)
                    }
                    SpaceDistribution::Even => {
                        let half = extra_width / 2;
                        (half, extra_width - half)
                    }
                    SpaceDistribution::Left => (extra_width, 0),
                    SpaceDistribution::Right => (0, extra_width),
                };
                CellWidth::Binary(
                    Box::new(left.distribute(left_increase)),
                    Box::new(right.distribute(right_increase)),
                    *strategy,
                )
            }
            CellWidth::Unary(w) => CellWidth::Unary(w + extra_width),
        }
    }
}

impl Default for CellWidth {
    fn default() -> Self {
        CellWidth::Unary(0)
    }
}

impl Add<&CellWidth> for &CellWidth {
    type Output = CellWidth;

    fn add(self, rhs: &CellWidth) -> Self::Output {
        if self.width() == 0 {
            return rhs.clone();
        }
        if rhs.width() == 0 {
            return self.clone();
        }
        CellWidth::Binary(
            Box::new(self.clone()),
            Box::new(rhs.clone()),
            SpaceDistribution::default(),
        )
    }
}

impl<'a> Sum<&'a CellWidth> for CellWidth {
    fn sum<I: Iterator<Item = &'a CellWidth>>(iter: I) -> Self {
        iter.fold(CellWidth::Unary(0), |acc, w| &acc + w)
    }
}

impl Sum<CellWidth> for CellWidth {
    fn sum<I: Iterator<Item = CellWidth>>(iter: I) -> Self {
        iter.fold(CellWidth::Unary(0), |acc, w| &acc + &w)
    }
}

impl PartialEq for CellWidth {
    fn eq(&self, other: &Self) -> bool {
        self.width() == other.width()
    }
}

impl PartialEq<usize> for CellWidth {
    fn eq(&self, other: &usize) -> bool {
        self.width() == *other
    }
}

impl Eq for CellWidth {}

impl PartialOrd for CellWidth {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl PartialOrd<usize> for CellWidth {
    fn partial_cmp(&self, other: &usize) -> Option<std::cmp::Ordering> {
        Some(self.width().cmp(other))
    }
}

impl Ord for CellWidth {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.width().cmp(&other.width())
    }
}
