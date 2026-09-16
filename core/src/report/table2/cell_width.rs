/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::report::table2::binary_tree::BinaryTree;

#[derive(Clone, Copy, Debug, Default)]
pub enum SpaceDistribution {
    Left,
    Right,
    Even,
    #[default]
    Proportional,
}

/// Returns a width that is at same total width as the wider of the two. If one of them is binary,
/// both sides of the binary will be increased proportionally to this width. This is repeated
/// recursively for all descendants. In this way, no part of the width tree is ever reduced.
pub fn distributed_max(left: &CellWidth, right: &CellWidth) -> CellWidth {
    use BinaryTree::*;
    match (left, right) {
        (Leaf(l), Leaf(r)) => Leaf(*l.max(r)),
        (Branch(l1, r1, d1), Branch(l2, r2, _d2)) => {
            Branch(Box::new(distributed_max(l1, l2)), Box::new(distributed_max(r1, r2)), *d1)
        }
        // The unary width is wider than the binary width, so we distribute the extra space to the binary width.
        (branch, Leaf(u2)) | (Leaf(u2), branch) if u2 > &branch.sum() => {
            distribute(branch, (*u2 as isize) - branch.sum() as isize)
        }
        // The binary width is wider than the unary width, so we can just return the binary width.
        (branch, Leaf(_)) | (Leaf(_), branch) => branch.clone(),
    }
}

pub fn distributed_min(left: &CellWidth, right: &CellWidth) -> CellWidth {
    use BinaryTree::*;
    match (left, right) {
        (Leaf(l), Leaf(r)) => Leaf(*l.min(r)),
        (Branch(l1, r1, d1), Branch(l2, r2, _d2)) => {
            Branch(Box::new(distributed_min(l1, l2)), Box::new(distributed_min(r1, r2)), *d1)
        }
        // The leaf width is narrower than the branch width, so we distribute the extra space to the binary width.
        (branch, Leaf(u2)) | (Leaf(u2), branch) if u2 < &branch.sum() => {
            distribute(branch, (*u2 as isize) - branch.sum() as isize)
        }
        // The branch width is narrower than the leaf width, so we can just return the binary width.
        (branch, Leaf(_)) | (Leaf(_), branch) => branch.clone(),
    }
}

/// Distributes extra space proportionally.
/// The `extra_width` parameter can be negative to shrink the extra space.
pub fn distribute(inner: &CellWidth, extra_width: isize) -> CellWidth {
    debug_assert!(
        inner.sum() as isize + extra_width >= 0,
        "Cannot distribute {} on cell width {}",
        extra_width,
        inner.sum()
    );

    if extra_width == 0 {
        return inner.clone();
    }

    use BinaryTree::*;
    match inner {
        Branch(left, right, strategy) => {
            let (left_increase, right_increase) = match strategy {
                SpaceDistribution::Proportional => {
                    let left_width = left.sum();
                    let right_width = right.sum();

                    let left_pc_increase: f64 =
                        left_width as f64 / (left_width + right_width) as f64;
                    let left_increase = (extra_width as f64 * left_pc_increase).round() as isize;
                    let right_increase = extra_width - left_increase;
                    (left_increase, right_increase)
                }
                SpaceDistribution::Even => {
                    let half = extra_width / 2;
                    (half, extra_width - half)
                }
                SpaceDistribution::Left => {
                    // When extra_width < 0, can't take more than left.sum() width
                    let below_zero = (left.sum() as isize + extra_width).min(0);
                    (extra_width - below_zero, below_zero)
                }
                SpaceDistribution::Right => {
                    // When extra_width < 0, can't take more than right.sum() width
                    let below_zero = (right.sum() as isize + extra_width).min(0);
                    (below_zero, extra_width - below_zero)
                }
            };
            Branch(
                Box::new(distribute(left, left_increase)),
                Box::new(distribute(right, right_increase)),
                *strategy,
            )
        }
        Leaf(w) if extra_width >= 0 => Leaf(w + extra_width.unsigned_abs()),
        Leaf(w) => Leaf(w - extra_width.unsigned_abs()),
    }
}

pub type CellWidth = BinaryTree<usize, SpaceDistribution>;

/*
/// Represents the content width of a cell.
#[derive(Clone, Debug)]
pub struct CellWidth {
    inner: BinaryTree<usize, SpaceDistribution>,
}*/

/*
#[derive(Clone, Debug)]
pub enum CellWidth {
    Unary(usize),
    Binary(Box<CellWidth>, Box<CellWidth>, SpaceDistribution),
}*/

/*
impl CellWidth {
    pub fn into_inner(self) -> BinaryTree<usize, SpaceDistribution> {
        self.inner
    }

    pub fn unary(width: usize) -> CellWidth {
        CellWidth { inner: BinaryTree::Leaf(width) }
    }

    pub fn binary(
        left: CellWidth,
        right: CellWidth,
        space_distribution: SpaceDistribution,
    ) -> CellWidth {
        CellWidth {
            inner: BinaryTree::Branch(
                Box::new(left.into_inner()),
                Box::new(right.into_inner()),
                space_distribution,
            ),
        }
    }

    pub fn is_binary(&self) -> bool {
        matches!(self.inner, BinaryTree::Branch(_, _, _))
    }

    /*
        pub fn width(&self) -> usize {
            match self {
                CellWidth::Unary(width) => *width,
                CellWidth::Binary(left, right, _) => left.width() + right.width(),
            }
        }
    */

    pub fn left_clone(&self) -> Option<CellWidth> {
        self.inner.left().cloned().map(Into::into)
    }

    pub fn right_clone(&self) -> Option<CellWidth> {
        self.inner.right().cloned().map(Into::into)
    }

    /// Grows the width tree by adding a new, top-level right node.
    pub fn push_right(&mut self, rhs: CellWidth, space_distribution: SpaceDistribution) {
        self.inner.push_right(rhs.inner, space_distribution);
    }

    /// This is the reverse of the last [Self::push_right(CellWidth)] operation, returning the rhs of the binary width if it exists.
    pub fn pop_right(&mut self) -> Option<CellWidth> {
        self.inner.pop_right().map(From::from)
    }

    /// Returns a width that is at same total width as the wider of the two. If one of them is binary,
    /// both sides of the binary will be increased proportionally to this width. This is repeated
    /// recursively for all descendants. In this way, no part of the width tree is ever reduced.
    pub fn distributed_max(&self, other: &CellWidth) -> CellWidth {
        Self::from(Self::distributed_max_inner(&self.inner, &other.inner))
    }

    fn distributed_max_inner(
        _self: &BinaryTree<usize, SpaceDistribution>,
        other: &BinaryTree<usize, SpaceDistribution>,
    ) -> BinaryTree<usize, SpaceDistribution> {
        use BinaryTree::*;
        match (_self, other) {
            (Leaf(l), Leaf(r)) => Leaf(*l.max(r)).into(),
            (Branch(l1, r1, d1), Branch(l2, r2, _d2)) => Branch(
                Box::new(Self::distributed_max_inner(&*l1, &*l2)),
                Box::new(Self::distributed_max_inner(&*r1, &*r2)),
                *d1,
            ),
            // The unary width is wider than the binary width, so we distribute the extra space to the binary width.
            (branch, Leaf(u2)) | (Leaf(u2), branch) if u2 > &branch.sum() => {
                Self::distribute_inner(branch, (*u2 as isize) - branch.sum() as isize)
            }
            // The binary width is wider than the unary width, so we can just return the binary width.
            (branch, Leaf(_)) | (Leaf(_), branch) => branch.clone(),
        }
    }

    pub fn distributed_min(&self, other: &CellWidth) -> CellWidth {
        Self::from(Self::distributed_min_inner(&self.inner, &other.inner))
    }

    fn distributed_min_inner(
        _self: &BinaryTree<usize, SpaceDistribution>,
        other: &BinaryTree<usize, SpaceDistribution>,
    ) -> BinaryTree<usize, SpaceDistribution> {
        use BinaryTree::*;
        match (_self, other) {
            (Leaf(l), Leaf(r)) => Leaf(*l.min(r)).into(),
            (Branch(l1, r1, d1), Branch(l2, r2, _d2)) => Branch(
                Box::new(Self::distributed_min_inner(l1, l2)),
                Box::new(Self::distributed_min_inner(r2, r2)),
                *d1,
            )
            .into(),
            // The leaf width is narrower than the branch width, so we distribute the extra space to the binary width.
            (branch, Leaf(u2)) | (Leaf(u2), branch) if u2 < &branch.sum() => {
                Self::distribute_inner(branch, (*u2 as isize) - branch.sum() as isize)
            }
            // The branch width is narrower than the leaf width, so we can just return the binary width.
            (branch, Leaf(_)) | (Leaf(_), branch) => branch.clone(),
        }
    }

    /// Distributes extra space proportionally.
    /// The `extra_width` parameter can be negative to shrink the extra space.
    pub fn distribute(&self, extra_width: isize) -> CellWidth {
        Self::from(Self::distribute_inner(&self.inner, extra_width))
    }

}

impl Deref for CellWidth {
    type Target = BinaryTree<usize, SpaceDistribution>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for CellWidth {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl From<BinaryTree<usize, SpaceDistribution>> for CellWidth {
    fn from(tree: BinaryTree<usize, SpaceDistribution>) -> Self {
        CellWidth { inner: tree }
    }
}

impl Default for CellWidth {
    fn default() -> Self {
        CellWidth { inner: BinaryTree::default() }
    }
}

 */

/*
impl Add<&CellWidth> for &CellWidth {
    type Output = CellWidth;

    fn add(self, rhs: &CellWidth) -> Self::Output {
        if self.width() == 0 {
            return rhs.clone();
        }
        if rhs.width() == 0 {
            return self.clone();
        }
        Self::from(BinaryTree::Branch(
            Box::new(self.clone()),
            Box::new(rhs.clone()),
            SpaceDistribution::default(),
        ))
    }
}*/

/*
impl Sub<&CellWidth> for &CellWidth {
    type Output = CellWidth;

    fn sub(self, rhs: &CellWidth) -> Self::Output {
        assert!(self.width() >= rhs.width(), "{} must be >= {}", self.width(), rhs.width());

        self.distribute(-(rhs.width() as isize))

        /*
        if self.width() == 0 {
            return self.clone();
        }

        if rhs.width() == 0 {
            return self.clone();
        }
        match (self, rhs) {
            (CellWidth::Unary(left), CellWidth::Unary(right)) => CellWidth::Unary(left - right),
            (CellWidth::Unary(left), CellWidth::Binary(bl, br, _)) => CellWidth::Unary(left - bl.width() - br.width()),
            (CellWidth::Binary(_bl, _br, _), CellWidth::Unary(right)) => self.distributed_min(CellWidth::Unary(self.width() - right.width())),
            (CellWidth::Binary(_))
        }*/
    }
}*/

/*
impl<'a> Sum<&'a CellWidth> for CellWidth {
    fn sum<I: Iterator<Item = &'a CellWidth>>(iter: I) -> Self {
        iter.fold(CellWidth::Unary(0), |acc, w| &acc + w)
    }
}

impl Sum<CellWidth> for CellWidth {
    fn sum<I: Iterator<Item = CellWidth>>(iter: I) -> Self {
        iter.fold(CellWidth::Unary(0), |acc, w| &acc + &w)
    }
}*/

/*
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
}*/

pub type ColumnWidth = CellWidth;

/*
pub struct ColumnWidth {
    width: CellWidth,
    num_columns: usize,
}
impl ColumnWidth {
    pub fn new(width: CellWidth, num_columns: usize) -> Self {
        Self { width, num_columns }
    }

    pub fn num_columns(&self) -> usize {
        self.num_columns
    }
}
impl Deref for ColumnWidth {
    type Target = CellWidth;

    fn deref(&self) -> &Self::Target {
        &self.width
    }
}
*/
