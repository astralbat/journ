/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use std::ops::Add;
use std::{fmt, mem};

#[derive(Clone)]
pub enum BinaryTree<T, X = ()> {
    Branch(Box<BinaryTree<T, X>>, Box<BinaryTree<T, X>>, X),
    Leaf(T),
}

impl<T, X> BinaryTree<T, X> {
    /// Returns the sum of all `T` from all leafs.
    pub fn sum(&self) -> T
    where
        T: Add<Output = T> + Clone,
    {
        match self {
            BinaryTree::Leaf(t) => t.clone(),
            BinaryTree::Branch(left, right, _) => left.sum() + right.sum(),
        }
    }

    pub fn is_branch(&self) -> bool {
        matches!(self, BinaryTree::Branch(..))
    }

    pub fn is_leaf(&self) -> bool {
        matches!(self, BinaryTree::Leaf(..))
    }

    /// Returns the left subtree.
    pub fn left(&self) -> Option<&BinaryTree<T, X>> {
        match self {
            BinaryTree::Branch(left, _, _) => Some(left),
            BinaryTree::Leaf(_) => None,
        }
    }

    pub fn left_mut(&mut self) -> Option<&mut BinaryTree<T, X>> {
        match self {
            BinaryTree::Branch(left, _, _) => Some(left),
            BinaryTree::Leaf(_) => None,
        }
    }

    /// Returns the right subtree.
    pub fn right(&self) -> Option<&BinaryTree<T, X>> {
        match self {
            BinaryTree::Branch(_, right, _) => Some(right),
            BinaryTree::Leaf(_) => None,
        }
    }

    pub fn right_mut(&mut self) -> Option<&mut BinaryTree<T, X>> {
        match self {
            BinaryTree::Branch(_, right, _) => Some(right),
            BinaryTree::Leaf(_) => None,
        }
    }

    /// Grows the tree by adding a new, top-level right node.
    pub fn push_right(&mut self, rhs: Self, extra: X) {
        // SAFETY: we immediately overwrite self before it's observed again
        let left = unsafe { std::ptr::read(self) };
        unsafe {
            std::ptr::write(self, BinaryTree::Branch(Box::new(left), Box::new(rhs), extra));
        }
    }

    pub fn grow_right(&mut self, rhs: Self, extra: X) {
        if self.is_balanced() {
            self.push_right(rhs, extra);
            return;
        }
        match self {
            BinaryTree::Branch(_left, right, _) => {
                right.grow_right(rhs, extra);
            }
            BinaryTree::Leaf(_) => {
                self.push_right(rhs, extra);
            }
        }
    }

    pub fn is_balanced(&self) -> bool {
        match self {
            BinaryTree::Leaf(_) => false,
            BinaryTree::Branch(left, right, _) => {
                if left.is_leaf() && right.is_leaf() {
                    return true;
                }
                left.is_balanced() && right.is_balanced()
            }
        }
    }

    /// This is the reverse of the last [Self::push_right(Self)] operation, returning the rhs of the tree if it exists.
    pub fn pop_right(&mut self) -> Option<BinaryTree<T, X>>
    where
        T: Default,
    {
        if matches!(self, BinaryTree::Leaf(_)) {
            return None;
        }

        match mem::take(self) {
            BinaryTree::Leaf(_) => unreachable!(),
            BinaryTree::Branch(left, right, _) => {
                *self = *left;
                Some(*right)
            }
        }
    }

    pub fn map<F, U>(self, f: F) -> BinaryTree<U, X>
    where
        F: Fn(T) -> U + Clone,
    {
        match self {
            BinaryTree::Leaf(t) => BinaryTree::Leaf(f(t)),
            BinaryTree::Branch(left, right, x) => {
                BinaryTree::Branch(Box::new(left.map(f.clone())), Box::new(right.map(f)), x)
            }
        }
    }

    /// Iterates through all leaf values in the tree, working from left to right.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        struct StackIter<'a, T, X> {
            stack: Vec<&'a BinaryTree<T, X>>,
            current: Option<&'a BinaryTree<T, X>>,
        }
        impl<'a, T, X> Iterator for StackIter<'a, T, X> {
            type Item = &'a T;
            fn next(&mut self) -> Option<Self::Item> {
                match self.current {
                    Some(current) => match current {
                        BinaryTree::Leaf(t) => {
                            self.current = self.stack.pop();
                            Some(t)
                        }
                        BinaryTree::Branch(left, right, _) => {
                            self.stack.push(right);
                            self.current = Some(left);
                            self.next()
                        }
                    },
                    None => None,
                }
            }
        }
        StackIter { stack: vec![], current: Some(self) }
    }
}

impl<T, X> Default for BinaryTree<T, X>
where
    T: Default,
{
    fn default() -> Self {
        Self::Leaf(T::default())
    }
}

impl<T, X> Add for &BinaryTree<T, X>
where
    T: Add<Output = T> + Clone,
    X: Default + Clone,
{
    type Output = BinaryTree<T, X>;

    fn add(self, rhs: Self) -> Self::Output {
        BinaryTree::Branch(Box::new(self.clone()), Box::new(rhs.clone()), X::default())
    }
}

impl<T: PartialEq + Add<Output = T> + Clone, X> PartialEq for BinaryTree<T, X> {
    fn eq(&self, other: &Self) -> bool {
        self.sum() == other.sum()
    }
}

impl<T: PartialEq + Add<Output = T> + Clone, X> PartialEq<T> for BinaryTree<T, X> {
    fn eq(&self, other: &T) -> bool {
        self.sum() == *other
    }
}

impl<T: Eq + Add<Output = T> + Clone, X> Eq for BinaryTree<T, X> {}

impl<T: PartialOrd + Add<Output = T> + Clone, X> PartialOrd for BinaryTree<T, X> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.sum().partial_cmp(&other.sum())
    }
}

impl<T: PartialOrd + Add<Output = T> + Clone, X> PartialOrd<T> for BinaryTree<T, X> {
    fn partial_cmp(&self, other: &T) -> Option<std::cmp::Ordering> {
        self.sum().partial_cmp(other)
    }
}

impl<T: Ord + Add<Output = T> + Clone, X> Ord for BinaryTree<T, X> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.sum().cmp(&other.sum())
    }
}

impl<T, X> FromIterator<T> for BinaryTree<T, X>
where
    X: Default,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut iter = iter.into_iter();
        let mut tree = match iter.next() {
            Some(first) => Self::Leaf(first),
            None => panic!("Can't build tree from empty iterator"),
        };
        for item in iter {
            tree.grow_right(Self::Leaf(item), X::default());
        }
        tree
    }
}

/*
impl<'a, T, X> Sum<&'a Self> for BinaryTree<T, X>
where
    T: Default + Add<Output = T> + Clone,
    X: Default + Clone,
{
    fn sum<I: Iterator<Item = &'a Self>>(iter: I) -> Self {
        iter.fold(Self::default(), |acc, w| &acc + w)
    }
}

impl<T, X> Sum<Self> for Option<BinaryTree<T, X>>
where
    T: Default + Add<Output = T> + Clone,
    X: Default + Clone,
{
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Option<Self> {
        iter.map(Some).fold(None, |acc, w| &acc + &w)
    }
}*/

impl<T, X> From<T> for BinaryTree<T, X> {
    fn from(t: T) -> Self {
        Self::Leaf(t)
    }
}

impl<T, X> From<(T, T, X)> for BinaryTree<T, X> {
    fn from(tuple: (T, T, X)) -> Self {
        Self::Branch(Box::new(Self::Leaf(tuple.0)), Box::new(Self::Leaf(tuple.1)), tuple.2)
    }
}

impl<T, X> From<(T, T)> for BinaryTree<T, X>
where
    X: Default,
{
    fn from(tuple: (T, T)) -> Self {
        Self::Branch(
            Box::new(Self::Leaf(tuple.0)),
            Box::new(Self::Leaf(tuple.1)),
            Default::default(),
        )
    }
}

impl<T, X> fmt::Debug for BinaryTree<T, X>
where
    T: fmt::Debug,
    X: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_tree(self, f, " ", "")
    }
}

fn fmt_tree<T: fmt::Debug, X: fmt::Debug>(
    tree: &BinaryTree<T, X>,
    f: &mut fmt::Formatter<'_>,
    prefix: &str,
    connector: &str,
) -> fmt::Result {
    match tree {
        BinaryTree::Leaf(t) => writeln!(f, "{prefix}{connector} {t:?}"),
        BinaryTree::Branch(left, right, x) => {
            writeln!(f, "{prefix}{connector}┬ ({x:?})")?;
            let extension = if connector == "└──" {
                "   "
            } else if connector.is_empty() {
                "" // root: children get no prefix
            } else {
                "│  "
            };
            let child_prefix = format!("{prefix}{extension}");
            fmt_tree(left, f, &child_prefix, "├──")?;
            fmt_tree(right, f, &child_prefix, "└──")
        }
    }
}
