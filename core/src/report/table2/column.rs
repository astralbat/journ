/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::report::table2::binary_tree::BinaryTree;
use crate::report::table2::binary_tree::BinaryTree::Leaf;
use crate::report::table2::table::ColumnPreferences;
use crate::report::table2::{
    Cell, CellWidth, ColumnWidth, SpaceDistribution, distribute, distributed_max, distributed_min,
};
use smallvec::{SmallVec, smallvec};

#[derive(Default)]
pub struct TableColumn<'t> {
    /// The cells in this column.
    cells: Vec<&'t dyn Cell>,
    /// The width of the column in characters, excluding borders.
    width: CellWidth,
    /// The indices of the longest rows. Useful to keep track of when it comes to shrinking the column.
    longest_cells: BinaryTree<usize>,
    preferences: ColumnPreferences,
}

impl<'t> TableColumn<'t> {
    pub fn new(preferences: ColumnPreferences) -> Self {
        Self {
            cells: Vec::new(),
            width: CellWidth::default(),
            longest_cells: BinaryTree::default(),
            preferences,
        }
    }

    pub fn cell_at(&self, row: usize) -> Option<&'t dyn Cell> {
        self.cells.get(row).copied()
    }

    pub fn iter(&self) -> impl Iterator<Item = &&'t dyn Cell> {
        self.cells.iter()
    }

    /// Adds a cell to the column at the specified row index where the first row is 0.
    pub fn append(&mut self, cell: &'t dyn Cell) {
        self.cells.push(cell);
        // If the cell spans multiple columns, a good algorithm is to distribute the extra needed width across the columns
        // proportionally. But we don't yet know the final width of the columns involved.
        if cell.hspan() == 1 {
            self.update_width(self.cells.len() - 1, cell.width())
        }
    }

    fn update_longest(
        longest_tree: &mut BinaryTree<usize>,
        cell_pos: usize,
        cell_width: &CellWidth,
        column_width: Option<&ColumnWidth>,
    ) -> bool {
        use BinaryTree::*;
        match cell_width {
            Leaf(width) if column_width.map(|cw| width > &cw.sum()).unwrap_or(true) => {
                *longest_tree = Leaf(cell_pos);
                true
            }
            Branch(cell_width_left, cell_width_right, _) => {
                if let Leaf(_) = longest_tree
                    && column_width
                        .map(|cw| cell_width_left.sum() + cell_width_right.sum() > cw.sum())
                        .unwrap_or(false)
                {
                    *longest_tree = Branch(Box::new(Leaf(cell_pos)), Box::new(Leaf(cell_pos)), ());
                }
                if let Branch(longest_left, longest_right, _) = longest_tree {
                    let mut updated = false;
                    updated |= Self::update_longest(
                        longest_left,
                        cell_pos,
                        cell_width_left,
                        column_width.and_then(|cw| cw.left()),
                    );
                    updated |= Self::update_longest(
                        longest_right,
                        cell_pos,
                        cell_width_right,
                        column_width.and_then(|cw| cw.right()),
                    );
                    updated
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn update_width(&mut self, row_index: usize, new_width: CellWidth) {
        Self::update_longest(&mut self.longest_cells, row_index, &new_width, Some(&self.width));
        self.width = distributed_max(&new_width, &self.width);
    }

    /// Recalculates the width of the column based on the widths of the cells in it. This is useful after shrinking cells.
    /// A call to [Self::finalise_spanned] should follow this to recalculate for spanned cells on this column.
    fn recalc_width(&mut self) {
        self.width = CellWidth::default();
        self.longest_cells = BinaryTree::default();
        for (i, cell) in self.cells.iter().enumerate().filter(|(_, c)| c.hspan() == 1) {
            let cell_width = cell.width();
            Self::update_longest(&mut self.longest_cells, i, &cell_width, Some(&self.width));
            self.width = distributed_max(&self.width, &cell_width);
        }
    }

    pub fn width(&self) -> &CellWidth {
        &self.width
    }

    /// Gets the mean average content width of the cells in this column. This excludes cells that span multiple columns.
    pub fn avg_width(&self) -> f32 {
        let mut total = 0f32;
        for cell in self.iter() {
            if cell.hspan() == 1 {
                total += cell.width().sum() as f32;
            }
        }
        if self.cells.is_empty() { 0f32 } else { total / self.cells.len() as f32 }
    }

    /// Gets the standard deviation of the cell widths of this column.
    pub fn std_deviation_width(&self) -> f32 {
        let avg = self.avg_width();
        let mut total = 0f32;
        let mut count = 0;
        for cell in self.iter() {
            if cell.hspan() == 1 {
                let diff = cell.width().sum() as f32 - avg;
                total += diff * diff;
                count += 1;
            }
        }
        if count == 0 { 0f32 } else { (total / count as f32).sqrt() }
    }

    /// Gets whether this column is shrinkable.
    pub fn is_shrinkable(&self) -> bool {
        self.longest_cells.iter().any(|index| self.cells[*index].as_shrinkable().is_some())
    }

    /// Score high if the column would shrink in a lossy way (e.g. truncating data), otherwise 0 (e.g. text wrapping)
    pub fn lossy_shrinkage_score(&self) -> f32 {
        let is_lossy = self
            .longest_cells
            .iter()
            .any(|index| self.cells[*index].as_shrinkable().map(|s| s.is_lossy()).unwrap_or(false));
        if is_lossy { 0.8 } else { 0.0 }
    }

    pub fn shrink_text(&mut self, to_reduce: usize) -> usize {
        let mut total_shrunk_amount = 0;
        loop {
            let mapped_to_cells = self.longest_cells.clone().map(|i| self.cells[i]);
            let shrunk_amount = Self::shrink_text_inner(&mapped_to_cells, to_reduce);
            if shrunk_amount > 0 {
                total_shrunk_amount += shrunk_amount;
                self.recalc_width();
            } else {
                break total_shrunk_amount;
            }
            if total_shrunk_amount >= to_reduce {
                break total_shrunk_amount;
            }
        }
    }

    fn shrink_text_inner(longest: &BinaryTree<&dyn Cell>, mut to_reduce: usize) -> usize {
        use BinaryTree::*;
        match longest {
            Leaf(cell) => {
                if let Some(shrinkable) = cell.as_shrinkable() {
                    let target_width = distributed_min(
                        &Leaf(
                            (shrinkable.width().sum() as isize - to_reduce as isize)
                                .max(shrinkable.min_width() as isize)
                                as usize,
                        ),
                        &shrinkable.width(),
                    );
                    shrinkable.try_shrink(&target_width)
                } else {
                    0
                }
            }
            Branch(left, right, _) => {
                let mut shrunk_amount = 0;
                shrunk_amount += Self::shrink_text_inner(
                    &left.clone().map(|c| c.as_binary().map(|b| b.left()).unwrap_or(c)),
                    to_reduce,
                );
                to_reduce -= shrunk_amount;
                shrunk_amount += Self::shrink_text_inner(
                    &right.clone().map(|c| c.as_binary().map(|b| b.right()).unwrap_or(c)),
                    to_reduce,
                );
                shrunk_amount
            }
        }
    }
}

pub trait ColumnsVec {
    /// Widths need finalising for cells that span multiple columns to ensure their contents
    /// will fit.
    /// Returns `true` if the columns were resized.
    fn finalise_spanned(&mut self, cell_separator: &str, try_shrink: bool) -> SmallVec<[usize; 4]>;

    /// The total width for all columns.
    fn width(&self) -> CellWidth;

    #[allow(dead_code)]
    fn fit_to_min_width(&mut self, width: usize);

    fn expand_to_width(&mut self, width: usize);

    fn fit_to_max_width(&mut self, width: usize, cell_separator: &str);
}

impl ColumnsVec for &mut [TableColumn<'_>] {
    #[allow(clippy::needless_range_loop)]
    fn finalise_spanned(&mut self, cell_separator: &str, try_shrink: bool) -> SmallVec<[usize; 4]> {
        let mut updated_columns = smallvec![];

        let nrows = self.iter().last().map(|c| c.cells.len()).unwrap_or(0);
        // For each cell...
        for icol in 0..self.len() {
            for irow in 0..nrows {
                let cell = self[icol].cell_at(irow).unwrap();

                // If it spans multiple columns...
                if cell.hspan() > 1 {
                    // Sum the widths of the spanned columns, and...
                    let available_width: CellWidth = self[icol..icol + cell.hspan()]
                        .iter()
                        .map(|c| c.width())
                        .fold(Leaf(0), |mut acc, w| {
                            acc.push_right(w.clone(), SpaceDistribution::default());
                            acc
                        });
                    let cell_width = cell.width().sum();
                    // The actual available width includes the width of the cell separators between the spanned columns.
                    let actual_available =
                        available_width.sum() + cell_separator.chars().count() * (cell.hspan() - 1);
                    let mut diff = cell_width as isize - actual_available as isize;

                    // If the cell is wider than the summed width and we should shrink first...
                    if diff > 0
                        && try_shrink
                        && let Some(shrinkable) = cell.as_shrinkable()
                    {
                        while shrinkable.try_shrink(&distribute(&available_width, -diff)) > 0 {
                            diff = cell.width().sum() as isize - actual_available as isize;
                        }
                    }

                    if diff > 0 {
                        // Distribute the extra width across the spanned columns
                        let mut new_width = distribute(&available_width, diff);
                        // Then dissect the updated width repeatedly to get the new widths for each column.
                        for i in (icol + 1..icol + cell.hspan()).rev() {
                            self[i].update_width(irow, new_width.pop_right().unwrap());
                        }
                        self[icol].update_width(irow, new_width);
                        (icol..icol + cell.hspan()).for_each(|icol| {
                            updated_columns.push(icol);
                        })
                    }
                }
            }
        }
        updated_columns
    }

    fn width(&self) -> CellWidth {
        let mut accum = None;
        for col in self.iter() {
            match accum.as_mut() {
                None => accum = Some(col.width().clone()),
                Some(accum) => *accum = &*accum + col.width(),
            }
        }
        accum.unwrap_or_default()
    }

    fn fit_to_min_width(&mut self, min_width: usize) {
        let mut total_width = CellWidth::Leaf(0);
        for col in self.iter() {
            total_width.push_right(col.width.clone(), SpaceDistribution::default());
        }
        if total_width < min_width {
            let diff = min_width - total_width.sum();
            let mut distributed = distribute(&total_width, diff as isize);
            for col in self.iter_mut().rev() {
                col.width = distributed.pop_right().unwrap();
            }
        }
    }

    fn expand_to_width(&mut self, max_width: usize) {
        let mut width_to_use = max_width;
        self.iter()
            .filter(|c| !c.preferences.allow_expand())
            .for_each(|c| width_to_use -= c.width.sum());

        let mut total_width = CellWidth::Leaf(0);
        for col in self.iter().filter(|c| c.preferences.allow_expand()) {
            total_width.push_right(col.width.clone(), SpaceDistribution::default());
        }
        let diff = width_to_use - total_width.sum();
        let mut distributed = distribute(&total_width, diff as isize);
        for col in self.iter_mut().filter(|c| c.preferences.allow_expand()).rev() {
            col.width = distributed.pop_right().unwrap();
        }
    }

    fn fit_to_max_width(&mut self, max_width: usize, cell_separator: &str) {
        let mut shrunk;
        // Column indices may become ineligible as we proceed
        let mut ineligible_cols: SmallVec<[usize; 4]> = smallvec![];

        while self.width() > max_width {
            let self_width = self.width().sum();
            let mut reduction_needed: isize = (self_width - max_width) as isize;
            shrunk = false;

            // Decide which columns to wrap first in such a way that the number of lines printed is minimized.
            // Score each column on its (max_width - avg_width) / std_dev. A higher score indicates that the column
            // has more outlier cells that we can wrap without producing too many more lines.
            let scores = self
                .iter()
                .enumerate()
                .filter(|(i, _)| !ineligible_cols.contains(i))
                .filter(|(_, c)| c.is_shrinkable())
                .map(|(i, col)| match col.std_deviation_width() {
                    0.0 => (i, 0.0),
                    dev => {
                        let mut score = (col.width.sum() as f32 - col.avg_width()) / dev;
                        // Provide a penalty for lossy shrinking. This could be scaled for later tuning.
                        score *= 1.0 - col.lossy_shrinkage_score();
                        (i, score)
                    }
                })
                .collect::<SmallVec<[_; 16]>>();
            // Weight the scores
            let sum_scores = scores.iter().map(|(_, s)| s).sum::<f32>();
            let mut column_reduction_amounts = scores
                .into_iter()
                // Weight the scores so they sum to 1.
                .map(|(i, s)| (i, s / sum_scores))
                // Scale the weight to the reduction needed
                .map(|(i, s)| (i, (s * (reduction_needed as f32)).floor() as usize))
                // Downsize the reduction to the max possible for the column
                .map(|(i, s)| (i, s.min(self[i].width().sum() - 1)))
                .collect::<SmallVec<[_; 16]>>();
            column_reduction_amounts.sort_by(|a, b| b.1.cmp(&a.1));

            for (i, reduction) in column_reduction_amounts.iter() {
                let shrunk_amount = self[*i].shrink_text(*reduction);
                if shrunk_amount > 0 {
                    shrunk = true;
                    reduction_needed -= shrunk_amount as isize;
                }
                if reduction_needed <= 0 {
                    break;
                }
            }
            if shrunk {
                // Once columns are expanded to fit spanned contents, they become ineligible for reduction.
                ineligible_cols.append(&mut self.finalise_spanned(cell_separator, true));
            }

            // No more shrinking possible.
            if !shrunk {
                break;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_update_longest() {
        // The tree updates with width increase: 5 -> 10
        let mut tree = BinaryTree::default();
        TableColumn::update_longest(&mut tree, 1, &10.into(), Some(&5.into()));
        assert_eq!(tree.iter().next(), Some(&1));

        // The tree does not update with width decrease: 10 -> 5
        let mut tree = [1].into_iter().collect();
        TableColumn::update_longest(&mut tree, 2, &5.into(), Some(&10.into()));
        let mut iter = tree.iter();
        assert_eq!(iter.next(), Some(&1));
        assert_eq!(iter.next(), None);

        // The tree does not update with small width tree
        let mut tree = [1].into_iter().collect();
        TableColumn::update_longest(&mut tree, 2, &(5, 5).into(), Some(&10.into()));
        let mut iter = tree.iter();
        assert_eq!(iter.next(), Some(&1));
        assert_eq!(iter.next(), None);

        // The tree is binary when total width is greater than column width
        let mut tree = BinaryTree::default();
        TableColumn::update_longest(&mut tree, 3, &(3, 3).into(), Some(&5.into()));
        assert_eq!(tree.iter().next(), Some(&3));
        assert_eq!(tree.iter().next(), Some(&3));

        // The tree updates the right branch even when the cell total (7) is
        // less than the current column (10).
        let mut tree = [0, 1].into_iter().collect();
        TableColumn::update_longest(&mut tree, 2, &(1, 6).into(), Some(&(5, 5).into()));
        let mut iter = tree.iter();
        assert_eq!(iter.next(), Some(&0));
        assert_eq!(iter.next(), Some(&2));

        // The tree converts branch to leaf when large leaf width is added
        let mut tree = [0, 1].into_iter().collect();
        TableColumn::update_longest(&mut tree, 2, &11.into(), Some(&(5, 5).into()));
        let mut iter = tree.iter();
        assert_eq!(iter.next(), Some(&2));
        assert_eq!(iter.next(), None);
    }
}
