/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table2::table::ColumnPreferences;
use crate::reporting::table2::{Cell, CellWidth};
use rust_decimal::Decimal;
use rust_decimal::prelude::{FromPrimitive, Zero};
use smallvec::SmallVec;

#[derive(Default)]
pub struct TableColumn<'t> {
    /// The cells in this column.
    cells: Vec<&'t dyn Cell>,
    /// The width of the column in characters, excluding borders.
    width: CellWidth,
    index: usize,
    preferences: ColumnPreferences,
}

impl<'t> TableColumn<'t> {
    pub fn new(index: usize, preferences: ColumnPreferences) -> Self {
        Self { cells: Vec::new(), width: CellWidth::default(), index, preferences }
    }

    pub fn index(&self) -> usize {
        self.index
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
            self.width = self.width.distributed_max(&cell.width());
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
                total += cell.width().width() as f32;
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
                let diff = cell.width().width() as f32 - avg;
                total += diff * diff;
                count += 1;
            }
        }
        if count == 0 { 0f32 } else { (total / count as f32).sqrt() }
    }
}

pub trait ColumnsVec {
    /// Widths need finalising for cells that span multiple columns to ensure their contents
    /// will fit.
    fn finalise_spanned(&mut self);

    /// The total width for all columns.
    fn width(&self) -> CellWidth;

    fn shrink_text(&mut self) -> bool;

    fn fit_to_min_width(&mut self, width: usize);

    fn expand_to_width(&mut self, width: usize);

    fn fit_to_max_width(&mut self, width: usize);
}

impl ColumnsVec for &mut [TableColumn<'_>] {
    #[allow(clippy::needless_range_loop)]
    fn finalise_spanned(&mut self) {
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
                        .fold(CellWidth::Unary(0), |mut acc, w| {
                            acc.push_right(w.clone());
                            acc
                        });
                    let diff = cell.width().width() as isize - available_width.width() as isize;

                    // If the cell is wider than the summed width...
                    if diff > 0 {
                        // Distribute the extra width across the spanned columns
                        let mut new_width = available_width.distribute(diff as usize);
                        // Then dissect the updated width repeatedly to get the new widths for each column.
                        for i in icol + cell.hspan() - 1..icol {
                            self[i].width = new_width.pop_right().unwrap();
                        }
                        self[icol].width = new_width;
                    }
                }
            }
        }
    }

    fn width(&self) -> CellWidth {
        self.iter().map(|c| c.width()).sum()
    }

    /// Shrinks text for the first column only. The columns width is updated if any shrinking occurs.
    /// A call to [Self::finalise_spanned] should follow this to ensure spanned cells are still valid.
    fn shrink_text(&mut self) -> bool {
        let mut shrunk_any = false;

        // The idea here is to focus on the first column, whilst having access to other columns its cells may span.
        // Then recurse for remaining columns.
        match self.first() {
            Some(col) => {
                col.iter().for_each(|cell| {
                    let max_width = self.iter().take(cell.hspan()).map(|c| c.width().width()).sum();
                    // |= will not short-circuit, so all cells get a chance to wrap.
                    shrunk_any |= cell
                        .as_shrinkable()
                        .map(|shrinkable| shrinkable.try_shrink(max_width))
                        .unwrap_or(false);
                });
                if shrunk_any {
                    self.first_mut().unwrap().width = col
                        .iter()
                        .filter(|c| c.hspan() == 1)
                        .map(|c| c.width())
                        .max()
                        .unwrap_or(CellWidth::default());
                }
                shrunk_any
            }
            None => false,
        }
    }

    fn fit_to_min_width(&mut self, min_width: usize) {
        let mut total_width = CellWidth::Unary(0);
        for col in self.iter() {
            total_width.push_right(col.width.clone());
        }
        if total_width < min_width {
            let diff = min_width - total_width.width();
            let mut distributed = total_width.distribute(diff);
            for col in self.iter_mut().rev() {
                col.width = distributed.pop_right().unwrap();
            }
        }
    }

    fn expand_to_width(&mut self, max_width: usize) {
        let mut width_to_use = max_width;
        self.iter()
            .filter(|c| !c.preferences.allow_expand())
            .for_each(|c| width_to_use -= c.width.width());

        let mut total_width = CellWidth::Unary(0);
        for col in self.iter().filter(|c| c.preferences.allow_expand()) {
            total_width.push_right(col.width.clone());
        }
        let diff = width_to_use - total_width.width();
        let mut distributed = total_width.distribute(diff);
        for col in self.iter_mut().filter(|c| c.preferences.allow_expand()).rev() {
            col.width = distributed.pop_right().unwrap();
        }
    }

    fn fit_to_max_width(&mut self, width: usize) {
        let mut wrapped;
        while self.width() > width {
            wrapped = false;

            // Decide which columns to wrap first in such a way that the number of lines printed is minimized.
            // Score each column on its (max_width - avg_width) / std_dev. A higher score indicates that the column
            // has more outliers that we can wrap without producing too many more lines.
            let mut max_to_avg_scores = self
                .iter()
                .enumerate()
                .map(|(i, col)| match col.std_deviation_width() {
                    0f32 => (i, Decimal::zero()),
                    dev => (
                        i,
                        (Decimal::from(col.width.width())
                            - Decimal::from_f32(col.avg_width()).unwrap())
                            / Decimal::from_f32(dev).unwrap(),
                    ),
                })
                .collect::<SmallVec<[_; 16]>>();
            max_to_avg_scores.sort_by_key(|(_i, s)| *s);

            for (i, _s) in max_to_avg_scores.iter().rev() {
                let mut child = &mut self[*i..];
                if child.shrink_text() {
                    wrapped = true;
                    break;
                }
            }
            if wrapped {
                self.finalise_spanned();
            }

            // No more wrapping possible.
            if !wrapped {
                break;
            }
        }
    }
}
