/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table2::fmt::RowFormatter;
use crate::reporting::table2::fmt::TableCellFormatter;
use crate::reporting::table2::row::Row;
use crate::reporting::table2::{CellRef, SeparatorCell};
use std::fmt;

pub struct Table<'cell> {
    rows: Vec<Row<'cell>>,
    color: bool,
    column_preferences: Vec<ColumnPreferences>,
}

impl<'cell> Table<'cell> {
    pub fn color(&self) -> bool {
        self.color
    }

    pub fn set_color(&mut self, color: bool) {
        self.color = color;
    }

    pub fn set_heading_row<C: Into<CellRef<'cell>>>(
        &mut self,
        headings: impl IntoIterator<Item = C>,
    ) {
        let mut row: Row<'cell> = Row::default();
        for heading in headings {
            row.append(heading.into());
        }
        row.set_header(true);
        self.push_row(row);
    }

    pub fn push_separator_row(&mut self, separator: char, span: usize) {
        self.push_row(Row::new([Box::new(SeparatorCell::new(separator, span))]));
    }

    pub fn rows(&self) -> &[Row<'cell>] {
        &self.rows
    }

    pub fn push_row<R>(&mut self, row: R)
    where
        R: Into<Row<'cell>>,
    {
        self.rows.push(row.into());
    }

    pub fn sort_rows_by<F>(&mut self, mut compare: F)
    where
        F: FnMut(&Row<'cell>, &Row<'cell>) -> std::cmp::Ordering,
    {
        self.rows.sort_by(|a, b| compare(a, b));
    }

    pub fn expand_column(&mut self, index: usize) {
        if index >= self.column_preferences.len() {
            self.column_preferences.resize_with(index + 1, Default::default);
        }
        self.column_preferences[index].set_allow_expand(true);
    }

    pub fn print<W: fmt::Write>(&self, writer: &mut W) -> fmt::Result {
        let mut formatter = TableCellFormatter::new(writer);
        formatter.set_color(self.color);
        formatter.set_striped();
        formatter.set_max_width(term_size::dimensions().map(|(w, _)| w));
        formatter.print(&self.rows, &self.column_preferences)
    }
}

impl Default for Table<'_> {
    fn default() -> Self {
        Self { rows: vec![], color: atty::is(atty::Stream::Stdout), column_preferences: vec![] }
    }
}

#[derive(Default, Clone, Copy)]
pub struct ColumnPreferences {
    allow_expand: bool,
}
impl ColumnPreferences {
    pub fn allow_expand(&self) -> bool {
        self.allow_expand
    }

    pub fn set_allow_expand(&mut self, allow: bool) {
        self.allow_expand = allow;
    }
}
