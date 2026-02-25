/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::report::table2::fmt::RowFormatter;
use crate::report::table2::fmt::TableCellFormatter;
use crate::report::table2::row::Row;
use crate::report::table2::{CellRef, SeparatorCell, SpannedCell, StyledCell};
use crate::report::term_style::{Style, Weight};
use std::fmt;

pub struct Table<'cell> {
    rows: Vec<Row<'cell>>,
    stripe: bool,
    column_preferences: Vec<ColumnPreferences>,
}

impl<'cell> Table<'cell> {
    pub fn set_striped(&mut self, striped: bool) {
        self.stripe = striped;
    }

    pub fn append_heading_row<C: Into<CellRef<'cell>>>(
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

    pub fn append_chain_separator(&mut self) {
        self.push_separator_row('-', self.column_count())
    }

    pub fn append_title_row<C: Into<CellRef<'cell>>>(&mut self, title: C, column_count: usize) {
        let mut row: Row<'cell> = Row::default();
        let style = Style::default().with_weight(Weight::Bold);
        let spanned = SpannedCell::new(StyledCell::new(title, style), column_count);
        row.append(spanned);
        self.push_row(row);
        self.push_separator_row('-', column_count);
    }

    pub fn push_separator_row(&mut self, separator: char, span: usize) {
        self.push_row(Row::new([Box::new(SeparatorCell::new(separator, span.max(1)))]));
    }

    pub fn column_count(&self) -> usize {
        self.rows.get(0).map(|r| r.column_count()).unwrap_or(0)
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

    pub fn indent_column(&mut self, index: usize, indent: usize) {
        if index >= self.column_preferences.len() {
            self.column_preferences.resize_with(index + 1, Default::default);
        }
        self.column_preferences[index].set_indent(indent);
    }

    pub fn print<W: fmt::Write>(&self, writer: &mut W) -> fmt::Result {
        if self.rows.iter().all(Row::is_header) {
            return Ok(());
        }
        let mut formatter = TableCellFormatter::new(writer);
        if self.stripe {
            formatter.set_striped();
        }
        formatter.set_max_width(term_size::dimensions().map(|(w, _)| w));
        formatter.print(&self.rows, &self.column_preferences)
    }
}

impl Default for Table<'_> {
    fn default() -> Self {
        Self { rows: vec![], column_preferences: vec![], stripe: true }
    }
}

impl fmt::Display for Table<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.print(f)
    }
}

#[derive(Default, Clone, Copy)]
pub struct ColumnPreferences {
    allow_expand: bool,
    indent: usize,
}
impl ColumnPreferences {
    pub fn allow_expand(&self) -> bool {
        self.allow_expand
    }

    pub fn set_allow_expand(&mut self, allow: bool) {
        self.allow_expand = allow;
    }

    pub fn indent(&self) -> usize {
        self.indent
    }

    pub fn set_indent(&mut self, indent: usize) {
        self.indent = indent;
    }
}
