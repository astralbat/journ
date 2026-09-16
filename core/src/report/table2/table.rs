/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::report::command::table_format_args::TableFormatCommand;
use crate::report::table2::fmt::RowFormatter;
use crate::report::table2::fmt::TableCellFormatter;
use crate::report::table2::row::{Row, RowKind};
use crate::report::table2::{CellRef, Rows, SeparatorCell, SpannedCell, StyledCell};
use crate::report::term_style::{Style, Weight};
use std::fmt;

pub struct Table<'cell> {
    rows: Vec<Row<'cell>>,
    stripe: bool,
    title_separator: char,
    chain_separator: char,
    total_separator: char,
    grand_total_separator: char,
    column_preferences: Vec<ColumnPreferences>,
}

impl<'cell> Table<'cell> {
    pub fn set_striped(&mut self, striped: bool) {
        self.stripe = striped;
    }

    pub fn create_heading_row<C: Into<CellRef<'cell>>>(
        &self,
        headings: impl IntoIterator<Item = C>,
    ) -> Row<'cell> {
        let mut row: Row<'cell> = Row::default();
        for heading in headings {
            row.append(heading.into());
        }
        row.set_kind(RowKind::Heading);
        row
    }

    pub fn create_chain_separator(&self) -> Row<'cell> {
        self.create_separator_row(
            RowKind::ChainSeparator,
            self.chain_separator,
            self.rows.column_count(),
        )
    }

    pub fn create_total_separator(&self) -> Row<'cell> {
        self.create_separator_row(
            RowKind::TotalSeparator,
            self.total_separator,
            self.rows.column_count(),
        )
    }

    pub fn create_grand_total_separator(&self) -> Row<'cell> {
        self.create_separator_row(
            RowKind::GrandTotalSeparator,
            self.grand_total_separator,
            self.rows.column_count(),
        )
    }

    pub fn create_title_row<C: Into<CellRef<'cell>>>(
        &self,
        title: C,
        column_count: usize,
    ) -> (Row<'cell>, Row<'cell>) {
        let mut row: Row<'cell> = Row::default();
        row.set_kind(RowKind::Title);
        let style = Style::default().with_weight(Weight::Bold);
        let spanned = SpannedCell::new(StyledCell::new(title, style), column_count);
        row.append(spanned);
        (
            row,
            self.create_separator_row(RowKind::TitleSeparator, self.title_separator, column_count),
        )
    }

    pub fn set_title_separator(&mut self, separator: char) {
        self.title_separator = separator;
    }

    pub fn set_chain_separator(&mut self, separator: char) {
        self.chain_separator = separator;
    }

    pub fn total_separator(&self) -> char {
        self.total_separator
    }

    pub fn set_total_separator(&mut self, separator: char) {
        self.total_separator = separator;
    }

    pub fn set_grand_total_separator(&mut self, separator: char) {
        self.grand_total_separator = separator;
    }

    pub fn create_separator_row(
        &self,
        row_type: RowKind,
        separator: char,
        span: usize,
    ) -> Row<'cell> {
        let mut row = Row::new([Box::new(SeparatorCell::new(separator, span.max(1)))]);
        row.set_kind(row_type);
        row
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
        if self.rows.iter().map(Row::kind).all(|k| k == RowKind::Heading) {
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
        Self {
            rows: vec![],
            title_separator: '-',
            chain_separator: '-',
            total_separator: '-',
            grand_total_separator: '=',
            column_preferences: vec![],
            stripe: true,
        }
    }
}

impl From<&TableFormatCommand> for Table<'_> {
    fn from(cmd: &TableFormatCommand) -> Self {
        let mut table = Table::default();
        table.set_title_separator(cmd.title_separator());
        table.set_chain_separator(cmd.chain_separator());
        table.set_total_separator(cmd.total_separator());
        table.set_grand_total_separator(cmd.grand_total_separator());
        table
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
