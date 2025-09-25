/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table2::cell_width::CellWidth;
use crate::reporting::table2::fmt::CellFormatter;
use crate::reporting::table2::{Cell, ColumnWidth};
use std::fmt;

pub struct SeparatorCell {
    padding_char: char,
    span: usize,
}
impl SeparatorCell {
    pub fn new(padding_char: char, span: usize) -> Self {
        Self { padding_char, span }
    }
}
impl Cell for SeparatorCell {
    fn print<'format>(
        &self,
        _f: &mut dyn CellFormatter,
        _line: usize,
        _width: Option<ColumnWidth>,
    ) -> fmt::Result {
        Err(fmt::Error)
    }

    fn width(&self) -> CellWidth {
        CellWidth::Unary(0)
    }

    fn height(&self) -> usize {
        1
    }

    fn hspan(&self) -> usize {
        self.span
    }

    fn padding_char(&self) -> char {
        self.padding_char
    }
}
