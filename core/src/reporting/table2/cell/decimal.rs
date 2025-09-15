/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table2::Cell;
use crate::reporting::table2::cell_width::CellWidth;
use crate::reporting::table2::fmt::CellFormatter;
use rust_decimal::Decimal;
use std::fmt;

impl Cell for Decimal {
    fn print<'format>(&self, f: &mut dyn CellFormatter<'format>, line: usize) -> fmt::Result {
        let s = self.to_string();
        s.print(f, line)
    }

    fn width(&self) -> CellWidth {
        let s = self.to_string();
        s.width()
    }
}
