/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table2::fmt::CellFormatter;
use crate::reporting::table2::{Cell, CellWidth};
use smartstring::{SmartString, SmartStringMode};
use std::fmt;

impl Cell for str {
    fn print<'format>(&self, f: &mut dyn CellFormatter<'format>, line: usize) -> fmt::Result {
        self.lines().nth(line).map_or(Err(fmt::Error), |line| write!(f, "{line}"))
    }

    fn width(&self) -> CellWidth {
        self.lines()
            .max_by(|a, b| a.chars().count().cmp(&b.chars().count()))
            .map_or_default(|l| CellWidth::Unary(l.chars().count()))
    }
}

impl Cell for &str {
    fn print<'format>(&self, f: &mut dyn CellFormatter<'format>, line: usize) -> fmt::Result {
        self.lines().nth(line).map_or(Err(fmt::Error), |line| write!(f, "{line}"))
    }

    fn width(&self) -> CellWidth {
        self.lines()
            .max_by(|a, b| a.chars().count().cmp(&b.chars().count()))
            .map_or_default(|l| CellWidth::Unary(l.chars().count()))
    }
}

impl Cell for String {
    fn print<'format>(&self, f: &mut dyn CellFormatter<'format>, line: usize) -> fmt::Result {
        self.as_str().print(f, line)
    }

    fn width(&self) -> CellWidth {
        self.as_str().width()
    }
}

impl<Mode: SmartStringMode> Cell for SmartString<Mode> {
    fn print<'format>(&self, f: &mut dyn CellFormatter<'format>, line: usize) -> fmt::Result {
        self.as_str().print(f, line)
    }

    fn width(&self) -> CellWidth {
        self.as_str().width()
    }
}
