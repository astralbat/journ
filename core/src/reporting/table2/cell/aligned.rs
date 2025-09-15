/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table2::CellRef;
use crate::reporting::table2::cell::Cell;
use crate::reporting::table2::cell_width::CellWidth;
use crate::reporting::table2::fmt::{CellFormatter, Transformer};
use std::fmt;
use std::fmt::Write;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Alignment {
    Left,
    Center,
    Right,
}
pub struct AlignedCell<'c> {
    inner: CellRef<'c>,
    alignment: Alignment,
}
impl<'c> AlignedCell<'c> {
    pub fn new<C: Into<CellRef<'c>>>(inner: C, alignment: Alignment) -> Self {
        Self { inner: inner.into(), alignment }
    }
}
impl Cell for AlignedCell<'_> {
    fn print<'format>(&self, f: &mut dyn CellFormatter<'format>, line: usize) -> fmt::Result {
        struct AlignedWriter {
            alignment: Alignment,
            total_width: usize,
            padding_char: char,
        }
        impl Transformer for AlignedWriter {
            fn transform(&mut self, s: &str, writer: &mut dyn Write) -> fmt::Result {
                let content_width = s.chars().count();
                let padding = self.total_width.saturating_sub(content_width);
                let (left_pad, right_pad) = match self.alignment {
                    Alignment::Left => (0, padding),
                    Alignment::Right => (padding, 0),
                    Alignment::Center => (padding / 2, padding - (padding / 2)),
                };
                for _ in 0..left_pad {
                    writer.write_char(self.padding_char)?;
                }
                writer.write_str(s)?;
                for _ in 0..right_pad {
                    writer.write_char(self.padding_char)?;
                }
                Ok(())
            }
        }
        let total_width = f.width().map_or(0, |w| w.width());
        f.push_transformer(Box::new(AlignedWriter {
            alignment: self.alignment,
            total_width,
            padding_char: self.inner.padding_char(),
        }));

        self.inner.print(f, line)?;
        f.pop_transformer();
        Ok(())
    }

    fn width(&self) -> CellWidth {
        self.inner.width()
    }
}
