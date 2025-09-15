/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table2::Cell;
use crate::reporting::table2::cell_width::CellWidth;
use crate::reporting::table2::column::{ColumnsVec, TableColumn};
use crate::reporting::table2::fmt::cell_formatter::{CellFormatter, Transformer, lines_and_cols};
use crate::reporting::table2::row::Row;
use crate::reporting::term_style::{Colour, Style};
use palette::{Hsl, IntoColor, Srgb};
use std::fmt;
use std::fmt::Write;
use supports_color::Stream;
use terminal_colorsaurus::{QueryOptions, ThemeMode, color_palette};

/// A table formatter for cells
pub struct TableCellFormatter<'w, 'format> {
    writer: &'w mut dyn Write,
    max_width: Option<usize>,
    /// The number of columns this cell is expected to span
    width: CellWidth,
    /// The current line index in the table, starting at 0.
    cursor_line: usize,
    /// The current column index in the table, starting at 0.
    cursor_col: usize,
    /// A stack of transformers set at each cell level. The last one is applied first. In this way, a pipeline of transformations can be built up.
    transformers: Vec<Box<dyn Transformer + 'format>>,
    transformer_output: String,
    cell_separator: &'w str,
    border: TableBorder<'w>,
    color: bool,
    odd_bg: Option<Style>,
    even_bg: Option<Style>,
}

impl<'w, 'format> TableCellFormatter<'w, 'format> {
    pub fn new(writer: &'w mut dyn Write) -> Self {
        Self {
            writer,
            max_width: None,
            width: CellWidth::default(),
            cursor_line: 0,
            cursor_col: 0,
            transformers: Vec::new(),
            transformer_output: String::new(),
            cell_separator: "  ",
            border: TableBorder::default(),
            color: false,
            odd_bg: None,
            even_bg: None,
        }
    }

    pub fn writer_mut(&mut self) -> &mut dyn Write {
        self.writer
    }

    pub fn set_max_width(&mut self, max_width: Option<usize>) {
        self.max_width = max_width;
    }

    /// The current line in the table, starting at 0. Note that this is not the same as rows, as
    /// a cell within a particular row may have content that spans multiple lines.
    pub fn line(&self) -> usize {
        self.cursor_line
    }

    /// The current cursor position in the table, starting at 0.
    pub fn col(&self) -> usize {
        self.cursor_col
    }

    pub fn set_cell_separator(&mut self, sep: &'w str) {
        self.cell_separator = sep;
    }

    pub fn set_border(&mut self, border: TableBorder<'w>) {
        self.border = border;
    }

    pub fn set_color(&mut self, color: bool) {
        self.color = color;
    }

    /// Sets whether rows are striped; that they have alternating background colors for distinguishing them.
    /// This tries to use a subtle change in background color or nothing at all. Reverse video is possible,
    /// but doesn't look good.
    ///
    /// This only works striped if colors are enabled and the terminal supports 16 million colors
    /// There are no golden rules on what terminals support. This implementation relies on the supports-color crate
    /// which pays most attention to COLORTERM environment variable.
    pub fn set_striped(&mut self) {
        if let Some(support) = supports_color::on(Stream::Stdout)
            && support.has_16m
        {
            Self::set_striped_16m(self)
        }
    }

    fn set_striped_16m(&mut self) {
        if let Ok(color_palette) = color_palette(QueryOptions::default()) {
            let scaled_bg = color_palette.background.scale_to_8bit();
            self.even_bg = None;
            const STRIPE_FACTOR: f32 = 0.07;
            match color_palette.theme_mode() {
                ThemeMode::Light => {
                    let (r, g, b) = (
                        (scaled_bg.0 as f32 * (1.0 - STRIPE_FACTOR)).round() as u8,
                        (scaled_bg.1 as f32 * (1.0 - STRIPE_FACTOR)).round() as u8,
                        (scaled_bg.2 as f32 * (1.0 - STRIPE_FACTOR)).round() as u8,
                    );
                    self.odd_bg = Some(Style::default().with_bg(Colour::RGB(r, g, b)));
                }
                ThemeMode::Dark => {
                    const MIN_COMPONENT: u8 = 30; // Avoid pure black which can be hard to read on some terminals
                    let (r, g, b) = (
                        (scaled_bg.0.max(MIN_COMPONENT) as f32 * (1.0 + STRIPE_FACTOR))
                            .round()
                            .min(255.0) as u8,
                        (scaled_bg.1.max(MIN_COMPONENT) as f32 * (1.0 + STRIPE_FACTOR))
                            .round()
                            .min(255.0) as u8,
                        (scaled_bg.2.max(MIN_COMPONENT) as f32 * (1.0 + STRIPE_FACTOR))
                            .round()
                            .min(255.0) as u8,
                    );
                    self.odd_bg = Some(Style::default().with_bg(Colour::RGB(r, g, b)));
                }
            }
        }
    }

    fn adjust_lightness(r: u8, g: u8, b: u8, lightness_delta: f32) -> (u8, u8, u8) {
        // Convert RGB to HSL
        let rbg = Srgb::new(r as f32 / 255.0, g as f32 / 255.0, b as f32 / 255.0);
        let mut hsl: Hsl = rbg.into_color();
        // Adjust lightness
        hsl.lightness = (hsl.lightness + lightness_delta).clamp(0.0, 1.0);
        // Convert back to RGB
        let rgb: Srgb = hsl.into_color();
        (
            (rgb.red * 255.0).round() as u8,
            (rgb.green * 255.0).round() as u8,
            (rgb.blue * 255.0).round() as u8,
        )
    }

    fn calculate_luminance(r: u8, g: u8, b: u8) -> f32 {
        // Convert RGB to linear RGB
        let r = r as f32 / 255.0;
        let g = g as f32 / 255.0;
        let b = b as f32 / 255.0;

        let r = if r <= 0.03928 { r / 12.92 } else { ((r + 0.055) / 1.055).powf(2.4) };
        let g = if g <= 0.03928 { g / 12.92 } else { ((g + 0.055) / 1.055).powf(2.4) };
        let b = if b <= 0.03928 { b / 12.92 } else { ((b + 0.055) / 1.055).powf(2.4) };

        // Calculate luminance
        0.2126 * r + 0.7152 * g + 0.0722 * b
    }
}

impl<'w, 'format, 'cell> CellFormatter<'format> for TableCellFormatter<'w, 'format> {
    fn format_cell(
        &mut self,
        cell: &dyn Cell,
        row: &Row,
        line: usize,
        cols: &[TableColumn],
    ) -> fmt::Result {
        self.transformers.clear();

        // Set the width.
        let sep_len = self.cell_separator.chars().count();
        let mut width: CellWidth = cols.iter().map(|col| col.width()).sum();
        width = width.distribute(sep_len * (cols.len() - 1));

        self.set_width(Some(width.clone()));

        // Not the first column, so add a separator
        if cols[0].index() > 0 {
            let sep = self.cell_separator;
            if row.is_header() {
                let width = sep.chars().count();
                write!(self, "{:width$}", "")
            } else {
                write!(self, "{sep}")
            }?;
        }

        let start_col = self.cursor_col;

        // Visit the cell to print its content
        let print_res = cell.print(self, line);

        // Pad to the column width
        for _ in self.cursor_col..(start_col + width.width()) {
            write!(self, "{}", cell.padding_char())?;
        }

        print_res
    }

    fn format_line_start(&mut self, row: &Row, row_num: usize, _line: usize) -> fmt::Result {
        if self.color {
            if row_num.is_multiple_of(2)
                && let Some(even_bg) = self.even_bg
            {
                even_bg.start(self)?;
            } else if !row_num.is_multiple_of(2)
                && let Some(odd_bg) = self.odd_bg
            {
                odd_bg.start(self)?;
            }
        }

        if let Some(left_border) = self.border.left {
            if row.is_header() {
                let width = left_border.chars().count();
                write!(self, "{:width$}", "")
            } else {
                write!(self, "{left_border}")
            }
        } else {
            Ok(())
        }
    }

    fn format_line_end(&mut self, row: &Row, row_num: usize, _line: usize) -> fmt::Result {
        // Reset bg at the end of every line. Some terminals do inconsistent rendering otherwise.
        if self.color {
            if row_num.is_multiple_of(2)
                && let Some(even_bg) = self.even_bg
            {
                even_bg.end(self)?;
            } else if !row_num.is_multiple_of(2)
                && let Some(odd_bg) = self.odd_bg
            {
                odd_bg.end(self)?;
            }
        }

        if let Some(right_border) = self.border.right {
            if row.is_header() {
                let width = right_border.chars().count();
                write!(self, "{:width$}", "", width = width)?;
            } else {
                write!(self, "{right_border}")?;
            }
        }
        self.cursor_line += 1;
        self.cursor_col = 0;
        writeln!(self.writer)
    }

    fn prepare_columns(&mut self, columns: &mut Vec<TableColumn>) {
        if let Some(max_width) = self.max_width {
            let adj_max_width =
                max_width.saturating_sub(self.cell_separator.chars().count() * (columns.len() - 1));

            columns.as_mut_slice().fit_to_max_width(adj_max_width);

            if columns.as_mut_slice().width() < adj_max_width {
                columns.as_mut_slice().expand_to_width(adj_max_width);
            }
        }
    }

    fn width(&self) -> Option<&CellWidth> {
        Some(&self.width)
    }

    fn set_width(&mut self, width: Option<CellWidth>) {
        assert!(width.is_some(), "TableCellFormatter requires a width to be set");

        self.width = width.unwrap();
    }

    fn color(&self) -> bool {
        self.color
    }

    fn push_transformer(&mut self, f: Box<dyn Transformer + 'format>) {
        self.transformers.push(f);
    }

    fn pop_transformer(&mut self) -> Option<Box<dyn Transformer + 'format>> {
        self.transformers.pop()
    }

    fn cursor_col(&self) -> usize {
        self.cursor_col
    }
}

impl<'w, 't> Write for TableCellFormatter<'w, 't> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        crate::reporting::table2::fmt::cell_formatter::apply_transformers(
            s,
            &mut self.transformers,
            &mut self.transformer_output,
        );
        let s = &self.transformer_output;

        if s.is_empty() {
            return Ok(());
        }

        write!(self.writer, "{s}").map_err(|_| fmt::Error)?;

        // Keep track of self.line and self.col
        let (lines, cols) = lines_and_cols(s);
        self.cursor_line += lines;
        if lines == 0 {
            self.cursor_col += cols;
        } else {
            self.cursor_col = cols;
        }
        Ok(())
    }
}

#[derive(Default)]
pub struct TableBorder<'s> {
    pub top: Option<&'s str>,
    pub bottom: Option<&'s str>,
    pub left: Option<&'s str>,
    pub right: Option<&'s str>,
}
impl<'s> TableBorder<'s> {
    pub fn new(
        top: Option<&'s str>,
        bottom: Option<&'s str>,
        left: Option<&'s str>,
        right: Option<&'s str>,
    ) -> Self {
        Self { top, bottom, left, right }
    }
}
