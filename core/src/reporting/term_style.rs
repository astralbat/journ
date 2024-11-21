/*
 * Copyright (c) 2023-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::arguments::Arguments;
use std::cell::RefCell;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Colour {
    Red = 31,
    Green = 32,
    Yellow = 33,
    Blue = 34,
    Magenta = 35,
    Cyan = 36,
    Black = 30,
    White = 37,
    BrightRed = 91,
    BrightGreen = 92,
    BrightYellow = 93,
    BrightBlue = 94,
    BrightMagenta = 95,
    BrightCyan = 96,
    BrightBlack = 90,
    BrightWhite = 97,
    /// RGB colour is 24-bit True Colour. Requires terminal support.
    RGB(u8, u8, u8) = 38,
}

impl From<Colour> for u8 {
    fn from(colour: Colour) -> u8 {
        // `as u8` coercion does not work when an enum variant has fields.
        match colour {
            Colour::Red => 31,
            Colour::Green => 32,
            Colour::Yellow => 33,
            Colour::Blue => 34,
            Colour::Magenta => 35,
            Colour::Cyan => 36,
            Colour::Black => 30,
            Colour::White => 37,
            Colour::BrightRed => 91,
            Colour::BrightGreen => 92,
            Colour::BrightYellow => 93,
            Colour::BrightBlue => 94,
            Colour::BrightMagenta => 95,
            Colour::BrightCyan => 96,
            Colour::BrightBlack => 90,
            Colour::BrightWhite => 97,
            Colour::RGB(_, _, _) => 38,
        }
    }
}

thread_local! {
    /// Memory of the previous style written to the terminal, so
    /// that the next style can be compared and only written if different.
    static CURR_STYLE: RefCell<Option<Style>> = RefCell::new(None);
}

#[derive(Default, Copy, Clone, PartialEq, Eq)]
pub struct Style {
    fg: Option<Colour>,
    bold: Option<bool>,
    faint: Option<bool>,
}

impl !Send for Style {}

impl Style {
    const ESC: &'static str = "\x1b";

    pub fn with_fg(mut self, fg: Colour) -> Self {
        self.fg = Some(fg);
        self
    }

    pub fn reset_fg(mut self) -> Self {
        self.fg = None;
        self
    }

    pub fn with_bold(mut self, bold: bool) -> Self {
        self.bold = Some(bold);
        self
    }

    pub fn with_faint(mut self, faint: bool) -> Self {
        self.faint = Some(faint);
        self
    }

    pub(crate) fn fmt_start<W: fmt::Write>(&self, w: &mut W) -> fmt::Result {
        // Clear all attributes.
        write!(w, "{}[0m", Style::ESC)?;

        if self.bold.unwrap_or(false) {
            write!(w, "{}[1m", Style::ESC)?;
        }
        if self.faint.unwrap_or(false) {
            write!(w, "{}[2m", Style::ESC)?;
        }
        if let Some(fg) = self.fg {
            match fg {
                Colour::RGB(red, green, blue) => {
                    write!(w, "{}[38;2;{};{};{}m", Style::ESC, red, green, blue)?
                }
                other_fg => write!(w, "{}[{}m", Style::ESC, u8::from(other_fg))?,
            }
        }

        Ok(())
    }

    /// Resets style information on the terminal. This is only necessary to invoke
    /// after writing is complete, though it must happen before the termination of the program.
    pub(crate) fn fmt_end<W: fmt::Write>(w: &mut W) -> fmt::Result {
        if Arguments::get().print_std_out_in_color() {
            write!(w, "{}[0m", Style::ESC)?;
        }
        CURR_STYLE.with(|curr_style| {
            *curr_style.borrow_mut() = None;
        });
        Ok(())
    }

    pub fn fmt<D: fmt::Display, W: fmt::Write>(&self, w: &mut W, item: D) -> fmt::Result {
        CURR_STYLE.with(|curr_style| {
            let mut curr = curr_style.borrow_mut();
            if *curr != Some(*self) {
                if Arguments::get().print_std_out_in_color() {
                    self.fmt_start(w)?;
                }
                *curr = Some(*self);
            }
            Ok(())
        })?;
        write!(w, "{}", item)?;
        Ok(())
    }

    pub fn paint<D: fmt::Display>(&self, item: D) -> StyledItem<D> {
        StyledItem { style: *self, item }
    }

    /// Lay `other` on top of this one, with `other` taking precedence over `self`.
    pub fn overlay(&self, other: Style) -> Style {
        Style {
            fg: other.fg.or(self.fg),
            bold: other.bold.or(self.bold),
            faint: other.faint.or(self.faint),
        }
    }
}

pub struct StyledItem<D> {
    style: Style,
    item: D,
}

impl<D: fmt::Display> fmt::Display for StyledItem<D> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if Arguments::get().print_std_out_in_color() {
            self.style.fmt_start(f)?;
            write!(f, "{}", self.item)?;
            Style::fmt_end(f)
        } else {
            write!(f, "{}", self.item)
        }
    }
}
