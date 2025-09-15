/*
 * Copyright (c) 2023-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::arguments::Arguments;
use std::cell::RefCell;
use std::fmt::Write;
use std::io::Read as IoRead;
use std::io::Write as IoWrite;
use std::sync::{LazyLock, Mutex};
use std::time::{Duration, Instant};
use std::{fmt, io};

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

static DEBUG_MODE: bool = false;
static STYLE_STACK: Mutex<Vec<Style>> = Mutex::new(Vec::new());

thread_local! {
    /// Memory of the previous style written to the terminal, so
    /// that the next style can be compared and only written if different.
    static CURR_STYLE: RefCell<Option<Style>> = const { RefCell::new(None) };
    /// Buffer for constructing escape sequences. The sequences are sent to the writer
    /// in a complete fashion so that the writer may recognize them as a single unit.
    static STYLE_BUFFER: RefCell<String> = RefCell::new(String::with_capacity(32));
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Weight {
    Bold,
    Faint,
}

#[derive(Default, Copy, Clone, PartialEq, Eq)]
pub struct Style {
    fg: Option<Colour>,
    bg: Option<Colour>,
    weight: Option<Weight>,
    reverse_video: Option<bool>,
}

//impl !Send for Style {}

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

    pub fn with_bg(mut self, bg: Colour) -> Self {
        self.bg = Some(bg);
        self
    }

    pub fn reset_bg(mut self) -> Self {
        self.bg = None;
        self
    }

    pub fn with_weight(mut self, weight: Weight) -> Self {
        self.weight = Some(weight);
        self
    }

    pub fn with_reverse_video(mut self, reverse: bool) -> Self {
        self.reverse_video = Some(reverse);
        self
    }

    pub(crate) fn start<W: fmt::Write + ?Sized>(&self, w: &mut W) -> fmt::Result {
        let mut ss = STYLE_STACK.lock().unwrap();
        let current = Style::current(&mut ss);
        ss.push(*self);
        if let Some(fg) = self.fg
            && current.and_then(|c| c.fg) != Some(fg)
        {
            Self::write_fg(w, fg)?;
        }
        if let Some(bg) = self.bg
            && current.and_then(|c| c.bg) != Some(bg)
        {
            Self::write_bg(w, bg)?;
        }
        if let Some(weight) = self.weight
            && current.and_then(|c| c.weight) != Some(weight)
        {
            Self::write_weight(w, weight)?;
        }
        if let Some(reverse_video) = self.reverse_video
            && current.and_then(|c| c.reverse_video) != Some(reverse_video)
        {
            Self::write_reverse_video(w, reverse_video)?;
        }

        Ok(())
    }

    /// Resets style information on the terminal. This is only necessary to invoke
    /// after writing is complete, though it must happen before the termination of the program.
    pub(crate) fn end<W: Write + ?Sized>(&self, w: &mut W) -> fmt::Result {
        let mut ss = STYLE_STACK.lock().unwrap();
        if let Some(prev) = ss.pop() {
            let current = Style::current(&mut ss);
            match current {
                None => {
                    if DEBUG_MODE {
                        write!(w, "<ESC>[0m")?;
                    }
                    write!(w, "\x1b[0m")
                }
                Some(restore) => {
                    match restore.fg {
                        Some(restore_fg) if prev.fg.is_some() && restore_fg != prev.fg.unwrap() => {
                            Self::write_fg(w, restore_fg)?;
                        }
                        None if prev.fg.is_some() => {
                            if DEBUG_MODE {
                                write!(w, "<ESC>[39m")?;
                            }
                            write!(w, "\x1b[39m")?
                        }
                        _ => {}
                    }
                    match restore.bg {
                        Some(restore_bg) if prev.bg.is_some() && restore_bg != prev.bg.unwrap() => {
                            Self::write_bg(w, restore_bg)?;
                        }
                        None if prev.bg.is_some() => {
                            if DEBUG_MODE {
                                write!(w, "<ESC>[49m")?;
                            }
                            write!(w, "\x1b[49m")?;
                        }
                        _ => {}
                    }
                    match restore.weight {
                        Some(restore_weight)
                            if prev.weight.is_some() && restore_weight != prev.weight.unwrap() =>
                        {
                            Self::write_weight(w, restore_weight)?;
                        }
                        None if prev.weight.is_some() => {
                            if DEBUG_MODE {
                                write!(w, "<ESC>[22m")?;
                            }
                            write!(w, "\x1b[22m")?;
                        }
                        _ => {}
                    }

                    match restore.reverse_video {
                        Some(restore_reverse)
                            if prev.reverse_video.is_some()
                                && restore_reverse != prev.reverse_video.unwrap() =>
                        {
                            Self::write_reverse_video(w, restore_reverse)?;
                        }
                        None if prev.reverse_video.is_some() => {
                            Self::write_reverse_video(w, false)?;
                        }
                        _ => {}
                    }
                    Ok(())
                }
            }
        } else {
            Ok(())
        }
    }

    pub(crate) fn reset<W: Write + ?Sized>(w: &mut W) -> fmt::Result {
        let mut ss = STYLE_STACK.lock().unwrap();
        if DEBUG_MODE {
            write!(w, "<ESC>[0m")?;
        }
        write!(w, "\x1b[0m")?;
        ss.clear();
        Ok(())
    }

    fn write_fg<W: fmt::Write + ?Sized>(w: &mut W, fg: Colour) -> fmt::Result {
        STYLE_BUFFER.with_borrow_mut(|buf| {
            buf.clear();
            match fg {
                Colour::RGB(red, green, blue) => {
                    if DEBUG_MODE {
                        write!(buf, "<ESC>[38;2;{};{};{}m", red, green, blue).unwrap();
                    }
                    write!(buf, "{}[38;2;{};{};{}m", Style::ESC, red, green, blue).unwrap();
                }
                other_fg => {
                    if DEBUG_MODE {
                        write!(buf, "<ESC>[{}m", u8::from(other_fg)).unwrap();
                    }
                    write!(buf, "{}[{}m", Style::ESC, u8::from(other_fg)).unwrap();
                }
            }
            write!(w, "{}", buf)
        })
    }

    fn write_bg<W: Write + ?Sized>(w: &mut W, bg: Colour) -> fmt::Result {
        STYLE_BUFFER.with_borrow_mut(|buf| {
            buf.clear();
            match bg {
                Colour::RGB(red, green, blue) => {
                    if DEBUG_MODE {
                        write!(buf, "<ESC>[48;2;{};{};{}m", red, green, blue).unwrap();
                    }
                    write!(buf, "{}[48;2;{};{};{}m", Style::ESC, red, green, blue).unwrap();
                }
                other_bg => {
                    if DEBUG_MODE {
                        write!(buf, "<ESC>[{}m", u8::from(other_bg) + 10).unwrap();
                    }
                    write!(buf, "{}[{}m", Style::ESC, u8::from(other_bg) + 10).unwrap();
                }
            }
            write!(w, "{}", buf)
        })
    }

    fn write_weight<W: Write + ?Sized>(w: &mut W, weight: Weight) -> fmt::Result {
        match weight {
            Weight::Bold => {
                if DEBUG_MODE {
                    write!(w, "<ESC>[1m")?;
                }
                write!(w, "\x1b[1m")
            }
            Weight::Faint => {
                if DEBUG_MODE {
                    write!(w, "<ESC>[2m")?;
                }
                write!(w, "\x1b[2m")
            }
        }
    }

    fn write_reverse_video<W: Write + ?Sized>(w: &mut W, reverse: bool) -> fmt::Result {
        if reverse {
            if DEBUG_MODE {
                write!(w, "<ESC>[7m")?;
            }
            write!(w, "\x1b[7m")
        } else {
            if DEBUG_MODE {
                write!(w, "<ESC>[27m")?;
            }
            write!(w, "\x1b[27m")
        }
    }

    fn current(ss: &mut [Style]) -> Option<Style> {
        let mut curr = Style::default();
        for style in ss.iter() {
            curr = curr.overlay(*style);
        }
        if curr == Style::default() { None } else { Some(curr) }
    }

    pub fn fmt<D: fmt::Display, W: fmt::Write>(&self, w: &mut W, item: D) -> fmt::Result {
        CURR_STYLE.with(|curr_style| {
            let mut curr = curr_style.borrow_mut();
            if *curr != Some(*self) {
                if Arguments::get().print_std_out_in_color() {
                    self.start(w)?;
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
            bg: other.bg.or(self.bg),
            weight: other.weight.or(self.weight),
            reverse_video: other.reverse_video.or(self.reverse_video),
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
            self.style.start(f)?;
            write!(f, "{}", self.item)?;
            Style::reset(f)
        } else {
            write!(f, "{}", self.item)
        }
    }
}
