/*
 * Copyright (c) 2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */

#[derive(Default, Copy, Clone, PartialEq, Eq)]
pub enum Alignment {
    #[default]
    Left,
    Right,
    Center,
}
/*
pub struct Content<'cell> {
    pub(super) text: Cow<'cell, str>,
    padding_left: usize,
    padding_right: usize,
    padding_char: char,
}
impl<'a> Content<'a> {
    pub fn new(text: impl Into<Cow<'a, str>>) -> Self {
        Self { text: text.into(), padding_left: 0, padding_right: 0, padding_char: ' ' }
    }

    pub fn set_padding_left(&mut self, padding_left: usize) {
        self.padding_left = padding_left;
    }

    pub fn set_padding_right(&mut self, padding_right: usize) {
        self.padding_right = padding_right;
    }

    pub fn with_padding(mut self, padding: usize) -> Self {
        self.padding_left = padding;
        self.padding_right = padding;
        self
    }

    pub fn with_padding_left(mut self, padding_left: usize) -> Self {
        self.padding_left = padding_left;
        self
    }

    pub fn with_padding_right(mut self, padding_right: usize) -> Self {
        self.padding_right = padding_right;
        self
    }

    pub fn with_padding_char(mut self, padding_char: char) -> Self {
        self.padding_char = padding_char;
        self
    }

    pub fn into_owned(self) -> Content<'static> {
        Content {
            text: Cow::Owned(self.text.into_owned()),
            padding_left: self.padding_left,
            padding_right: self.padding_right,
            padding_char: self.padding_char,
        }
    }
}

impl Deref for Content<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.text
    }
}

impl fmt::Display for Content<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for _ in 0..self.padding_left {
            write!(f, "{}", self.padding_char)?;
        }
        write!(f, "{}", self.text)?;
        for _ in 0..self.padding_right {
            write!(f, "{}", self.padding_char)?;
        }
        Ok(())
    }
}
*/
