/*
 * Copyright (c) 2022-2023. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use std::lazy::OnceCell;
use std::ops::RangeBounds;

#[derive(Debug, Clone)]
pub struct LazyParser<T> {
    raw: String,
    parsed: OnceCell<T>,
}

impl<T> LazyParser<T> {
    pub fn new(raw: String) -> LazyParser<T> {
        LazyParser { raw, parsed: OnceCell::new() }
    }
    pub fn raw(&self) -> &str {
        &self.raw
    }

    pub fn parse<F>(&self, init: F) -> &T
    where
        F: for<'r> Fn(&'r str) -> T,
    {
        self.parsed.get_or_init(|| (init)(&self.raw))
    }

    pub fn set<R: RangeBounds<usize>>(&mut self, range: R, value: &str) {
        self.raw.replace_range(range, value);
        self.parsed = OnceCell::new();
    }
}
