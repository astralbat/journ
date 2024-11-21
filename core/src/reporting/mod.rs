/*
 * Copyright (c) 2023-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use std::fmt;

pub mod balance;
pub mod table;
pub mod term_style;

/// A wrapper around a `fmt::Write` that counts the number of characters written.
/// This is useful to determine the length of an item it it were to be displayed without allocating a new string.
#[derive(Default)]
struct CharCounter {
    count: usize,
}
impl CharCounter {
    pub fn count(&self) -> usize {
        self.count
    }
}
impl fmt::Write for CharCounter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.count += s.chars().count();
        Ok(())
    }
}
pub fn width<I: fmt::Display>(item: I) -> usize {
    let mut counter = CharCounter::default();
    fmt::write(&mut counter, format_args!("{}", item)).unwrap();
    counter.count()
}

/// Gets the maximum display width of the items in the iterator where the iterator yields `Option<D>`.
pub fn optional_max_width<I: IntoIterator<Item = Option<D>>, D: fmt::Display>(iter: I) -> usize {
    let mut max = 0;
    for item in iter.into_iter().flatten() {
        max = max.max(width(item));
    }
    max
}

/// Gets the maximum width of the display of the items in the iterator.
pub fn max_width<I: IntoIterator>(iter: I) -> usize
where
    I::Item: fmt::Display,
{
    let mut max = 0;
    for item in iter.into_iter() {
        max = max.max(width(item));
    }
    max
}

/*
/// Trait that allows the display of a list of items in a column.
/// The type `T` is used here as a work around for issue [https://github.com/rust-lang/rfcs/pull/1672].
pub trait DisplayColumn<T> {
    /// Prepares the display of a column of items. The returned function accepts a row
    /// index beginning at 0 of the item to display.
    /// Returns `None` if the number of items has been exhausted.
    fn col_fmt(self) -> impl FnMut(&mut fmt::Formatter) -> Option<fmt::Result>;
}*/

/*

pub trait IntoCells {
    fn into_cells(self) -> impl IntoIterator<Item = table::Cell>;
}*/

/*
impl<'a, I> DisplayColumn<&str> for I
where
    I: Iterator<Item = Option<&'a str>> + Clone,
{
    fn col_fmt(self) -> impl FnMut(&mut fmt::Formatter) -> Option<fmt::Result> {
        let max_width = optional_max_width(self.clone());
        let mut iter = self;
        move |f| {
            iter.next().map(|s| match s {
                Some(s) => write!(f, "{:max_width$}", s),
                None => write!(f, "{:max_width$}", ""),
            })
        }
    }
}*/
