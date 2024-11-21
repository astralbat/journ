/*
 * Copyright (c) 2019-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::err;
use crate::error::JournResult;
use crate::unit::{NegativeStyle, NumberFormat};
use itertools::{EitherOrBoth, Itertools};
use rust_decimal::Decimal;
use std::cell::LazyCell;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashSet;
use std::ops::{
    Bound, Deref, DerefMut, Range, RangeBounds, RangeFrom, RangeInclusive, RangeTo,
    RangeToInclusive,
};
use std::str::FromStr;

/// Generic wrapping type to enable the implementation of foreign traits.
#[derive(Debug)]
pub struct Wrapped<T>(pub T);
impl<T> Wrapped<T> {
    pub fn new(wrapped: T) -> Self {
        Self(wrapped)
    }
}
impl<T> Deref for Wrapped<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<T> DerefMut for Wrapped<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub struct SplitNWhitespace<'a> {
    string: &'a str,
    n: usize,
    split_count: usize,
}
impl<'a> Iterator for SplitNWhitespace<'a> {
    type Item = &'a str;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.split_count == self.n {
            return None;
        }
        self.split_count += 1;
        let mut start_pos = None;
        for (pos, c) in self.string.char_indices() {
            match c {
                c if c.is_whitespace() && self.split_count < self.n => {
                    // Started collecting chars and now we've hit some space
                    if let Some(start_pos) = start_pos {
                        let ret = Some(&self.string[start_pos..pos]);
                        self.string = &self.string[pos + 1..];
                        return ret;
                    }
                }
                _ => {
                    // Start collecting
                    if start_pos.is_none() {
                        start_pos = Some(pos);
                    }
                }
            }
        }

        // Reached the end of the string. Did we collect anything?
        if let Some(start_pos) = start_pos {
            let ret = Some(&self.string[start_pos..]);
            self.string = &self.string[self.string.len()..];
            ret
        } else {
            None
        }
    }
}

pub struct SplitFields<'a, 'b> {
    string: &'a str,
    field_seps: &'b [char],
}
impl<'a, 'b> Iterator for SplitFields<'a, 'b> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        let mut in_quotes = false;
        let mut pos = 0;
        for c in self.string.chars() {
            match c {
                '"' => in_quotes = !in_quotes,
                a if !in_quotes => {
                    for field_sep in self.field_seps {
                        if *field_sep == a {
                            let ret = Some(self.string[0..pos].trim_quotes());
                            self.string = &self.string[pos + a.len_utf8()..];
                            return ret;
                        }
                    }
                }
                _ => {}
            }
            pos += c.len_utf8()
        }
        if !self.string.is_empty() {
            let ret = Some(self.string.trim_end().trim_quotes());
            self.string = "";
            return ret;
        }
        None
    }
}

pub struct SplitNFields<'a, 'b> {
    split_fields: SplitFields<'a, 'b>,
    n: usize,
    split_count: usize,
}
impl<'a, 'b> Iterator for SplitNFields<'a, 'b> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        if self.split_count == self.n {
            return None;
        }
        self.split_count += 1;
        if self.split_count == self.n {
            Some(self.split_fields.string.trim_end().trim_quotes())
        } else {
            self.split_fields.next()
        }
    }
}

pub trait StrExt {
    fn splitn_whitespace(&self, n: usize) -> SplitNWhitespace;
    /// Splits some char separated text in to fields. Text surrounded by double quotes
    /// is treated as one field.
    fn split_fields<'a, 'b>(&'a self, field_seps: &'b [char]) -> SplitFields<'a, 'b>;
    fn splitn_fields<'a, 'b>(&'a self, field_seps: &'b [char], n: usize) -> SplitNFields<'a, 'b>;
    /// Gets whether the string is quoted with double or single quotes.
    fn is_quoted(&self) -> bool;
    /// If the string is quoted with double or single quotes, trim them from the start and end.
    fn trim_quotes(&self) -> &str;
    fn ellipses(&self, n: usize) -> String;
    fn to_decimal(&self, format: &NumberFormat) -> JournResult<Decimal>;
    /// Interns the specified string.
    fn intern(&self) -> &'static str;
    /// Leading space and tab chars.
    fn leading_whitespace(&self) -> &str;
    /// Quantifies the indentation of the string's contents; how many spaces precedes its content. An ascii space character counts
    /// as 1 and a tab counts as 8.
    fn indented_amount(&self) -> u16;
    /// Trims the start of the string according to the indented amount from [`Self::indented_amount()`].
    /// Returns `None` if outdenting was not successful.
    fn outdent(&self, outdent_amount: u16) -> Option<&str>;
    /// Compares ignoring case for the ascii characters. Non-ascii characters are compared without
    /// any conversion.
    fn cmp_ignore_case_ascii(&self, b: &str) -> Ordering;
}
pub trait StringExt {
    fn trim_newline(&mut self);
}
impl StrExt for str {
    #[inline]
    fn splitn_whitespace(&self, n: usize) -> SplitNWhitespace {
        SplitNWhitespace { string: self, n, split_count: 0 }
    }
    fn split_fields<'a, 'b>(&'a self, field_seps: &'b [char]) -> SplitFields<'a, 'b> {
        SplitFields { string: self, field_seps }
    }
    fn splitn_fields<'a, 'b>(&'a self, field_seps: &'b [char], n: usize) -> SplitNFields<'a, 'b> {
        SplitNFields { split_fields: SplitFields { string: self, field_seps }, n, split_count: 0 }
    }
    fn is_quoted(&self) -> bool {
        (self.starts_with('"') && self.ends_with('"'))
            || (self.starts_with('\'') && self.ends_with('\''))
    }
    fn trim_quotes(&self) -> &str {
        if self.is_quoted() {
            &self[1..self.len() - 1]
        } else {
            self
        }
    }
    fn ellipses(&self, n: usize) -> String {
        let mut count = 0;
        let mut pos = 0;
        for c in self.chars() {
            count += 1;
            if count < n - 3 {
                pos += c.len_utf8();
            }
            if count > n {
                return self[0..pos].to_string() + "...";
            }
        }
        self.to_string()
    }
    fn to_decimal(&self, format: &NumberFormat) -> JournResult<Decimal> {
        let normalized_s = format.remove_thousands(self);
        NumberFormat::to_non_scientific(&mut normalized_s.borrow_mut());
        let s = normalized_s.borrow();

        let dot_pos = s.bytes().rposition(|c| c == format.decimal_separator());
        let parse_128 = |s| -> JournResult<i128> {
            i128::from_str(s).map_err(|e| err!(e; "Cannot parse as number: '{}'", s))
        };

        let negative = match format.negative_format() {
            NegativeStyle::NegativeSign => s.as_bytes().first().copied() == Some(b'-'),
            NegativeStyle::Brackets => {
                s.as_bytes().first().copied() == Some(b'(')
                    && s.as_bytes().last().copied() == Some(b')')
            }
        };
        let (first_pos, last_pos) = if negative {
            match format.negative_format() {
                NegativeStyle::NegativeSign => (1, s.len()),
                NegativeStyle::Brackets => (1, s.len() - 1),
            }
        } else {
            (0, s.len())
        };

        let significand = 'sig: {
            if let Some(dot_pos) = dot_pos {
                if dot_pos + 1 < s.len() {
                    let mut first_part = parse_128(&s[first_pos..dot_pos])?;
                    let mut second_part = parse_128(&s[dot_pos + 1..last_pos])?;
                    if negative {
                        first_part *= -1;
                        second_part *= -1;
                    }
                    for _ in dot_pos + 1..last_pos {
                        match first_part.checked_mul(10i128) {
                            Some(n) => first_part = n,
                            None => return Err(err!("Cannot parse number; too big: {}", s)),
                        }
                    }
                    break 'sig match first_part.checked_add(second_part) {
                        Some(n) => n,
                        None => return Err(err!("Cannot parse number; too big: {}", s)),
                    };
                }
            }
            parse_128(s.as_str())?
        };
        let scale: u32 = dot_pos.map_or_else(|| 0, |p| last_pos as u32 - p as u32 - 1);
        if scale > 28 {
            return Err(err!("Cannot parse number; too big: {}", s));
        }
        Ok(Decimal::from_i128_with_scale(significand, scale))
    }

    fn intern(&self) -> &'static str {
        thread_local! {
            static INTERN_POOL: LazyCell<RefCell<HashSet<&'static str>>> = LazyCell::new(|| RefCell::new(HashSet::new()));
        }
        INTERN_POOL.with(|pool| {
            let mut set = pool.borrow_mut();
            *set.get_or_insert_with(self, |s| Box::leak(Box::new(s.to_string())))
        })
    }

    fn leading_whitespace(&self) -> &str {
        &self[..self.bytes().take_while(|b| *b == b' ' || *b == b'\t').count()]
    }

    fn indented_amount(&self) -> u16 {
        let mut count = 0;
        for c in self.chars() {
            match c {
                ' ' => count += 1,
                '\t' => count += 8,
                _ => break,
            }
        }
        count
    }

    fn outdent(&self, outdent_amount: u16) -> Option<&str> {
        let mut outdent_amount = outdent_amount as isize;
        let mut i = 0;
        for c in self.chars() {
            if outdent_amount <= 0 {
                return Some(&self[i..]);
            }
            match c {
                ' ' => outdent_amount -= 1,
                '\t' => outdent_amount -= 8,
                _ => return None,
            }
            i += c.len_utf8();
        }
        Some("")
    }

    fn cmp_ignore_case_ascii(&self, b: &str) -> Ordering {
        self.bytes()
            .zip_longest(b.bytes())
            .map(|ab| match ab {
                EitherOrBoth::Left(_) => Ordering::Greater,
                EitherOrBoth::Right(_) => Ordering::Less,
                EitherOrBoth::Both(a, b) => a.to_ascii_lowercase().cmp(&b.to_ascii_lowercase()),
            })
            .find(|&ordering| ordering != Ordering::Equal)
            .unwrap_or(Ordering::Equal)
    }
}
impl StrExt for String {
    #[inline]
    fn splitn_whitespace(&self, n: usize) -> SplitNWhitespace {
        SplitNWhitespace { string: self, n, split_count: 0 }
    }
    fn split_fields<'a, 'b>(&'a self, field_seps: &'b [char]) -> SplitFields<'a, 'b> {
        SplitFields { string: self, field_seps }
    }
    fn splitn_fields<'a, 'b>(&'a self, field_seps: &'b [char], n: usize) -> SplitNFields<'a, 'b> {
        SplitNFields { split_fields: SplitFields { string: self, field_seps }, n, split_count: 0 }
    }
    fn is_quoted(&self) -> bool {
        self.as_str().is_quoted()
    }
    fn trim_quotes(&self) -> &str {
        self.as_str().trim_quotes()
    }
    fn ellipses(&self, n: usize) -> String {
        (self as &str).ellipses(n)
    }
    fn to_decimal(&self, format: &NumberFormat) -> JournResult<Decimal> {
        self.as_str().to_decimal(format)
    }

    fn intern(&self) -> &'static str {
        self.as_str().intern()
    }

    fn leading_whitespace(&self) -> &str {
        self.as_str().leading_whitespace()
    }
    fn indented_amount(&self) -> u16 {
        self.as_str().indented_amount()
    }

    fn outdent(&self, outdent_amount: u16) -> Option<&str> {
        self.as_str().outdent(outdent_amount)
    }
    fn cmp_ignore_case_ascii(&self, b: &str) -> Ordering {
        self.as_str().cmp_ignore_case_ascii(b)
    }
}
impl StringExt for String {
    /// Removes a single line feed or carriage return and line feed from the end of the string
    /// if present.
    fn trim_newline(&mut self) {
        if self.ends_with('\n') {
            self.pop();
            if self.ends_with('\r') {
                self.pop();
            }
        }
    }
}

pub trait RangeBoundsExt<T> {
    fn intersection<R>(&self, other: &R) -> Option<(Bound<T>, Bound<T>)>
    where
        R: RangeBounds<T>,
        Self: Sized;
}

macro_rules! impl_range_bounds_ext {
    ($ty:ty) => {
        impl<T> RangeBoundsExt<T> for $ty
        where
            T: Clone + PartialOrd,
        {
            fn intersection<R>(&self, other: &R) -> Option<(Bound<T>, Bound<T>)>
            where
                R: RangeBounds<T>,
            {
                let start: Option<Bound<T>> = if range_contains(self.start_bound(), true, other) {
                    Some(self.start_bound().cloned())
                } else if range_contains(other.start_bound(), true, self) {
                    Some(other.start_bound().cloned())
                } else if let Bound::Unbounded = self.start_bound() {
                    Some(other.start_bound().cloned())
                } else if let Bound::Unbounded = other.start_bound() {
                    Some(self.start_bound().cloned())
                } else {
                    None
                };
                let end = if range_contains(self.end_bound(), false, other) {
                    Some(self.end_bound().cloned())
                } else if range_contains(other.end_bound(), false, self) {
                    Some(other.end_bound().cloned())
                } else if let Bound::Unbounded = self.end_bound() {
                    Some(other.end_bound().cloned())
                } else if let Bound::Unbounded = other.end_bound() {
                    Some(self.end_bound().cloned())
                } else {
                    None
                };
                if start.is_some() && end.is_some() {
                    Some((start.unwrap(), end.unwrap()))
                } else {
                    None
                }
            }
        }
    };
}
impl_range_bounds_ext!(Range<T>);
impl_range_bounds_ext!(RangeInclusive<T>);
impl_range_bounds_ext!(RangeFrom<T>);
impl_range_bounds_ext!(RangeTo<T>);
impl_range_bounds_ext!(RangeToInclusive<T>);
impl_range_bounds_ext!((Bound<T>, Bound<T>));

fn range_contains<T, R: RangeBounds<T>>(bound: Bound<&T>, bound_is_start: bool, range: &R) -> bool
where
    T: PartialOrd,
{
    if range_empty(range) {
        return false;
    }
    match bound {
        Bound::Included(s) => range.contains(s),
        Bound::Excluded(s) => {
            if bound_is_start {
                match range.start_bound() {
                    Bound::Included(rs) | Bound::Excluded(rs) => {
                        s >= rs
                            && match range.end_bound() {
                                Bound::Included(re) | Bound::Excluded(re) => s < re,
                                Bound::Unbounded => true,
                            }
                    }
                    Bound::Unbounded => match range.end_bound() {
                        Bound::Included(re) | Bound::Excluded(re) => s < re,
                        Bound::Unbounded => true,
                    },
                }
            } else {
                match range.end_bound() {
                    Bound::Included(re) | Bound::Excluded(re) => {
                        s <= re
                            && match range.start_bound() {
                                Bound::Included(rs) | Bound::Excluded(rs) => s > rs,
                                Bound::Unbounded => true,
                            }
                    }
                    Bound::Unbounded => match range.start_bound() {
                        Bound::Included(rs) | Bound::Excluded(rs) => s > rs,
                        Bound::Unbounded => true,
                    },
                }
            }
        }
        Bound::Unbounded => false,
    }
}

fn range_empty<T, R: RangeBounds<T>>(range: &R) -> bool
where
    T: PartialOrd,
{
    match (range.start_bound(), range.end_bound()) {
        (Bound::Included(start), Bound::Included(end))
        | (Bound::Included(start), Bound::Excluded(end))
        | (Bound::Excluded(start), Bound::Included(end))
        | (Bound::Excluded(start), Bound::Excluded(end)) => end <= start,
        _ => false,
    }
}

pub trait NumExt {
    /// Gets the minimum absolute value of the two numbers; the closest to zero.
    fn min_abs(self, other: Self) -> Self;

    /// Gets whether both values have the same sign, or are
    /// compatible where zero is considered compatible with both
    /// positive and negative values
    fn is_sign_compatible(&self, other: Self) -> bool;
}
macro_rules! impl_num_ext {
    ($ty:ty) => {
        impl NumExt for $ty {
            fn min_abs(self, other: Self) -> Self {
                if self.abs() < other.abs() {
                    self
                } else {
                    other
                }
            }

            fn is_sign_compatible(&self, other: Self) -> bool {
                if *self == 0 || other == 0 {
                    true
                } else {
                    self.signum() == other.signum()
                }
            }
        }
    };
}
impl_num_ext!(i8);
impl_num_ext!(i16);
impl_num_ext!(i32);
impl_num_ext!(i64);
impl_num_ext!(i128);
impl_num_ext!(isize);

impl NumExt for Decimal {
    fn min_abs(self, other: Self) -> Self {
        if self.abs() < other.abs() {
            self
        } else {
            other
        }
    }

    fn is_sign_compatible(&self, other: Self) -> bool {
        if self.is_zero() || other.is_zero() {
            true
        } else {
            (self.is_sign_positive() && other.is_sign_positive())
                || (self.is_sign_negative() && other.is_sign_negative())
        }
    }
}

pub trait FloatingExt {
    /// Function based on https://stackoverflow.com/a/76572321 by Traumflug.
    ///
    /// Round to significant digits (rather than digits after the decimal).
    ///
    /// Not implemented for `f32`, because such an implementation showed precision
    /// glitches (e.g. `precision_f32(12300.0, 2) == 11999.999`), so for `f32`
    /// floats, convert to `f64` for this function and back as needed.
    fn precision(&self, decimals: u32) -> f64;
}
impl FloatingExt for f64 {
    fn precision(&self, decimals: u32) -> f64 {
        if *self == 0. || decimals == 0 {
            0.
        } else {
            let shift = decimals as i32 - self.abs().log10().ceil() as i32;
            let shift_factor = 10_f64.powi(shift);

            (self * shift_factor).round() / shift_factor
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ext::RangeBoundsExt;
    use crate::ext::StrExt;
    use crate::unit::NumberFormat;
    use rust_decimal::Decimal;
    use std::ops::Bound;
    use std::str::FromStr;

    #[test]
    fn test_to_decimal() {
        let f = NumberFormat::parse("1,000.00", true);
        assert_eq!("0.75177512".to_decimal(&f), Ok(Decimal::from_str("0.75177512").unwrap()));
        assert_eq!("-0.75177512".to_decimal(&f), Ok(Decimal::from_str("-0.75177512").unwrap()));

        let f = NumberFormat::parse("1.000,00", true);
        assert_eq!("0,12".to_decimal(&f), Ok(Decimal::from_str("0.12").unwrap()));

        let f = NumberFormat::parse("(1.000,00)", true);
        assert_eq!("(0,12)".to_decimal(&f), Ok(Decimal::from_str("-0.12").unwrap()));
        assert_eq!("(100.000,12)".to_decimal(&f), Ok(Decimal::from_str("-100000.12").unwrap()));
    }

    #[test]
    fn test_range_intersection() {
        //
        // Range
        //
        // R1 wholly inside R2
        assert_eq!((1..5).intersection(&(0..10)), Some((Bound::Included(1), Bound::Excluded(5))));
        // R2 wholly inside R1
        assert_eq!((0..10).intersection(&(1..5)), Some((Bound::Included(1), Bound::Excluded(5))));
        // R1.start inside R2
        assert_eq!((1..5).intersection(&(0..3)), Some((Bound::Included(1), Bound::Excluded(3))));
        // R2.start inside R1
        assert_eq!((0..3).intersection(&(1..5)), Some((Bound::Included(1), Bound::Excluded(3))));

        //
        // RangeFrom
        //
        // Unbounded R1.start
        assert_eq!((..5).intersection(&(1..3)), Some((Bound::Included(1), Bound::Excluded(3))));
        // Unbounded R2.start
        assert_eq!((1..3).intersection(&(..5)), Some((Bound::Included(1), Bound::Excluded(3))));

        //
        // RangeInclusive
        //
        // R1 wholly inside R2
        assert_eq!((1..=5).intersection(&(0..10)), Some((Bound::Included(1), Bound::Included(5))));
        // R2 wholly inside R1
        assert_eq!((0..=10).intersection(&(1..5)), Some((Bound::Included(1), Bound::Excluded(5))));

        //
        // Range From Excluded
        //
        // R1 wholly inside R2
        assert_eq!(
            (Bound::Excluded(1), Bound::Excluded(5)).intersection(&(0..10)),
            Some((Bound::Excluded(1), Bound::Excluded(5)))
        );
        // R2 wholly inside R1
        assert_eq!(
            (Bound::Excluded(0), Bound::Excluded(10)).intersection(&(1..5)),
            Some((Bound::Included(1), Bound::Excluded(5)))
        );
        // R1.start inside R2
        assert_eq!(
            (Bound::Excluded(1), Bound::Excluded(5)).intersection(&(0..3)),
            Some((Bound::Excluded(1), Bound::Excluded(3)))
        );
        // R2.start inside R1
        assert_eq!(
            (Bound::Excluded(0), Bound::Excluded(3)).intersection(&(1..5)),
            Some((Bound::Included(1), Bound::Excluded(3)))
        );
    }
}
