/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::datetime::{DateTimeFormat, DateTimePrecision};
use crate::error::parsing::IParseError;
use crate::parsing::IParseResult;
use crate::parsing::input::TextInput;
use chrono::NaiveDate;
use chrono::format::{DelayedFormat, Item, Parsed, parse_and_remainder};
use nom::{Err as NomErr, InputLength};
use std::cmp::Ordering;
use std::hash::Hash;
use std::ops::Deref;

#[derive(Copy, Clone, Debug)]
pub struct JDate {
    pub(super) date: NaiveDate,
}

impl JDate {
    pub fn new(date: NaiveDate) -> Self {
        Self { date }
    }

    /*
    pub fn formatter(&self) -> &'h DateFormat<'h> {
        self.format
    }*/

    /// Parses a date from a string. The date format is required to parse the date.
    /// This will only return a simple error message due to performance reasons.
    pub fn parse<'h, 'i, I: TextInput<'i>>(
        date_format: &'h DateTimeFormat<'h>,
    ) -> impl Fn(I) -> IParseResult<'i, I, Self> {
        move |input: I| {
            let mut parsed = Parsed::new();
            let parsed_remainder =
                parse_and_remainder(&mut parsed, input.text(), date_format.items())
                    .map_err(|_| NomErr::Error(IParseError::new("Invalid date", input.clone())))?;
            let rem = input.slice(input.input_len() - parsed_remainder.input_len()..);
            // If the date is not complete, we assume the first day of the month/year.
            if parsed.month.is_none() {
                parsed.set_month(1).unwrap();
            }
            if parsed.day.is_none() {
                parsed.set_day(1).unwrap();
            }
            let date = parsed.to_naive_date().map_err(|_| {
                NomErr::Error(IParseError::new("Cannot parse date from string", input))
            })?;

            Ok((rem, Self::new(date)))
        }
    }

    pub fn format<'a, 'h>(
        &'a self,
        format: &'a DateTimeFormat<'h>,
    ) -> DelayedFormat<impl Iterator<Item = &'a Item<'h>> + Clone + 'a> {
        format.format(Some(self.date), None, DateTimePrecision::Day)
    }
    /*
    pub fn with_format(self, format: &'h DateFormat<'h>) -> Self {
        JDate { date: self.date, format }
    }*/
}

/*
impl table2::Cell for JDate {
    fn print<'format>(
        &self,
        f: &mut dyn CellFormatter,
        line: usize,
        _width: Option<ColumnWidth>,
    ) -> fmt::Result {
        if line == 0 { write!(f, "{}", self) } else { Err(fmt::Error) }
    }

    fn width(&self) -> CellWidth {
        CellWidth::Unary(self.to_string().chars().count())
    }
}*/

impl Deref for JDate {
    type Target = NaiveDate;

    fn deref(&self) -> &Self::Target {
        &self.date
    }
}

/*
impl fmt::Display for JDate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format.format(self.date))
    }
}*/

impl PartialEq for JDate {
    fn eq(&self, other: &Self) -> bool {
        self.date == other.date
    }
}

impl Eq for JDate {}

impl PartialOrd for JDate {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for JDate {
    fn cmp(&self, other: &Self) -> Ordering {
        self.date.cmp(&other.date)
    }
}

impl Hash for JDate {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.date.hash(state);
    }
}
/*
impl From<JDate> for SS {
    fn from(value: JDate) -> Self {
        let mut s = SS::new();
        fmt::write(&mut s, format_args!("{}", value)).unwrap();
        s
    }
}*/
