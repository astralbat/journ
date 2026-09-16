/*
 * Copyright (c) 2019-2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::datetime::{DateTimeFormat, DateTimePrecision};
use crate::error::parsing::IParseError;
use crate::parsing::IParseResult;
use crate::parsing::input::TextInput;
use chrono::NaiveTime;
use chrono::format::{DelayedFormat, Item, Parsed, parse_and_remainder};
use nom::{Err as NomErr, InputLength};
use std::cmp::Ordering;
use std::hash::Hash;
use std::iter;
use std::ops::Deref;

#[derive(Copy, Clone, Debug)]
pub struct JTime {
    time: NaiveTime,
}

impl JTime {
    pub fn new(time: NaiveTime) -> Self {
        Self { time }
    }

    /// Parses a time from a string. The time format is required to parse the time.
    /// This will only return a simple error message due to performance reasons.
    ///
    /// Returns the parsed time with a `bool` indicating if this time is UTC (i.e. if the string ended with 'Z').
    pub fn parse<'h, 'i, I: TextInput<'i>>(
        time_format: &'h DateTimeFormat<'h>,
    ) -> impl Fn(I) -> IParseResult<'i, I, (Self, bool)> {
        move |input| {
            let mut parsed = Parsed::new();

            // Try and read the date/time separator and time items, allowing for shortening.
            // E.g. " 10:15" as a shortcut for " 10:15:00"
            // Any prefix is valid here. E.g. " 10:" but not " ".
            let saved_remainder = input.text();
            let mut parsed_remainder = saved_remainder;
            for item in time_format.time_items() {
                match parse_and_remainder(&mut parsed, parsed_remainder, iter::once(item)) {
                    Ok(rem) => parsed_remainder = rem,
                    Err(_) => break,
                }
            }
            // We just read spaces, so rollback the remainder.
            if saved_remainder[..saved_remainder.len() - parsed_remainder.len()]
                .trim_start()
                .is_empty()
            {
                return Err(NomErr::Error(IParseError::new("Invalid time", input)));
            }

            let mut explicit_utc = false;
            if parsed_remainder.starts_with('Z') {
                parsed_remainder = &parsed_remainder[1..];
                explicit_utc = true;
            }

            let rem = input.slice(input.input_len() - parsed_remainder.input_len()..);
            let time = parsed.to_naive_time().map_err(|_| {
                NomErr::Error(IParseError::new("Cannot parse time from string", input))
            })?;

            Ok((rem, (Self::new(time), explicit_utc)))
        }
    }

    pub fn format_with_precision<'h, 'a>(
        &'a self,
        format: &'a DateTimeFormat<'h>,
        precision: DateTimePrecision,
    ) -> DelayedFormat<impl Iterator<Item = &'a Item<'h>> + Clone + 'a> {
        format.format(None, Some(self.time), precision)
    }
}

impl Deref for JTime {
    type Target = NaiveTime;

    fn deref(&self) -> &Self::Target {
        &self.time
    }
}

impl PartialEq for JTime {
    fn eq(&self, other: &Self) -> bool {
        self.time == other.time
    }
}
impl Eq for JTime {}

impl PartialOrd for JTime {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for JTime {
    fn cmp(&self, other: &Self) -> Ordering {
        self.time.cmp(&other.time)
    }
}

impl Hash for JTime {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.time.hash(state);
    }
}
