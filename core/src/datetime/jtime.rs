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
use std::ops::Deref;

#[derive(Copy, Clone, Debug)]
pub struct JTime {
    time: NaiveTime,
    //pub(super) format: &'h TimeFormat<'h>,
    //pub(super) print_utc_marker: bool,
}

impl JTime {
    pub fn new(time: NaiveTime) -> Self {
        Self { time }
    }

    /*
    pub fn formatter(&self) -> &'h TimeFormat<'h> {
        self.format
    }*/

    /// Parses a time from a string. The time format is required to parse the time.
    /// This will only return a simple error message due to performance reasons.
    pub fn parse<'h, 'i, I: TextInput<'i>>(
        time_format: &'h DateTimeFormat<'h>,
    ) -> impl Fn(I) -> IParseResult<'i, I, Self> {
        move |input| {
            let mut parsed = Parsed::new();
            let mut parsed_remainder =
                parse_and_remainder(&mut parsed, input.text(), time_format.items())
                    .map_err(|_| NomErr::Error(IParseError::new("Invalid time", input.clone())))?;
            if parsed_remainder.starts_with('Z') {
                parsed_remainder = &parsed_remainder[1..];
            }
            let rem = input.slice(input.input_len() - parsed_remainder.input_len()..);
            let time = parsed.to_naive_time().map_err(|_| {
                NomErr::Error(IParseError::new("Cannot parse time from string", input))
            })?;

            Ok((rem, Self::new(time)))
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

/*
impl fmt::Display for JTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format.format(self.time))
    }
}*/

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
