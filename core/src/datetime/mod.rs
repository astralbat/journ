/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
mod date_and_time;
mod datetime_format;
mod jdate;
mod jdatetime;
mod jdatetime_range;
mod jtime;

use crate::unit::NumberFormat;
use chrono::format::{Item, Numeric, Parsed};
use chrono_tz::Tz;
pub use date_and_time::DateAndTime;
pub use jdate::JDate;
pub use jdatetime::JDateTime;
pub use jdatetime_range::JDateTimeRange;
pub use jtime::JTime;
use nom::Finish;
use std::sync::LazyLock;
pub use {
    datetime_format::DateFormat, datetime_format::DateTimeFormat, datetime_format::TimeFormat,
};

pub static DEFAULT_DATE_FORMAT: LazyLock<&'static DateTimeFormat<'static>> = LazyLock::new(|| {
    DateTimeFormat::parse_format(DateFormatMode::Date)("yyyy-mm-dd")
        .finish()
        .map(|(_, df)| df.into_owned())
        .unwrap()
});
pub static DEFAULT_TIME_FORMAT: LazyLock<&'static DateTimeFormat<'static>> = LazyLock::new(|| {
    DateTimeFormat::parse_format(DateFormatMode::Time)("hh:mm:ss")
        .finish()
        .map(|(_, df)| df.into_owned())
        .unwrap()
});
pub static DEFAULT_DATETIME_FORMAT: LazyLock<&'static DateTimeFormat<'static>> =
    LazyLock::new(|| {
        DateTimeFormat::parse_format(DateFormatMode::DateTime)("yyyy-mm-dd hh:mm:ss")
            .finish()
            .map(|(_, df)| df.into_owned())
            .unwrap()
    });
pub static DEFAULT_TIMEZONE: LazyLock<Tz> = LazyLock::new(|| Tz::UTC);
pub static DEFAULT_NUMBER_FORMAT: LazyLock<NumberFormat> = LazyLock::new(NumberFormat::default);

/// The order of fields is such that shall consider a second to be the _most_ precise.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DateTimePrecision {
    Year,
    Month,
    Week,
    Day,
    Hour,
    Minute,
    Second,
}

impl From<&Parsed> for DateTimePrecision {
    fn from(parsed: &Parsed) -> Self {
        if parsed.second().is_some() {
            DateTimePrecision::Second
        } else if parsed.minute().is_some() {
            DateTimePrecision::Minute
        } else if parsed.hour_div_12().is_some() || parsed.hour_mod_12().is_some() {
            DateTimePrecision::Hour
        } else if parsed.day().is_some() {
            DateTimePrecision::Day
        } else if parsed.month().is_some() {
            DateTimePrecision::Month
        } else {
            DateTimePrecision::Year
        }
    }
}

impl<'a> TryFrom<&Item<'a>> for DateTimePrecision {
    type Error = ();
    fn try_from(item: &Item) -> Result<Self, ()> {
        match item {
            Item::Numeric(num, _) => match num {
                Numeric::Day => Ok(DateTimePrecision::Day),
                Numeric::Month => Ok(DateTimePrecision::Month),
                Numeric::Year => Ok(DateTimePrecision::Year),
                Numeric::Minute => Ok(DateTimePrecision::Minute),
                Numeric::Second => Ok(DateTimePrecision::Second),
                Numeric::Hour => Ok(DateTimePrecision::Hour),
                Numeric::Hour12 => Ok(DateTimePrecision::Hour),
                Numeric::IsoWeek
                | Numeric::WeekdayFromMon
                | Numeric::WeekFromMon
                | Numeric::WeekFromSun => Ok(DateTimePrecision::Week),
                _ => Err(()),
            },
            _ => Err(()),
        }
    }
}

pub enum DateFormatMode {
    Date,
    Time,
    DateTime,
}
