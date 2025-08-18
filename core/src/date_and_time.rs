/*
 * Copyright (c) 2019-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::arguments::DateTimeArguments;
use crate::error::parsing::IParseError;
use crate::error::{JournError, JournResult};
use crate::ext::RangeBoundsExt;
use crate::parsing::text_input::TextInput;
use crate::parsing::{IParseResult, JParseResult};
use crate::reporting::table;
use crate::reporting::table::{Cell, WrapPolicy};
use crate::{err, match_parser, match_parsers};
use chrono::format::{DelayedFormat, Fixed, Item, Numeric, Pad, Parsed, parse};
use chrono::{
    DateTime, Datelike, Duration, LocalResult, Months, NaiveDate, NaiveDateTime, NaiveTime,
    TimeZone, Timelike, Utc,
};
use chrono_tz::Tz;
use nom::bytes::complete::{tag, tag_no_case, take};
use nom::character::complete::{none_of, space1};
use nom::combinator::{map_res, opt, rest};
use nom::error::context;
use nom::sequence::{pair, preceded};
use nom::{Err as NomErr, Finish};
use std::borrow::Cow;
use std::cmp::Ordering;
use std::ops::{Add, Deref, Range, Sub};
use std::str::FromStr;
use std::sync::LazyLock;
use std::{fmt, iter};
use yaml_rust::Yaml;
use yaml_rust::yaml::Hash;

pub static DEFAULT_DATE_FORMAT: LazyLock<DateFormat<'static>> =
    LazyLock::new(|| "yyyy-mm-dd".parse().unwrap());
pub static DEFAULT_TIME_FORMAT: LazyLock<TimeFormat<'static>> =
    LazyLock::new(|| "hh:mm:ss".parse().unwrap());
static DEFAULT_TIMEZONE: LazyLock<Tz> = LazyLock::new(|| Tz::UTC);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DateTimeParams<'h> {
    date_format: &'h DateFormat<'h>,
    time_format: &'h TimeFormat<'h>,
    timezone: Tz,
}

impl<'h> DateTimeParams<'h> {
    pub fn new(
        date_format: &'h DateFormat<'h>,
        time_format: &'h TimeFormat<'h>,
        timezone: Tz,
    ) -> Self {
        Self { date_format, time_format, timezone }
    }

    pub fn date_format(&self) -> &'h DateFormat<'h> {
        self.date_format
    }

    pub fn set_date_format(&mut self, date_format: &'h DateFormat<'h>) {
        self.date_format = date_format;
    }

    pub fn time_format(&self) -> &'h TimeFormat<'h> {
        self.time_format
    }

    pub fn set_time_format(&mut self, time_format: &'h TimeFormat<'h>) {
        self.time_format = time_format;
    }

    pub fn timezone(&self) -> Tz {
        self.timezone
    }

    pub fn set_timezone(&mut self, timezone: Tz) {
        self.timezone = timezone;
    }

    pub fn set_timezone_from_str(&mut self, tz_str: &str) -> JournResult<()> {
        let tz = Tz::from_str(tz_str).map_err(|e| err!(e; "Unable to set timezone"))?;
        self.timezone = tz;
        Ok(())
    }

    /// Converts the specified `range` to have the timezone, date format and time format
    /// specified by the arguments.
    ///
    /// This is a rather complex task as we want to retain the format of the supplied range as far
    /// as possible.
    ///
    /// # Examples
    /// * 2000-06-01 BST -> 2000-05-31 23:00:00..2000-06-01 23:00:00 UTC
    pub fn convert_datetime_range(&self, range: &JDateTimeRange<'h>) -> JDateTimeRange<'h> {
        let new_start_time = self.timezone.from_utc_datetime(&range.start().datetime().naive_utc());
        // Use a time format if the range has one or if the time is not midnight.
        let use_start_tf = range.start().time_format().is_some()
            || new_start_time.hour() != 0
            || new_start_time.minute() != 0
            || new_start_time.second() != 0;
        let start = JDateTime::from_datetime(
            new_start_time,
            Some(self.date_format),
            if use_start_tf { Some(self.time_format) } else { None },
        );

        let new_end_time = self.timezone.from_utc_datetime(&range.end().datetime().naive_utc());
        let use_end_df = range.end_opt().and_then(|e| e.date_format()).is_some()
            || new_end_time.day() != new_start_time.day();
        let use_end_tf = range.end_opt().and_then(|e| e.time_format()).is_some()
            || new_end_time.hour() != 0
            || new_end_time.minute() != 0
            || new_end_time.second() != 0;
        let implied_end = JDateTime::from_datetime(
            new_end_time,
            if use_end_df { Some(self.date_format) } else { None },
            if use_end_tf { Some(self.time_format) } else { None },
        );
        if range.end_opt().is_some() {
            JDateTimeRange::new(start, Some(implied_end))
        } else {
            // The specified range does not have an end but the converted range may require one
            // if the precision changes in the new timezone.
            if range.start().precision() != start.precision() {
                JDateTimeRange::new(start, Some(implied_end))
            } else {
                JDateTimeRange::new(start, None)
            }
        }
    }
}

impl<'h> Default for DateTimeParams<'h> {
    fn default() -> Self {
        Self {
            date_format: &DEFAULT_DATE_FORMAT,
            time_format: &DEFAULT_TIME_FORMAT,
            timezone: *DEFAULT_TIMEZONE,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct JDate<'h> {
    date: NaiveDate,
    format: &'h DateFormat<'h>,
}

impl<'h> JDate<'h> {
    pub fn new(date: NaiveDate, format: &'h DateFormat<'h>) -> Self {
        Self { date, format }
    }

    /// Parses a date from a string. The date format is required to parse the date.
    /// This will only return a simple error message due to performance reasons.
    pub fn parse<'i, I: TextInput<'i>>(
        date_format: &'h DateFormat<'h>,
    ) -> impl Fn(I) -> IParseResult<'i, I, Self> {
        move |input: I| {
            let (rem, date) = date_format.date_string()(input.clone())?;

            let mut parsed = Parsed::new();
            parse(&mut parsed, date.text(), date_format.items().iter())
                .map_err(|_| NomErr::Error(IParseError::new("Invalid date", input.clone())))?;
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

            Ok((rem, Self::new(date, date_format)))
        }
    }
}

impl From<JDate<'_>> for Cell<'_> {
    fn from(value: JDate) -> Self {
        Cell::from(value.date.format(value.format.format_str()).to_string())
    }
}

impl Deref for JDate<'_> {
    type Target = NaiveDate;

    fn deref(&self) -> &Self::Target {
        &self.date
    }
}

impl fmt::Display for JDate<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", DelayedFormat::new(Some(self.date), None, self.format.items().iter()))
    }
}

impl PartialEq for JDate<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.date == other.date
    }
}

impl Eq for JDate<'_> {}

#[derive(Copy, Clone, Debug)]
pub struct JTime<'h> {
    time: NaiveTime,
    format: &'h TimeFormat<'h>,
    print_utc_marker: bool,
}

impl<'h> JTime<'h> {
    pub fn new(time: NaiveTime, format: &'h TimeFormat<'h>, print_utc_marker: bool) -> Self {
        Self { time, format, print_utc_marker }
    }

    /// Parses a time from a string. The time format is required to parse the time.
    /// This will only return a simple error message due to performance reasons.
    pub fn parse<'i, I: TextInput<'i>>(
        time_format: &'h TimeFormat<'h>,
    ) -> impl Fn(I) -> IParseResult<'i, I, Self> {
        move |input| {
            let (rem, time) = time_format.time_string()(input.clone())?;

            let mut time = time.text();
            let has_utc_marker = time.ends_with('Z');
            if has_utc_marker {
                time = &time[0..time.len() - 1];
            }

            let mut parsed = Parsed::new();
            parse(&mut parsed, time, time_format.items().iter())
                .map_err(|_| NomErr::Error(IParseError::new("Invalid time", input.clone())))?;
            let time = parsed.to_naive_time().map_err(|_| {
                NomErr::Error(IParseError::new("Cannot parse time from string", input))
            })?;

            Ok((rem, Self::new(time, time_format, has_utc_marker)))
        }
    }
}

impl Deref for JTime<'_> {
    type Target = NaiveTime;

    fn deref(&self) -> &Self::Target {
        &self.time
    }
}

impl fmt::Display for JTime<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", DelayedFormat::new(None, Some(self.time), self.format.items().iter()))
    }
}

#[derive(Copy, Clone, Debug)]
pub struct JDateTime<'h> {
    datetime: DateTime<Tz>,
    // TODO: Look at getting rid of these formats and replacing with a boolean/flags for whether the time should be printed. The formats themselves can be taken from the configuration. This should save memory.
    date_format: Option<&'h DateFormat<'h>>,
    time_format: Option<&'h TimeFormat<'h>>,
    print_utc_marker: bool,
}

impl<'h> JDateTime<'h> {
    /// Gets the current date/time, useful for date comparisons.
    pub fn now() -> Self {
        JDateTime {
            datetime: Tz::UTC.from_utc_datetime(&Utc::now().naive_local()),
            date_format: Some(&DEFAULT_DATE_FORMAT),
            time_format: Some(&DEFAULT_TIME_FORMAT),
            print_utc_marker: true,
        }
    }

    pub fn parse<'i, I: TextInput<'i>>(
        df: &'h DateFormat<'h>,
        tf: &'h TimeFormat<'h>,
        tz: Tz,
    ) -> impl Fn(I) -> IParseResult<'i, I, Self> {
        move |input| {
            map_res(
                pair(JDate::parse(df), opt(preceded(tag(" "), JTime::parse(tf)))),
                |(date, time)| JDateTime::from_date_time(date, time, tz),
            )(input)
        }
    }

    pub fn from_date_time(date: JDate<'h>, time: Option<JTime<'h>>, tz: Tz) -> JournResult<Self> {
        let timezone = if time.map(|t| t.print_utc_marker).unwrap_or(false) { Tz::UTC } else { tz };

        Ok(Self {
            datetime: match timezone.from_local_datetime(&NaiveDateTime::new(
                *date,
                time.map(|t| *t).unwrap_or_else(|| NaiveTime::from_hms_opt(0, 0, 0).unwrap()),
            )) {
                LocalResult::Single(t) => t,
                LocalResult::Ambiguous(t, _) => {
                    warn!("Ambiguous local datetime: {}", t);
                    t
                }
                _ => return Err(err!("Time not valid in timezone on this date")),
            },
            date_format: Some(date.format),
            time_format: time.map(|t| t.format),
            print_utc_marker: time.map(|t| t.print_utc_marker).unwrap_or(false),
        })
    }

    pub fn from_datetime(
        datetime: DateTime<Tz>,
        date_format: Option<&'h DateFormat<'h>>,
        time_format: Option<&'h TimeFormat<'h>>,
    ) -> JDateTime<'h> {
        JDateTime { datetime, date_format, time_format, print_utc_marker: false }
    }

    pub fn datetime(&self) -> DateTime<Tz> {
        self.datetime
    }

    pub fn utc_date(&self) -> NaiveDate {
        self.datetime.naive_utc().date()
    }

    /// Gets the date in the local timezone. I.e. not a UTC date unless the timezone is UTC.
    pub fn date(&self) -> JDate<'h> {
        JDate::new(
            self.datetime.naive_local().date(),
            self.date_format.unwrap_or(&DEFAULT_DATE_FORMAT),
        )
    }

    pub fn time(&self) -> NaiveTime {
        self.datetime.time()
    }

    pub fn date_format(&self) -> Option<&'h DateFormat<'h>> {
        self.date_format
    }

    pub fn with_date_format(&self, date_format: &'h DateFormat<'h>) -> JDateTime<'h> {
        JDateTime { date_format: Some(date_format), ..*self }
    }

    pub fn time_format(&self) -> Option<&'h TimeFormat<'h>> {
        self.time_format
    }

    pub fn with_time_format(&self, time_format: &'h TimeFormat<'h>) -> JDateTime<'h> {
        JDateTime { time_format: Some(time_format), ..*self }
    }

    pub fn with_timezone(&self, tz: Tz) -> JDateTime<'h> {
        JDateTime {
            datetime: self.datetime.with_timezone(&tz),
            date_format: self.date_format,
            time_format: self.time_format,
            print_utc_marker: self.print_utc_marker && tz == Tz::UTC,
        }
    }

    /// Gets the same datetime but with the time set as far back as possible towards midnight.
    /// Since midnight is not always a valid time in the timezone, this instead works by subtracting the hours, mins
    /// and secs from the datetime and then adding increments of 30 minutes until the date is the same.
    /// This heuristic approach should be good enough to be accurate in practice.
    pub fn with_earliest_time(&self) -> JDateTime<'h> {
        let time_duration =
            self.time().signed_duration_since(NaiveTime::from_hms_opt(0, 0, 0).unwrap());
        let mut approx_midnight = self.datetime - time_duration;
        while approx_midnight.naive_local().date() < self.datetime.naive_local().date() {
            // 30 mins is an educated guess. If this resolution is not enough, it can be reduced.
            approx_midnight += Duration::minutes(30);
        }
        JDateTime { datetime: approx_midnight, ..*self }
    }

    pub fn with_utc_datetime(&self, datetime_utc: &NaiveDateTime) -> JDateTime<'h> {
        JDateTime {
            datetime: self.timezone().from_utc_datetime(datetime_utc),
            date_format: self.date_format,
            time_format: self.time_format,
            print_utc_marker: self.print_utc_marker,
        }
    }

    /// Gets how precise this datetime is if it were to be printed; its resolution.
    pub fn precision(&self) -> DateTimePrecision {
        match self.time_format {
            Some(tf) => tf.precision(),
            None => match self.date_format {
                Some(df) => df.precision(),
                None => DateTimePrecision::Second,
            },
        }
    }

    /// Increments the datetime based on the most precise component of the time or date format.
    ///
    /// Returns `None` if no time or date format is set.
    pub fn increment(&self) -> Self {
        let inc = match self.precision() {
            DateTimePrecision::Second => *self + Duration::seconds(1),
            DateTimePrecision::Minute => *self + Duration::minutes(1),
            DateTimePrecision::Hour => *self + Duration::hours(1),
            DateTimePrecision::Day => *self + Duration::days(1),
            DateTimePrecision::Month => {
                // This should always be fine in UTC, and if we are at the precision of
                // months, our day should be 1.
                let utc_date =
                    self.datetime.naive_utc().checked_add_months(Months::new(1)).unwrap();
                self.with_utc_datetime(&utc_date)
            }
            DateTimePrecision::Year => {
                // Convert to and from UTC in case the date with simply the year being incremented
                // would not be valid in the local timezone. It should always be valid in UTC unless
                // we are out of range.
                let utc_date = self.datetime.naive_utc().with_year(self.year() + 1).unwrap();
                self.with_utc_datetime(&utc_date)
            }
            DateTimePrecision::Week => *self + Duration::days(7),
        };
        inc
    }
}

impl fmt::Display for JDateTime<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let naive_local = self.datetime.naive_local();

        match (self.date_format, self.time_format) {
            (Some(df), Some(tf)) => {
                write!(
                    f,
                    "{}",
                    DelayedFormat::new(Some(naive_local.date()), None, df.items().iter())
                )?;
                write!(
                    f,
                    " {}",
                    DelayedFormat::new(None, Some(naive_local.time()), tf.items().iter())
                )?;
            }
            (Some(df), None) => {
                write!(
                    f,
                    "{}",
                    DelayedFormat::new(Some(naive_local.date()), None, df.items().iter())
                )?;
            }
            (None, Some(tf)) => {
                write!(
                    f,
                    "{}",
                    DelayedFormat::new(None, Some(naive_local.time()), tf.items().iter())
                )?;
            }
            (None, None) => {}
        }
        if self.print_utc_marker {
            write!(f, "Z")?;
        }
        Ok(())
    }
}

impl From<JDateTime<'_>> for Cell<'_> {
    fn from(value: JDateTime) -> Self {
        Cell::from(value.to_string())
    }
}

impl PartialEq for JDateTime<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.datetime == other.datetime
    }
}

impl Eq for JDateTime<'_> {}

impl PartialOrd for JDateTime<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for JDateTime<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.datetime.cmp(&other.datetime)
    }
}

impl Deref for JDateTime<'_> {
    type Target = DateTime<Tz>;

    fn deref(&self) -> &Self::Target {
        &self.datetime
    }
}

impl Add<Duration> for JDateTime<'_> {
    type Output = Self;

    fn add(self, rhs: Duration) -> Self::Output {
        JDateTime { datetime: self.datetime.add(rhs), ..self }
    }
}

impl Sub<Duration> for JDateTime<'_> {
    type Output = Self;

    fn sub(self, rhs: Duration) -> Self::Output {
        JDateTime { datetime: self.datetime.sub(rhs), ..self }
    }
}

/// For implementing date ranges.
#[derive(Debug, Clone, Copy)]
pub struct JDateTimeRange<'h> {
    /// The first datetime in the range.
    start: JDateTime<'h>,
    /// The last datetime in the range, not inclusive.
    /// This is optional to indicate when no end was originally specified.
    end: Option<JDateTime<'h>>,
    /// Indicator to print only the common date parts of the range.
    brief_format: bool,
}

impl<'h> JDateTimeRange<'h> {
    pub fn new(start: JDateTime<'h>, end: Option<JDateTime<'h>>) -> Self {
        assert!(end.map(|e| start <= e).unwrap_or(true), "{} must be <= {}", start, end.unwrap());
        assert!(start.date_format.is_some(), "Start date must have a date format");

        Self { start, end, brief_format: false }
    }

    pub fn date_format(&self) -> &'h DateFormat<'h> {
        self.start.date_format.unwrap()
    }

    pub fn time_format(&self) -> Option<&'h TimeFormat<'h>> {
        self.start.time_format
    }

    pub fn timezone(&self) -> Tz {
        self.start.timezone()
    }

    pub fn brief_format(&self) -> bool {
        self.brief_format
    }

    /// Sets whether the range is printed in brief mode, i.e. just the date parts that
    /// encapsulate the range. E.g. '2019-01' instead of '2019-01-01..2019-01-02'.
    pub fn set_brief_format(&mut self, brief: bool) {
        self.brief_format = brief;
    }

    pub fn with_brief_format(mut self, brief: bool) -> Self {
        self.set_brief_format(brief);
        self
    }

    pub fn with_time_format(mut self, time_format: &'h TimeFormat<'h>) -> Self {
        self.start.time_format = Some(time_format);
        if let Some(end) = self.end.as_mut() {
            end.time_format = Some(time_format);
        }
        self
    }

    pub fn without_time(mut self) -> Self {
        self.start.time_format = None;

        if self.start.date().date == self.end().date().date {
            self.end = None;
        } else if let Some(end) = self.end.as_mut() {
            end.time_format = None;
        }
        self
    }

    /// Gets where the datetime range starts.
    pub fn start(&self) -> JDateTime<'h> {
        self.start
    }

    pub fn set_start(&mut self, start: JDateTime<'h>) {
        assert!(start < self.end(), "Start must be < end");

        self.start = start;
    }

    /// Gets where the datetime range ends, exclusive.
    pub fn end(&self) -> JDateTime<'h> {
        match self.end {
            Some(end) => end,
            None => self.start.increment(),
        }
    }

    pub fn end_opt(&self) -> Option<JDateTime<'h>> {
        self.end
    }

    pub fn set_end(&mut self, end: JDateTime<'h>) {
        assert!(self.start < end, "Start must be < end");

        self.end = Some(end);
    }

    pub fn intersects(&self, other: &Self) -> bool {
        self.start <= other.end() && self.end() >= other.start()
    }

    fn format_full(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.end {
            Some(end) => {
                // There are no times and the end date is just until the next day,
                // so no need to print the end as well.
                if self.start.time_format.is_none()
                    && end.time_format().is_none()
                    && end.date().date == self.start.date().date + Duration::days(1)
                {
                    write!(f, "{}", self.start)
                } else {
                    write!(f, "{}..{}", self.start, end)
                }
            }
            None => write!(f, "{}", self.start),
        }
    }

    fn format_brief(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.end {
            Some(end) => {
                // The end can be implied from the start. Just print the start.
                if self.start.increment() == end {
                    return write!(f, "{}", self.start);
                }
                if self.start.year() == end.year() {
                    if self.start.month() == end.month() {
                        if self.start.day() == end.day() {
                            write!(
                                f,
                                "{}",
                                DelayedFormat::new(
                                    Some(self.start.date().date),
                                    None,
                                    self.start.date_format.unwrap().items().iter(),
                                )
                            )
                        } else {
                            write!(
                                f,
                                "{}",
                                DelayedFormat::new(
                                    Some(self.start.date().date),
                                    None,
                                    self.start.date_format.unwrap().items_no_day(),
                                )
                            )
                        }
                    } else {
                        write!(
                            f,
                            "{}",
                            DelayedFormat::new(
                                Some(self.start.date().date),
                                None,
                                self.start.date_format.unwrap().items_no_day_no_month(),
                            )
                        )
                    }
                } else {
                    self.format_full(f)
                }
            }
            None => {
                write!(f, "{}", self.start)
            }
        }
    }

    /// Converts this range to have the timezone, date format and time format
    /// specified by the arguments.
    ///
    /// This is a rather complex task as we want to retain the format of the supplied range as far
    /// as possible.
    ///
    /// # Examples
    /// * 2000-06-01 BST -> 2000-05-31 23:00:00..2000-06-01 23:00:00 UTC
    pub fn convert_datetime_range(&self, dt_args: &'h DateTimeArguments) -> JDateTimeRange<'h> {
        let new_start_time =
            dt_args.timezone.from_utc_datetime(&self.start().datetime().naive_utc());
        // Use a time format if the range has one or if the time is not midnight.
        let use_start_tf = self.start().time_format().is_some()
            || new_start_time.hour() != 0
            || new_start_time.minute() != 0
            || new_start_time.second() != 0;
        let start = JDateTime::from_datetime(
            new_start_time,
            Some(&dt_args.date_format),
            if use_start_tf { Some(&dt_args.time_format) } else { None },
        );

        let new_end_time = dt_args.timezone.from_utc_datetime(&self.end().datetime().naive_utc());
        let use_end_df = self.end_opt().and_then(|e| e.date_format()).is_some()
            || new_end_time.day() != new_start_time.day();
        let use_end_tf = self.end_opt().and_then(|e| e.time_format()).is_some()
            || new_end_time.hour() != 0
            || new_end_time.minute() != 0
            || new_end_time.second() != 0;
        let implied_end = JDateTime::from_datetime(
            new_end_time,
            if use_end_df { Some(&dt_args.date_format) } else { None },
            if use_end_tf { Some(&dt_args.time_format) } else { None },
        );
        if self.end_opt().is_some() {
            JDateTimeRange::new(start, Some(implied_end))
        } else {
            // The specified range does not have an end but the converted range may require one
            // if the precision changes in the new timezone.
            if self.start().precision() != start.precision() {
                JDateTimeRange::new(start, Some(implied_end))
            } else {
                JDateTimeRange::new(start, None)
            }
        }
    }
}

impl<'h> fmt::Display for JDateTimeRange<'h> {
    /// Formats the common components in the date range. E.g. just the year, or the range if there
    /// are no components in common.
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if self.brief_format() { self.format_brief(f) } else { self.format_full(f) }
    }
}

impl From<JDateTimeRange<'_>> for table::Cell<'_> {
    fn from(range: JDateTimeRange) -> Self {
        let mut cell = table::Cell::new(range.to_string());
        cell.set_wrap_policy(WrapPolicy::With(|cell| match cell.as_leaf().unwrap().find("..") {
            Some(idx) => WrapPolicy::Position(idx + 2),
            None => WrapPolicy::Never,
        }));
        cell
    }
}

impl From<JDateTimeRange<'_>> for Yaml {
    fn from(range: JDateTimeRange) -> Self {
        let mut map = Hash::new();
        // Format in ISO UTC for consistency.
        map.insert(
            Yaml::String("start".to_string()),
            Yaml::String(range.start().datetime().naive_utc().format("%FT%TZ").to_string()),
        );
        map.insert(
            Yaml::String("end".to_string()),
            Yaml::String(range.end().datetime().naive_utc().format("%FT%TZ").to_string()),
        );
        Yaml::Hash(map)
    }
}

impl<'h> PartialEq for JDateTimeRange<'h> {
    fn eq(&self, other: &Self) -> bool {
        self.start == other.start && self.end() == other.end()
    }
}

impl<'h> Eq for JDateTimeRange<'h> {}

impl PartialOrd for JDateTimeRange<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for JDateTimeRange<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.start.cmp(&other.start) {
            Ordering::Equal => self.end().cmp(&other.end()),
            ord => ord,
        }
    }
}

impl Add for JDateTimeRange<'_> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        if self.timezone() != rhs.timezone() {
            // This may be something to support in the future if required by first converting
            // the rhs to the lhs' timezone.
            panic!("Cannot add datetime ranges having different timezones");
        }
        JDateTimeRange {
            start: self.start.min(rhs.start),
            end: match (self.end, rhs.end) {
                (Some(e1), Some(e2)) => Some(e1.max(e2)),
                (Some(e1), None) => Some(e1),
                (None, Some(e2)) => Some(e2),
                (None, None) => None,
            },
            brief_format: self.brief_format,
        }
    }
}

#[derive(Debug, Clone)]
pub struct DateAndTime<'h> {
    datetime_range: JDateTimeRange<'h>,
    datetime_aux: Option<JDateTime<'h>>,
}

impl<'h> DateAndTime<'h> {
    pub fn new(range: JDateTimeRange<'h>, aux: Option<JDateTime<'h>>) -> Self {
        Self { datetime_range: range, datetime_aux: aux }
    }

    pub fn datetime_range(&self) -> JDateTimeRange<'h> {
        self.datetime_range
    }

    pub fn datetime_from(&self) -> DateTime<Tz> {
        self.datetime_range.start.datetime()
    }

    pub fn datetime_to(&self) -> DateTime<Tz> {
        self.datetime_range.end().datetime()
    }

    pub fn date_from(&self) -> JDate<'h> {
        self.datetime_range.start.date()
    }

    pub fn date_to(&self) -> JDate<'h> {
        self.datetime_range.end().date()
    }

    pub fn aux_date_time(&self) -> Option<JDateTime<'h>> {
        self.datetime_aux
    }

    pub fn aux_date(&self) -> Option<NaiveDate> {
        self.datetime_aux.as_ref().map(|adt| adt.datetime().naive_local().date())
    }

    pub fn aux_date_or_date_from(&self) -> JDate<'h> {
        self.datetime_aux
            .as_ref()
            .map(|adt| adt.date())
            .unwrap_or_else(|| self.datetime_range.start.date())
    }

    pub fn aux_time(&self) -> Option<NaiveTime> {
        self.datetime_aux.as_ref().map(|adt| adt.time())
    }

    pub fn aux_time_or_time_from(&self) -> NaiveTime {
        self.datetime_aux
            .as_ref()
            .map(|adt| adt.time())
            .unwrap_or_else(|| self.datetime_range.start.time())
    }

    pub fn time_from(&self) -> NaiveTime {
        self.datetime_range.start.time()
    }

    pub fn time_to(&self) -> NaiveTime {
        self.datetime_range.end().time()
    }

    pub fn with_timezone(&self, tz: Tz) -> Self {
        DateAndTime {
            datetime_range: JDateTimeRange::new(
                self.datetime_range.start.with_timezone(tz),
                self.datetime_range.end.map(|dt| dt.with_timezone(tz)),
            ),
            datetime_aux: self.datetime_aux.as_ref().map(|aux| aux.with_timezone(tz)),
        }
    }

    /// Creates an inclusive range of time depending on the entry input.
    /// Examples:
    /// 2019-01-01 => 2019-01-01 00:00:00..=2019-01-01 23:59:59
    /// 2019-01-01 15:00:00 => 2019-01-01 15:00:00..=2019-01-01 15:00:00
    /// 2019-01-01 15:00:00..16:00:00 => 2019-01-01 15:00:00..=2019-01-01 16:00:00
    /// 2019-01-01 15:00:00..01:00:00 => 2019-01-01 15:00:00..=2019-01-02 01:00:00
    pub fn utc_range(&self) -> Range<NaiveDateTime> {
        self.datetime_range.start.datetime().naive_utc()
            ..self.datetime_range.end().datetime().naive_utc()
    }

    pub fn average(&self) -> JDateTime<'h> {
        let duration: Duration =
            self.datetime_range.end().datetime() - self.datetime_range.start.datetime();
        JDateTime::from_datetime(
            self.datetime_range.start.datetime() + duration / 2,
            Some(self.datetime_range().date_format()),
            self.datetime_range().time_format(),
        )
    }

    /// Gets the mid point between the time range.
    pub fn utc_average(&self) -> NaiveDateTime {
        let range = self.utc_range();
        let duration = range.end - range.start;
        range.start + duration
    }

    /// Gets whether this date/time intersects another date/time based on the date (ignoring aux date).
    pub fn intersects_date(&self, other: &DateAndTime<'h>) -> bool {
        let r1 = self.utc_range();
        let r2 = other.utc_range();
        r1.intersection(&r2).is_some()
    }

    pub fn union(&self, other: &DateAndTime<'h>) -> DateAndTime<'h> {
        // It is easier to convert both to UTC and clean up after
        let t1 = self.utc_range();
        let t2 = other.utc_range();
        DateAndTime {
            datetime_range: JDateTimeRange::new(
                JDateTime {
                    datetime: Tz::UTC.from_utc_datetime(&t1.start.min(t2.start)),
                    date_format: self.datetime_range.start.date_format,
                    time_format: self.datetime_range.start.time_format,
                    print_utc_marker: true,
                },
                Some(JDateTime {
                    datetime: Tz::UTC.from_utc_datetime(&t1.end.max(t2.end)),
                    date_format: self.datetime_range.start.date_format,
                    time_format: self.datetime_range.start.time_format,
                    print_utc_marker: true,
                }),
            ),
            datetime_aux: None,
        }
    }

    pub fn aux_utc_time(&self) -> Option<NaiveDateTime> {
        self.datetime_aux.as_ref().map(|adt| adt.datetime().naive_utc())
    }
}

impl fmt::Display for DateAndTime<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.datetime_range)?;

        // Aux Time
        if let Some(datetime_aux) = &self.datetime_aux {
            write!(f, "={datetime_aux}")?;
        }

        Ok(())
    }
}

enum DateFormatMode {
    Date,
    Time,
}

#[derive(Debug, Clone, Eq)]
struct DateTimeFormatInner<'h> {
    format_str: Cow<'h, str>,
    items: Vec<Item<'h>>,
    // The number of ascii space chars
    spaces: usize,
}

impl<'h> DateTimeFormatInner<'h> {
    pub fn parse_format<I>(mode: DateFormatMode) -> impl FnMut(I) -> JParseResult<I, Self>
    where
        I: TextInput<'h>,
    {
        move |input: I| {
            let (rem, items) = match mode {
                DateFormatMode::Date => {
                    context("Invalid date format", DateFormat::translate)(input.clone())?
                }
                DateFormatMode::Time => {
                    context("Invalid time format", TimeFormat::translate)(input.clone())?
                }
            };
            let mut total_spaces = 0;
            for item in items.clone() {
                match item {
                    Item::OwnedLiteral(s) => {
                        total_spaces += s.chars().filter(|c| *c == ' ').count()
                    }
                    Item::Literal(s) | Item::Space(s) => {
                        total_spaces += s.chars().filter(|c| *c == ' ').count()
                    }
                    _ => {}
                }
            }
            let inner =
                Self { format_str: Cow::Borrowed(input.text()), spaces: total_spaces, items };
            Ok((rem, inner))
        }
    }

    /// The number of ASCII space characters to scan when parsing.
    pub fn spaces(&self) -> usize {
        self.spaces
    }

    /// Gets the max width in number of characters that dates/times in this format may
    /// consume.
    pub fn max_formatting_width(&self) -> usize {
        let mut max_width = 0;
        for item in self.items.iter() {
            match item {
                Item::OwnedLiteral(s) => max_width += s.chars().count(),
                Item::Literal(s) => max_width += s.chars().count(),
                Item::Numeric(num, _pad) => match num {
                    Numeric::Year => max_width += 4,
                    Numeric::YearMod100 => max_width += 2,
                    Numeric::Month => max_width += 2,
                    Numeric::Day => max_width += 2,
                    Numeric::Hour => max_width += 2,
                    Numeric::Minute => max_width += 2,
                    Numeric::Second => max_width += 2,
                    _ => {}
                },
                Item::Fixed(fixed) => match fixed {
                    Fixed::LongMonthName => max_width += 9,
                    Fixed::ShortMonthName => max_width += 3,
                    _ => {}
                },
                _ => {}
            }
        }
        max_width
    }

    fn items_filtered<'a, F>(
        &'a self,
        filtered: F,
    ) -> impl Iterator<Item = &'a Item<'h>> + Clone + 'a
    where
        F: Fn(usize) -> bool + Clone + 'a,
    {
        let mut iter = self.items.iter().enumerate();
        iter::from_fn(move || {
            for (i, item) in iter.by_ref() {
                if filtered(i) {
                    continue;
                } else {
                    match item {
                        Item::Literal(_) | Item::OwnedLiteral(_) => {
                            if filtered(i + 1) || (i == 1 && filtered(0)) {
                                continue;
                            }
                        }
                        _ => {}
                    }
                    return Some(item);
                }
            }
            None
        })
    }

    /// Helper to read the date/time string to parse as the underlying parser is required to read
    /// the whole of its input. A date/time string in a particular format may be a variable length, so a better
    /// heuristic is to count the number of expected spaces instead. For example: "DD MMMM YY" means two spaces to read.
    /// This assumes no extra spaces in date parts e.g. month names.
    fn date_or_time_string<'i, I: TextInput<'i>>(&self) -> impl FnMut(I) -> IParseResult<'i, I, I> {
        let mut spaces_to_go = self.spaces();
        move |input: I| {
            let mut n: usize = 0;
            for c in input.text().chars() {
                match c {
                    ' ' if spaces_to_go > 0 => spaces_to_go -= 1,
                    ' ' => break,
                    // Date range separator
                    '.' => break,
                    // Aux date separator
                    '=' => break,
                    // Newline reached
                    '\n' | '\r' => break,
                    _ => {}
                }
                n += 1;
            }
            if spaces_to_go > 0 {
                return Err(NomErr::Error(IParseError::new(
                    "End of line reached before date or time could be read",
                    input,
                )));
            }
            take(n)(input)
        }
    }

    /// Gets the format's precision i.e. the precision this format is most precise to.
    pub fn precision(&self) -> DateTimePrecision {
        let mut smallest = DateTimePrecision::Year;
        for item in self.items.iter() {
            if let Item::Numeric(num, ..) = item {
                match num {
                    Numeric::Hour if DateTimePrecision::Hour < smallest => {
                        smallest = DateTimePrecision::Hour
                    }
                    Numeric::Minute if DateTimePrecision::Minute < smallest => {
                        smallest = DateTimePrecision::Minute
                    }
                    Numeric::Second if DateTimePrecision::Second < smallest => {
                        smallest = DateTimePrecision::Second
                    }
                    Numeric::Day if DateTimePrecision::Day < smallest => {
                        smallest = DateTimePrecision::Day
                    }
                    Numeric::WeekFromMon | Numeric::WeekFromMon | Numeric::WeekFromSun
                        if DateTimePrecision::Week < smallest =>
                    {
                        smallest = DateTimePrecision::Week
                    }
                    Numeric::Month if DateTimePrecision::Month < smallest => {
                        smallest = DateTimePrecision::Month
                    }
                    _ => {}
                }
            }
        }
        smallest
    }

    /// Leaks the inner values to create a `DateFormat`
    pub fn into_owned(self) -> DateTimeFormatInner<'static> {
        let mut items = vec![];
        for item in self.items.into_iter() {
            match item {
                Item::OwnedLiteral(s) => items.push(Item::OwnedLiteral(s.clone())),
                Item::Literal(s) => items.push(Item::OwnedLiteral(s.to_string().into_boxed_str())),
                Item::Numeric(num, pad) => items.push(Item::Numeric(num.clone(), pad)),
                Item::Fixed(fixed) => items.push(Item::Fixed(fixed.clone())),
                Item::Space(s) => items.push(Item::OwnedSpace(s.to_string().into_boxed_str())),
                Item::OwnedSpace(s) => items.push(Item::OwnedSpace(s.clone())),
                Item::Error => items.push(Item::Error),
            }
        }
        DateTimeFormatInner {
            format_str: Cow::Owned(self.format_str.to_string()),
            items,
            spaces: self.spaces,
        }
    }
}

impl PartialEq for DateTimeFormatInner<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.format_str == other.format_str
    }
}

impl PartialOrd for DateTimeFormatInner<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for DateTimeFormatInner<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.format_str.cmp(&other.format_str)
    }
}

/// Dates format specifies that the date and time formats are fixed width to make parsing possible. In practical terms,
/// this shouldn't be a big limitation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DateFormat<'h> {
    inner: DateTimeFormatInner<'h>,
}

impl<'h> DateFormat<'h> {
    pub fn parse_format<I>(input: I) -> JParseResult<I, Self>
    where
        I: TextInput<'h>,
    {
        DateTimeFormatInner::parse_format(DateFormatMode::Date)(input)
            .map(|(rem, inner)| (rem, Self { inner }))
    }

    fn translate<I>(input: I) -> JParseResult<I, Vec<Item<'h>>>
    where
        I: TextInput<'h>,
    {
        let mut items = vec![];

        let lit = |input: I| -> IParseResult<I, I> {
            none_of("\r\n")(input.clone())?;
            take(1usize)(input)
        };

        let input = match_parsers!(input,
            tag_no_case("YYYY") => |_| Ok(items.push(Item::Numeric(Numeric::Year, Pad::None))),
            tag_no_case("YY") => |_| Ok(items.push(Item::Numeric(Numeric::YearMod100, Pad::None))),
            tag_no_case("MMMM") => |_| Ok(items.push(Item::Fixed(Fixed::LongMonthName))),
            tag_no_case("MMM") => |_| Ok(items.push(Item::Fixed(Fixed::ShortMonthName))),
            tag_no_case("MM") => |_| Ok(items.push(Item::Numeric(Numeric::Month, Pad::Zero))),
            tag_no_case("M") => |_| Ok(items.push(Item::Numeric(Numeric::Month, Pad::None))),
            tag_no_case("DD") => |_| Ok(items.push(Item::Numeric(Numeric::Day, Pad::Zero))),
            tag_no_case("D") => |_| Ok(items.push(Item::Numeric(Numeric::Day, Pad::None))),
            space1 => |sp: I| Ok(items.push(Item::Space(sp.text()))),
            lit => |lit: I| Ok(items.push(Item::Literal(lit.text()))),
            rest => |rest: I| Err(NomErr::Failure(rest.into_err("Date format must not span multiple lines")))
        )?
            .0;
        Ok((input, items))
    }

    pub fn items(&self) -> &Vec<Item<'h>> {
        &self.inner.items
    }

    /// The number of ASCII space characters to scan when parsing.
    pub fn spaces(&self) -> usize {
        self.inner.spaces
    }

    /// Gets the max width in number of characters that dates/times in this format may
    /// consume.
    pub fn max_formatting_width(&self) -> usize {
        self.inner.max_formatting_width()
    }

    /// Gets an iterator over the items that does not include the day or
    /// the day separator.
    pub fn items_no_day(&self) -> impl Iterator<Item = &Item<'h>> + Clone {
        self.inner.items_filtered(move |i| {
            matches!(self.inner.items.get(i), Some(Item::Numeric(Numeric::Day, _)))
        })
    }

    pub fn items_no_day_no_month(&self) -> impl Iterator<Item = &Item<'h>> + Clone {
        self.inner.items_filtered(move |i| {
            matches!(
                self.inner.items.get(i),
                Some(Item::Numeric(Numeric::Day, _)) | Some(Item::Numeric(Numeric::Month, _))
            )
        })
    }

    /// Helper to read the date/time string to parse as the underlying parser is required to read
    /// the whole of its input. A date/time string in a particular format may be a variable length, so a better
    /// heuristic is to count the number of expected spaces instead. For example: "DD MMMM YY" means two spaces to read.
    /// This assumes no extra spaces in date parts e.g. month names.
    fn date_string<'i, I: TextInput<'i>>(&self) -> impl FnMut(I) -> IParseResult<'i, I, I> {
        self.inner.date_or_time_string()
    }

    pub fn format_str(&self) -> &str {
        self.inner.format_str.as_ref()
    }

    /// Gets the format's precision i.e. the precision this format is most precise to.
    pub fn precision(&self) -> DateTimePrecision {
        self.inner.precision()
    }

    /// Leaks the inner values to create a `DateFormat`
    pub fn into_owned(self) -> DateFormat<'static> {
        DateFormat { inner: self.inner.into_owned() }
    }
}

impl FromStr for DateFormat<'static> {
    type Err = JournError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        DateFormat::parse_format(s).finish().map(|(_, df)| df.into_owned())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TimeFormat<'h> {
    inner: DateTimeFormatInner<'h>,
}

impl<'h> TimeFormat<'h> {
    pub fn parse_format<I>(input: I) -> JParseResult<I, Self>
    where
        I: TextInput<'h>,
    {
        DateTimeFormatInner::parse_format(DateFormatMode::Time)(input)
            .map(|(rem, inner)| (rem, Self { inner }))
    }

    fn translate<I>(input: I) -> JParseResult<I, Vec<Item<'h>>>
    where
        I: TextInput<'h>,
    {
        let mut items = vec![];

        let lit = |input: I| -> IParseResult<'h, I, I> {
            none_of("\r\n")(input.clone())?;
            take(1usize)(input)
        };

        let input = match_parsers!(input,
            tag_no_case("HH") => |_| Ok(items.push(Item::Numeric(Numeric::Hour, Pad::Zero))),
            tag_no_case("H") => |_| Ok(items.push(Item::Numeric(Numeric::Hour, Pad::None))),
            tag_no_case("II") => |_| Ok(items.push(Item::Numeric(Numeric::Hour12, Pad::Zero))),
            tag_no_case("I") => |_| Ok(items.push(Item::Numeric(Numeric::Hour12, Pad::None))),
            tag_no_case("MM") => |_| Ok(items.push(Item::Numeric(Numeric::Minute, Pad::Zero))),
            tag_no_case("M") => |_| Ok(items.push(Item::Numeric(Numeric::Minute, Pad::None))),
            tag_no_case("SS") => |_| Ok(items.push(Item::Numeric(Numeric::Second, Pad::Zero))),
            tag_no_case("S") => |_| Ok(items.push(Item::Numeric(Numeric::Second, Pad::None))),
            tag("P") => |_| Ok(items.push(Item::Fixed(Fixed::UpperAmPm))),
            tag("p") => |_| Ok(items.push(Item::Fixed(Fixed::LowerAmPm))),
            space1 => |sp: I| Ok(items.push(Item::Space(sp.text()))),
            lit => |lit: I| Ok(items.push(Item::Literal(lit.text()))),
            rest => |rest: I| Err(NomErr::Failure(rest.into_err("Time format must not span multiple lines")))
        )?
            .0;
        Ok((input, items))
    }

    pub fn items(&self) -> &Vec<Item<'h>> {
        &self.inner.items
    }

    /// The number of ASCII space characters to scan when parsing.
    pub fn spaces(&self) -> usize {
        self.inner.spaces
    }

    /// Gets the max width in number of characters that dates/times in this format may
    /// consume.
    pub fn max_formatting_width(&self) -> usize {
        self.inner.max_formatting_width()
    }

    /// Helper to read the date/time string to parse as the underlying parser is required to read
    /// the whole of its input. A date/time string in a particular format may be a variable length, so a better
    /// heuristic is to count the number of expected spaces instead. For example: "DD MMMM YY" means two spaces to read.
    /// This assumes no extra spaces in date parts e.g. month names.
    fn time_string<'i, I: TextInput<'i>>(&self) -> impl FnMut(I) -> IParseResult<'i, I, I> {
        self.inner.date_or_time_string()
    }

    pub fn format_str(&self) -> &str {
        self.inner.format_str.as_ref()
    }

    /// Gets the format's precision i.e. the precision this format is most precise to.
    pub fn precision(&self) -> DateTimePrecision {
        self.inner.precision()
    }

    /// Leaks the inner values to create a `DateFormat`
    pub fn into_owned(self) -> TimeFormat<'static> {
        TimeFormat { inner: self.inner.into_owned() }
    }
}

impl FromStr for TimeFormat<'_> {
    type Err = JournError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        TimeFormat::parse_format(s).finish().map(|(_, tf)| tf.into_owned())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DateTimePrecision {
    Second,
    Minute,
    Hour,
    Day,
    Week,
    Month,
    Year,
}
