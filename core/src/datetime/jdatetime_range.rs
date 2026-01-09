/*
 * Copyright (c) 2019-2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::configuration::Configuration;
use crate::datetime::{DateTimeFormat, DateTimePrecision, JDateTime};
use crate::report::command::arguments::Cmd;
use chrono::Duration;
use chrono_tz::Tz;
use std::cmp::Ordering;
use std::fmt;
use std::ops::Add;

/// For implementing date ranges.
#[derive(Debug, Clone, Copy, Hash)]
pub struct JDateTimeRange {
    /// The first datetime in the range.
    start: JDateTime,
    /// The last datetime in the range, not inclusive.
    /// This is optional to indicate when no end was originally specified.
    end: Option<JDateTime>,
    /// Indicator to print only the common date parts of the range.
    brief_format: bool,
}

impl JDateTimeRange {
    pub fn new(start: JDateTime, end: Option<JDateTime>) -> Self {
        assert!(
            end.map(|e| start <= e).unwrap_or(true),
            "{:?} must be <= {:?}",
            start,
            end.unwrap()
        );

        Self { start, end, brief_format: false }
    }

    /*
    pub fn date_format(&self) -> &'h DateFormat<'h> {
        self.start.date_format.unwrap()
    }

    pub fn time_format(&self) -> Option<&'h TimeFormat<'h>> {
        self.start.time_format
    }*/

    pub fn timezone(&self) -> Tz {
        self.start.timezone()
    }

    pub fn with_timezone(&self, tz: Tz) -> Self {
        JDateTimeRange::new(self.start.with_timezone(tz), self.end.map(|dt| dt.with_timezone(tz)))
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

    /*
    pub fn with_time_format(mut self, time_format: &'h TimeFormat<'h>) -> Self {
        self.start.time_format = Some(time_format);
        if let Some(end) = self.end.as_mut() {
            end.time_format = Some(time_format);
        }
        self
    }*/

    /*
    pub fn without_time(mut self) -> Self {
        self.start.time_format = None;

        if self.start.date().date == self.end().date().date {
            self.end = None;
        } else if let Some(end) = self.end.as_mut() {
            end.time_format = None;
        }
        self
    }*/

    /// Gets where the datetime range starts.
    pub fn start(&self) -> JDateTime {
        self.start
    }

    pub fn set_start(&mut self, start: JDateTime) {
        assert!(start < self.end(), "Start must be < end");

        self.start = start;
    }

    /// Gets where the datetime range ends, exclusive.
    pub fn end(&self) -> JDateTime {
        match self.end {
            Some(end) => end,
            None => self.start.increment(),
        }
    }

    pub fn end_opt(&self) -> Option<JDateTime> {
        self.end
    }

    pub fn set_end(&mut self, end: JDateTime) {
        assert!(self.start < end, "Start must be < end");

        self.end = Some(end);
    }

    pub fn intersects(&self, other: &Self) -> bool {
        self.start <= other.end() && self.end() >= other.start()
    }

    pub fn write<'h, W: fmt::Write>(
        &self,
        writer: &mut W,
        formatter: &DateTimeFormat<'h>,
    ) -> fmt::Result {
        self.write_internal(writer, formatter, None)
    }

    pub fn write_for_entry<'h, W: fmt::Write>(
        &self,
        writer: &mut W,
        entry_config: &Configuration<'h>,
    ) -> fmt::Result {
        self.write_internal(writer, entry_config.datetime_format(), Some(entry_config))
    }

    fn write_internal<'h, W: fmt::Write>(
        &self,
        writer: &mut W,
        formatter: &DateTimeFormat<'h>,
        entry_config: Option<&Configuration<'h>>,
    ) -> fmt::Result {
        match entry_config {
            Some(config) => write!(writer, "{}", self.start.format_for_entry(config)),
            None => write!(writer, "{}", self.start.format(formatter)),
        }?;

        // There are no times and the end date is just until the next day,
        // so no need to print the end as well.
        if let Some(end) = self.end
            && (self.start.precision() > DateTimePrecision::Day
                || end.precision() > DateTimePrecision::Day
                || end.date().date != self.start.date().date + Duration::days(1))
        {
            write!(writer, "..")?;
            match entry_config {
                Some(config) => write!(writer, "{}", end.format_for_entry(config)),
                None => write!(writer, "{}", end.format(formatter)),
            }?;
        }
        Ok(())
    }

    /*
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
                                self.start.date_format.unwrap().format(self.start.date().date)
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
    }*/

    /*
    /// Converts this range to have the timezone, date format and time format
    /// specified by the arguments.
    ///
    /// This is a rather complex task as we want to retain the format of the supplied range as far
    /// as possible.
    ///
    /// # Examples
    /// * 2000-06-01 BST -> 2000-05-31 23:00:00..2000-06-01 23:00:00 UTC
    pub fn convert_datetime_range(&self, dt_cmd: &'h DateTimeFormatCommand) -> JDateTimeRange<'h> {
        let df = dt_cmd.date_format_or_default();
        let tf = dt_cmd.time_format_or_default();
        let tz = dt_cmd.timezone_or_default();

        let new_start_time = tz.from_utc_datetime(&self.start().datetime().naive_utc());
        // Use a time format if the range has one or if the time is not midnight.
        let use_start_tf = self.start().time_format().is_some()
            || new_start_time.hour() != 0
            || new_start_time.minute() != 0
            || new_start_time.second() != 0;
        let start = JDateTime::from_datetime(
            new_start_time,
            DateTimeFormat::from((Some(df), if use_start_tf { Some(tf) } else { None })),
        );

        let new_end_time = tz.from_utc_datetime(&self.end().datetime().naive_utc());
        let use_end_df = self.end_opt().and_then(|e| e.date_format()).is_some()
            || new_end_time.day() != new_start_time.day();
        let use_end_tf = self.end_opt().and_then(|e| e.time_format()).is_some()
            || new_end_time.hour() != 0
            || new_end_time.minute() != 0
            || new_end_time.second() != 0;
        let implied_end = JDateTime::from_datetime(
            new_end_time,
            if use_end_df { Some(df) } else { None },
            if use_end_tf { Some(tf) } else { None },
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
    }*/
}

impl fmt::Display for JDateTimeRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let cmd = Cmd::get();
        self.write_internal(f, cmd.datetime_fmt_cmd().datetime_format_or_default(), None)
    }
}

/*
impl From<JDateTimeRange<'_>> for Yaml {
    fn from(range: JDateTimeRange) -> Self {
        let mut map = yaml_rust2::yaml::Hash::new();
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
}*/

impl PartialEq for JDateTimeRange {
    fn eq(&self, other: &Self) -> bool {
        self.start == other.start && self.end() == other.end()
    }
}

impl Eq for JDateTimeRange {}

impl PartialOrd for JDateTimeRange {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for JDateTimeRange {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.start.cmp(&other.start) {
            Ordering::Equal => self.end().cmp(&other.end()),
            ord => ord,
        }
    }
}

impl Add for JDateTimeRange {
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
