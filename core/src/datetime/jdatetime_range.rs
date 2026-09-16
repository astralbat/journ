/*
 * Copyright (c) 2019-2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::configuration::Configuration;
use crate::datetime::{DateTimeFormat, JDateTime};
use crate::journal_context::JContext;
use chrono::Duration;
use chrono_tz::Tz;
use std::cmp::Ordering;
use std::fmt;
use std::ops::Add;
use std::range::{Bound, RangeBounds};

/// For implementing date ranges.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct JDateTimeRange {
    /// The first datetime in the range.
    start: JDateTime,
    end: JDateTime,
}

impl JDateTimeRange {
    pub fn new(start: JDateTime, end: Option<JDateTime>) -> Self {
        assert!(
            end.map(|e| start <= e).unwrap_or(true),
            "{:?} must be <= {:?}",
            start,
            end.unwrap()
        );

        Self { start, end: end.unwrap_or(start.increment()) }
    }

    pub fn timezone(&self) -> Tz {
        self.start.timezone()
    }

    pub fn with_timezone(&self, tz: Tz) -> Self {
        JDateTimeRange::new(self.start.with_timezone(tz), Some(self.end.with_timezone(tz)))
    }

    /// Gets where the datetime range starts.
    pub fn start(&self) -> JDateTime {
        self.start
    }

    pub fn start_ref(&self) -> &JDateTime {
        &self.start
    }

    pub fn set_start(&mut self, start: JDateTime) {
        assert!(start < self.end(), "Start must be < end");

        self.start = start;
    }

    /// Gets where the datetime range ends, exclusive.
    pub fn end(&self) -> JDateTime {
        self.end
    }

    pub fn set_end(&mut self, end: JDateTime) {
        assert!(self.start < end, "Start must be < end");

        self.end = end;
    }

    pub fn average(&self) -> JDateTime {
        let duration: Duration = self.end().datetime() - self.start().datetime();
        JDateTime::new(self.start().datetime() + duration / 2, self.start().precision())
    }

    pub fn intersects(&self, other: &Self) -> bool {
        self.start < other.end() && self.end() > other.start()
    }

    /// Gets whether this range is a super range of the other. i.e. whether
    /// the `self` range contains the `other`.
    ///
    /// This will always be `true` when both ranges are equal.
    pub fn contains(&self, other: &Self) -> bool {
        self.start() <= other.start() && self.end() >= other.end()
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

        if self.start.increment() != self.end {
            write!(writer, "..")?;
            match entry_config {
                Some(config) => write!(writer, "{}", self.end.format_for_entry(config)),
                None => write!(writer, "{}", self.end.format(formatter)),
            }?;
        }
        Ok(())
    }
}

impl fmt::Display for JDateTimeRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let cmd = JContext::get().cmd();
        self.write_internal(f, cmd.datetime_fmt_cmd().datetime_format_or_default(), None)
    }
}

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
        JDateTimeRange { start: self.start.min(rhs.start), end: self.end.max(rhs.end) }
    }
}

impl RangeBounds<JDateTime> for JDateTimeRange {
    fn start_bound(&self) -> Bound<&JDateTime> {
        Bound::Included(&self.start)
    }

    fn end_bound(&self) -> Bound<&JDateTime> {
        Bound::Excluded(&self.end)
    }
}
