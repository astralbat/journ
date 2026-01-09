/*
 * Copyright (c) 2019-2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::configuration::Configuration;
use crate::datetime::{JDate, JDateTime, JDateTimeRange};
use crate::ext::RangeBoundsExt;
use chrono::{DateTime, Duration, NaiveDate, NaiveDateTime, NaiveTime};
use chrono_tz::Tz;
use std::fmt;
use std::ops::Range;

#[derive(Debug, Clone)]
pub struct DateAndTime {
    datetime_range: JDateTimeRange,
    datetime_aux: Option<JDateTime>,
}

impl DateAndTime {
    pub fn new(range: JDateTimeRange, aux: Option<JDateTime>) -> Self {
        Self { datetime_range: range, datetime_aux: aux }
    }

    pub fn datetime_range(&self) -> JDateTimeRange {
        self.datetime_range
    }

    /// Writes the entry date and time with the entry `config`.
    pub fn write<'h, W: fmt::Write>(
        &self,
        writer: &mut W,
        config: &Configuration<'h>,
    ) -> fmt::Result {
        self.datetime_range.write_for_entry(writer, config)?;

        // Aux Time
        if let Some(datetime_aux) = &self.datetime_aux {
            write!(writer, "={}", datetime_aux.format_for_entry(config))?;
        }
        Ok(())
    }

    pub fn from(&self) -> JDateTime {
        self.datetime_range.start()
    }

    pub fn datetime_from(&self) -> DateTime<Tz> {
        self.datetime_range.start().datetime()
    }

    pub fn datetime_to(&self) -> DateTime<Tz> {
        self.datetime_range.end().datetime()
    }

    pub fn date_from(&self) -> JDate {
        self.datetime_range.start().date()
    }

    pub fn date_to(&self) -> JDate {
        self.datetime_range.end().date()
    }

    pub fn aux_date_time(&self) -> Option<JDateTime> {
        self.datetime_aux
    }

    pub fn aux_date(&self) -> Option<NaiveDate> {
        self.datetime_aux.as_ref().map(|adt| adt.datetime().naive_local().date())
    }

    pub fn aux_date_or_date_from(&self) -> JDate {
        self.datetime_aux
            .as_ref()
            .map(|adt| adt.date())
            .unwrap_or_else(|| self.datetime_range.start().date())
    }

    pub fn aux_time(&self) -> Option<NaiveTime> {
        self.datetime_aux.as_ref().map(|adt| adt.time())
    }

    pub fn aux_time_or_time_from(&self) -> NaiveTime {
        self.datetime_aux
            .as_ref()
            .map(|adt| adt.time())
            .unwrap_or_else(|| self.datetime_range.start().time())
    }

    pub fn time_from(&self) -> NaiveTime {
        self.datetime_range.start().time()
    }

    pub fn time_to(&self) -> NaiveTime {
        self.datetime_range.end().time()
    }

    pub fn with_timezone(&self, tz: Tz) -> Self {
        DateAndTime {
            datetime_range: self.datetime_range.with_timezone(tz),
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
        self.datetime_range.start().datetime().naive_utc()
            ..self.datetime_range.end().datetime().naive_utc()
    }

    pub fn average(&self) -> JDateTime {
        let duration: Duration =
            self.datetime_range.end().datetime() - self.datetime_range.start().datetime();
        JDateTime::new(
            self.datetime_range.start().datetime() + duration / 2,
            self.datetime_range.start().precision(),
        )
    }

    /// Gets the mid point between the time range.
    pub fn utc_average(&self) -> NaiveDateTime {
        let range = self.utc_range();
        let duration = range.end - range.start;
        range.start + duration
    }

    /// Gets whether this date/time intersects another date/time based on the date (ignoring aux date).
    pub fn intersects_date(&self, other: &DateAndTime) -> bool {
        let r1 = self.utc_range();
        let r2 = other.utc_range();
        r1.intersection(&r2).is_some()
    }
}

/*
impl fmt::Display for DateAndTime {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.write(f, "{}")
        write!(f, "{}", self.datetime_range)?;

        // Aux Time
        if let Some(datetime_aux) = &self.datetime_aux {
            write!(f, "={datetime_aux}")?;
        }

        Ok(())
    }
}*/
