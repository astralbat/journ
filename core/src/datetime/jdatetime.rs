/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::configuration::Configuration;
use crate::datetime::{DateTimeFormat, DateTimePrecision};
use crate::datetime::{JDate, JTime};
use crate::err;
use crate::error::JournResult;
use crate::error::parsing::IParseError;
use crate::parsing::IParseResult;
use crate::parsing::input::TextInput;
use crate::report::command::arguments::Cmd;
use chrono::format::{DelayedFormat, Item, Parsed, parse_and_remainder};
use chrono::{
    DateTime, Datelike, Duration, LocalResult, Months, NaiveDate, NaiveDateTime, NaiveTime,
    TimeZone, Utc,
};
use chrono_tz::Tz;
use nom::{Err as NomErr, InputLength};
use std::cmp::Ordering;
use std::fmt;
use std::hash::Hash;
use std::ops::{Add, Deref, Sub};
use std::sync::LazyLock;

#[derive(Copy, Clone, Debug)]
pub struct JDateTime {
    datetime: DateTime<Tz>,
    precision: DateTimePrecision, //pub(super) datetime_format: &'h DateTimeFormat<'h>,
                                  //print_utc_marker: bool,
}

impl JDateTime {
    pub fn new(
        datetime: DateTime<Tz>,
        precision: DateTimePrecision,
        //format: &'h DateTimeFormat<'h>,
        //print_utc_marker: bool,
    ) -> Self {
        Self { datetime, precision } //, datetime_format: format, print_utc_marker }
    }

    pub fn precision(&self) -> DateTimePrecision {
        self.precision
    }

    /// Gets the current date/time, useful for date comparisons.
    pub fn now() -> Self {
        JDateTime {
            datetime: Tz::UTC.from_utc_datetime(&Utc::now().naive_local()),
            precision: DateTimePrecision::Second,
        }
    }

    /// Parses a `JDateTime`.
    ///
    /// The input must positioned at the start of the datetime to read. The first attempt will
    /// try to read the date and time according to the configured JDateTimeFormat. If that fails,
    /// it will try to read the date only from the date items of the same format.
    pub fn parse<'h, 'i, I: TextInput<'i>>(
        dtf: &'h DateTimeFormat<'h>,
        mut tz: Tz,
    ) -> impl FnMut(I) -> IParseResult<'i, I, Self> {
        move |input: I| {
            //let (rem, datetime) = dtf.datetime_string()(input.clone())?;

            //let mut datetime = datetime.text();
            //let has_utc_marker = datetime.ends_with('Z');
            //if has_utc_marker {
            //    datetime = &datetime[0..datetime.len() - 1];
            //    tz = Tz::UTC;
            //}

            let mut parsed = Parsed::new();
            // We must be able to at least read the date part
            let mut parsed_remainder =
                parse_and_remainder(&mut parsed, input.text(), dtf.date_items()).map_err(|_| {
                    NomErr::Error(IParseError::new("Invalid datetime", input.clone()))
                })?;
            // Try and read the date/time separator and time items
            if let Ok(remainder) =
                parse_and_remainder(&mut parsed, parsed_remainder, dtf.non_date_items())
            {
                parsed_remainder = remainder;
            }
            if parsed_remainder.starts_with('Z') {
                parsed_remainder = &parsed_remainder[1..];
                tz = Tz::UTC;
            }

            let rem = input.slice(input.input_len() - parsed_remainder.input_len()..);

            let precision = DateTimePrecision::from(&parsed);
            // If the date is not complete, we assume the first day of the month/year.
            if parsed.month.is_none() {
                parsed.set_month(1).unwrap();
            }
            if parsed.day.is_none() {
                parsed.set_day(1).unwrap();
            }
            if parsed.hour_div_12().is_none() {
                parsed.set_hour(0).unwrap();
            }
            if parsed.minute().is_none() {
                parsed.set_minute(0).unwrap();
            }
            if parsed.second().is_none() {
                parsed.set_second(0).unwrap();
            }
            let datetime = parsed.to_datetime_with_timezone(&tz).map_err(|_| {
                NomErr::Error(IParseError::new("Cannot parse datetime from string", input))
            })?;

            Ok((rem, Self::new(datetime, precision)))
        }
    }

    pub fn with_time(&self, time: JTime) -> JournResult<Self> {
        Ok(Self {
            datetime: match self
                .timezone()
                .from_local_datetime(&NaiveDateTime::new(self.naive_local().date(), *time))
            {
                LocalResult::Single(t) => t,
                LocalResult::Ambiguous(t, _) => {
                    warn!("Ambiguous local datetime: {}", t);
                    t
                }
                _ => return Err(err!("Time not valid in timezone on this date")),
            },
            precision: self.precision,
            //datetime_format: dtf,
            //print_utc_marker: time.map(|t| t.print_utc_marker).unwrap_or(false),
        })
    }

    /*
    pub fn from_date_time(
        date: JDate,
        time: Option<JTime>,
        tz: Tz,
        //dtf: &'h DateTimeFormat<'h>,
    ) -> JournResult<Self> {
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
            //datetime_format: dtf,
            //print_utc_marker: time.map(|t| t.print_utc_marker).unwrap_or(false),
        })
    }*/
    /*
    pub fn from_datetime(
        datetime: DateTime<Tz>,
        //datetime_format: &'h DateTimeFormat<'h>,
    ) -> JDateTime<'h> {
        JDateTime { datetime } //, datetime_format, print_utc_marker: false }
    }*/

    pub fn datetime(&self) -> DateTime<Tz> {
        self.datetime
    }

    pub fn utc_date(&self) -> NaiveDate {
        self.datetime.naive_utc().date()
    }

    /// Gets the date in the local timezone. I.e. not a UTC date unless the timezone is UTC.
    pub fn date(&self) -> JDate {
        JDate::new(
            self.datetime.naive_local().date(),
            //self.date_format.unwrap_or(&DEFAULT_DATE_FORMAT),
        )
    }

    pub fn time(&self) -> NaiveTime {
        self.datetime.time()
    }

    pub fn format<'h, 'a>(
        &'a self,
        format: &'a DateTimeFormat<'h>,
    ) -> DelayedFormat<impl Iterator<Item = &'a Item<'h>> + Clone + 'a> {
        self.format_with_precision(format, self.precision)
    }

    pub fn format_with_precision<'h, 'a>(
        &'a self,
        format: &'a DateTimeFormat<'h>,
        precision: DateTimePrecision,
    ) -> DelayedFormat<impl Iterator<Item = &'a Item<'h>> + Clone + 'a> {
        let naive_local = self.datetime.naive_local();
        format.format(Some(naive_local.date()), Some(naive_local.time()), precision)
    }

    /*
    pub fn write<'h, W: fmt::Write>(
        &self,
        writer: &mut W,
        format: &DateTimeFormat<'h>,
    ) -> fmt::Result {
        let naive_local = self.datetime.naive_local();

        write!(
            writer,
            "{}",
            format.format(Some(naive_local.date()), Some(naive_local.time()), self.precision)
        )
    }*/
    pub fn format_for_entry<'h, 'a>(
        &'a self,
        entry_config: &'a Configuration<'h>,
    ) -> DelayedFormat<impl Iterator<Item = &'a Item<'h>> + Clone + 'a> {
        static ZULU_SUFFIX: LazyLock<Item> =
            LazyLock::new(|| Item::OwnedLiteral("Z".to_string().into_boxed_str()));

        let naive_local = self.datetime.naive_local();
        let format = entry_config.datetime_format();

        // Optionally add back stripped zulu time suffix
        let items_iter = format.items_to_precision(self.precision).chain(
            if self.datetime.timezone() == Tz::UTC && entry_config.timezone() != Tz::UTC {
                Some(&*ZULU_SUFFIX)
            } else {
                None
            },
        );

        DelayedFormat::new(Some(naive_local.date()), Some(naive_local.time()), items_iter)
    }

    /*
    pub fn formatter(&self) -> &'h DateTimeFormat<'h> {
        self.datetime_format
    }*/

    /*
    pub fn with_date_and_time_format(
        &self,
        date_format: &'h DateFormat<'h>,
        time_format: &'h TimeFormat<'h>,
    ) -> JDateTime<'h> {
        JDateTime { date_format: Some(date_format), ..*self }
    }*/
    /*
    pub fn time_format(&self) -> Option<&'h TimeFormat<'h>> {
        self.time_format
    }

    pub fn with_time_format(&self, time_format: &'h TimeFormat<'h>) -> JDateTime<'h> {
        JDateTime { time_format: Some(time_format), ..*self }
    }

    pub fn with_datetime_format(&self, datetime_format: &'h DateTimeFormat<'h>) -> JDateTime<'h> {
        JDateTime { datetime_format: Some(datetime_format), ..*self }
    }*/

    pub fn with_timezone(&self, tz: Tz) -> JDateTime {
        JDateTime {
            datetime: self.datetime.with_timezone(&tz),
            ..*self //date_format: self.date_format,
                    //time_format: self.time_format,
                    //print_utc_marker: self.print_utc_marker && tz == Tz::UTC,
        }
    }

    /// Gets the same datetime but with the time set as far back as possible towards midnight.
    /// Since midnight is not always a valid time in the timezone, this instead works by subtracting the hours, mins
    /// and secs from the datetime and then adding increments of 30 minutes until the date is the same.
    /// This heuristic approach should be good enough to be accurate in practice.
    pub fn with_earliest_time(&self) -> JDateTime {
        let time_duration =
            self.time().signed_duration_since(NaiveTime::from_hms_opt(0, 0, 0).unwrap());
        let mut approx_midnight = self.datetime - time_duration;
        while approx_midnight.naive_local().date() < self.datetime.naive_local().date() {
            // 30 mins is an educated guess. If this resolution is not enough, it can be reduced.
            approx_midnight += Duration::minutes(30);
        }
        JDateTime { datetime: approx_midnight, ..*self }
    }

    pub fn with_utc_datetime(
        &self,
        datetime_utc: &NaiveDateTime,
        precision: DateTimePrecision,
    ) -> JDateTime {
        JDateTime {
            datetime: self.timezone().from_utc_datetime(datetime_utc),
            precision, //date_format: self.date_format,
                       //time_format: self.time_format,
                       //print_utc_marker: self.print_utc_marker,
        }
    }

    /*
    /// Gets how precise this datetime is if it were to be printed; its resolution.
    pub fn precision(&self) -> DateTimePrecision {
        match self.time_format {
            Some(tf) => tf.precision(),
            None => match self.date_format {
                Some(df) => df.precision(),
                None => DateTimePrecision::Second,
            },
        }
    }*/

    /// Increments the datetime based on the most precise component of the time or date format.
    pub fn increment(&self) -> Self {
        match self.precision {
            DateTimePrecision::Second => *self + Duration::seconds(1),
            DateTimePrecision::Minute => *self + Duration::minutes(1),
            DateTimePrecision::Hour => *self + Duration::hours(1),
            DateTimePrecision::Day => *self + Duration::days(1),
            DateTimePrecision::Month => {
                // This should always be fine in UTC, and if we are at the precision of
                // months, our day should be 1.
                let utc_date =
                    self.datetime.naive_utc().checked_add_months(Months::new(1)).unwrap();
                self.with_utc_datetime(&utc_date, DateTimePrecision::Month)
            }
            DateTimePrecision::Year => {
                // Convert to and from UTC in case the date with simply the year being incremented
                // would not be valid in the local timezone. It should always be valid in UTC unless
                // we are out of range.
                let utc_date = self.datetime.naive_utc().with_year(self.year() + 1).unwrap();
                self.with_utc_datetime(&utc_date, DateTimePrecision::Year)
            }
            DateTimePrecision::Week => *self + Duration::days(7),
        }
    }

    /*
    pub fn with_date_and_time_format(
        &self,
        date_format: Option<&'h DateFormat<'h>>,
        time_format: Option<&'h TimeFormat<'h>>,
    ) -> JDateTime<'h> {
        JDateTime { date_format, time_format, ..*self }
    }*/
}

impl fmt::Display for JDateTime {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let cmd = Cmd::get();
        let dtf = cmd.datetime_fmt_cmd().datetime_format_or_default();
        let tz = cmd.datetime_fmt_cmd().timezone_or_default();
        write!(f, "{}", self.with_timezone(tz).format(dtf))
    }
}

/*
impl From<JDateTime> for SS {
    fn from(value: JDateTime) -> Self {
        let mut s = SS::new();
        fmt::write(&mut s, format_args!("{}", value)).unwrap();
        s
    }
}*/

/*
impl table2::Cell for JDateTime {
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

impl PartialEq<JDateTime> for JDateTime {
    fn eq(&self, other: &JDateTime) -> bool {
        self.datetime == other.datetime
    }
}

impl Eq for JDateTime {}

impl PartialOrd for JDateTime {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for JDateTime {
    fn cmp(&self, other: &Self) -> Ordering {
        self.datetime.cmp(&other.datetime)
    }
}

impl Hash for JDateTime {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.datetime.hash(state);
    }
}

impl Deref for JDateTime {
    type Target = DateTime<Tz>;

    fn deref(&self) -> &Self::Target {
        &self.datetime
    }
}

impl Add<Duration> for JDateTime {
    type Output = Self;

    fn add(self, rhs: Duration) -> Self::Output {
        JDateTime { datetime: self.datetime.add(rhs), ..self }
    }
}

impl Sub<Duration> for JDateTime {
    type Output = Self;

    fn sub(self, rhs: Duration) -> Self::Output {
        JDateTime { datetime: self.datetime.sub(rhs), ..self }
    }
}
