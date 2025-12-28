/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::date_and_time::JDateTime;
use crate::err;
use crate::error::JournError;
use crate::reporting::command::arguments::DateTimeFormatCommand;
use crate::reporting::command::print_jerror;
use nom::Finish;
use std::ops::{Bound, RangeBounds};
use std::process::exit;

#[derive(clap::Args, Debug, Default)]
pub struct BeginAndEndArguments {
    #[arg(short = 'b', long = "begin", value_name = "BEGIN_DATE", help = "The date to begin")]
    begin: Option<String>,
    #[arg(short = 'e', long = "end", value_name = "END_DATE", help = "The date to end")]
    end: Option<String>,
}

impl BeginAndEndArguments {
    /// Parse any specified begin and end date strings to `JDateTime`.
    pub fn into_cmd(self, datetime_fmt_cmd: &DateTimeFormatCommand) -> BeginAndEndCommand {
        let df = datetime_fmt_cmd.date_format_or_default();
        let tf = datetime_fmt_cmd.time_format_or_default();
        let tz = datetime_fmt_cmd.timezone_or_default();

        let begin = if let Some(begin) = self.begin {
            match JDateTime::parse(df, tf, tz)(begin.as_str()).finish() {
                Ok((_, date)) => Some(date),
                Err(e) => {
                    print_jerror(err!("Expected format {} [{}]", df, tf).with_source(e.into_err()));
                    exit(1)
                }
            }
        } else {
            None
        };

        let end = if let Some(end) = self.end {
            match JDateTime::parse(df, tf, tz)(end.as_str()).finish() {
                Ok((_, date)) => Some(date),
                Err(e) => {
                    print_jerror(err!("Expected format {} [{}]", df, tf).with_source(e.into_err()));
                    exit(1)
                }
            }
        } else {
            None
        };
        BeginAndEndCommand { begin, end }
    }
}

#[derive(Default, Debug)]
pub struct BeginAndEndCommand {
    begin: Option<JDateTime<'static>>,
    end: Option<JDateTime<'static>>,
}

impl BeginAndEndCommand {
    pub fn begin_end_range(&self) -> (Bound<JDateTime<'static>>, Bound<JDateTime<'static>>) {
        let begin = self.begin.map(Bound::Included);
        let end = self.end.map(Bound::Excluded);
        (begin.unwrap_or(Bound::Unbounded), end.unwrap_or(Bound::Unbounded))
    }

    pub fn until_end_range(&self) -> impl RangeBounds<JDateTime<'static>> {
        let end = self.end.map(Bound::Excluded);
        (Bound::Unbounded, end.unwrap_or(Bound::Unbounded))
    }
}
