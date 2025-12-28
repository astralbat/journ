/*
 * Copyright (c) 2022-2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::configuration::Configuration;
use crate::date_and_time::{
    DEFAULT_DATE_FORMAT, DEFAULT_TIME_FORMAT, DateFormat, JDateTime, TimeFormat,
};
use crate::reporting::command::ExecCommand;
use crate::reporting::command::cmd_line::BeginAndEndCommand;
use chrono::{NaiveDateTime, TimeZone};
use chrono_tz::Tz;
use std::any::Any;
use std::default::Default;
use std::fmt::Debug;
use std::ops::Deref;
use std::sync::OnceLock;

/// The command we're executing.
pub trait Command: Debug + Send + Sync + Any {
    fn datetime_fmt_cmd(&self) -> &DateTimeFormatCommand;

    fn begin_and_end_cmd(&self) -> &BeginAndEndCommand;
}

static ARGS: OnceLock<Arguments> = OnceLock::new();
static CMD: OnceLock<Box<dyn ExecCommand>> = OnceLock::new();
pub struct Cmd;
impl Cmd {
    pub fn args() -> &'static Arguments {
        ARGS.get().expect("Arguments not initialized")
    }

    pub fn set_args(args: Arguments) -> &'static Arguments {
        ARGS.set(args).expect("arguments already initialized");
        Self::args()
    }

    /// Gets the `Command` being run
    ///
    /// # Panics
    /// If the command hasn't been set yet
    pub fn get() -> &'static dyn ExecCommand {
        CMD.get().expect("command not initialized").deref()
    }

    /// Gets and converts the `Command` being run
    ///
    /// # Panics
    /// If the command hasn't been set yet, or the type of the command is not correct.
    pub fn cast<Cmd: ExecCommand>() -> &'static Cmd {
        (Self::get() as &dyn Any).downcast_ref().expect("Command not of expected type")
    }

    /// Sets the command being run. This is done early, shortly after the command has been first
    /// created.
    pub fn set(command: Box<dyn ExecCommand>) -> &'static dyn ExecCommand {
        CMD.set(command).expect("command already initialized");
        Self::get()
    }
}

#[derive(Debug, Default)]
pub struct Arguments {
    pub datetime_cmd: DateTimeFormatCommand,
    //pub begin: Option<DateTime<Tz>>,
    //pub end: Option<DateTime<Tz>>,
    pub aux_date: bool,
    pub color: bool,
    pub no_color: bool,
    pub real_postings: bool,
    //cmd: OnceLock<Box<dyn Command>>,
}

#[derive(Debug, Default)]
pub struct DateTimeFormatCommand {
    date_format: Option<&'static DateFormat<'static>>,
    time_format: Option<&'static TimeFormat<'static>>,
    timezone: Option<Tz>,
}

impl DateTimeFormatCommand {
    pub fn new(
        df: Option<&'static DateFormat<'static>>,
        tf: Option<&'static TimeFormat<'static>>,
        tz: Option<Tz>,
    ) -> Self {
        Self { date_format: df, time_format: tf, timezone: tz }
    }

    /// If any formats have been specified on the command line (`DateTimeFormatCommand`),
    /// then use these.
    /// Otherwise, use the journal configurations.
    pub fn from_args_or_config(args: &Arguments, config: &Configuration) -> Self {
        let dtfc = &args.datetime_cmd;
        let (df, tf, tz) = if dtfc.date_format().is_some()
            || dtfc.time_format().is_some()
            || dtfc.timezone().is_some()
        {
            (dtfc.date_format(), dtfc.time_format(), dtfc.timezone())
        } else {
            (
                Some(config.date_format().clone().into_owned()),
                Some(config.time_format().clone().into_owned()),
                Some(config.timezone()),
            )
        };

        DateTimeFormatCommand { date_format: df, time_format: tf, timezone: tz }
    }

    pub fn datetime_from_utc(&self, datetime: &NaiveDateTime) -> JDateTime<'_> {
        JDateTime::from_datetime(
            self.timezone_or_default().from_utc_datetime(datetime),
            Some(self.date_format_or_default()),
            Some(self.time_format_or_default()),
        )
    }

    pub fn date_format(&self) -> Option<&'static DateFormat<'static>> {
        self.date_format
    }

    pub fn date_format_or_default(&self) -> &'static DateFormat<'static> {
        self.date_format.unwrap_or(&DEFAULT_DATE_FORMAT)
    }

    pub fn time_format(&self) -> Option<&'static TimeFormat<'static>> {
        self.time_format
    }

    pub fn time_format_or_default(&self) -> &'static TimeFormat<'static> {
        self.time_format.unwrap_or(&DEFAULT_TIME_FORMAT)
    }

    pub fn timezone(&self) -> Option<Tz> {
        self.timezone
    }

    pub fn timezone_or_default(&self) -> Tz {
        self.timezone.unwrap_or(Tz::UTC)
    }
}

impl Arguments {
    /*
    pub fn set_cmd<C: Command>(&self, cmd: C) -> &C {
        self.cmd.set(Box::new(cmd)).expect("Command already set");
        self.cast_cmd().unwrap()
    }

    pub fn cmd(&self) -> &dyn Command {
        self.cmd.get().map(|cmd| cmd.as_ref()).expect("Command not set")
    }

    pub fn cast_cmd<Cmd: Command>(&self) -> Option<&Cmd> {
        self.cmd.get().and_then(|cmd| (cmd.as_ref() as &dyn Any).downcast_ref())
    }*/

    pub fn print_std_err_in_color(&self) -> bool {
        atty::is(atty::Stream::Stderr) || self.color
    }

    pub fn print_std_out_in_color(&self) -> bool {
        if self.no_color {
            return false;
        }
        atty::is(atty::Stream::Stdout) || self.color
    }

    pub fn set_color(&mut self, color: bool) {
        self.color = color
    }

    pub fn set_no_color(&mut self, no_color: bool) {
        self.no_color = no_color
    }

    /*
    /// The begin date time parsed in the format specified by [`Configuration::date_format`]
    pub fn begin(&self) -> Option<DateTime<Tz>> {
        self.begin
    }

    pub fn set_begin(&mut self, begin: Option<DateTime<Tz>>) {
        self.begin = begin
    }

    pub fn end(&self) -> Option<DateTime<Tz>> {
        self.end
    }

    pub fn set_end(&mut self, end: Option<DateTime<Tz>>) {
        self.end = end
    }

    pub fn begin_end_range(&self) -> (Bound<DateTime<Tz>>, Bound<DateTime<Tz>>) {
        let begin = self.begin().map(Bound::Included);
        let end = self.end().map(Bound::Excluded);
        (begin.unwrap_or(Bound::Unbounded), end.unwrap_or(Bound::Unbounded))
    }

    pub fn until_end_range(&self) -> impl RangeBounds<DateTime<Tz>> {
        let end = self.end().map(Bound::Excluded);
        (Bound::Unbounded, end.unwrap_or(Bound::Unbounded))
    }*/

    pub fn aux_date(&self) -> bool {
        self.aux_date
    }

    pub fn set_aux_date(&mut self, aux_date: bool) {
        self.aux_date = aux_date
    }

    pub fn real_postings(&self) -> bool {
        self.real_postings
    }

    pub fn set_real_postings(&mut self, real_postings: bool) {
        self.real_postings = real_postings
    }
}
