/*
 * Copyright (c) 2022-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::account::Account;
use crate::configuration::{
    AccountFilter, DescriptionFilter, FileFilter, Filter, create_unit_filter,
};
use crate::date_and_time::{
    DEFAULT_DATE_FORMAT, DEFAULT_TIME_FORMAT, DateFormat, JDateTime, TimeFormat,
};
use crate::err;
use crate::error::JournResult;
use crate::journal_node::JournalNode;
use crate::unit::Unit;
use chrono::{DateTime, NaiveDateTime, TimeZone};
use chrono_tz::Tz;
use std::any::Any;
use std::default::Default;
use std::fmt;
use std::fmt::Debug;
use std::ops::{Bound, RangeBounds};
use std::sync::OnceLock;

/// The command we're executing.
pub trait Command: fmt::Debug + Send + Sync + Any {}

#[derive(Debug, Default)]
pub struct Arguments {
    pub datetime_args: DateTimeArguments,
    pub begin: Option<DateTime<Tz>>,
    pub end: Option<DateTime<Tz>>,
    pub aux_date: bool,
    pub color: bool,
    pub no_color: bool,
    pub real_postings: bool,
    cmd: OnceLock<Box<dyn Command>>,
}

#[derive(Debug)]
pub struct DateTimeArguments {
    pub date_format: DateFormat<'static>,
    pub time_format: TimeFormat<'static>,
    pub timezone: Tz,
}

impl DateTimeArguments {
    pub fn datetime_from_utc(&self, datetime: &NaiveDateTime) -> JDateTime<'_> {
        JDateTime::from_datetime(
            self.timezone.from_utc_datetime(datetime),
            Some(&self.date_format),
            Some(&self.time_format),
        )
    }
}

impl Default for DateTimeArguments {
    fn default() -> Self {
        DateTimeArguments {
            date_format: DEFAULT_DATE_FORMAT.clone(),
            time_format: DEFAULT_TIME_FORMAT.clone(),
            timezone: Tz::UTC,
        }
    }
}

static ARGS: OnceLock<Arguments> = OnceLock::new();

impl Arguments {
    pub fn get() -> &'static Self {
        ARGS.get_or_init(Arguments::default)
    }

    /// Sets the arguments. This is called at the start of the program
    /// and invoking again will not allow them to be changed.
    pub fn set(args: Self) -> &'static Self {
        ARGS.get_or_init(|| args)
    }

    pub fn set_cmd<C: Command>(&self, cmd: C) -> &C {
        self.cmd.set(Box::new(cmd)).expect("Command already set");
        self.cast_cmd().unwrap()
    }

    pub fn cmd(&self) -> &dyn Command {
        self.cmd.get().map(|cmd| cmd.as_ref()).expect("Command not set")
    }

    pub fn cast_cmd<Cmd: Command>(&self) -> Option<&Cmd> {
        self.cmd.get().and_then(|cmd| (cmd.as_ref() as &dyn Any).downcast_ref())
    }

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

    pub fn begin_end_range(&self) -> impl RangeBounds<DateTime<Tz>> {
        let begin = self.begin().map(Bound::Included);
        let end = self.end().map(Bound::Excluded);
        (begin.unwrap_or(Bound::Unbounded), end.unwrap_or(Bound::Unbounded))
    }

    pub fn until_end_range(&self) -> impl RangeBounds<DateTime<Tz>> {
        let end = self.end().map(Bound::Excluded);
        (Bound::Unbounded, end.unwrap_or(Bound::Unbounded))
    }

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

#[derive(Default, Debug)]
pub struct PrintCommand {
    pub account_filter: Option<String>,
    pub print_file: Option<String>,
}

impl Command for PrintCommand {}

#[derive(Default, Debug)]
pub struct CsvCommand {
    pub datetime_args: DateTimeArguments,
    pub file_filter: Vec<String>,
    pub account_filter: Vec<String>,
    pub unit_filter: Vec<String>,
    pub description_filter: Vec<String>,
    pub group_postings_by: Option<String>,
}

impl CsvCommand {
    pub fn account_filter(&self) -> impl for<'t> Filter<Account<'t>> + '_ {
        AccountFilter::new(&self.account_filter)
    }

    pub fn set_account_filter(&mut self, accounts: Vec<String>) {
        self.account_filter = accounts;
    }

    pub fn unit_filter(&self) -> impl for<'t> Filter<Unit<'t>> + '_ {
        create_unit_filter(&self.unit_filter)
    }

    pub fn set_unit_filter(&mut self, units: Vec<String>) {
        self.unit_filter = units;
    }

    pub fn file_filter(&self) -> impl for<'h> Filter<JournalNode<'h>> + '_ {
        FileFilter(&self.file_filter)
    }

    pub fn set_file_filter(&mut self, files: Vec<String>) {
        self.file_filter = files;
    }

    pub fn description_filter(&self) -> impl Filter<str> + '_ {
        DescriptionFilter(&self.description_filter)
    }

    pub fn set_description_filter(&mut self, descriptions: Vec<String>) {
        self.description_filter = descriptions
    }

    pub fn group_postings_by(&self) -> Option<&String> {
        self.group_postings_by.as_ref()
    }

    pub fn set_group_postings_by(&mut self, group_by: Option<String>) -> JournResult<()> {
        validate_group_postings_by(group_by.as_ref())?;
        self.group_postings_by = group_by.map(|s| s.to_lowercase());
        Ok(())
    }
}

impl Command for CsvCommand {}

fn validate_group_postings_by(group_by: Option<&String>) -> JournResult<()> {
    let valid_cols = ["date", "description"];
    if let Some(group_by) = group_by.as_ref() {
        let cols = group_by.split(',');
        for col in cols {
            let lowercase_col = col.to_lowercase();
            if !valid_cols.contains(&lowercase_col.as_str()) {
                return Err(err!(col.to_string(); "Invalid group column"));
            }
        }
    }
    Ok(())
}
