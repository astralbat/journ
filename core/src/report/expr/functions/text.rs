/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::datetime::{DateFormatMode, DateTimeFormat};
use crate::err;
use crate::error::JournResult;
use crate::report::command::arguments::Cmd;
use crate::report::expr::{ColumnValue, Expr, IdentifierContext};
use crate::unit::UnitFormat;
use chrono_tz::Tz;
use smartstring::alias::String as SS;
use std::fmt::Write;
use std::mem;
use std::str::FromStr;

pub fn text<'h>(
    args: &[Expr<'h>],
    context: &mut dyn IdentifierContext<'h>,
) -> JournResult<ColumnValue<'h>> {
    text_of_values(
        &mut args.iter().map(|a| a.eval(context)).collect::<Result<Vec<ColumnValue>, _>>()?,
    )
}

fn text_of_values<'h>(values: &mut [ColumnValue<'h>]) -> JournResult<ColumnValue<'h>> {
    if values.is_empty() {
        return Err(err!("Function 'text' requires at least one argument"));
    }
    let value = mem::take(&mut values[0]);

    if value.is_undefined() {
        return Ok(ColumnValue::Undefined);
    }

    match value {
        ColumnValue::Amount(amount) => match values.len() {
            1 => Ok(ColumnValue::String(amount.format())),
            2 => {
                let format_value = &values[1];
                let format = format_value.as_str().ok_or_else(|| {
                    err!("Function 'text(amount, format)' requires the format argument to be of type `String`")
                })?;
                let unit_format = UnitFormat::from_str(format)
                    .map_err(|err| err!("Function 'text' format error: {}", err))?;
                Ok(ColumnValue::String(unit_format.format(
                    amount.quantity(),
                    amount.unit().code(),
                    amount.unit().rounding_strategy(),
                )))
            }
            _ => Err(err!("Function 'text(amount [,format])' requires one or two arguments")),
        },
        ColumnValue::Date(date) => match values.len() {
            1 => {
                let df = Cmd::get().datetime_fmt_cmd().datetime_format_or_default();
                let mut ss = SS::new();
                write!(ss, "{}", date.format(df)).unwrap();
                Ok(ColumnValue::String(ss))
            }
            2 => {
                let df = datetime_format(&values[1], DateFormatMode::Date, date_usage())?;
                let mut ss = SS::new();
                write!(ss, "{}", date.format(df)).unwrap();
                Ok(ColumnValue::String(ss))
            }
            _ => Err(err!("Function 'text(date [,date_format])' requires one or two arguments")),
        },
        ColumnValue::Datetime(datetime) => match values.len() {
            1 => {
                let df = Cmd::get().datetime_fmt_cmd().datetime_format_or_default();
                let mut ss = SS::new();
                write!(ss, "{}", datetime.format(df)).unwrap();
                Ok(ColumnValue::String(ss))
            }
            2 => {
                let dtf = datetime_format(&values[1], DateFormatMode::DateTime, datetime_usage())?;
                let mut ss = SS::new();
                write!(ss, "{}", datetime.format(dtf)).unwrap();
                Ok(ColumnValue::String(ss))
            }
            3 => {
                let dtf = datetime_format(&values[1], DateFormatMode::DateTime, datetime_usage())?;
                let tz = time_zone(&values[2])?;
                let dt_in_tz = datetime.with_timezone(tz);
                let mut ss = SS::new();
                write!(ss, "{}", dt_in_tz.format(dtf)).unwrap();
                Ok(ColumnValue::String(ss))
            }
            _ => Err(err!(
                "Function '{}' requires one, two, three or four arguments",
                datetime_usage()
            )),
        },
        ColumnValue::Account(account) => Ok(ColumnValue::String(account.name().into())),
        ColumnValue::String(s) => Ok(ColumnValue::String(s)),
        ColumnValue::StringRef(s) => Ok(ColumnValue::StringRef(s)),
        ColumnValue::Boolean(b) => Ok(ColumnValue::StringRef(if b { "true" } else { "false" })),
        ColumnValue::List(l) => Ok(ColumnValue::List(
            l.into_iter()
                .map(|v| {
                    values[0] = v;
                    text_of_values(values)
                })
                .collect::<Result<Vec<_>, _>>()?,
        )),
        _ => Err(err!(
            "Function 'text' requires the first argument to be of type `Amount`, `Date`, `Datetime` or `String`: {}",
            value
        )),
    }
}

fn date_usage() -> &'static str {
    "Function 'text(date [,date_format])'"
}

fn datetime_usage() -> &'static str {
    "Function 'text(datetime [,datetime_format [,time_zone]])'"
}

fn datetime_format<'h>(
    format: &ColumnValue<'h>,
    mode: DateFormatMode,
    usage: &'static str,
) -> JournResult<&'static DateTimeFormat<'static>> {
    let date_format_value = format;
    let date_format = date_format_value.as_str().ok_or_else(|| {
        err!("{} requires the date_format argument to be of type `String`", usage)
    })?;
    DateTimeFormat::parse_format(mode)(date_format)
        .map(|(_, df)| df)
        .map(DateTimeFormat::into_owned)
        .map_err(move |err| err!("Datetime format error: {}", err))
}

fn time_zone(format: &ColumnValue) -> JournResult<Tz> {
    let timezone_value = format;
    let timezone = timezone_value.as_str().ok_or_else(|| {
        err!("{} requires the time_zone argument to be of type `String`", datetime_usage())
    })?;
    let tz = Tz::from_str(timezone).map_err(|_| {
        err!("'{}' is not a valid timezone. Use a valid timezone. E.g. 'Europe/London'", timezone,)
    })?;

    Ok(tz)
}
