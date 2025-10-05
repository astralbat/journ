/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::column_value::ColumnValue;
use crate::expr::context::EvalContext;
use journ_core::arguments::Arguments;
use journ_core::date_and_time::{JDate, JDateTime};
use journ_core::err;
use journ_core::error::JournResult;

/// Parse a date from a string, returning a Date value.
pub fn datevalue<'h, 'a, 's>(args: &[ColumnValue<'h>]) -> JournResult<ColumnValue<'h>> {
    if args.len() < 1 || args.len() > 4 {
        return Err(err!(
            "Function 'datevalue(text [,date_format [,time_format [,time_zone]])' requires one, two, three or four arguments"
        ));
    }
    let text_arg = &args[0];
    let date_format_arg = args.get(1);
    let time_format_arg = args.get(2);
    let time_zone_arg = args.get(3);

    if text_arg.is_undefined() {
        return Ok(ColumnValue::Undefined);
    }

    let text = text_arg
        .as_str()
        .ok_or_else(|| err!("Function 'datevalue' requires a String argument`"))?;

    let df = date_format_arg
        .map(|v| {
            v.as_str()
                .ok_or_else(|| err!("Function 'datevalue' second argument must be a String"))
                .and_then(str::parse)
        })
        .unwrap_or_else(|| Ok(Arguments::get().datetime_args.date_format))?;
    let tf = time_format_arg
        .map(|v| {
            v.as_str()
                .ok_or_else(|| err!("Function 'datevalue' third argument must be a String"))
                .and_then(str::parse)
        })
        .unwrap_or_else(|| Ok(Arguments::get().datetime_args.time_format))?;
    let tz = time_zone_arg
        .map(|v| {
            v.as_str()
                .ok_or_else(|| err!("Function 'datevalue' fourth argument must be a valid time zone string. E.g. 'UTC', 'America/New_York'"))
                .and_then(|s| s.parse().map_err(|e| err!("Function 'datevalue' could not parse time zone: {}", e)))
        })
        .unwrap_or_else(|| Ok(Arguments::get().datetime_args.timezone))?;

    if args.len() < 3 {
        JDate::parse(df)(text)
            .map(|(_, d)| d)
            .map_err(|e| {
                err!(
                    "Function 'datevalue' could not parse date '{}' with format '{}': {}",
                    text,
                    df,
                    e
                )
            })
            .map(ColumnValue::Date)
    } else {
        JDateTime::parse(df, tf, tz)(text)
        .map(|(_, d)| d)
        .map_err(|e| {
            err!(
                "Function 'datevalue' could not parse date '{}' with date format '{}', time format '{}' and time zone '{}': {}",
                text,
                df,
                tf,
                tz,
                e
            )
        })
        .map(ColumnValue::Datetime)
    }
}
