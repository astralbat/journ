/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::date_and_time::JDate;
use crate::err;
use crate::error::JournResult;
use crate::reporting::command::arguments::Cmd;
use crate::reporting::expr::column_value::ColumnValue;
use crate::reporting::expr::{Expr, IdentifierContext};
use chrono::NaiveDate;
use rust_decimal::prelude::ToPrimitive;

/// Constructs a date from year, month, and day arguments.
pub fn date<'h>(
    args: &[Expr<'h>],
    context: &mut dyn IdentifierContext<'h>,
) -> JournResult<ColumnValue<'h>> {
    if args.len() != 3 {
        return Err(err!("Function 'date' requires three arguments: year, month, day"));
    }
    let year = args[0]
        .eval(context)?
        .as_number()
        .and_then(|d| d.to_i32())
        .ok_or_else(|| err!("Invalid year argument to function 'date'"))?;
    let month = args[1]
        .eval(context)?
        .as_number()
        .and_then(|d| d.to_u32())
        .ok_or_else(|| err!("Invalid month argument to function 'date'"))?;
    let day = args[2]
        .eval(context)?
        .as_number()
        .and_then(|d| d.to_u32())
        .ok_or_else(|| err!("Invalid day argument to function 'date'"))?;
    let naive = NaiveDate::from_ymd_opt(year, month, day)
        .ok_or_else(|| err!("Invalid date in function 'date'"))?;

    Ok(ColumnValue::Date(JDate::new(naive, Cmd::get().datetime_fmt_cmd().date_format_or_default())))
}
