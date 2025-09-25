/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::column_expr::{ColumnExpr, ColumnValue, EvalContext};
use journ_core::err;
use journ_core::error::JournResult;
use journ_core::unit::UnitFormat;
use std::str::FromStr;

pub fn text<'h, 'a, 's>(
    args: &[ColumnExpr<'a>],
    eval_context: &mut EvalContext<'h, 'a>,
) -> JournResult<ColumnValue<'h, 'a>> {
    if args.len() != 2 {
        return Err(err!("Function 'text' requires two arguments"));
    }
    let value = args[0].eval(eval_context)?;
    let format_value = args[1].eval(eval_context)?;
    let format = format_value.as_str().ok_or_else(|| {
        err!("Function 'text' requires the second argument to be of type `String`")
    })?;

    if value.is_nan() {
        return Ok(ColumnValue::nan());
    }

    match value {
        ColumnValue::Amount { amount, .. } => {
            let unit_format = UnitFormat::from_str(format)
                .map_err(|err| err!("Function 'text' format error: {}", err))?;
            Ok(ColumnValue::String(unit_format.format(amount)))
        }
        _ => Err(err!("Function 'text' requires the first argument to be of type `Amount`")),
    }
}
