/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::err;
use crate::error::JournResult;
use crate::reporting::expr::{ColumnValue, Expr, IdentifierContext};

pub fn round<'h>(
    args: &[Expr<'h>],
    context: &mut dyn IdentifierContext<'h>,
) -> JournResult<ColumnValue<'h>> {
    if args.is_empty() || args.len() > 2 {
        return Err(err!("Function 'round' requires one or two arguments"));
    }
    let mut value = args[0].eval(context)?;
    let num_dp = args.get(1).map(|a| a.eval(context)).transpose()?;

    if value.is_undefined() || num_dp.as_ref().map(|v| v.is_undefined()).unwrap_or(false) {
        return Ok(value);
    }

    for v in value.as_list_mut() {
        match (
            v.as_amount_mut(),
            num_dp.as_ref().map(|n| n.as_number().and_then(|d| u8::try_from(d).ok())),
        ) {
            (Some(amount), Some(Some(num_dp))) => {
                *amount = amount.rounded_dec_places(num_dp);
            }
            (Some(amount), None) => {
                *amount = amount.rounded();
            }
            _ => {
                return Err(err!(
                    "Function 'round' requires the first argument to be of type `Amount` and the second argument to be of type `Number`: {}, {}",
                    value,
                    num_dp.unwrap_or(ColumnValue::Undefined)
                ));
            }
        }
    }
    Ok(value)
}
