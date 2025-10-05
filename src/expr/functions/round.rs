/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::ColumnValue;
use crate::expr::context::EvalContext;
use journ_core::err;
use journ_core::error::JournResult;
use std::mem;

pub fn round<'h, 'a, 's>(args: &mut [ColumnValue<'h>]) -> JournResult<ColumnValue<'h>> {
    if args.len() < 1 || args.len() > 2 {
        return Err(err!("Function 'round' requires one or two arguments"));
    }
    let mut value = mem::take(&mut args[0]);
    let num_dp = if args.len() == 2 {
        mem::take(&mut args[1])
    } else {
        ColumnValue::Amount(journ_core::amount::Amount::nil())
    };

    if value.is_undefined() || num_dp.is_undefined() {
        return Ok(value);
    }

    for v in value.as_list_mut() {
        match (v.as_amount_mut(), num_dp.as_number().and_then(|d| u8::try_from(d).ok())) {
            (Some(amount), Some(num_dp)) => {
                *amount = amount.rounded_dec_places(num_dp);
            }
            _ => {
                return Err(err!(
                    "Function 'round' requires the first argument to be of type `Amount` and the second argument to be of type `Number`: {}, {}",
                    value,
                    num_dp
                ));
            }
        }
    }
    Ok(value)
}
