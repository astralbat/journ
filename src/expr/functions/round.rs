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

pub fn round<'h, 'a, 's>(
    args: &[ColumnExpr<'a>],
    eval_context: &mut EvalContext<'h, 'a>,
) -> JournResult<ColumnValue<'h, 'a>> {
    if args.len() < 1 || args.len() > 2 {
        return Err(err!("Function 'round' requires one or two arguments"));
    }
    let mut value = args[0].eval(eval_context)?;
    let num_dp = if args.len() == 2 {
        args[1].eval(eval_context)?
    } else {
        ColumnValue::Amount { amount: journ_core::amount::Amount::nil(), is_valuation: false }
    };

    if value.is_nan() || num_dp.is_nan() {
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
