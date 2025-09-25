/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::column_expr::{ColumnExpr, ColumnValue, EvalContext};
use journ_core::amount::Amount;
use journ_core::err;
use journ_core::error::JournResult;

pub fn num<'h, 'a, 's>(
    args: &[ColumnExpr<'a>],
    eval_context: &mut EvalContext<'h, 'a>,
) -> JournResult<ColumnValue<'h, 'a>> {
    if args.len() != 1 {
        return Err(err!("Function 'num' requires exactly one argument"));
    }
    let mut arg = args[0].eval(eval_context)?;

    for mut a in arg.as_list_mut() {
        match a {
            ColumnValue::Amount { amount, is_valuation } => {
                *a = ColumnValue::Amount {
                    amount: Amount::nil().with_quantity(amount.quantity()),
                    is_valuation: *is_valuation,
                };
            }
            _ => return Err(err!("Function 'num' requires an argument of type `Amount`")),
        }
    }
    Ok(arg)
}
