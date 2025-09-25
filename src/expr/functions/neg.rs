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

pub fn neg<'h, 'a>(
    args: &[ColumnExpr<'a>],
    eval_context: &mut EvalContext<'h, 'a>,
) -> JournResult<ColumnValue<'h, 'a>> {
    if args.len() != 1 {
        return Err(err!("Function '-' requires one argument"));
    }
    let arg = args[0].eval(eval_context)?;

    match arg {
        ColumnValue::Amount { amount, is_valuation } => {
            Ok(ColumnValue::Amount { amount: -amount, is_valuation })
        }
        ColumnValue::List(mut values) => {
            let is_valuation =
                values.iter().all(|v| matches!(v, ColumnValue::Amount { is_valuation: true, .. }));
            for amt in &mut values {
                *amt = ColumnValue::Amount {
                    amount: -amt
                        .as_amount()
                        .ok_or_else(|| err!("Only `Amount` types may be negated"))?,
                    is_valuation,
                };
            }
            Ok(ColumnValue::List(values))
        }
        _ => Err(err!("Function '-' requires an `Amount`, `List` type argument")),
    }
}
