/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::column_expr::{ColumnExpr, ColumnValue, DataContext, EvalContext};
use journ_core::err;
use journ_core::error::JournResult;

pub fn sumif<'h, 'a>(
    args: &[ColumnExpr<'a>],
    eval_context: &mut EvalContext<'h, 'a>,
) -> JournResult<ColumnValue<'h, 'a>> {
    if args.len() < 2 {
        return Err(err!("Function 'sumif' requires at least two arguments"));
    }
    let condition = &args[0];
    let sum_expr = &args[1];
    match eval_context.data_context() {
        DataContext::Group(group) => {
            let mut totals = vec![];
            let mut as_values = false;
            group.evaluate(|context| {
                let mut context = &mut EvalContext::new(eval_context.journal(), context);
                let cond_value = condition.eval(&mut context)?.as_bool().ok_or_else(|| err!(
                            "Function 'sumif' requires the first argument to evaluate to a `Boolean` type"
                        ))?;
                if cond_value {
                    let cv = sum_expr.eval(&mut context)?;
                    if matches!(cv, ColumnValue::Amount { is_valuation: true, .. }) {
                        as_values = true;
                    }
                    totals += cv.as_amount().ok_or(err!(
                                "Function 'sumif' requires the second argument to be an `Amount` or `Value` type"
                            ))?;
                }
                Ok(())
            })?;
            Ok(ColumnValue::List(
                totals
                    .into_iter()
                    .map(|t| {
                        if as_values {
                            ColumnValue::Amount { amount: t, is_valuation: true }
                        } else {
                            ColumnValue::Amount { amount: t, is_valuation: false }
                        }
                    })
                    .collect(),
            ))
        }
        _ => Err(err!("Function 'sumif' is not valid in scalar context, only in group context")),
    }
}
