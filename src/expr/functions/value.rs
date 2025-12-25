/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::context::{EvalContext, IdentifierContext};
use crate::expr::{ColumnValue, Expr};
use journ_core::err;
use journ_core::error::JournResult;
use journ_core::unit::Unit;
use journ_core::valuer::{SystemValuer, Valuer};

pub fn value<'h>(
    args: &[Expr<'h>],
    context: &mut dyn IdentifierContext<'h>,
) -> JournResult<ColumnValue<'h>> {
    match context.as_posting_context_mut() {
        Some(context) => {
            if args.is_empty() || args.len() > 3 {
                return Err(err!(
                    "Function 'value()' requires one to three arguments: a unit, an identifier, and an optional date. E.g. value(€, amount, 2023-01-01)."
                ));
            }
            // The first argument is a string representing the unit. This is looked up in the journal or created if unknown.
            let quote_unit = args[0]
                .eval(context)?
                .as_str()
                .ok_or(err!(
                    "Function 'value()' requires the first argument to be a string representing a unit"
                ))
                .map(|s| {
                    context
                        .journal()
                        .config()
                        .get_unit(s)
                        .unwrap_or(context.journal().allocator().alloc(Unit::new(s)))
                })?;

            // The second argument is evaluated as the base amount that needs to be valued.
            let base_amount_col = args
                .get(1)
                .map(|a| a.eval(context))
                .transpose()?
                .unwrap_or(context.eval_identifier("amount").unwrap());
            let base_amount = base_amount_col.as_amount().ok_or(err!(
                "Function 'value()' requires the second argument to be an `Amount` type"
            ))?;

            // The third argument is an optional datetime. If not provided, the current entry's datetime is used.
            let datetime_col = args
                .get(2)
                .map(|a| a.eval(context))
                .transpose()?
                .unwrap_or(context.eval_identifier("datetime_mid").unwrap());
            let datetime = datetime_col.as_datetime().ok_or(err!(
                "Function 'value' requires the third argument to be a `Datetime` type"
            ))?;

            // If we are using the 'datetime_mid' column, we can proceed to use the entry to value.
            // By getting this far, we know that we are in a scalar context.
            let valuation_res = if datetime_col == context.eval_identifier("datetime_mid").unwrap()
            {
                SystemValuer::from(context.entry()).value(quote_unit, base_amount)
            } else {
                SystemValuer::on_date(context.journal().config().clone(), datetime)
                    .value(quote_unit, base_amount)
            };
            match valuation_res {
                Ok(val) => {
                    let value = val.value();
                    context.append_identifier(
                        "valueSource",
                        val.into_sources().into_iter().map(ColumnValue::String).collect(),
                    );
                    Ok(ColumnValue::Amount(value))
                }
                Err(e) => Err(err!(e;
                    "Unable to value {} in '{}' on '{}'",
                    base_amount,
                    quote_unit.code(),
                    datetime
                )),
            }
        }
        None => Err(err!("Function 'value()' is not supported in this context")),
    }
}
