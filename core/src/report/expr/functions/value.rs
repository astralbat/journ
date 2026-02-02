/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::Amount;
use crate::err;
use crate::error::JournResult;
use crate::report::expr::context::IdentifierContext;
use crate::report::expr::{ColumnValue, Expr};
use crate::unit::Unit;

pub fn value<'h>(
    args: &[Expr<'h>],
    context: &mut dyn IdentifierContext<'h>,
) -> JournResult<ColumnValue<'h>> {
    match context.as_valuer_context_mut() {
        Some(context) => {
            if args.is_empty() || args.len() > 3 {
                return Err(err!(
                    "Function 'value()' requires one to three arguments: a unit, an identifier, and an optional date. E.g. value(€, amount, 2023-01-01)."
                ));
            }
            // The first argument is a string representing the unit. This is looked up in the journal or created if unknown.
            let quote_col = args[0].eval(context)?;
            if quote_col.is_undefined() {
                return Ok(ColumnValue::Undefined);
            }
            let quote_unit = quote_col
                .as_str()
                .ok_or(err!(
                    "Function 'value()' requires the first argument to be a string representing a unit"
                ))
                .map(|s| {
                    context
                        .config()
                        .get_unit(s)
                        .unwrap_or(context.config().allocator().alloc(Unit::new(s)))
                })?;

            // The second argument is evaluated as the base amount that needs to be valued.
            let base_amount_col = args
                .get(1)
                .map(|a| a.eval(context))
                .transpose()?
                .unwrap_or_else(|| context.eval_identifier("amount").unwrap());
            if base_amount_col.is_undefined() {
                return Ok(ColumnValue::Undefined);
            }
            let mut total_value = Amount::nil();
            for base_amount_val in base_amount_col.into_list() {
                let base_amount = base_amount_val.as_amount().ok_or(err!(
                    "Function 'value()' requires the second argument to be an `Amount` type"
                ))?;

                // The third argument is an optional datetime.
                let datetime_col = args.get(2).map(|a| a.eval(context)).transpose()?;
                if datetime_col.as_ref().map(|c| c.is_undefined()).unwrap_or(false) {
                    return Ok(ColumnValue::Undefined);
                }
                let datetime = datetime_col
                    .map(|c| {
                        c.as_datetime().ok_or_else(|| {
                            err!("Function 'value' requires the third argument to be a `Datetime` type")
                        })
                    })
                    .transpose()?;

                // Get the valuer from the context and query it.
                let val = context.valuer(datetime)?.value(quote_unit, base_amount)?;
                total_value += val.value();
                context.append_identifier(
                    "valueSource",
                    val.into_sources().into_iter().map(ColumnValue::String).collect(),
                );
            }
            Ok(ColumnValue::Amount(total_value))
        }
        None => Err(err!("Function 'value()' is not supported in this context")),
    }
}
