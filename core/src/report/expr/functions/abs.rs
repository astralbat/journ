/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::err;
use crate::error::JournResult;
use crate::report::expr::{ColumnValue, Expr, IdentifierContext};

pub fn abs<'h, 'a>(
    args: &[Expr],
    context: &mut dyn IdentifierContext<'h, 'a>,
) -> JournResult<ColumnValue<'h>>
where
    'h: 'a,
{
    if args.len() != 1 {
        return Err(err!("Function 'abs' requires two arguments"));
    }

    let val0 = args[0].eval(context)?;
    if val0.is_undefined() {
        return Ok(ColumnValue::Undefined);
    }

    let err = || err!("Function 'abs' requires an `Amount`, `Number` or `List` argument");

    match val0 {
        ColumnValue::Amount(amount, p) => Ok(ColumnValue::Amount(amount.abs(), p)),
        ColumnValue::Number(num) => Ok(ColumnValue::Number(num.abs())),
        ColumnValue::List(mut values) => {
            for amt in &mut values {
                let (abs, precise) = amt
                    .as_amount()
                    .map(|(a, p)| (a.abs(), p))
                    .ok_or_else(|| err!("Only `Amount` types may absolute"))?;
                *amt = ColumnValue::Amount(abs, precise);
            }
            Ok(ColumnValue::List(values))
        }
        _ => Err(err()),
    }
}
