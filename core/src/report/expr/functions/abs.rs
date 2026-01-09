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

pub fn abs<'h>(
    args: &[Expr<'h>],
    context: &mut dyn IdentifierContext<'h>,
) -> JournResult<ColumnValue<'h>> {
    if args.len() != 1 {
        return Err(err!("Function 'abs' requires two arguments"));
    }

    let val0 = args[0].eval(context)?;
    if val0.is_undefined() {
        return Ok(ColumnValue::Undefined);
    }

    let err = || err!("Function 'abs' requires an `Amount` argument");

    val0.as_amount()
        .map(|a| ColumnValue::Amount(a.abs()))
        .or_else(|| val0.as_number().map(|n| ColumnValue::Number(n.abs())))
        .ok_or_else(err)
}
