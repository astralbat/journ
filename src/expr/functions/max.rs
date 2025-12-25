/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::{ColumnValue, Expr, IdentifierContext};
use journ_core::err;
use journ_core::error::JournResult;

pub fn max<'h>(
    args: &[Expr<'h>],
    context: &mut dyn IdentifierContext<'h>,
) -> JournResult<ColumnValue<'h>> {
    if args.len() != 2 {
        return Err(err!("Function 'max' requires two arguments"));
    }

    let val0 = args[0].eval(context)?;
    let val1 = args[1].eval(context)?;
    if val0.is_undefined() || val1.is_undefined() {
        return Ok(ColumnValue::Undefined);
    }

    let err = || err!("Function 'max' requires both arguments to be of the same type: `Amount`");
    let left_as_num = val0.as_number().ok_or_else(|| err())?;
    let right_as_num = val1.as_number().ok_or_else(|| err())?;

    if right_as_num > left_as_num { Ok(val1) } else { Ok(val0) }
}
