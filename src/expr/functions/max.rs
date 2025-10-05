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

pub fn max<'h, 'a, 's>(args: &[ColumnValue<'h>]) -> JournResult<ColumnValue<'h>> {
    if args.len() != 2 {
        return Err(err!("Function 'max' requires two arguments"));
    }

    if args[0].is_undefined() || args[1].is_undefined() {
        return Ok(ColumnValue::Undefined);
    }

    let err = || err!("Function 'max' requires both arguments to be of the same type: `Amount`");
    let left_as_num = args[0].as_number().ok_or_else(|| err())?;
    let right_as_num = args[1].as_number().ok_or_else(|| err())?;

    if right_as_num > left_as_num { Ok(args[1].clone()) } else { Ok(args[0].clone()) }
}
