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

pub fn min<'h, 'a, 's>(
    args: &[ColumnExpr<'a>],
    eval_context: &mut EvalContext<'h, 'a>,
) -> JournResult<ColumnValue<'h, 'a>> {
    if args.len() != 2 {
        return Err(err!("Function 'min' requires two arguments"));
    }
    let err = || err!("Function 'min' requires both arguments to be of the same type: `Amount`");
    let left = args[0].eval(eval_context)?;
    let left_as_num = left.as_number().ok_or_else(|| err())?;
    let right = args[1].eval(eval_context)?;
    let right_as_num = right.as_number().ok_or_else(|| err())?;

    if right_as_num < left_as_num { Ok(right) } else { Ok(left) }
}
