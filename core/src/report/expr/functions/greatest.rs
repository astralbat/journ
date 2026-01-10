/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::err;
use crate::error::JournResult;
use crate::report::expr::column_value::try_sort;
use crate::report::expr::{ColumnValue, Expr, IdentifierContext};

pub fn greatest<'h>(
    args: &[Expr<'h>],
    context: &mut dyn IdentifierContext<'h>,
) -> JournResult<ColumnValue<'h>> {
    if args.len() < 2 {
        return Err(err!("Function 'greatest' requires at least two arguments"));
    }

    let mut cols = args.iter().map(|arg| arg.eval(context)).collect::<Result<Vec<_>, _>>()?;
    try_sort(&mut cols)?;
    Ok(cols.into_iter().last().unwrap())
}
