/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::err;
use crate::error::JournResult;
use crate::report::expr::{ColumnValue, Expr, IdentifierContext};
use smartstring::alias::String as SS;
use std::fmt::Write;

pub fn unichar<'h, 'a>(
    args: &[Expr],
    context: &mut dyn IdentifierContext<'h, 'a>,
) -> JournResult<ColumnValue<'h>>
where
    'h: 'a,
{
    if args.is_empty() || args.len() > 1 {
        return Err(err!("Function 'unichar' requires one argument"));
    }

    let val0 = args[0].eval(context)?;
    match val0 {
        ColumnValue::Number(num) => {
            let num: u32 = num.try_into().map_err(|_| {
                err!("Function 'unichar' requires a non-negative integer argument: {}", num)
            })?;
            let c = char::from_u32(num).ok_or_else(|| {
                err!("Function 'unichar' requires a valid Unicode code point: {}", num)
            })?;
            let mut buf = SS::new();
            write!(&mut buf, "{}", c).ok();
            Ok(ColumnValue::String(buf))
        }
        _ => Err(err!("Function 'unichar' requires a `Number` argument: {}", val0)),
    }
}
