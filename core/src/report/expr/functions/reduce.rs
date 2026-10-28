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
use std::mem;

pub fn reduce<'h, 'a>(
    args: &[Expr],
    context: &mut dyn IdentifierContext<'h, 'a>,
) -> JournResult<ColumnValue<'h>>
where
    'h: 'a,
{
    if args.len() != 1 {
        return Err(err!("Function 'reduce' requires exactly one argument"));
    }

    let arg = args[0].eval(context)?;
    let mut list = arg.into_list();
    if list.is_empty() {
        return Err(err!("Function 'reduce' requires a non-empty list"));
    }
    let mut head = mem::take(&mut list[0]);
    for val in list.into_iter().skip(1) {
        match (head, val) {
            (ColumnValue::Amount(a1, p1), ColumnValue::Amount(a2, p2)) => {
                head = ColumnValue::Amount(a1 + a2, p1 && p2);
            }
            (ColumnValue::Number(n1), ColumnValue::Number(n2)) => {
                head = ColumnValue::Number(n1 + n2);
            }
            (head, val) => {
                return Err(err!(
                    "Function 'reduce' requires all elements of the list to be of the same type: {}, {}",
                    head.as_type_string(),
                    val.as_type_string()
                ));
            }
        }
    }
    Ok(head)
}
