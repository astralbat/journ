/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::column_value::ColumnValue;
use crate::expr::{Expr, IdentifierContext};
use journ_core::error::{JournError, JournResult};
use smartstring::alias::String as SS;

pub fn concat<'h>(
    args: &[Expr<'h>],
    context: &mut dyn IdentifierContext<'h>,
) -> JournResult<ColumnValue<'h>> {
    let mut result = SS::new();

    for i in 0..args.len() {
        match args[i].eval(context)? {
            // If there are two or more lists in arguments, combine them combinatorically.
            ColumnValue::List(l) => {
                return Ok(ColumnValue::List(
                    l.into_iter()
                        .flatten()
                        .map(|v| {
                            let mut res = SS::new();
                            res.push_str(&v.to_string());
                            res.push_str(concat(&args[i + 1..], context)?.to_string().as_str());
                            Ok::<_, JournError>(ColumnValue::String(res))
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                ));
            }
            arg => result.push_str(&arg.to_string()),
        }
    }
    Ok(ColumnValue::String(result))
}
