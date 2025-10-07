/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::column_value::ColumnValue;
use crate::expr::context::EvalContext;
use journ_core::error::JournResult;
use smartstring::alias::String as SS;
use std::mem;

pub fn concat<'h, 'a, 's>(mut args: &mut [ColumnValue<'h>]) -> JournResult<ColumnValue<'h>> {
    let mut result = SS::new();

    for i in 0..args.len() {
        match mem::take(&mut args[i]) {
            ColumnValue::List(l) => {
                return Ok(ColumnValue::List(
                    l.into_iter()
                        .flatten()
                        .map(|v| {
                            args[i] = v;
                            concat(&mut Vec::from_iter(args.iter().cloned()))
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                ));
            }
            arg => result.push_str(&arg.to_string()),
        }
    }
    Ok(ColumnValue::String(result))
}
