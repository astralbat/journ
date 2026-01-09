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

pub fn cond<'h>(
    args: &[Expr<'h>],
    context: &mut dyn IdentifierContext<'h>,
) -> JournResult<ColumnValue<'h>> {
    if args.len() < 2 || args.len() > 3 {
        return Err(err!(
            "Function 'if(condition, value_if_true [,value_if_false])' requires two or three arguments"
        ));
    }
    let cond_value = args[0].eval(context)?;
    let cond =
        cond_value.as_bool().or(cond_value.as_undefined().map(|_| false)).ok_or_else(|| {
            err!("If() condition argument must evaluate to true or false:")
                .with_source(err!("{}", args[0]))
        })?;
    if cond {
        Ok(args[1].eval(context)?)
    } else if args.len() == 3 {
        Ok(args[2].eval(context)?)
    } else {
        Ok(ColumnValue::Undefined)
    }
}
