/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::Amount;
use crate::err;
use crate::error::JournResult;
use crate::report::expr::{ColumnValue, Expr, IdentifierContext};

pub fn num<'h>(
    args: &[Expr<'h>],
    context: &mut dyn IdentifierContext<'h>,
) -> JournResult<ColumnValue<'h>> {
    if args.len() != 1 {
        return Err(err!("Function 'num' requires exactly one argument"));
    }

    let mut arg = args[0].eval(context)?;
    for a in arg.as_list_mut() {
        match a {
            ColumnValue::Amount(amount) => {
                *a = ColumnValue::Amount(Amount::nil().with_quantity(amount.quantity()));
            }
            _ => return Err(err!("Function 'num' requires an argument of type `Amount`")),
        }
    }
    Ok(arg)
}
