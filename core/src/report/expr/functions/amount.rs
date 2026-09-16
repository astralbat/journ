/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::Amount;
use crate::configuration::Configuration;
use crate::error::JournResult;
use crate::journal_context::JContext;
use crate::report::expr::{ColumnValue, Expr, IdentifierContext};
use crate::{err, parsing};
use nom_locate::LocatedSpan;
use std::cell::RefCell;
use std::ops::Deref;

/// Parses a String value to an Amount value.
pub fn amount<'h, 'a>(
    args: &[Expr],
    context: &mut dyn IdentifierContext<'h, 'a>,
) -> JournResult<ColumnValue<'h>>
where
    'h: 'a,
{
    if args.len() != 1 {
        return Err(err!("Function 'amount' requires exactly one argument"));
    }

    let mut config: Configuration<'h> = JContext::get().journal().config().clone();
    let mut arg = args[0].eval(context)?;
    for a in arg.as_list_mut() {
        match a {
            ColumnValue::String(s) => {
                let v = from_str(s.deref(), &mut config);
                *a = v
            }
            ColumnValue::StringRef(s) => *a = from_str(s, &mut config),
            ColumnValue::Number(n) => {
                *a = ColumnValue::Amount(Amount::nil().with_quantity(*n), true)
            }
            ColumnValue::Amount(amnt, p) => *a = ColumnValue::Amount(*amnt, *p),
            ColumnValue::Undefined => *a = ColumnValue::Undefined,
            _ => {
                return Err(err!(
                    "Function 'amount' requires an argument of type `String`: {}",
                    a.as_type_string()
                ));
            }
        }
    }
    Ok(arg)
}

fn from_str<'h>(s: &str, config: &mut Configuration<'h>) -> ColumnValue<'h> {
    let input = LocatedSpan::new_extra(s, RefCell::new(config.clone()));
    match parsing::amount::amount(input) {
        Ok((_, a)) => ColumnValue::Amount(a, true),
        Err(_) => ColumnValue::Undefined,
    }
}
