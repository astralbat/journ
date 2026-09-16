/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::Amount;
use crate::err;
use crate::error::JournResult;
use crate::report::expr::{ColumnValue, Expr, IdentifierContext};

fn usage() -> &'static str {
    "bal(account)"
}

pub fn bal<'h, 'a>(
    args: &[Expr],
    context: &mut dyn IdentifierContext<'h, 'a>,
) -> JournResult<ColumnValue<'h>>
where
    'h: 'a,
{
    if args.len() != 1 {
        return Err(err!("bal() requires 1 argument: {}", usage()));
    }
    match context.as_posting_context_mut() {
        Some(posting_context) => {
            let account_pattern = args[0].eval(posting_context)?;
            match account_pattern.as_str() {
                Some(acc_pattern) => {
                    let mut balance = Amount::nil();
                    for pst in posting_context.entry().postings() {
                        if pst.account().matches(acc_pattern) {
                            balance += pst.amount();
                        }
                    }
                    Ok(ColumnValue::Amount(balance, true))
                }
                None => Err("bal(account) argument must evaluate as a string".into()),
            }
        }
        None => Err(err!("bal() can only be used in a posting context")),
    }
}
