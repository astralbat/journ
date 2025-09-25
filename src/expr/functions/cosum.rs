/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::column_expr::{ColumnExpr, ColumnValue, DataContext, EvalContext};
use journ_core::amount::Amount;
use journ_core::configuration::{AccountFilter, Filter};
use journ_core::err;
use journ_core::error::JournResult;
use smallvec::SmallVec;

pub fn cosum<'h, 'a, 's>(
    args: &[ColumnExpr<'s>],
    eval_context: &mut EvalContext<'h, 's>,
) -> JournResult<ColumnValue<'h, 'a>> {
    let accounts = args.iter().map(|arg| arg.eval(eval_context)?.into_string().ok_or(err!("cosum() requires arguments of type `String`, representing account patterns to match"))).collect::<Result<Vec<_>, _>>()?;
    let account_filter = AccountFilter::new(&accounts);
    match eval_context.data_context() {
        DataContext::Group(group) => {
            let mut total = SmallVec::<[Amount; 2]>::new();
            for entry in group.entries() {
                if entry.postings().filter(|pst| account_filter.is_included(pst.account())).count()
                    == 0
                {
                    continue;
                }

                for pst in entry.postings() {
                    if account_filter.is_included(pst.account()) {
                        total += pst.amount();
                    }
                }
            }
            Ok(total.into())
        }
        _ => Err(err!("Function 'cosum' is not valid in scalar context, only in group context")),
    }
}
