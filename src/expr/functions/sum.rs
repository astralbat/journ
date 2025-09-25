/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::column_expr::{ColumnExpr, ColumnValue, DataContext, EvalContext};
use journ_core::amount::Amount;
use journ_core::err;
use journ_core::error::JournResult;
use smallvec::{SmallVec, smallvec};

pub fn sum<'h, 'a>(
    args: &[ColumnExpr<'a>],
    eval_context: &mut EvalContext<'h, 'a>,
) -> JournResult<ColumnValue<'h, 'a>> {
    match eval_context.data_context() {
        DataContext::Group(group) => {
            let mut totals: SmallVec<[Amount; 2]> = smallvec![];
            if args.is_empty() {
                return Err(err!("Function 'sum' requires one argument"));
            }
            let mut as_values = false;
            group.evaluate(|context| {
                let cv = args[0].eval(&mut EvalContext::new(eval_context.journal(), context))?;
                if matches!(cv, ColumnValue::Amount { is_valuation: true, .. }) {
                    as_values = true;
                }
                totals += cv
                    .as_amount()
                    .ok_or_else(|| err!("Function 'sum' requires an `Amount` type argument"))?;
                Ok(())
            })?;
            Ok(totals.into())
        }
        _ => Err(err!("Function 'sum' is not valid in scalar context, only in group context")),
    }
}
