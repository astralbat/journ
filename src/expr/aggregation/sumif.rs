/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::aggregation::AggState;
use crate::expr::context::IdentifierContext;
use crate::expr::{ColumnValue, Expr};
use journ_core::amount::Amount;
use journ_core::err;
use journ_core::error::JournResult;
use smallvec::SmallVec;

#[derive(Debug, PartialEq)]
pub struct SumIf<'h> {
    expr: Expr<'h>,
    cond: Expr<'h>,
    totals: SmallVec<[Amount<'h>; 2]>,
}
impl<'h> SumIf<'h> {
    pub fn new(mut args: Vec<Expr<'h>>) -> JournResult<Self> {
        if args.len() != 2 {
            return Err(err!("Function 'SumIf' requires exactly two arguments"));
        }
        let expr = args.pop().unwrap();
        let cond = args.pop().unwrap();
        Ok(Self { cond, expr, totals: SmallVec::new() })
    }
}

impl<'h> AggState<'h> for SumIf<'h> {
    fn add(&mut self, context: &mut dyn IdentifierContext<'h>) -> JournResult<()> {
        let cond_eval = self.cond.eval(context)?;
        let cond_result =
            cond_eval.as_bool().or(cond_eval.as_undefined().map(|_| false)).ok_or_else(|| {
                err!("Function 'sumif' requires the first argument to evaluate to a `Boolean` type")
            })?;
        if cond_result {
            match self.expr.eval(context)? {
                ColumnValue::Amount(amount) => {
                    self.totals += amount;
                }
                ColumnValue::List(list) => {
                    for v in list {
                        if let ColumnValue::Amount(amount) = v {
                            self.totals += amount;
                        }
                    }
                }
                _ => return Err(err!("Function 'sum' requires an `Amount` type argument")),
            }
        }
        Ok(())
    }

    fn finalize(&self) -> ColumnValue<'h> {
        match self.totals.len() {
            0 => ColumnValue::Amount(Amount::nil()),
            1 => ColumnValue::Amount(self.totals[0]),
            _ => ColumnValue::List(self.totals.iter().map(|a| ColumnValue::Amount(*a)).collect()),
        }
    }
}
