/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::err;
use crate::error::JournResult;
use crate::report::expr::aggregation::{AggState, Sum};
use crate::report::expr::context::IdentifierContext;
use crate::report::expr::{ColumnValue, Expr};

#[derive(Debug, PartialEq)]
pub struct SumIf<'h> {
    inner: Sum<'h>,
    cond: Expr<'h>,
}
impl<'h> SumIf<'h> {
    pub fn new(mut args: Vec<Expr<'h>>) -> JournResult<Self> {
        if args.len() != 2 {
            return Err(err!("Function 'SumIf' requires exactly two arguments"));
        }
        let expr = args.pop().unwrap();
        let cond = args.pop().unwrap();
        Ok(Self { cond, inner: Sum::new(vec![expr])? })
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
            self.inner.add(context)?;
        }
        Ok(())
    }

    fn merge(&mut self, other: &dyn AggState<'h>) -> JournResult<()> {
        self.inner.merge(other)
    }

    fn finalize(&self) -> ColumnValue<'h> {
        self.inner.finalize()
    }
}
