/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::Amount;
use crate::err;
use crate::error::{JournError, JournResult};
use crate::reporting::expr::aggregation::AggState;
use crate::reporting::expr::context::IdentifierContext;
use crate::reporting::expr::{ColumnValue, Expr};
use smallvec::SmallVec;

#[derive(Debug, PartialEq)]
pub struct Sum<'h> {
    expr: Expr<'h>,
    totals: SmallVec<[Amount<'h>; 2]>,
}
impl<'h> Sum<'h> {
    pub fn new(mut args: Vec<Expr<'h>>) -> JournResult<Self> {
        if args.len() != 1 {
            return Err(err!("Function 'sum' requires exactly one argument"));
        }
        Ok(Self { expr: args.pop().unwrap(), totals: SmallVec::new() })
    }
}

impl<'h> AggState<'h> for Sum<'h> {
    fn add(&mut self, context: &mut dyn IdentifierContext<'h>) -> JournResult<()> {
        self.expr
            .eval(context)?
            .as_list()
            .iter()
            .try_fold(&mut self.totals, |acc, v| {
                let amount =
                    v.as_amount().ok_or_else(|| err!("Sum() can only sum `Amount` types"))?;
                if !amount.is_nil() {
                    *acc += amount;
                }
                Ok::<_, JournError>(acc)
            })
            .map(|_| ())
    }

    fn merge(&mut self, other: &dyn AggState<'h>) -> JournResult<()> {
        let b = other.finalize();

        b.as_list()
            .iter()
            .try_fold(&mut self.totals, |acc, v| {
                let amount =
                    v.as_amount().ok_or_else(|| err!("Sum() can only sum `Amount` types"))?;
                if !amount.is_nil() {
                    *acc += amount;
                }
                Ok::<_, JournError>(acc)
            })
            .map(|_| ())
    }

    fn finalize(&self) -> ColumnValue<'h> {
        match self.totals.len() {
            0 => ColumnValue::Amount(Amount::nil()),
            1 => ColumnValue::Amount(self.totals[0]),
            _ => ColumnValue::List(self.totals.iter().map(|a| ColumnValue::Amount(*a)).collect()),
        }
    }
}
