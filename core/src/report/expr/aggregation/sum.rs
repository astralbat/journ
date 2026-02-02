/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::err;
use crate::error::JournResult;
use crate::report::expr::aggregation::AggState;
use crate::report::expr::context::IdentifierContext;
use crate::report::expr::{ColumnValue, Expr};

#[derive(Debug, PartialEq)]
pub struct Sum<'h> {
    expr: Expr<'h>,
    total: Option<ColumnValue<'h>>,
}
impl<'h> Sum<'h> {
    pub fn new(mut args: Vec<Expr<'h>>) -> JournResult<Self> {
        if args.len() != 1 {
            return Err(err!("Function 'sum' requires exactly one argument"));
        }
        Ok(Self { expr: args.pop().unwrap(), total: None })
    }
}

impl<'h> AggState<'h> for Sum<'h> {
    fn add(&mut self, context: &mut dyn IdentifierContext<'h>) -> JournResult<()> {
        let b = self.expr.eval(context)?;

        match self.total.take() {
            Some(mut total) => {
                total += b;
                self.total = Some(total);
            }
            None => self.total = Some(b),
        }
        Ok(())
    }

    fn merge(&mut self, other: &dyn AggState<'h>) -> JournResult<()> {
        let b = other.finalize();

        match self.total.take() {
            Some(mut total) => {
                total += b;
                self.total = Some(total)
            }
            None => self.total = Some(b),
        }
        Ok(())
    }

    fn finalize(&self) -> ColumnValue<'h> {
        match &self.total {
            None => ColumnValue::Undefined,
            Some(t) => t.clone(),
        }
    }
}
