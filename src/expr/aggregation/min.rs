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

#[derive(Debug, PartialEq)]
pub struct Min<'h> {
    arg: Expr<'h>,
    min: ColumnValue<'h>,
}
impl<'h> Min<'h> {
    pub fn new(args: Vec<Expr<'h>>) -> JournResult<Self> {
        if args.len() != 1 {
            return Err(err!("Function 'min' requires one arguments"));
        }

        Ok(Self { arg: args.into_iter().next().unwrap(), min: ColumnValue::Undefined })
    }
}
impl<'h> AggState<'h> for Min<'h> {
    fn add(&mut self, context: &mut dyn IdentifierContext<'h>) -> JournResult<()> {
        let val = self.arg.eval(context)?;

        if val.is_undefined() {
            return Ok(());
        }

        let num =
            val.as_number().ok_or_else(|| err!("Function 'min' requires an `Number` argument"))?;
        if self.min.is_undefined() || num < self.min.as_number().unwrap() {
            self.min = val
        }
        Ok(())
    }

    fn finalize(&self) -> ColumnValue<'h> {
        if self.min.is_undefined() { ColumnValue::Amount(Amount::nil()) } else { self.min.clone() }
    }
}
