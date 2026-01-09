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
use crate::report::expr::aggregation::AggState;
use crate::report::expr::{ColumnValue, Expr, IdentifierContext};

#[derive(Debug, PartialEq)]
pub struct Max<'h> {
    arg: Expr<'h>,
    max: ColumnValue<'h>,
}
impl<'h> Max<'h> {
    pub fn new(args: Vec<Expr<'h>>) -> JournResult<Self> {
        if args.len() != 1 {
            return Err(err!("Function 'max' requires one arguments"));
        }

        Ok(Self { arg: args.into_iter().next().unwrap(), max: ColumnValue::Undefined })
    }
}
impl<'h> AggState<'h> for Max<'h> {
    fn add(&mut self, context: &mut dyn IdentifierContext<'h>) -> JournResult<()> {
        let val = self.arg.eval(context)?;

        if val.is_undefined() {
            return Ok(());
        }

        let num =
            val.as_number().ok_or_else(|| err!("Function 'max' requires an `Number` argument"))?;
        if self.max.is_undefined() || num > self.max.as_number().unwrap() {
            self.max = val
        }
        Ok(())
    }

    fn finalize(&self) -> ColumnValue<'h> {
        if self.max.is_undefined() { ColumnValue::Amount(Amount::nil()) } else { self.max.clone() }
    }
}
