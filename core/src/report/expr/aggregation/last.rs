/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::err;
use crate::error::JournResult;
use crate::report::expr::aggregation::AggState;
use crate::report::expr::{ColumnValue, Expr, IdentifierContext};

/// Always the last defined value.
#[derive(Debug, PartialEq)]
pub struct Last<'h> {
    arg: Expr<'h>,
    last: ColumnValue<'h>,
}
impl<'h> Last<'h> {
    pub fn new(args: Vec<Expr<'h>>) -> JournResult<Self> {
        if args.len() != 1 {
            return Err(err!("Function 'last' requires one argument"));
        }

        Ok(Self { arg: args.into_iter().next().unwrap(), last: ColumnValue::Undefined })
    }
}
impl<'h> AggState<'h> for Last<'h> {
    fn add(&mut self, context: &mut dyn IdentifierContext<'h>) -> JournResult<()> {
        let val = self.arg.eval(context)?;

        if !val.is_undefined() {
            self.last = val;
        }
        Ok(())
    }

    fn finalize(&self) -> ColumnValue<'h> {
        self.last.clone()
    }
}
