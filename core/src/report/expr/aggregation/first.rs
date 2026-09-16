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

/// Always the first defined value.
#[derive(Debug, PartialEq)]
pub struct First<'h> {
    arg: Expr,
    first: ColumnValue<'h>,
}
impl<'h> First<'h> {
    pub fn new(args: Vec<Expr>) -> JournResult<Self> {
        if args.len() != 1 {
            return Err(err!("Function 'first' requires one argument"));
        }

        Ok(Self { arg: args.into_iter().next().unwrap(), first: ColumnValue::Undefined })
    }
}
impl<'h, 'a> AggState<'h, 'a> for First<'h>
where
    'h: 'a,
{
    fn add(&mut self, context: &mut dyn IdentifierContext<'h, 'a>) -> JournResult<()> {
        let val = self.arg.eval(context)?;

        if self.first.is_undefined() {
            self.first = val;
        }
        Ok(())
    }

    fn finalize(&self) -> ColumnValue<'h> {
        self.first.clone()
    }
}
