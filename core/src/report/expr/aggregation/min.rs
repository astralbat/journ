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
use crate::report::expr::{ColumnValue, Expr, IdentifierContext};
use std::mem;

#[derive(Debug, PartialEq)]
pub struct Min<'h> {
    arg: Expr<'h>,
    min: Option<ColumnValue<'h>>,
}
impl<'h> Min<'h> {
    pub fn new(args: Vec<Expr<'h>>) -> JournResult<Self> {
        if args.len() != 1 {
            return Err(err!("Function 'min' requires one argument"));
        }

        Ok(Self { arg: args.into_iter().next().unwrap(), min: None })
    }
}
impl<'h> AggState<'h> for Min<'h> {
    fn add(&mut self, context: &mut dyn IdentifierContext<'h>) -> JournResult<()> {
        let val = self.arg.eval(context)?;

        if self.min.is_none() {
            self.min = Some(val);
        } else if let Some(amnt) = val.as_amount()
            && let Some(self_amnt) = self.min.as_ref().and_then(|m| m.as_amount())
            && amnt.unit() == self_amnt.unit()
        {
            self.min = Some(ColumnValue::Amount(amnt.min(self_amnt)));
        } else if let Some(min) = self.min.as_ref()
            && mem::discriminant(min) == mem::discriminant(&val)
        {
            if &val > min {
                self.min = Some(val)
            }
        } else {
            self.min = Some(ColumnValue::Undefined)
        }

        Ok(())
    }

    fn finalize(&self) -> ColumnValue<'h> {
        match &self.min {
            Some(min) => min.clone(),
            None => ColumnValue::Undefined,
        }
    }
}
