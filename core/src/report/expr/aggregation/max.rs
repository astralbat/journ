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

/// If any value should be `Undefined`, then `Max()` will also be `Undefined`.
#[derive(Debug, PartialEq)]
pub struct Max<'h> {
    arg: Expr,
    max: Option<ColumnValue<'h>>,
}
impl<'h> Max<'h> {
    pub fn new(args: Vec<Expr>) -> JournResult<Self> {
        if args.len() != 1 {
            return Err(err!("Function 'max' requires one argument"));
        }

        Ok(Self { arg: args.into_iter().next().unwrap(), max: None })
    }
}
impl<'h, 'a> AggState<'h, 'a> for Max<'h>
where
    'h: 'a,
{
    fn add(&mut self, context: &mut dyn IdentifierContext<'h, 'a>) -> JournResult<()> {
        let val = self.arg.eval(context)?;

        if self.max.is_none() {
            self.max = Some(val);
        } else if let Some((amnt, p_1)) = val.as_amount()
            && let Some((self_amnt, p_2)) = self.max.as_ref().and_then(|m| m.as_amount())
            && amnt.unit() == self_amnt.unit()
        {
            self.max = Some(ColumnValue::Amount(amnt.max(self_amnt), p_1 || p_2));
        } else if let Some(max) = self.max.as_ref()
            && mem::discriminant(max) == mem::discriminant(&val)
        {
            if &val > max {
                self.max = Some(val)
            }
        } else {
            self.max = Some(ColumnValue::Undefined)
        }

        Ok(())
    }

    fn finalize(&self) -> ColumnValue<'h> {
        match &self.max {
            Some(max) => max.clone(),
            None => ColumnValue::Undefined,
        }
    }
}
