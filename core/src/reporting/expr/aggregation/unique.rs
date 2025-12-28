/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::err;
use crate::error::{JournError, JournResult};
use crate::reporting::expr::aggregation::AggState;
use crate::reporting::expr::{ColumnValue, Expr, IdentifierContext};
use std::collections::HashSet;

#[derive(Debug, PartialEq)]
pub struct Unique<'h> {
    expr: Expr<'h>,
    set: HashSet<ColumnValue<'h>>,
}
impl<'h> Unique<'h> {
    pub fn new(mut args: Vec<Expr<'h>>) -> JournResult<Self> {
        if args.len() != 1 {
            return Err(err!("Function 'unique' requires exactly one argument"));
        }
        Ok(Self { expr: args.pop().unwrap(), set: HashSet::new() })
    }
}

impl<'h> AggState<'h> for Unique<'h> {
    fn add(&mut self, context: &mut dyn IdentifierContext<'h>) -> JournResult<()> {
        self.expr
            .eval(context)
            .map_err(|e| err!(e; "Unable to evaluate Unique() expression"))?
            .into_list()
            .into_iter()
            .try_fold(&mut self.set, |acc, v| {
                acc.insert(v);
                Ok::<_, JournError>(acc)
            })
            .map(|_| ())
    }

    fn merge(&mut self, other: &dyn AggState<'h>) -> JournResult<()> {
        let b = other.finalize();

        b.into_list()
            .into_iter()
            .try_fold(&mut self.set, |acc, v| {
                acc.insert(v);
                Ok::<_, JournError>(acc)
            })
            .map(|_| ())
    }

    fn finalize(&self) -> ColumnValue<'h> {
        match self.set.len() {
            1 => self.set.iter().next().cloned().unwrap(),
            _ => ColumnValue::List(self.set.iter().cloned().collect()),
        }
    }
}
