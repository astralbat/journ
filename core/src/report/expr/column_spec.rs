/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::report::expr::Expr;
use crate::report::expr::parser::AggKind;

/// A column specification consists of column expressions. If any column `Expr` should reference an aggregation function,
/// it's referenced by an index into `agg_functions`.
#[derive(Clone)]
pub struct ColumnSpec<'h> {
    exprs: Vec<Expr<'h>>,
    agg_functions: Vec<AggKind<'h>>,
    /// Whether an error should be reported if any expr cannot be evaluated
    lenient: bool,
}

impl<'h> ColumnSpec<'h> {
    pub fn new(exprs: Vec<Expr<'h>>, agg_functions: Vec<AggKind<'h>>) -> Self {
        Self { exprs, agg_functions, lenient: false }
    }

    pub fn exprs(&self) -> &[Expr<'h>] {
        &self.exprs
    }

    /// Gets the aggregate function at column `index` if one exists there,
    /// otherwise, returns `None`.
    pub fn get_agg_function(&self, index: usize) -> Option<&AggKind<'h>> {
        for expr in self.exprs[index].iter() {
            if let Expr::AggFunction(_, _, j) = expr {
                return Some(&self.agg_functions[*j]);
            }
        }
        None
    }

    pub fn agg_functions(&self) -> &[AggKind<'h>] {
        &self.agg_functions
    }

    pub fn set_lenient(&mut self, lenient: bool) {
        self.lenient = lenient;
    }

    pub fn lenient(&self) -> bool {
        self.lenient
    }
}
