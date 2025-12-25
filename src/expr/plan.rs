/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::Expr;
use crate::expr::parser::AggKind;
use journ_core::err;
use journ_core::error::JournResult;

pub struct Plan<'h> {
    column_spec: Vec<Expr<'h>>,
    group_by: Vec<Expr<'h>>,
    agg_functions: Vec<AggKind<'h>>,
}

impl<'h> Plan<'h> {
    pub fn new(
        column_spec: Vec<Expr<'h>>,
        group_by: Vec<Expr<'h>>,
        agg_functions: Vec<AggKind<'h>>,
    ) -> Self {
        Plan { column_spec, group_by, agg_functions }
    }

    pub fn column_spec(&self) -> &[Expr<'h>] {
        &self.column_spec
    }

    pub fn agg_functions(&self) -> &[AggKind<'h>] {
        &self.agg_functions
    }

    pub fn group_by(&self) -> &[Expr<'h>] {
        &self.group_by
    }

    /// Checks whether aggregation functions are used correctly in the plan.
    /// - If `group_by` is empty, no aggregation functions are allowed in `column_spec`.
    /// - If `group_by` is not empty, all identifiers in `column_spec` must either appear in `group_by`,
    ///   refer to an alias of another column or be used within an aggregation function.
    /// - Nested aggregation functions are not allowed.
    pub fn validate(&self) -> JournResult<()> {
        if self.group_by.is_empty() {
            // No grouping, so no aggregation allowed
            if self
                .column_spec
                .iter()
                .any(|expr| expr.iter().any(|e| matches!(e, Expr::AggFunction { .. })))
            {
                return Err(err!("Aggregation functions are not allowed without grouping"));
            }
            Ok(())
        } else {
            self.validate_no_nested_aggregates()?;
            Ok(())
        }
    }

    fn validate_no_nested_aggregates(&self) -> JournResult<()> {
        for expr in &self.column_spec {
            if expr.iter().any(|e| {
                matches!(e, Expr::AggFunction { .. })
                    && e.children().any(|inner| matches!(inner, Expr::AggFunction { .. }))
            }) {
                return Err(err!("Nested aggregate functions are not allowed"));
            }
        }
        Ok(())
    }
}
