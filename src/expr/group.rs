/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::aggregation::AggState;
use crate::expr::context::IdentifierContext;
use crate::expr::parser::AggKind;
use crate::expr::plan::Plan;
use crate::expr::{ColumnValue, Expr};
use journ_core::error::JournResult;
use std::hash::Hash;
use std::iter::Sum;

#[derive(Debug, Clone)]
pub struct GroupKey<'h> {
    values: Vec<(Expr<'h>, ColumnValue<'h>)>,
}
impl<'h> GroupKey<'h> {
    pub fn new(values: Vec<(Expr<'h>, ColumnValue<'h>)>) -> Self {
        GroupKey { values }
    }

    pub fn values(&self) -> &[(Expr<'h>, ColumnValue<'h>)] {
        &self.values
    }

    /// Gets the value for the given expression, also matching by alias if necessary.
    pub fn get(&self, expr: &Expr<'h>) -> Option<&ColumnValue<'h>> {
        self.values.iter().find_map(|(e, v)| if e.eq_expr_or_alias(expr) { Some(v) } else { None })
    }

    pub fn aliases(&self) -> impl Iterator<Item = (&'h str, &ColumnValue<'h>)> {
        self.values.iter().filter_map(|(e, v)| {
            if let Expr::Aliased(_, alias) = e { Some((*alias, v)) } else { None }
        })
    }
}
impl PartialEq for GroupKey<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.values == other.values
    }
}
impl Eq for GroupKey<'_> {}

impl PartialOrd for GroupKey<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        for (i, (_expr, value)) in self.values.iter().enumerate() {
            match value.partial_cmp(&other.values[i].1) {
                Some(std::cmp::Ordering::Equal) => continue,
                non_eq => return non_eq,
            }
        }
        Some(std::cmp::Ordering::Equal)
    }
}

impl Ord for GroupKey<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Hash for GroupKey<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (_expr, value) in &self.values {
            value.hash(state);
        }
    }
}

pub struct GroupState<'h> {
    aggs: Vec<Box<dyn AggState<'h> + 'h>>,
}

impl<'h> GroupState<'h> {
    pub fn new(plan: &Plan<'h>) -> JournResult<Self> {
        let aggs = plan.agg_functions().iter().map(AggKind::make).collect::<Result<_, _>>()?;
        Ok(GroupState { aggs })
    }

    pub fn add(&mut self, context: &mut dyn IdentifierContext<'h>) -> JournResult<()> {
        for agg in &mut self.aggs {
            agg.add(context)?;
        }
        Ok(())
    }

    pub fn finalize(&self) -> Vec<ColumnValue<'h>> {
        self.aggs.iter().map(|agg| agg.finalize()).collect()
    }
}
