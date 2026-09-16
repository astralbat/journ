/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::err;
use crate::error::{JournError, JournResult};
use crate::report::expr::aggregation::AggState;
use crate::report::expr::column_spec::ColumnSpec;
use crate::report::expr::context::IdentifierContext;
use crate::report::expr::parser::AggKind;
use crate::report::expr::{ColumnValue, Expr, ScalarExpr};
use std::hash::Hash;
use std::ops::Deref;

#[derive(Debug, Clone)]
pub struct GroupKey<'h> {
    values: Vec<(ScalarExpr, ColumnValue<'h>)>,
}
impl<'h> GroupKey<'h> {
    pub fn new(values: Vec<(ScalarExpr, ColumnValue<'h>)>) -> Self {
        GroupKey { values }
    }

    pub fn values(&self) -> &[(ScalarExpr, ColumnValue<'h>)] {
        &self.values
    }

    /// Gets the value for the given expression, also matching by alias if necessary.
    pub fn get(&self, expr: &Expr) -> Option<&ColumnValue<'h>> {
        self.values.iter().find_map(|(e, v)| if e.eq_expr_or_alias(expr) { Some(v) } else { None })
    }

    pub fn aliases(&self) -> impl Iterator<Item = (&str, &ColumnValue<'h>)> {
        self.values.iter().filter_map(|(e, v)| {
            if let Expr::Aliased(_, alias) = e.deref() { Some((alias.as_str(), v)) } else { None }
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
        Some(self.cmp(other))
    }
}

impl Ord for GroupKey<'_> {
    /// Compare each value in the group.
    /// Here, an undefined value < defined value.
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        for (i, (_expr, value)) in self.values.iter().enumerate() {
            match value.partial_cmp(&other.values[i].1) {
                Some(std::cmp::Ordering::Equal) => continue,
                None if value.is_undefined() && other.values[i].1.is_undefined() => continue,
                None if value.is_undefined() => return std::cmp::Ordering::Less,
                None if other.values[i].1.is_undefined() => return std::cmp::Ordering::Greater,
                non_eq => return non_eq.unwrap(),
            }
        }
        std::cmp::Ordering::Equal
    }
}

impl Hash for GroupKey<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (_expr, value) in &self.values {
            value.hash(state);
        }
    }
}

pub struct GroupState<'h, 'a> {
    exprs: Vec<Expr>,
    aggs: Vec<Box<dyn AggState<'h, 'a> + 'h>>,
}

impl<'h, 'a> GroupState<'h, 'a> {
    pub fn new(exprs: Vec<Expr>, aggs: Vec<Box<dyn AggState<'h, 'a> + 'h>>) -> Self {
        GroupState { exprs, aggs }
    }

    pub fn aggs(&self) -> &[Box<dyn AggState<'h, 'a> + 'h>] {
        &self.aggs
    }

    pub fn exprs(&self) -> &[Expr] {
        &self.exprs
    }

    pub fn add(&mut self, context: &mut dyn IdentifierContext<'h, 'a>) -> JournResult<()> {
        for agg in &mut self.aggs {
            agg.add(context)?;
        }
        Ok(())
    }

    pub fn merge(&mut self, other: &GroupState<'h, 'a>) -> JournResult<()> {
        if self.aggs.len() != other.aggs.len() {
            return Err(err!("Cannot merge GroupState with different number of aggregations"));
        }
        for (agg_self, agg_other) in self.aggs.iter_mut().zip(other.aggs.iter()) {
            // Assuming each AggState implementation has a merge method
            agg_self.merge(agg_other.as_ref())?;
        }
        Ok(())
    }

    pub fn finalize(&self) -> Vec<ColumnValue<'h>> {
        self.aggs.iter().map(|agg| agg.finalize()).collect()
    }
}

impl<'h, 'a> TryFrom<ColumnSpec> for GroupState<'h, 'a> {
    type Error = JournError;
    fn try_from(spec: ColumnSpec) -> JournResult<Self> {
        let aggs = spec.agg_functions().iter().map(AggKind::make).collect::<Result<_, _>>()?;
        Ok(GroupState { exprs: spec.into_exprs(), aggs })
    }
}
