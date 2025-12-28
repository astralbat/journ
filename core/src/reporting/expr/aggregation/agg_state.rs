/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::err;
use crate::error::JournResult;
use crate::reporting::expr::{ColumnValue, IdentifierContext};

/// A trait for maintaining the state of an aggregation operation. E.g. sum(), count(), avg(), etc.
pub trait AggState<'h> {
    /// Add a value to the aggregation state.
    fn add(&mut self, context: &mut dyn IdentifierContext<'h>) -> JournResult<()>;

    fn merge(&mut self, _other: &dyn AggState<'h>) -> JournResult<()> {
        Err(err!("Merging aggregation states is not supported for this aggregation"))
    }

    /// Finalize the aggregation and return the result.
    fn finalize(&self) -> ColumnValue<'h>;
}
