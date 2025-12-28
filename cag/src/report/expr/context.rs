/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use journ_core::journal::Journal;
use journ_core::reporting::expr::{ColumnValue, EvalContext, IdentifierContext};
use smartstring::alias::String as SS;
use std::collections::HashMap;

pub struct CagContext<'h> {
    journal: Journal<'h>,
    variables: HashMap<SS, ColumnValue<'h>>,
}

impl<'h> EvalContext<'h> for CagContext<'h> {
    fn journal(&self) -> &Journal<'h> {
        &self.journal
    }
}

impl<'h> IdentifierContext<'h> for CagContext<'h> {
    fn variables(&self) -> &HashMap<SS, ColumnValue<'h>> {
        &self.variables
    }

    fn variables_mut(&mut self) -> &mut HashMap<SS, ColumnValue<'h>> {
        &mut self.variables
    }

    fn eval_identifier(&self, identifier: &str) -> Option<ColumnValue<'h>> {
        self.variables().get(identifier.to_lowercase().as_str()).cloned()
    }
}
