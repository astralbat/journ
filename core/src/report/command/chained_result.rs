/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::report::expr::GroupState;
use crate::report::table2::Table;
use yaml_rust2::Yaml;

pub enum ChainingResult<'h, 'a, 'cell> {
    Table { table: Table<'cell>, grand_total: Option<GroupState<'h, 'a>> },
    Yaml(Yaml),
}

impl<'h, 'a, 'cell> ChainingResult<'h, 'a, 'cell> {
    pub fn into_table(self) -> Option<(Table<'cell>, Option<GroupState<'h, 'a>>)> {
        match self {
            Self::Table { table, grand_total } => Some((table, grand_total)),
            Self::Yaml(_) => None,
        }
    }

    pub fn into_yaml(self) -> Option<Yaml> {
        match self {
            Self::Yaml(yaml) => Some(yaml),
            Self::Table { table: _, grand_total: _ } => None,
        }
    }
}
