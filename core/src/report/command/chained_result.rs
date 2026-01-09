/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::report::table2::Table;
use yaml_rust2::Yaml;

pub enum ChainingResult<'cell> {
    Table(Table<'cell>),
    Yaml(Yaml),
}

impl<'cell> ChainingResult<'cell> {
    pub fn into_table(self) -> Option<Table<'cell>> {
        match self {
            Self::Table(table) => Some(table),
            Self::Yaml(_) => None,
        }
    }

    pub fn into_yaml(self) -> Option<Yaml> {
        match self {
            Self::Yaml(yaml) => Some(yaml),
            Self::Table(_) => None,
        }
    }
}
