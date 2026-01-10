/*
 * Copyright (c) 2021-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::pool_event::PoolEvent;

pub struct CapitalGains<'h> {
    events: Vec<PoolEvent<'h>>,
}

impl<'h> CapitalGains<'h> {
    pub fn new(events: Vec<PoolEvent<'h>>) -> Self {
        Self { events }
    }

    pub fn events(&self) -> &[PoolEvent<'h>] {
        &self.events
    }
}
