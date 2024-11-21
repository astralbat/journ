/*
 * Copyright (c) 2017-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
#![feature(box_into_inner)]
#![feature(get_mut_unchecked)]
#![feature(allocator_api)]

pub mod adjustment;
//pub mod capital_gains;
mod capital_gains;
pub mod cgt_configuration;
pub mod cgt_journal_entry;
pub mod computer;
pub mod deal;
mod deal_group;
mod deal_holding;
pub mod dealing_event;
mod expenses;
mod mod_cgt;
pub mod module_init;
pub mod pool;
mod pool_event;
mod pool_manager;
pub mod ruleset;
mod test_util;

#[macro_use]
extern crate lazy_static;
extern crate core;
extern crate log;
