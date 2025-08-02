/*
 * Copyright (c) 2017-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
//#![feature(drain_filter)]
#![feature(cell_leak)]
#![feature(hash_set_entry)]
//#![feature(const_box)]
#![feature(allocator_api)]
#![feature(btreemap_alloc)]
#![feature(type_alias_impl_trait)]
#![feature(negative_impls)]
#![feature(iter_intersperse)]
#![feature(trait_upcasting)]
#![feature(box_into_inner)]
#![feature(once_cell_get_mut)]
#![feature(cow_is_borrowed)]
#![feature(try_trait_v2)]
#![feature(mapped_lock_guards)]
extern crate chrono_tz;
#[macro_use]
extern crate rust_decimal_macros;
#[macro_use]
extern crate log;
extern crate core;

pub mod account;
pub mod amount;
pub mod arguments;
pub mod configuration;
pub mod date_and_time;
pub mod directive;
pub mod error;
pub mod ext;
pub mod journal;
pub mod journal_entry;
pub mod journal_entry_flow;
pub mod journal_node;
pub mod metadata;
pub mod money_util;
pub mod parsing;
pub mod posting;
pub mod postings_aggregation;
pub mod price;
pub mod price_db;
pub mod python;
pub mod reporting;
pub mod valued_amount;

pub mod alloc;
//pub mod apportionment;
mod journal_node_segment;
pub mod journal_util;
pub mod module;
#[cfg(test)]
pub mod test_util;
pub mod unit;
pub mod valuer;
