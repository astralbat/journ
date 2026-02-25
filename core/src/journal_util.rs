/*
 * Copyright (c) 2023-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */

#[macro_export]
macro_rules! unit {
    ($code:expr) => {{
        let unit = $crate::unit::Unit::new($code);
        Box::leak(Box::new(unit))
    }};
}

#[macro_export]
macro_rules! val {
    ($str:expr) => {{
        let mut config = $crate::config!();
        let config_cell = std::cell::RefCell::new(&mut config);
        let (_, va) = $crate::parsing::entry::valued_amount(
            $crate::parsing::StringInput::new_extra($str, &config_cell),
        )
        .unwrap();
        va
    }};
}

/// Handy utility for quickly matching and mapping enums. See https://github.com/rust-lang/rfcs/issues/2960
#[macro_export]
macro_rules! match_map {
    ($expression:expr, $( $pattern:pat_param )|+ $( if $guard: expr )? => $ret:expr) => {
        match $expression {
            $( $pattern )|+ $( if $guard )? => Some($ret),
            _ => None
        }
    }
}

/// Handy utility for matching a pattern and doing something with it.
/// # Panics
/// If the match fails.
#[macro_export]
macro_rules! match_then {
    ($expression:expr, $( $pattern:pat_param )|+ $( if $guard: expr )? => $then:expr) => {
        match $expression {
            $( $pattern )|+ $( if $guard )? => { $then },
            _ => panic!("Match failed")
        }
    }
}
