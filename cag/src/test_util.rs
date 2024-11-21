/*
 * Copyright (c) 2022-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
/// Creates a Deal object, parsing the input string provided.
#[macro_export]
macro_rules! deal {
    ($config:expr, $date:expr, $str:expr) => {{
        let je = match_map!(entry_dir!($config, $date), journ_core::directive::Directive::Entry(e) => e).unwrap();
        let md = journ_core::metadata::Metadata::new("    ", "CGT-Deal".into(), "  ".into(), Some($str.into()));
        $crate::cgt_journal_entry::CapitalGainsEntryMetadata::parse_deal(&md, je, 0).unwrap()
    }};
}

#[macro_export]
macro_rules! adjustment {
    ($config:expr, $date:expr, $str:expr) => {{
        let je = match_map!(entry_dir!($config, $date), journ_core::directive::Directive::Entry(e) => e).unwrap();
        let md = journ_core::metadata::Metadata::new("    ", "CGT-Adjustment".into(), "  ".into(), Some($str.into()));
        $crate::cgt_journal_entry::CapitalGainsEntryMetadata::parse_adjustment(&md, &je, 0).unwrap()
    }};
}
