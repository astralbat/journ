/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use chrono::NaiveDate;
use journ_core::amount::Amount;
use journ_core::date_and_time::JDate;
use journ_core::journal_entry::JournalEntry;
use journ_core::posting::Posting;
use std::collections::HashMap;
use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Grouping {
    /// Group by date, with the date as the key.
    Date,
    /// Group by account, with the account name as the key.
    Account,
    /// Group by metadata, with the metadata key as the key.
    Metadata,
    /// Group by description, with the description as the key.
    Description,
}

/// Represents single item in a grouped report.
pub enum GroupKey<'h, 'a> {
    Date(JDate<'h>),
    Account(&'a str),
    Description(&'a str),
}

impl fmt::Display for GroupKey<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GroupKey::Date(date) => write!(f, "Date={}", date),
            GroupKey::Account(account) => write!(f, "Account={}", account),
            GroupKey::Description(desc) => write!(f, "Description={}", desc),
        }
    }
}

pub enum GroupedItemValue<'h> {
    Amounts(Vec<Amount<'h>>),
    String(String),
}

impl<'h> std::ops::AddAssign<Amount<'h>> for GroupedItemValue<'h> {
    fn add_assign(&mut self, other: Amount<'h>) {
        if let GroupedItemValue::Amounts(amounts) = self {
            *amounts += other;
        } else {
            panic!("Cannot add Amount to GroupedItemValue::String");
        }
    }
}

pub struct GroupingValues<'h> {
    grouping: Grouping,
    values: HashMap<String, GroupedItemValue<'h>>,
}
