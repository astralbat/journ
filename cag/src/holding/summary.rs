/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjusted_value::AdjustedValue;
use crate::holding::DealHolding;
use journ_core::amount::Amount;
use journ_core::datetime::JDateTimeRange;
use journ_core::journal_entry::JournalEntry;
use journ_core::metadata::Metadata;
use linked_hash_set::LinkedHashSet;
use std::rc::Rc;

/// A snapshot summary of a deal holding for including in a parent tree of deal holdings and for events.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct DealHoldingSummary<'h> {
    parent: Option<Rc<Self>>,
    date: JDateTimeRange,
    adjusted_value: AdjustedValue<'h>,
    // Entries included for provenance: metadata, descriptions and file info.
    entries: Vec<&'h JournalEntry<'h>>,
}

impl<'h> DealHoldingSummary<'h> {
    pub fn new(
        parent: Option<Rc<Self>>,
        date: JDateTimeRange,
        adjusted_value: AdjustedValue<'h>,
        entries: Vec<&'h JournalEntry<'h>>,
    ) -> Self {
        Self { parent, date, adjusted_value, entries }
    }

    pub fn parent(&self) -> Option<&Rc<Self>> {
        self.parent.as_ref()
    }

    pub fn datetime(&self) -> JDateTimeRange {
        self.date
    }

    pub fn adjusted_value(&self) -> AdjustedValue<'h> {
        self.adjusted_value
    }

    pub fn amount(&self) -> Amount<'h> {
        self.adjusted_value.amount()
    }

    pub fn value(&self) -> Amount<'h> {
        self.adjusted_value.value()
    }

    pub fn expenses(&self) -> Amount<'h> {
        self.adjusted_value.expenses()
    }

    pub fn entry_description(&self) -> LinkedHashSet<&'h str> {
        let mut descriptions = LinkedHashSet::new();
        for entry in &self.entries {
            descriptions.insert(entry.description());
        }
        descriptions
    }

    pub fn entry_metadata_by_key(&self, key: &str) -> LinkedHashSet<&Metadata<'h>> {
        let mut metadata_set = LinkedHashSet::new();
        for entry in &self.entries {
            for metadata in entry.metadata_by_key(key) {
                metadata_set.insert(metadata);
            }
        }
        metadata_set
    }
}

impl<'h> From<&DealHolding<'h>> for DealHoldingSummary<'h> {
    fn from(holding: &DealHolding<'h>) -> Self {
        Self {
            parent: holding.split_parent().map(Rc::clone),
            date: holding.datetime(),
            adjusted_value: holding.adjusted_value(),
            entries: holding.entries().collect(),
        }
    }
}
