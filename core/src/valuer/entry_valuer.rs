/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::Amount;
use crate::err;
use crate::journal_entry::JournalEntry;
use crate::metadata::Metadata;
use crate::unit::Unit;
use crate::valuer::{ValuationError, ValuationResult, Valuer};

/// An entry valuer that looks to its own postings for valuations.
/// This is a simple valuer that does not analyze as a system of linear equations.
/// See [`LinearSystemValuer`] for that.
pub struct EntryValuer<'h, 'e> {
    entry: &'e JournalEntry<'h>,
}
impl<'h, 'e> EntryValuer<'h, 'e> {
    pub fn new(entry: &'e JournalEntry<'h>) -> Self {
        Self { entry }
    }
}

impl<'h, 'e> From<&'e JournalEntry<'h>> for EntryValuer<'h, 'e> {
    fn from(entry: &'e JournalEntry<'h>) -> Self {
        Self::new(entry)
    }
}

impl<'h> Valuer<'h> for EntryValuer<'h, '_> {
    fn value(&mut self, quote_unit: &'h Unit<'h>, amount: Amount<'h>) -> ValuationResult<'h> {
        let mut valuation = None;
        // Look for a direct unit valuation. These take precedence over total valuations.
        for pst in self.entry.postings() {
            if pst.unit() == amount.unit()
                && let Ok(val) = pst.valued_amount().unit_valuer().value(quote_unit, amount)
            {
                valuation = Some(val);
                break;
            }
        }
        // Next, look for a total valuation
        for pst in self.entry.postings() {
            if let Ok(val) = pst.valued_amount().total_valuer().value(quote_unit, amount) {
                valuation = Some(val);
                break;
            }
        }
        match valuation {
            Some(mut valuation) => {
                // Set the sources based in +valueSource metadata if possible, clearing the "@@ Entry"
                self.entry
                    .metadata()
                    .filter(|m| m.key() == "valueSource")
                    .filter_map(Metadata::value)
                    .enumerate()
                    .for_each(|(i, v)| {
                        if i == 0 {
                            valuation.clear_sources();
                        }
                        valuation.add_source(v.trim());
                    });
                Ok(valuation)
            }
            None => Err(ValuationError::Undetermined(err!("No @ or @@ value found"))),
        }
    }
}
