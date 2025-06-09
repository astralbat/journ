/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::journal_entry::JournalEntry;
use pyo3::prelude::*;

/// A Python iterator over JournalEntry.
#[pyclass(unsendable)]
pub struct EntryIterator {
    iter: Box<dyn Iterator<Item = JournalEntry>>, // A boxed Rust iterator
}

impl EntryIterator {
    pub fn new(iter: Box<dyn Iterator<Item = JournalEntry>>) -> Self {
        EntryIterator { iter }
    }
}

#[pymethods]
impl EntryIterator {
    fn __iter__(slf: PyRef<Self>) -> PyRef<Self> {
        slf
    }

    fn __next__(mut slf: PyRefMut<Self>) -> Option<JournalEntry> {
        slf.iter.next()
    }
}
