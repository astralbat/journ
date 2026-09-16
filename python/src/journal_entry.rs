/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::bindings_pyo3::PyLedgerResult;
use crate::file_id::FileId;
use crate::posting::Posting;
use journ_core::error::JournError;
use journ_core::journal_context::JContext;
use journ_core::journal_entry::EntryObject;
use journ_core::journal_entry::JournalEntry as CoreJournalEntry;
use journ_core::metadata::Metadata;
use journ_core::python::conversion::DateTimeWrapper;
use journ_core::valued_amount::ValuedAmount;
use journ_core::valuer::{SystemValuer, Valuer};
use journ_core::{err, parse, parsing};
use pyo3::PyResult;
use rust_decimal::Decimal;
use std::collections::HashSet;
use std::sync::{Arc, Mutex};

#[pyclass(unsendable)]
pub struct JournalEntry {
    context: Arc<JContext<'static>>,
    entry: Arc<Mutex<CoreJournalEntry<'static>>>,
    file_id: FileId,
}

impl JournalEntry {
    pub fn new(
        context: Arc<JContext<'static>>,
        entry: Arc<Mutex<CoreJournalEntry<'static>>>,
        file_id: FileId,
    ) -> Self {
        JournalEntry { context, entry, file_id }
    }
}

#[pymethods]
impl JournalEntry {
    fn file_id(&self) -> PyLedgerResult<FileId> {
        Ok(self.file_id.clone())
    }

    fn datetime_from(&self) -> PyLedgerResult<DateTimeWrapper> {
        let entry = self.entry.lock().unwrap();
        Ok(DateTimeWrapper(entry.datetime_range().start().datetime()))
    }

    fn description(&self) -> PyLedgerResult<String> {
        let entry = self.entry.lock().unwrap();
        Ok(entry.description().to_string())
    }

    fn postings(&self) -> PyLedgerResult<Vec<Posting>> {
        let entry = self.entry.lock().unwrap();
        Ok(entry
            .postings()
            .map(|pst| Posting::new(Arc::clone(&self.context), Arc::clone(&self.entry), pst.id()))
            .collect())
    }

    #[pyo3(signature = (account, amount=None))]
    fn append_posting(&self, account: &str, amount: Option<&str>) -> PyLedgerResult<Posting> {
        self.context.with(|| {
            let mut entry = self.entry.lock().unwrap();
            let mut config = entry.config().clone();
            let account = config.get_or_create_account(account);
            let money = amount
                .map(|a| {
                    let alloc_amount = crate::bindings_pyo3::ALLOCATOR.alloc(a.to_string());
                    let amount = parse!(alloc_amount, parsing::amount::amount_expr, &mut config)
                        .map(|r| r.1)
                        .map_err(|e| err!(e; "append_posting()"))?;
                    Ok::<_, JournError>(amount)
                })
                .transpose()?;
            let pst = journ_core::posting::Posting::new(
                None,
                account,
                money
                    .map(|a| ValuedAmount::new_in(a, &crate::bindings_pyo3::ALLOCATOR))
                    .unwrap_or(ValuedAmount::nil()),
                None,
                None,
            );
            let posting_id = entry.append_posting(pst).id();
            Ok(Posting::new(Arc::clone(&self.context), Arc::clone(&self.entry), posting_id))
        })
    }

    fn metadata_value(&self, key: &str) -> Option<String> {
        self.context.with(|| {
            let entry = self.entry.lock().unwrap();
            entry
                .metadata()
                .filter(|m| m.key() == key)
                .next()
                .and_then(|m| m.value().map(|v| v.to_string()))
        })
    }

    #[pyo3(signature = (key, value=None))]
    fn append_metadata(&self, key: &str, value: Option<&str>) -> PyLedgerResult<()> {
        self.context.with(|| {
            let mut entry = self.entry.lock().unwrap();
            let config = entry.config().clone();
            let alloc_key = config.alloc(key.to_string()).as_str();
            let alloc_value = value.map(|v| config.alloc(v.to_string()).as_str());
            entry.append_object(EntryObject::Metadata(Metadata::new(
                config,
                alloc_key,
                alloc_value,
            )));
            Ok(())
        })
    }

    fn append_comment(&self, comment: &str) -> PyLedgerResult<()> {
        let mut entry = self.entry.lock().unwrap();
        if !comment.trim_start().starts_with(";") {
            return Err(err!("Comments need to start with ';'").into());
        }
        entry.append_object(EntryObject::Comment(
            crate::bindings_pyo3::ALLOCATOR.alloc(comment.to_string()),
        ));
        Ok(())
    }

    fn insert(&self) -> PyLedgerResult<()> {
        self.context.with(|| {
            let mut journal = self.context.journal_mut();
            let entry = self.entry.lock().unwrap().clone();
            journal.insert_entry(entry, &self.file_id.0)?;
            Ok(())
        })
    }

    fn append(&self) -> PyLedgerResult<()> {
        self.context.with(|| {
            let mut journal = self.context.journal_mut();
            let entry = self.entry.lock().unwrap().clone();
            journal.append_entry(entry, &self.file_id.0)?;
            Ok(())
        })
    }

    fn replace(&self) -> PyLedgerResult<bool> {
        self.context.with(|| {
            let mut journal = self.context.journal_mut();
            let entry = self.entry.lock().unwrap().clone();
            let replaced = journal.replace_entry(entry)?;
            Ok(replaced.map(|_| true).unwrap_or(false))
        })
    }

    fn remove(&self) -> bool {
        self.context.with(|| {
            let mut journal = self.context.journal_mut();
            let entry = self.entry.lock().unwrap().clone();
            journal.remove_entry(&entry)
        })
    }

    /// Checks the entry is balanced and derives elided posting amounts.
    fn check_and_derive(&self) -> PyLedgerResult<()> {
        self.context.with(|| {
            let mut entry = self.entry.lock().unwrap();
            entry.check()?;
            Ok(())
        })
    }

    /// Checks but does not elide missing posting amounts.
    fn check(&self) -> PyResult<bool> {
        self.context.with(|| {
            let entry = self.entry.lock().unwrap();
            // Use a clone so as not to change the entry; this would be an unexpected side effect.
            let mut entry_clone = entry.clone();
            Ok(entry_clone.check().is_ok())
        })
    }

    /// Performs a valuation lookup from `base_unit` @ `quantity` to `quote_unit`
    /// using this entry as the starting point.
    #[pyo3(signature = (quantity, base_unit, quote_unit))]
    fn value_amount<'py>(
        &self,
        quantity: Decimal,
        base_unit: &str,
        quote_unit: &str,
    ) -> PyLedgerResult<Decimal> {
        self.context.with(|| {
            let entry = self.entry.lock().unwrap();
            let mut config = JContext::get().journal().config().clone();
            let quote_unit = config.get_or_create_unit(quote_unit);
            let base_unit = config.get_or_create_unit(base_unit);

            let qty = {
                let valuation = SystemValuer::from(&*entry)
                    .value(quote_unit, base_unit.with_quantity(quantity))
                    .map_err(JournError::from)?;
                valuation.value().quantity()
            };
            Ok(qty)
        })
    }

    fn __str__(&self) -> String {
        // Debug format includes elided postings, which after calling check_and_derive(),
        // the user might like to see.
        format!("{:?}", self.entry.lock().unwrap())
    }

    fn __eq__(&self, other: &Self) -> bool {
        let entry = self.entry.lock().unwrap();
        let other_entry = other.entry.lock().unwrap();

        // Check date and description are the same
        if entry.datetime_range() != other_entry.datetime_range()
            || entry.description() != other_entry.description()
        {
            return false;
        }

        // Check postings are the same
        let entry_postings: HashSet<&journ_core::posting::Posting> = entry.postings().collect();
        let other_entry_postings: HashSet<&journ_core::posting::Posting> =
            other_entry.postings().collect();
        if entry_postings != other_entry_postings {
            return false;
        }

        // Check metadata is the same
        let entry_metadata: HashSet<&Metadata> = entry.metadata().collect();
        let other_entry_metadata: HashSet<&Metadata> = other_entry.metadata().collect();
        if entry_metadata != other_entry_metadata {
            return false;
        }
        true
    }
}

/*
impl JournalEntry {
    pub(super) fn entry_ref(&self) -> MutexGuard<'_, CoreJournalEntry<'static>> {
        self.entry.lock().unwrap()
    }
}*/
