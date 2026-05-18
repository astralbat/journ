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
use journ_core::journal_context::JournalContext;
use journ_core::journal_entry::EntryObject;
use journ_core::journal_entry::JournalEntry as CoreJournalEntry;
use journ_core::journal_node::NodeId;
use journ_core::metadata::Metadata;
use journ_core::python::conversion::DateTimeWrapper;
use journ_core::tree_id::TreeId;
use journ_core::valued_amount::ValuedAmount;
use journ_core::{err, parse, parsing};
use pyo3::PyResult;
use std::sync::{Arc, Mutex, MutexGuard};

#[pyclass(unsendable)]
pub struct JournalEntry {
    context: Arc<JournalContext<'static>>,
    entry: Arc<Mutex<CoreJournalEntry<'static>>>,
    file_id: FileId,
}

impl JournalEntry {
    pub fn new(
        context: Arc<JournalContext<'static>>,
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
        Ok(entry.postings().map(|pst| Posting::new(Arc::clone(&self.entry), pst.id())).collect())
    }

    #[pyo3(signature = (account, amount=None))]
    fn append_posting(&self, account: &str, amount: Option<&str>) -> PyLedgerResult<Posting> {
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
        Ok(Posting::new(Arc::clone(&self.entry), posting_id))
    }

    fn metadata_value(&self, key: &str) -> Option<String> {
        let entry = self.entry.lock().unwrap();
        entry
            .metadata()
            .filter(|m| m.key() == key)
            .next()
            .and_then(|m| m.value().map(|v| v.to_string()))
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

    fn is_balanced(&self) -> PyResult<bool> {
        let entry = self.entry.lock().unwrap();
        // Use a clone so as not to change the entry; this would be an unexpected side effect.
        let mut entry_clone = entry.clone();
        Ok(entry_clone.check().is_ok())
    }

    fn __str__(&self) -> String {
        format!("{}", self.entry.lock().unwrap())
    }

    fn __eq__(&self, other: &Self) -> bool {
        let entry = self.entry.lock().unwrap();
        let other_entry = other.entry.lock().unwrap();
        entry.is_duplicate_of(&*other_entry)
    }
}

impl JournalEntry {
    pub(super) fn entry_ref(&self) -> MutexGuard<CoreJournalEntry<'static>> {
        self.entry.lock().unwrap()
    }
}
