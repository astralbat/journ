/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::bindings_pyo3::{PyLedgerError, PyLedgerResult};
use crate::posting::Posting;
use journ_core::amount::AmountExpr;
use journ_core::journal::Journal as CoreJournal;
use journ_core::journal_entry::EntryObject;
use journ_core::journal_entry::JournalEntry as CoreJournalEntry;
use journ_core::journal_node::NodeId;
use journ_core::metadata::Metadata;
use journ_core::parsing::text_block::TextBlock;
use journ_core::python::conversion::DateTimeWrapper;
use journ_core::valued_amount::ValuedAmount;
use journ_core::{err, parse, parsing};
use pyo3::PyResult;
use std::sync::{Arc, Mutex};

#[pyclass(unsendable)]
pub struct JournalEntry {
    journal: Arc<Mutex<CoreJournal<'static>>>,
    entry: Arc<Mutex<CoreJournalEntry<'static>>>,
    node_id: NodeId<'static>,
}

impl JournalEntry {
    pub fn new(
        journal: Arc<Mutex<CoreJournal<'static>>>,
        entry: Arc<Mutex<CoreJournalEntry<'static>>>,
        node_id: NodeId<'static>,
    ) -> Self {
        JournalEntry { journal, entry, node_id }
    }
}

#[pymethods]
impl JournalEntry {
    fn file_id(&self) -> PyLedgerResult<u32> {
        Ok(self.node_id.id())
    }

    fn datetime_from(&self) -> PyLedgerResult<DateTimeWrapper> {
        let entry = self.entry.lock().unwrap();
        Ok(DateTimeWrapper(entry.date_and_time().datetime_from()))
    }

    fn aux_datetime(&self) -> PyLedgerResult<Option<DateTimeWrapper>> {
        let entry = self.entry.lock().unwrap();
        Ok(entry.date_and_time().aux_date_time().map(|d| DateTimeWrapper(d.datetime())))
    }

    fn description(&self) -> PyLedgerResult<String> {
        let entry = self.entry.lock().unwrap();
        Ok(entry.description().to_string())
    }

    fn postings(&self) -> PyLedgerResult<Vec<Posting>> {
        let entry = self.entry.lock().unwrap();
        Ok(entry
            .postings()
            .map(|pst| Posting::new(Arc::clone(&self.journal), Arc::clone(&self.entry), pst.id()))
            .collect())
    }

    fn append_posting(&self, account: &str, amount: Option<&str>) -> PyLedgerResult<Posting> {
        let mut entry = self.entry.lock().unwrap();
        let mut config = entry.config().clone();
        let account = config.get_or_create_account(account);
        let money = match amount {
            Some(amount) => {
                let alloc_amount = crate::bindings_pyo3::ALLOCATOR.alloc(amount.to_string());
                Some(
                    parse!(alloc_amount, parsing::amount::amount, &mut config)
                        .0
                        .map(|r| r.1)
                        .map_err(|e| err!(e; "append_posting()"))?,
                )
            }
            None => None,
        };
        let pst = journ_core::posting::Posting::new(
            None,
            "  ",
            account,
            money
                .map(|a| {
                    ValuedAmount::new_in(
                        AmountExpr::new(a, "  ", None),
                        &crate::bindings_pyo3::ALLOCATOR,
                    )
                })
                .unwrap_or(ValuedAmount::nil()),
            None,
            None,
        );
        let posting_id = entry.append_posting(pst).id();
        Ok(Posting::new(Arc::clone(&self.journal), Arc::clone(&self.entry), posting_id))
    }

    fn append_metadata(&self, key: &str, value: &str) -> PyLedgerResult<()> {
        let mut entry = self.entry.lock().unwrap();
        let block_str = crate::bindings_pyo3::ALLOCATOR.alloc(if entry.objects().is_empty() {
            format!("  +{}  {}", key, value)
        } else {
            format!("\n  +{}  {}", key, value)
        });
        let block = crate::bindings_pyo3::ALLOCATOR.alloc(TextBlock::from(block_str.as_str()));
        entry.append_object(EntryObject::Metadata(Metadata::from(&*block)));
        Ok(())
    }

    fn append_comment(&self, comment: &str) -> PyLedgerResult<()> {
        let mut entry = self.entry.lock().unwrap();
        if !comment.trim_start().starts_with(";") {
            return Err(err!("Comments need to start with ';'").into());
        }
        entry.append_object(EntryObject::Comments(
            crate::bindings_pyo3::ALLOCATOR.alloc(comment.to_string()),
        ));
        Ok(())
    }

    fn insert(&self) -> PyResult<()> {
        let mut journal = self.journal.lock().unwrap();
        let entry = self.entry.lock().unwrap().clone();
        journal.insert_entry(entry, &self.node_id).map_err(|e| PyLedgerError(e))?;
        Ok(())
    }

    fn append(&self) -> PyLedgerResult<()> {
        let mut journal = self.journal.lock().unwrap();
        let entry = self.entry.lock().unwrap().clone();
        journal.append_entry(entry, &self.node_id)?;
        Ok(())
    }

    fn print(&self) -> PyLedgerResult<()> {
        println!("{}", self.entry.lock().unwrap());
        Ok(())
    }
}
