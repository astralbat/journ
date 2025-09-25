/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::bindings_pyo3::{PyLedgerError, PyLedgerResult};
use journ_core::journal::Journal as CoreJournal;
use journ_core::journal_entry::JournalEntry as CoreJournalEntry;
use journ_core::posting::PostingId;
use journ_core::valued_amount::Valuation;
use journ_core::{err, parse, parsing};
use rust_decimal::Decimal;
use std::sync::{Arc, Mutex};

#[pyclass(unsendable)]
pub struct Posting {
    journal: Arc<Mutex<CoreJournal<'static>>>,
    entry: Arc<Mutex<CoreJournalEntry<'static>>>,
    posting_id: PostingId<'static>,
}
impl Posting {
    pub fn new(
        journal: Arc<Mutex<journ_core::journal::Journal<'static>>>,
        entry: Arc<Mutex<CoreJournalEntry<'static>>>,
        posting_id: PostingId<'static>,
    ) -> Self {
        Posting { journal, entry, posting_id }
    }
}

#[pymethods]
impl Posting {
    fn account(&self) -> PyLedgerResult<String> {
        let entry = self.entry.lock().unwrap();
        let pst = entry.find_posting(self.posting_id).unwrap();
        Ok(pst.account().to_string())
    }

    fn amount_quantity(&self) -> PyLedgerResult<Decimal> {
        let entry = self.entry.lock().unwrap();
        let pst = entry.find_posting(self.posting_id).unwrap();
        Ok(pst.amount().quantity())
    }

    fn set_unit_value(&self, unit_value: &str) -> PyLedgerResult<()> {
        let mut entry = self.entry.lock().unwrap();
        let mut config = entry.config().clone();
        let pst = entry.find_posting_mut(self.posting_id).unwrap();
        let unit_value_alloc = crate::bindings_pyo3::ALLOCATOR.alloc(unit_value.to_string());

        let parse_res = parse!(unit_value_alloc, parsing::amount::amount, &mut config).0?.1;

        if !parse_res.is_positive() {
            return Err(PyLedgerError(err!("Unit value must be positive")));
        }
        if pst.value_units().find(|u| *u == parse_res.unit()).is_some() {
            return Err(PyLedgerError(err!(
                "Valuation already set for unit: {}",
                parse_res.unit()
            )));
        }

        pst.set_valuation(Valuation::new_unit(parse_res));
        Ok(())
    }

    fn set_total_value(&self, total_value: &str) -> PyLedgerResult<()> {
        let mut entry = self.entry.lock().unwrap();
        let mut config = entry.config().clone();
        let pst = entry.find_posting_mut(self.posting_id).unwrap();
        let total_value_alloc = crate::bindings_pyo3::ALLOCATOR.alloc(total_value.to_string());

        let parse_res = parse!(total_value_alloc, parsing::amount::amount, &mut config).0?.1;
        if !parse_res.is_positive() {
            return Err(PyLedgerError(err!("Total value must be positive")));
        }
        if pst.value_units().find(|u| *u == parse_res.unit()).is_some() {
            return Err(PyLedgerError(err!(
                "Valuation already set for unit: {}",
                parse_res.unit()
            )));
        }

        pst.set_valuation(Valuation::new_total(parse_res, false));
        Ok(())
    }
}
