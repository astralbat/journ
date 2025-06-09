/*
 * Copyright (c) 2020-2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::entry_iterator::EntryIterator;
use crate::journal_entry::JournalEntry;
use crate::posting::Posting;
use ansi_term::{Color, Style};
use atty::Stream;
use bumpalo_herd::Herd;
use env_logger::Builder;
use journ_core::alloc::HerdAllocator;
use journ_core::amount::Amount;
use journ_core::date_and_time::{DateAndTime, JDateTime, JDateTimeRange};
use journ_core::err;
use journ_core::error::JournError;
use journ_core::journal_node::NodeId;
use journ_core::module::MODULES;
use journ_core::parsing::text_block::TextBlock;
use journ_core::python::conversion::DateTimeWrapper;
use journ_core::python::mod_ledger;
use journ_core::reporting::balance::AccountBalances;
use journ_core::unit::{Unit, UnitFormat};
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::types::PyDateTime;
use rust_decimal::Decimal;
use std::error::Error;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::ops::Bound;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::{Arc, LazyLock, Mutex};
use std::time::SystemTime;

static START: LazyLock<SystemTime> = LazyLock::new(|| SystemTime::now());

#[pymodule]
fn journ(py: Python, m: &PyModule) -> PyResult<()> {
    std::env::set_var("RUST_BACKTRACE", "full");
    Builder::from_default_env()
        .format(|buf, record| {
            writeln!(
                buf,
                "{:08} [{}] - {}",
                START.elapsed().unwrap().as_micros(),
                record.level(),
                record.args()
            )
        })
        .init();

    m.add_class::<Journal>()?;
    m.add_class::<Configuration>()?;
    m.add_class::<JournalEntry>()?;
    m.add_class::<Posting>()?;
    m.add_class::<EntryIterator>()?;
    m.add("JournError", py.get_type::<JournPyError>())?;
    m.add_function(wrap_pyfunction!(format_amount, m)?)?;
    Ok(())
}

/// Create a wrapper error so that we can override its display behaviour.
#[derive(Debug)]
pub(crate) struct PyLedgerError(pub JournError);

pub(crate) type PyLedgerResult<T> = Result<T, PyLedgerError>;

impl Error for PyLedgerError {}

impl Display for PyLedgerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut err_style = Style::default();
        if atty::is(Stream::Stderr) {
            err_style = err_style.fg(Color::Red);
        }

        write!(f, "{}", err_style.paint(&self.0.to_string()))
    }
}

impl From<JournError> for PyLedgerError {
    fn from(err: JournError) -> Self {
        PyLedgerError(err)
    }
}

impl From<PyLedgerError> for PyErr {
    fn from(err: PyLedgerError) -> Self {
        JournPyError::new_err(err.to_string())
    }
}
create_exception!(journ, JournPyError, pyo3::exceptions::PyException);

/// This needs to be a static global as the Journal pyclass cannot have lifetime parameters.
/// The inner 'h lifetime is thus 'static.
/// If many new Journals would be created, we could have a pool of HERDS that
/// are deallocated when the Journal is dropped.
static HERD: LazyLock<Herd> = LazyLock::new(|| Herd::new());
pub(crate) static ALLOCATOR: LazyLock<HerdAllocator<'static>> =
    LazyLock::new(|| HerdAllocator::new(&HERD));

/// Declaring as unsendable means that the class will panic when accessed by another thread.
/// Journal isn't Send through the use of Rc.
#[pyclass(unsendable)]
struct Journal {
    journal: Arc<Mutex<journ_core::journal::Journal<'static>>>,
    //allocator: HerdAllocator<'static>,
    edit_node_id: Mutex<NodeId<'static>>,
}

#[pymethods]
impl Journal {
    #[new]
    fn new(filename: &str) -> PyLedgerResult<Self> {
        //let allocator = HerdAllocator::new(&HERD);
        let args =
            journ_core::arguments::Arguments::set(journ_core::arguments::Arguments::default());
        let filename = &**ALLOCATOR.alloc(PathBuf::from(filename));

        if MODULES.lock().unwrap().is_empty() {
            MODULES.lock().unwrap().push(journ_cag::module_init::initialize());
        }
        /*
        let rust_journal = TextBlock::from_file(filename, &ALLOCATOR, None).and_then(|block| {
            journ_core::journal::Journal::parse(args, filename, block, &ALLOCATOR)
        })?;*/

        let rust_journal = Python::with_gil(|py| {
            let python = py;

            // Create the ledger module if it doesn't exist.
            let sys_modules = py.import("sys").unwrap().getattr("modules").unwrap();
            if sys_modules.get_item("ledger").is_err() {
                let mod_ledger = PyModule::new(py, "ledger").unwrap();
                mod_ledger::ledger(py, mod_ledger).unwrap();
                // Insert the module into sys.modules
                sys_modules.set_item("ledger", mod_ledger).unwrap();
            }

            // The main script can exit with an exception during shutdown unless we import the
            // threading module in the main thread. See https://bugs.python.org/issue31517.
            python.import("threading").unwrap();

            // The parser works with Python in another thread so we need to release the gil
            // temporarily.
            python.allow_threads(|| {
                TextBlock::from_file(filename, &ALLOCATOR, None).and_then(|block| {
                    journ_core::journal::Journal::parse(args, filename, block, &ALLOCATOR)
                })
            })
        })
        .map_err(PyLedgerError)?;
        let node_id = rust_journal.find_node_by_filename(filename).unwrap().id();
        let journal = Journal {
            journal: Arc::new(Mutex::new(rust_journal)),
            //allocator,
            edit_node_id: Mutex::new(*node_id),
        };
        Ok(journal)
    }

    fn edit_file(&self, name: &str) -> PyLedgerResult<()> {
        let journal = self.journal.lock().unwrap();
        match journal.find_node_by_filename(&PathBuf::from(name)) {
            Some(jf) => {
                *self.edit_node_id.lock().unwrap() = *jf.id();
                Ok(())
            }
            None => Err(PyLedgerError(err!("No filename found: {}", name))),
        }
    }

    fn edit_file_id(&self) -> PyLedgerResult<u32> {
        Ok(self.edit_node_id.lock().unwrap().id())
    }

    fn with_entry_range<'a>(
        &'a self,
        py: Python,
        start_time: &PyDateTime,
        end_time: &PyDateTime,
        callback: PyObject,
    ) -> PyResult<()> {
        let journal = self.journal.lock().unwrap();
        let chrono_start: DateTimeWrapper = start_time
            .extract()
            .expect("datetime must be constructed with a valid Python datetime object");
        let chrono_end: DateTimeWrapper = end_time
            .extract()
            .expect("datetime must be constructed with a valid Python datetime object");

        for entry in journal.entry_range(chrono_start.0..chrono_end.0) {
            let entry = JournalEntry::new(
                Arc::clone(&self.journal),
                Arc::new(Mutex::new(journ_core::journal_entry::JournalEntry::clone(entry))),
                *entry.id().node_id(),
            );
            callback.call1(py, (entry,))?;
        }
        Ok(())
    }

    /// Finds all entries between `start_time` and `end_time` exclusive, and having an entry description
    /// equal to `description`.
    fn find_entries(
        &self,
        start_time: &PyDateTime,
        end_time: &PyDateTime,
        description: &str,
    ) -> PyResult<Vec<JournalEntry>> {
        let journal = self.journal.lock().unwrap();
        let chrono_start: DateTimeWrapper = start_time
            .extract()
            .expect("datetime must be constructed with a valid Python datetime object");
        let chrono_end: DateTimeWrapper = end_time
            .extract()
            .expect("datetime must be constructed with a valid Python datetime object");

        let mut entries = vec![];
        for entry in journal.find_entries(chrono_start.0..chrono_end.0, description) {
            entries.push(JournalEntry::new(
                Arc::clone(&self.journal),
                Arc::new(Mutex::new(journ_core::journal_entry::JournalEntry::clone(entry))),
                *entry.id().node_id(),
            ));
        }
        Ok(entries)
    }

    /// Gets the quantity balance of the specified `account`, in the specified `unit` between `start`..`end`.
    #[pyo3(signature = (account, unit, start=None, end=None))]
    fn account_bal(
        &self,
        account: &str,
        unit: &str,
        start: Option<&PyDateTime>,
        end: Option<&PyDateTime>,
    ) -> PyLedgerResult<Option<String>> {
        let mut bals = AccountBalances::new(vec![]);
        let time_range = (
            start
                .map(|t| {
                    let chrono_start: DateTimeWrapper = t
                        .extract()
                        .expect("datetime must be constructed with a valid Python datetime object");
                    Bound::Included(chrono_start.0)
                })
                .unwrap_or_else(|| Bound::Unbounded),
            end.map(|t| {
                let chrono_end: DateTimeWrapper = t
                    .extract()
                    .expect("datetime must be constructed with a valid Python datetime object");
                Bound::Excluded(chrono_end.0)
            })
            .unwrap_or_else(|| Bound::Unbounded),
        );
        let journal = self.journal.lock().unwrap();
        let account_obj = journal.root().config().get_or_create_account(account);
        for entry in journal.entry_range(time_range) {
            for pst in entry.postings() {
                if pst.account() != &account_obj {
                    continue;
                }
                bals.update_balance(pst.account(), pst.valued_amount(), false);
            }
        }
        let quantity = bals
            .account_balances(&account_obj)
            .find(|a| a.unit().code() == unit)
            .map(|a| a.quantity().to_string());
        Ok(quantity)
    }

    #[pyo3(signature = (py_datetime, description, aux_datetime=None, write_time=true))]
    fn new_entry(
        &self,
        py_datetime: &PyDateTime,
        mut description: String,
        aux_datetime: Option<&PyDateTime>,
        write_time: bool,
    ) -> PyLedgerResult<JournalEntry> {
        let journal = self.journal.lock().unwrap();
        let rust_jf = journal.node(&self.edit_node_id.lock().unwrap());
        let config = rust_jf.config().clone();

        // Convert the python datetime object in to a DateAndTime object.
        let chrono_date: DateTimeWrapper = py_datetime
            .extract()
            .expect("datetime must be constructed with a valid Python datetime object");
        let dt = DateAndTime::new(
            JDateTimeRange::new(
                JDateTime::from_datetime(
                    chrono_date.0.with_timezone(&config.timezone()),
                    Some(config.as_herd_ref().date_format()),
                    if write_time { Some(config.as_herd_ref().time_format()) } else { None },
                ),
                None,
            ),
            aux_datetime.map(|d| {
                let chrono_aux_date: DateTimeWrapper = d
                    .extract()
                    .expect("datetime must be constructed with a valid Python datetime object");
                JDateTime::from_datetime(
                    chrono_aux_date.0.with_timezone(&config.timezone()),
                    Some(config.as_herd_ref().date_format()),
                    if write_time { Some(config.as_herd_ref().time_format()) } else { None },
                )
            }),
        );

        description.insert_str(0, "  ");
        let entry = journ_core::journal_entry::JournalEntry::new(
            ALLOCATOR.alloc(*self.edit_node_id.lock().unwrap()),
            rust_jf.config().clone(),
            dt,
            ALLOCATOR.alloc(description),
            Vec::new_in(&ALLOCATOR),
        );

        Ok(JournalEntry::new(
            Arc::clone(&self.journal),
            Arc::new(Mutex::new(entry)),
            *self.edit_node_id.lock().unwrap(),
        ))
    }

    fn print(&self) -> PyLedgerResult<()> {
        let journal = self.journal.lock().unwrap();
        journal.root().print(None)?;
        Ok(())
    }

    fn print_file(&self, name: &str) -> PyLedgerResult<()> {
        let journal = self.journal.lock().unwrap();
        journal
            .find_node_by_filename(&PathBuf::from(name))
            .ok_or(PyLedgerError(err!("No filename ends with: {}", name)))?
            .print(None)?;
        Ok(())
    }

    fn write(&self) -> PyLedgerResult<()> {
        let journal = self.journal.lock().unwrap();
        journal.root().overwrite_all()?;
        Ok(())
    }
}

#[pyclass]
struct Configuration {}

/// Format's an amount using the unit's format specification, the unit and an amount.
/// The `quantity` must be a string or an object that can be converted to one.
///
/// # Examples
/// * `format_amount(1234.56, "##,###.00 USD", "USD") -> "1,234.56 USD"`
#[pyfunction]
fn format_amount(quantity: &PyAny, format_spec: &str, unit: &str) -> PyResult<String> {
    let uf: UnitFormat = format_spec.parse().map_err(|e| {
        PyValueError::new_err(format!("Invalid format specification: {}: {}", format_spec, e))
    })?;

    let dec = match Decimal::from_str(quantity.str()?.to_str()?) {
        Ok(d) => d,
        Err(e) => match Decimal::from_scientific(quantity.str()?.to_str()?) {
            Ok(d) => d,
            Err(_) => {
                return Err(PyValueError::new_err(format!(
                    "quantity cannot be parsed: '{}': {}",
                    quantity,
                    e.to_string()
                )))
            }
        },
    };

    let formatted = uf.format_precise(Amount::new(&Unit::new(unit), dec));
    Ok(formatted)
}
