/*
 * Copyright (c) 2020-2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::entry_iterator::EntryIterator;
use crate::file_id::FileId;
use crate::journal_entry::JournalEntry;
use crate::posting::Posting;
use bumpalo_herd::Herd;
use chrono::DateTime;
use chrono_tz::Tz;
use env_logger::Builder;
use journ_core::alloc::HerdAllocator;
use journ_core::configuration::AlwaysIncluded;
use journ_core::datetime::{DateTimePrecision, JDateTime, JDateTimeRange};
use journ_core::err;
use journ_core::error::{BlockContextError, JournError};
use journ_core::journal_context::JContext;
use journ_core::module::MODULES;
use journ_core::parsing::text_block::{PaddingPolicy, TextBlock};
use journ_core::python::mod_ledger;
use journ_core::report::balance::AccountBalances;
use journ_core::unit::{RoundingStrategy, UnitFormat};
use journ_core::valuer::{SystemValuer, Valuer};
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::types::{PyAnyMethods, PyDateTime, PyList, PyStringMethods};
use rust_decimal::Decimal;
use std::error::Error;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::{Arc, LazyLock, Mutex};
use std::time::SystemTime;

static START: LazyLock<SystemTime> = LazyLock::new(|| SystemTime::now());

#[pymodule]
fn journ<'py>(m: &Bound<'py, PyModule>) -> PyResult<()> {
    // Should be safe as we're not threaded ourselves.
    unsafe {
        std::env::set_var("RUST_BACKTRACE", "full");
    }

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
    m.add("JournError", m.py().get_type::<JournPyError>())?;
    m.add_function(wrap_pyfunction!(format_amount, m)?)?;
    Ok(())
}

/// Create a wrapper error so that we can override its display behaviour.
#[derive(Debug)]
pub(crate) struct PyLedgerError {
    inner: JournError,
}
impl PyLedgerError {
    pub(crate) fn new(mut error: JournError) -> Self {
        error.prune_except_last::<BlockContextError>();
        Self { inner: error }
    }
}

pub(crate) type PyLedgerResult<T> = Result<T, PyLedgerError>;

impl Error for PyLedgerError {}

impl Display for PyLedgerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        //let mut err_style = Style::default();
        //if atty::is(Stream::Stderr) {
        //    err_style = err_style.fg(Color::Red);
        //}

        write!(f, "{}", self.inner)
        //write!(f, "{}", err_style.paint(&self.0.to_string()))
    }
}

impl From<JournError> for PyLedgerError {
    fn from(err: JournError) -> Self {
        PyLedgerError::new(err)
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
    context: Arc<JContext<'static>>,
    //allocator: HerdAllocator<'static>,
    edit_node_id: Mutex<FileId>,
}

#[pymethods]
impl Journal {
    #[new]
    fn new(filename: &str) -> PyLedgerResult<Self> {
        let filename = &**ALLOCATOR.alloc(PathBuf::from(filename));

        if MODULES.lock().unwrap().is_empty() {
            MODULES.lock().unwrap().push(journ_cag::module_init::initialize());
        }

        let parse_result = Python::attach(|py| {
            let python = py;

            // Create the ledger module if it doesn't exist.
            let sys = py.import("sys")?;
            let sys_modules = sys.getattr("modules")?;
            if sys_modules.get_item("ledger").is_err() {
                let mod_ledger = PyModule::new(py, "ledger")?;
                mod_ledger::ledger(py, &mod_ledger)?;
                // Insert the module into sys.modules
                sys_modules.set_item("ledger", mod_ledger)?;
            }
            // Import the ledger module to ensure it is initialized.
            // The main script can exit with an exception during shutdown unless we import the
            // threading module in the main thread. See https://bugs.python.org/issue31517.
            python.import("threading")?;

            // Add the directory of the ledger file to sys.path. This allows Python code
            // within the ledger to import modules from the relative path.
            if let Some(parent) = filename.parent() {
                // We haven't validated yet so we need this check. Also, the parent may be "".
                if let Ok(parent) = parent.canonicalize()
                    && parent.is_dir()
                {
                    let path = sys.getattr("path")?.cast_into::<PyList>()?;
                    path.insert(0, parent.display().to_string())?;
                }
            }

            // The parser works with Python in another thread so we need to release the gil
            // temporarily.
            python.detach(|| {
                TextBlock::from_file(filename, &ALLOCATOR, None).and_then(|block| {
                    journ_core::journal::Journal::parse(Some(filename), block, &ALLOCATOR)
                })
            })
        });

        let context = parse_result.map_err(PyLedgerError::new)?;
        let node_id =
            context.journal().find_node_by_filename(filename).unwrap().id().as_ref().clone();
        let journal =
            Journal { context: Arc::new(context), edit_node_id: Mutex::new(FileId(node_id)) };
        Ok(journal)
    }

    fn edit_file(&self, name: &str) -> PyLedgerResult<()> {
        *self.edit_node_id.lock().unwrap() = self.file_id(name)?;
        Ok(())
    }

    fn file_id(&self, name: &str) -> PyLedgerResult<FileId> {
        self.context.with(|| {
            match self.context.journal().find_node_by_filename(&PathBuf::from(name)) {
                Some(jf) => Ok(FileId(jf.id().as_ref().clone())),
                None => Err(PyLedgerError::new(err!("No filename found: {}", name))),
            }
        })
    }

    fn edit_file_id(&self) -> PyLedgerResult<FileId> {
        Ok(self.edit_node_id.lock().unwrap().clone())
    }

    #[pyo3(signature = (start_time, end_time, callback))]
    fn with_entry_range<'py>(
        &self,
        start_time: &Bound<'py, PyDateTime>,
        end_time: &Bound<'py, PyDateTime>,
        callback: &Bound<'py, PyAny>,
    ) -> PyResult<()> {
        self.context.with(|| {
            let journal = self.context.journal();
            let chrono_start =
                JDateTime::new(start_time.extract::<DateTime<Tz>>()?, DateTimePrecision::Second);
            let chrono_end =
                JDateTime::new(end_time.extract::<DateTime<Tz>>()?, DateTimePrecision::Second);

            for entry in journal.entry_range(chrono_start..chrono_end) {
                let entry = JournalEntry::new(
                    Arc::clone(&self.context),
                    Arc::new(Mutex::new(journ_core::journal_entry::JournalEntry::clone(entry))),
                    FileId(entry.id().parent().unwrap().clone()),
                );
                callback.call1((entry,))?;
            }
            Ok(())
        })
    }

    /// Finds all entries between `start_time` and `end_time` exclusive.
    ///
    /// If `description` is provided, only those entries whose description is
    /// equal to `description` are returned.
    ///
    /// If `file_id` is provided, only those entries found within that file specified
    /// are returned. This can also include entries within nested files (those branched/included).
    ///
    /// The entries are returned in date/location order, with any duplicates later found being ignored.
    #[pyo3(signature = (start_time, end_time=None, description=None, file_id=None))]
    fn find_entries<'py>(
        &self,
        start_time: &Bound<'py, PyDateTime>,
        end_time: Option<&Bound<'py, PyDateTime>>,
        description: Option<&str>,
        file_id: Option<&FileId>,
    ) -> PyResult<Vec<JournalEntry>> {
        self.context.with(|| {
            let journal = self.context.journal();
            let chrono_start =
                JDateTime::new(start_time.extract::<DateTime<Tz>>()?, DateTimePrecision::Second);
            let chrono_end = end_time
                .map(|et| {
                    Ok::<_, PyErr>(JDateTime::new(
                        et.extract::<DateTime<Tz>>()?,
                        DateTimePrecision::Second,
                    ))
                })
                .transpose()?;

            let mut entries = vec![];
            for entry in journal.find_entries(
                JDateTimeRange::new(chrono_start, chrono_end),
                description,
                file_id.map(|fid| &fid.0),
            ) {
                entries.push(JournalEntry::new(
                    Arc::clone(&self.context),
                    Arc::new(Mutex::new(journ_core::journal_entry::JournalEntry::clone(entry))),
                    FileId(entry.id().parent().unwrap().clone()),
                ));
            }
            Ok(entries)
        })
    }

    /*
    fn contains_entry(&self, entry: &JournalEntry) -> bool {
        self.context.with(|| {
            let journal = self.context.journal();
            journal.contains_entry(&*entry.entry_ref())
        })
    }*/

    /// Gets the quantity balance of the specified `account`, in the specified `unit` between `start`..`end`.
    #[pyo3(signature = (account, unit, start=None, end=None))]
    fn account_bal<'py>(
        &self,
        account: &str,
        unit: &str,
        start: Option<&Bound<'py, PyDateTime>>,
        end: Option<&Bound<'py, PyDateTime>>,
    ) -> PyLedgerResult<Option<String>> {
        let mut bals = AccountBalances::new(true, vec![]);
        let time_range = (
            match start {
                Some(t) => {
                    let chrono_start: DateTime<Tz> = t
                        .extract()
                        .map_err(|e| PyLedgerError::new(err!("Invalid start time: {}", e)))?;
                    std::ops::Bound::Included(JDateTime::new(
                        chrono_start,
                        DateTimePrecision::Second,
                    ))
                }
                None => std::ops::Bound::Unbounded,
            },
            match end {
                Some(t) => {
                    let chrono_end: DateTime<Tz> = t
                        .extract()
                        .map_err(|e| PyLedgerError::new(err!("Invalid end time: {}", e)))?;
                    std::ops::Bound::Excluded(JDateTime::new(chrono_end, DateTimePrecision::Second))
                }
                None => std::ops::Bound::Unbounded,
            },
        );
        self.context.with(|| {
            let journal = self.context.journal();
            let account_obj = journal.config().clone().get_or_create_account(account);
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
        })
    }

    #[pyo3(signature = (begin, description, end=None, write_time=true))]
    fn new_entry<'py>(
        &self,
        begin: DateTime<Tz>,
        description: String,
        end: Option<DateTime<Tz>>,
        write_time: bool,
    ) -> PyLedgerResult<JournalEntry> {
        self.context.with(|| {
            let journal = self.context.journal();
            let rust_jf = journal.node(&self.edit_node_id.lock().unwrap().0);
            let config = rust_jf.segments().last().unwrap().config().clone();

            let dt = JDateTimeRange::new(
                JDateTime::new(
                    begin.with_timezone(&config.timezone()),
                    if write_time { DateTimePrecision::Second } else { DateTimePrecision::Day },
                ),
                end.map(|e| {
                    JDateTime::new(
                        e.with_timezone(&config.timezone()),
                        if write_time { DateTimePrecision::Second } else { DateTimePrecision::Day },
                    )
                }),
            );

            let entry = journ_core::journal_entry::JournalEntry::new(
                rust_jf,
                config.clone(),
                dt,
                ALLOCATOR.alloc(description),
                Vec::new_in(&ALLOCATOR),
            );

            Ok(JournalEntry::new(
                Arc::clone(&self.context),
                Arc::new(Mutex::new(entry)),
                self.edit_node_id.lock().unwrap().clone(),
            ))
        })
    }

    fn print(&self) -> PyLedgerResult<()> {
        self.context.with(|| {
            let journal = self.context.journal();
            journal.root().print(&AlwaysIncluded)?;
            Ok(())
        })
    }

    fn print_file(&self, name: &str) -> PyLedgerResult<()> {
        self.context.with(|| {
            let journal = self.context.journal();
            journal
                .find_node_by_filename(&PathBuf::from(name))
                .ok_or(PyLedgerError::new(err!("No filename ends with: {}", name)))?
                .print(&AlwaysIncluded)?;
            Ok(())
        })
    }

    #[pyo3(signature = (padding=None))]
    fn write(&self, padding: Option<usize>) -> PyLedgerResult<()> {
        self.context.with(|| {
            let journal = self.context.journal();
            journal
                .root()
                .write_file_recursive(padding.map(PaddingPolicy::Retain).unwrap_or_default())?;
            Ok(())
        })
    }

    /// Performs a valuation lookup from `base_unit` @ `quantity` to `quote_unit`.
    ///
    /// If no `date` is provided, the now date is used instead.
    #[pyo3(signature = (quantity, base_unit, quote_unit, date=None))]
    fn value_amount<'py>(
        &self,
        quantity: Decimal,
        base_unit: &str,
        quote_unit: &str,
        date: Option<DateTime<Tz>>,
    ) -> PyLedgerResult<Decimal> {
        self.context.with(|| {
            let mut config = JContext::get().journal().config().clone();
            let quote_unit = config.get_or_create_unit(quote_unit);
            let base_unit = config.get_or_create_unit(base_unit);

            let qty = {
                let valuation = SystemValuer::on_date(
                    config.clone(),
                    date.map(|d| JDateTime::new(d, DateTimePrecision::Second))
                        .unwrap_or(JDateTime::now()),
                )
                .value(quote_unit, base_unit.with_quantity(quantity))
                .map_err(JournError::from)?;
                valuation.value().quantity()
            };
            Ok(qty)
        })
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
fn format_amount<'py>(
    quantity: &Bound<'py, PyAny>,
    format_spec: &str,
    unit: &str,
) -> PyResult<String> {
    let uf: UnitFormat = format_spec.parse().map_err(|e| {
        PyValueError::new_err(format!("Invalid format specification: {}: {}", format_spec, e))
    })?;

    let dec = match Decimal::from_str(&quantity.str()?.to_cow()?) {
        Ok(d) => d,
        Err(e) => match Decimal::from_scientific(&quantity.str()?.to_cow()?) {
            Ok(d) => d,
            Err(_) => {
                return Err(PyValueError::new_err(format!(
                    "quantity cannot be parsed: '{}': {}",
                    quantity,
                    e.to_string()
                )));
            }
        },
    };

    let s = uf.format(dec, unit, RoundingStrategy::default()).to_string();
    Ok(s)
}
