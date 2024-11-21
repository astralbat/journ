/*
 * Copyright (c) 2020-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use ansi_term::{Color, Style};
use atty::Stream;
use bumpalo_herd::Herd;
use env_logger::Builder;
use journ_core::alloc::HerdAllocator;
use journ_core::amount::{Amount, AmountExpr};
use journ_core::date_and_time::{DateAndTime, JDateTime, JDateTimeRange};
use journ_core::error::JournError;
use journ_core::journal_entry::EntryObject;
use journ_core::journal_node::NodeId;
use journ_core::metadata::Metadata;
use journ_core::module::MODULES;
use journ_core::parsing;
use journ_core::parsing::text_block::TextBlock;
use journ_core::posting::PostingId;
use journ_core::python::conversion::DateTimeWrapper;
use journ_core::python::mod_ledger;
use journ_core::unit::{Unit, UnitFormat};
use journ_core::valued_amount::{Valuation, ValuedAmount};
use journ_core::{err, parse};
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::types::PyDateTime;
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
    m.add("JournError", py.get_type::<JournPyError>())?;
    m.add_function(wrap_pyfunction!(format_amount, m)?)?;
    Ok(())
}

/// Create a wrapper error so that we can override its display behaviour.
#[derive(Debug)]
struct PyLedgerError(JournError);

type PyLedgerResult<T> = Result<T, PyLedgerError>;

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
static ALLOCATOR: LazyLock<HerdAllocator<'static>> = LazyLock::new(|| HerdAllocator::new(&HERD));

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

    fn find_entries(
        &self,
        py_datetime: &PyDateTime,
        description: &str,
    ) -> PyResult<Vec<JournalEntry>> {
        let journal = self.journal.lock().unwrap();
        let chrono_date: DateTimeWrapper = py_datetime
            .extract()
            .expect("datetime must be constructed with a valid Python datetime object");

        let mut entries = vec![];
        for entry in journal.find_entries(chrono_date.0, description) {
            entries.push(JournalEntry {
                journal: Arc::clone(&self.journal),
                entry: Arc::new(Mutex::new(journ_core::journal_entry::JournalEntry::clone(entry))),
                node_id: *entry.id().node_id(),
            });
        }
        Ok(entries)
    }

    // TODO: Combine the py_datetime and write_time into a single argument (new type needed).
    fn new_entry(
        &self,
        py_datetime: &PyDateTime,
        write_time: bool,
        mut description: String,
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
            None,
        );

        description.insert_str(0, "  ");
        let entry = journ_core::journal_entry::JournalEntry::new(
            ALLOCATOR.alloc(*self.edit_node_id.lock().unwrap()),
            rust_jf.config().clone(),
            dt,
            ALLOCATOR.alloc(description),
            Vec::new_in(&ALLOCATOR),
        );

        Ok(JournalEntry {
            journal: Arc::clone(&self.journal),
            entry: Arc::new(Mutex::new(entry)),
            node_id: *self.edit_node_id.lock().unwrap(),
        })
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

#[pyclass(unsendable)]
struct JournalEntry {
    journal: Arc<Mutex<journ_core::journal::Journal<'static>>>,
    entry: Arc<Mutex<journ_core::journal_entry::JournalEntry<'static>>>,
    node_id: NodeId<'static>,
}

#[pymethods]
impl JournalEntry {
    fn file_id(&self) -> PyLedgerResult<u32> {
        Ok(self.node_id.id())
    }

    fn append_posting(&self, account: &str, amount: Option<&str>) -> PyLedgerResult<Posting> {
        let journal = self.journal.lock().unwrap();
        let node = journal.node(&self.node_id);
        let account = node.config().get_or_create_account(account);
        let money = match amount {
            Some(amount) => {
                let alloc_amount = ALLOCATOR.alloc(amount.to_string());
                Some(
                    parse!(alloc_amount, parsing::amount::amount, node.config())
                        .0
                        .map(|r| r.1)
                        .map_err(|e| err!(e; "append_posting()"))?,
                )
            }
            None => None,
        };
        let mut entry = self.entry.lock().unwrap();
        let pst = journ_core::posting::Posting::new(
            "  ",
            account,
            money
                .map(|a| ValuedAmount::new_in(AmountExpr::new(a, "  ", None), &ALLOCATOR))
                .unwrap_or(ValuedAmount::nil()),
            None,
            None,
        );
        let posting_id = entry.append_posting(pst).id();
        Ok(Posting {
            journal: Arc::clone(&self.journal),
            entry: Arc::clone(&self.entry),
            posting_id,
        })
    }

    fn append_metadata(&self, key: &str, value: &str) -> PyLedgerResult<()> {
        let mut entry = self.entry.lock().unwrap();
        let block_str = ALLOCATOR.alloc(if entry.objects().is_empty() {
            format!("  +{}  {}", key, value)
        } else {
            format!("\n  +{}  {}", key, value)
        });
        let block = ALLOCATOR.alloc(TextBlock::from(block_str.as_str()));
        entry.append_object(EntryObject::Metadata(Metadata::from(&*block)));
        Ok(())
    }

    fn append_comment(&self, comment: &str) -> PyLedgerResult<()> {
        let mut entry = self.entry.lock().unwrap();
        if !comment.trim_start().starts_with(";") {
            return Err(err!("Comments need to start with ';'").into());
        }
        entry.append_object(EntryObject::Comments(ALLOCATOR.alloc(comment.to_string())));
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

#[pyclass(unsendable)]
struct Posting {
    journal: Arc<Mutex<journ_core::journal::Journal<'static>>>,
    entry: Arc<Mutex<journ_core::journal_entry::JournalEntry<'static>>>,
    posting_id: PostingId<'static>,
}

#[pymethods]
impl Posting {
    fn set_unit_value(&self, unit_value: &str) -> PyLedgerResult<()> {
        let mut entry = self.entry.lock().unwrap();
        let pst = entry.find_posting_mut(self.posting_id).unwrap();
        let journal = self.journal.lock().unwrap();
        let node = journal.node(self.posting_id.entry_id().node_id());
        let node_config = node.config();
        let unit_value_alloc = ALLOCATOR.alloc(unit_value.to_string());

        let parse_res = parse!(unit_value_alloc, parsing::amount::amount, node_config).0?.1;

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
        let pst = entry.find_posting_mut(self.posting_id).unwrap();
        let journal = self.journal.lock().unwrap();
        let node = journal.node(self.posting_id.entry_id().node_id());
        let node_config = node.config();
        let total_value_alloc = ALLOCATOR.alloc(total_value.to_string());

        let parse_res = parse!(total_value_alloc, parsing::amount::amount, node_config).0?.1;
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
