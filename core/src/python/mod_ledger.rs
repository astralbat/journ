/*
 * Copyright (c) 2022-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::Amount;
use crate::configuration::Configuration;
use crate::date_and_time::JDateTime;
use crate::err;
use crate::error::JournResult;
use crate::ext::StrExt;
use crate::price::Price;
use crate::price_db::PriceDatabase;
use crate::python::conversion::DateTimeWrapper;
use crate::python::environment::PythonEnvironment;
use crate::unit::Unit;
use chrono::TimeZone;
use chrono_tz::Tz;
use pyo3::basic::CompareOp;
use pyo3::exceptions::PyException;
use pyo3::types::{PyAnyMethods, PyDateTime, PyDict, PyDictMethods, PyModule, PyModuleMethods};
use pyo3::{
    Bound, IntoPyObject, Py, PyErr, PyObject, PyResult, Python, create_exception, wrap_pyfunction,
};
use pyo3::{pyclass, pyfunction, pymethods, pymodule};
use rust_decimal::Decimal;
use std::ffi::CString;
use std::sync::Arc;

/// The rust-facing interface to the module.
pub struct PythonLedgerModule;

impl PythonLedgerModule {
    /// Statically sets the price database for a particular unit code. This call is invoked in preparation
    /// of python invoking price lookups. Because this is statically set, we need to be able to match which
    /// journal incarnation this is for.
    pub fn set_price_database(
        unit_code: &str,
        price_db: &Arc<PriceDatabase>,
        journal_incarnation: u32,
    ) {
        let alias_owned = unit_code.to_string();
        let price_db = Arc::clone(price_db);

        PythonEnvironment::wait_for();
        Python::with_gil(|py| {
            let journal_dict = PythonEnvironment::journal_dict(py, journal_incarnation, None);

            match journal_dict
                .get_item("__price_dbs")
                .ok()
                .flatten()
                .map(|v| v.downcast_into::<PyDict>().unwrap())
            {
                Some(db_map) => {
                    db_map.set_item(&alias_owned, Arc::as_ptr(&price_db) as usize).unwrap()
                }
                None => {
                    let price_db_map = PyDict::new(py);
                    price_db_map.set_item(&alias_owned, Arc::as_ptr(&price_db) as usize).unwrap();
                    journal_dict.set_item("__price_dbs", price_db_map).unwrap();
                }
            }
        })
    }

    pub fn set_default_price_database(price_db: &Arc<PriceDatabase>, journal_incarnation: u32) {
        PythonLedgerModule::set_price_database("__default", price_db, journal_incarnation)
    }

    fn get_price_database<'a, 'py>(
        mod_ledger: &'a Bound<'py, PyModule>,
        globals: Bound<'py, PyDict>,
        unit_code: &str,
    ) -> Option<&'a PriceDatabase<'a>> {
        let journal_incarnation = globals
            .get_item("__active_journal")
            .ok()
            .flatten()
            .expect("Active journal not set")
            .extract::<u32>()
            .unwrap();
        let dict = PythonEnvironment::journal_dict(mod_ledger.py(), journal_incarnation, None);

        let get_db = |unit_code| {
            dict.get_item("__price_dbs")
                .ok()
                .flatten()
                .map(|db| db.downcast_into::<PyDict>().unwrap())
                .and_then(|db| {
                    db.get_item(unit_code).ok().flatten().map(|r| r.extract::<usize>().unwrap())
                })
        };

        match get_db(unit_code) {
            Some(db_ptr) => unsafe {
                let db = db_ptr as *const PriceDatabase;
                Some(db.as_ref().unwrap())
            },
            // Fallback to default price database
            None => match get_db("__default") {
                Some(db_ptr) => unsafe {
                    let db = db_ptr as *const PriceDatabase;
                    Some(db.as_ref().unwrap())
                },
                None => None,
            },
        }
    }
}

create_exception!(ledger, PriceLookupError, PyException);

#[pymodule]
#[pyo3(name = "ledger")]
pub fn ledger<'py>(py: Python<'py>, m: &Bound<'py, PyModule>) -> PyResult<()> {
    m.add_class::<PyPrice>()?;
    m.add_function(wrap_pyfunction!(price_db_lookup, m)?)?;
    m.add("PriceLookupError", py.get_type::<PriceLookupError>())?;
    Ok(())
}

#[pyfunction]
#[pyo3(pass_module)]
fn price_db_lookup<'py>(
    mod_ledger: &Bound<'py, PyModule>,
    bc: &str,
    qc: &str,
    dt: PyObject,
    within_seconds: i64,
) -> PyResult<Option<Py<PyPrice>>> {
    let globals = mod_ledger
        .py()
        .eval(c"globals()", None, None)
        .unwrap()
        .extract::<Bound<'py, PyDict>>()
        .unwrap();
    match PythonLedgerModule::get_price_database(mod_ledger, globals, bc) {
        Some(price_db) => {
            let py = mod_ledger.py();
            let datetime =
                dt.call_method(py, "timestamp", (), None)?.extract::<f64>(py)?.round() as i64;
            let base_curr = Unit::new(bc.intern());
            let quote_curr = Unit::new(qc.intern());
            let found_price = price_db.get_closest(
                Tz::UTC.timestamp_opt(datetime, 0).unwrap(),
                within_seconds,
                &base_curr,
                &quote_curr,
            );
            if found_price.is_none() {
                debug!("Price not found in price database. Net lookup required.")
            }
            Ok(found_price.map(|p| (*p).clone().into_pyobject(py).unwrap().unbind()))
        }
        None => Ok(None),
    }
}

#[pyclass]
#[derive(Clone)]
pub struct PyPrice {
    #[pyo3(get, set, name = "base_curr")]
    base_currency: String,
    #[pyo3(get, set, name = "quote_curr")]
    quote_currency: String,
    // A python datetime
    #[pyo3(get, set)]
    datetime: DateTimeWrapper,
    // A python decimal
    #[pyo3(get, set)]
    price: Decimal,
    #[pyo3(get, set)]
    sources: Vec<String>,
}

#[pymethods]
impl PyPrice {
    #[new]
    fn new(
        base_currency: String,
        quote_currency: String,
        datetime: DateTimeWrapper,
        price: Decimal,
        sources: Vec<String>,
    ) -> PyPrice {
        PyPrice { base_currency, quote_currency, datetime, price, sources }
    }
    fn base_unit(&self) -> &String {
        &self.base_currency
    }
    fn quote_unit(&self) -> &String {
        &self.quote_currency
    }
    fn price(&self) -> Decimal {
        self.price
    }
    fn __str__(&self) -> PyResult<String> {
        Ok(format!(
            "BC='{}', QC='{}', D={}, P={}, S='{:?}'",
            self.base_currency, self.quote_currency, self.datetime.0, self.price, self.sources
        ))
    }
    fn __repr__(&self) -> PyResult<String> {
        self.__str__()
    }
    fn __richcmp__(&self, py: Python, other: &PyPrice, op: CompareOp) -> PyResult<bool> {
        match op {
            CompareOp::Eq => Ok(self.base_currency == other.base_currency
                && self.quote_currency == other.quote_currency
                && self.datetime.0 == other.datetime.0),
            CompareOp::Ne => self.__richcmp__(py, other, CompareOp::Eq).map(|b| !b),
            CompareOp::Lt => {
                let timestamp: i64 = self.datetime.0.timestamp();
                let other_timestamp: i64 = other.datetime.0.timestamp();
                if timestamp == other_timestamp {
                    if self.base_currency == other.base_currency {
                        Ok(self.quote_currency < other.quote_currency)
                    } else {
                        Ok(self.base_currency < other.base_currency)
                    }
                } else {
                    Ok(timestamp < other_timestamp)
                }
            }
            CompareOp::Gt => other.__richcmp__(py, self, CompareOp::Lt),
            CompareOp::Ge => self.__richcmp__(py, other, CompareOp::Lt).map(|b| !b),
            CompareOp::Le => other.__richcmp__(py, self, CompareOp::Lt).map(|b| !b),
        }
    }
}

impl PyPrice {
    pub fn sources(&self) -> &Vec<String> {
        &self.sources
    }
    pub fn into_sources(self) -> Vec<String> {
        self.sources
    }
}

impl<'py> IntoPyObject<'py> for Price<'_> {
    type Target = PyPrice;
    type Output = Bound<'py, Self::Target>;
    type Error = PyErr;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        Ok(Bound::new(
            py,
            PyPrice::new(
                self.base_unit().to_string(),
                self.quote_unit().to_string(),
                DateTimeWrapper(self.datetime().datetime()),
                self.price().quantity(),
                self.sources().map(str::to_string).collect(),
            ),
        )
        .unwrap())
    }
}

impl<'py> IntoPyObject<'py> for JDateTime<'_> {
    type Target = PyDateTime;
    type Output = Bound<'py, Self::Target>;
    type Error = PyErr;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        Ok(py
            .eval(
                &CString::new(format!(
                    "datetime.strptime(\"{}\", \"%Y-%m-%dT%H:%M:%S%z\")",
                    self.datetime().format("%FT%T%z")
                ))
                .unwrap(),
                None,
                None,
            )?
            .into_pyobject(py)?
            .downcast_into()
            .unwrap())
    }
}

/*
impl<'h, 'source> FromPyObject<'source> for Price<'h> {
    fn extract(ob: &'source PyAny) -> PyResult<Price<'h>> {
        match ob.extract::<PyPrice>() {
            Ok(py_price) => {
                let mut units = JOURNAL_UNITS.as_ref().expect("mod_ledger units uninitialised").lock().unwrap();
                let base_unit = Unit::new(py_price.base_unit());
                let quote_unit = Unit::new(py_price.quote_unit());
                let timestamp: i64 = py_price.datetime.call_method(ob.py(), "timestamp", (), None)?.extract(ob.py())?;
                let chrono_date = Tz::UTC.timestamp(timestamp, 0);
                let datetime = match base_unit.prices().and_then(|db| db.file()).map(|file| file.config()) {
                    Some(config) => {
                        JDateTime::from_datetime(chrono_date, Some(config.date_format()), Some(config.time_format()))
                    }
                    None => JDateTime::from_datetime(chrono_date, None, None),
                };

                let price_dict = PyDict::new(ob.py());
                price_dict.set_item("p", py_price.price())?;
                let price_str = ob.py().eval("str(p)", None, Some(price_dict))?.extract::<String>()?;
                let price_dec = if price_str.contains('e') || price_str.contains('E') {
                    Decimal::from_scientific(&price_str)
                } else {
                    Decimal::from_str(&price_str)
                }
                .unwrap();
                let allocator = ThreadAllocator::new(HERD.expect("mod_ledger allocator not set"));
                let mut sources = SmallVec::with_capacity(1);
                sources.extend(py_price.sources().into_iter().map(|s| allocator.alloc(s).as_str()));
                let price = Price::new(datetime, base_unit, Amount::new(quote_unit, price_dec), sources);
                Ok(price)
            }
            Err(_e) => Err(PyDowncastError::new(ob, "PyPrice").into()),
        }
    }
}*/

impl PyPrice {
    pub fn as_price<'h>(
        self,
        py: Python,
        config: &mut Configuration<'h>,
    ) -> JournResult<Price<'h>> {
        let allocator = config.allocator();
        let base_unit = config
            .get_unit(self.base_unit())
            .unwrap_or_else(|| config.merge_unit(&Unit::new(self.base_unit().clone()), allocator));
        let quote_unit = config
            .get_unit(self.quote_unit())
            .unwrap_or_else(|| config.merge_unit(&Unit::new(self.quote_unit().clone()), allocator));
        let chrono_date: DateTimeWrapper = self.datetime;
        let date = JDateTime::from_datetime(
            chrono_date.0,
            Some(config.as_herd_ref().date_format()),
            Some(config.as_herd_ref().time_format()),
        );

        /*
        let price_dict = PyDict::new(py);
        price_dict.set_item("p", &self.price).unwrap();
        let price_str: String = py
            .eval("str(p)", None, Some(price_dict))
            .map_err(|_| err!("Unable to call str(<price>)"))?
            .extract()
            .unwrap();
        let price_dec = price_str.to_decimal(&config.number_format())?;*/
        let price_dec = self.price;

        let sources = match self.into_sources() {
            sources if !sources.is_empty() => {
                let sources_string = sources
                    .into_iter()
                    .map(|mut s| {
                        s.insert(0, '"');
                        s.push('"');
                        s
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                Some(allocator.alloc(sources_string).as_str())
            }
            _ => None,
        };
        Ok(Price::new(date, base_unit, Amount::new(quote_unit, price_dec), sources))
    }
}
/*
impl<'source> FromPyObject<'source> for Price<'_, '_> {
    fn extract(obj: &'source PyAny) -> PyResult<Self> {
        let py = obj.py();
        Ok(obj.extract::<PyPrice>()?.as_price(py).unwrap())
    }
}*/
/*
impl IntoPy for Price<'_, '_> {
    fn to_object(&self, py: Python) -> PyObject {
        let date = py
            .eval(
                &format!("datetime.strptime(\"{}\", \"%Y-%m-%dT%H:%M:%S%z\")", self.date().format("%FT%T%z")),
                None,
                None,
            )
            .unwrap();
        let price = py.eval(&format!("Decimal(\"{}\")", self.price().amount()), None, None).unwrap();
        PyPrice::create_instance(
            py,
            self.base_currency().code().to_string(),
            self.quote_currency().code().to_string(),
            date,
            price,
            self.sources().clone(),
        )
        .unwrap()
    }
}*/
