/*
 * Copyright (c) 2023-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
pub mod error;

use chrono::{DateTime, Datelike, TimeZone, Timelike};
use chrono_tz::Tz;
use pyo3::prelude::PyAnyMethods;
use pyo3::types::{PyDateTime, PyTzInfo};
use pyo3::{Bound, BoundObject, FromPyObject, IntoPyObject, PyAny, PyErr, PyResult, Python};

#[derive(Copy, Clone)]
pub struct DateTimeWrapper(pub DateTime<Tz>);

impl DateTimeWrapper {}

impl<'py> IntoPyObject<'py> for DateTimeWrapper {
    type Target = PyAny;
    type Output = Bound<'py, Self::Target>;
    type Error = PyErr;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        let zone_info = py.import("zoneinfo").expect("Unable to import ZoneInfo module");
        let zone_info_cls = zone_info.getattr("ZoneInfo").expect("Unable to get ZoneInfo class");
        let tz_info = zone_info_cls
            .call1((self.0.timezone().name(),))
            .expect("Failed to create ZoneInfo instance")
            .downcast_into::<PyTzInfo>()
            .expect("Failed to downcast to PyTzInfo");

        /*
        let mut tz_eval = String::with_capacity(100);
        tz_eval.push_str("ZoneInfo(\"");
        tz_eval.push_str(self.0.timezone().name());
        tz_eval.push_str("\")");
        let tz_info = py
            .eval(&tz_eval, None, None)
            .expect("Incompatible timezone")
            .downcast::<PyTzInfo>()
            .expect("Failed to downcast to PyTzInfo");*/

        let py_dt = PyDateTime::new(
            py,
            self.0.year(),
            self.0.month() as u8,
            self.0.day() as u8,
            self.0.hour() as u8,
            self.0.minute() as u8,
            self.0.second() as u8,
            self.0.timestamp_subsec_micros(),
            Some(&tz_info),
        )?;
        Ok(py_dt.into_any())
    }
}

impl<'source> FromPyObject<'source> for DateTimeWrapper {
    fn extract_bound(ob: &Bound<'source, PyAny>) -> PyResult<Self> {
        let timestamp: f64 = ob.call_method0("timestamp")?.extract()?;
        Ok(DateTimeWrapper(Tz::UTC.timestamp_opt(timestamp as i64, 0).unwrap()))
    }
}

pub trait DeferredArg: Send + Sync + 'static {
    fn to_pyobject<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>>;
}

impl DeferredArg for String {
    fn to_pyobject<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        Ok(self
            .into_pyobject(py)
            .map_err(|_| {
                PyErr::new::<pyo3::exceptions::PyTypeError, _>(
                    "Failed to convert String to PyObject",
                )
            })?
            .into_any()
            .into_bound())
    }
}

impl DeferredArg for DateTimeWrapper {
    fn to_pyobject<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        self.clone()
            .into_pyobject(py)
            .map_err(|_| {
                PyErr::new::<pyo3::exceptions::PyTypeError, _>(
                    "Failed to convert DateTimeWrapper to PyObject",
                )
            })
            .map(move |obj| obj.into_any().into_bound())
    }
}

/*
impl<'py2, T> DeferredArg for T
where
    T: IntoPyObject<'py2> + Send + Sync + Clone + 'static,
    <T as IntoPyObject<'py2>>::Output: BoundObject<'py2, T::Target>,
{
    fn to_pyobject<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        self.clone()
            .into_pyobject(py)
            .map_err(|e| {
                PyErr::new::<pyo3::exceptions::PyTypeError, _>(
                    "Failed to convert DeferredArg to PyObject",
                )
            })
            .map(move |obj| obj.into_any().into_bound())
    }
}*/
