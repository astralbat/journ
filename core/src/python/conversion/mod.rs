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
use pyo3::types::{PyDateTime, PyTzInfo};
use pyo3::{FromPyObject, IntoPy, PyAny, PyObject, PyResult, Python};

#[derive(Clone)]
pub struct DateTimeWrapper(pub DateTime<Tz>);

impl IntoPy<PyObject> for DateTimeWrapper {
    fn into_py(self, py: Python) -> PyObject {
        let mut tz_eval = String::with_capacity(100);
        tz_eval.push_str("ZoneInfo(\"");
        tz_eval.push_str(self.0.timezone().name());
        tz_eval.push_str("\")");
        let tz_info = py
            .eval(&tz_eval, None, None)
            .expect("Incompatible timezone")
            .downcast::<PyTzInfo>()
            .expect("Failed to downcast to PyTzInfo");

        let py_dt = PyDateTime::new(
            py,
            self.0.year(),
            self.0.month() as u8,
            self.0.day() as u8,
            self.0.hour() as u8,
            self.0.minute() as u8,
            self.0.second() as u8,
            self.0.timestamp_subsec_micros(),
            Some(tz_info),
        )
        .expect("Failed to construct PyDateTime");
        py_dt.into()
    }
}

impl<'source> FromPyObject<'source> for DateTimeWrapper {
    fn extract(ob: &'source PyAny) -> PyResult<Self> {
        let timestamp: f64 = ob.call_method0("timestamp")?.extract()?;
        Ok(Self(Tz::UTC.timestamp_opt(timestamp as i64, 0).unwrap()))
    }
}
