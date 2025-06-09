/*
 * Copyright (c) 2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::err;
use crate::error::JournError;
use pyo3::types::PyTracebackMethods;
use pyo3::{PyErr, Python};

/// Converts a `PyErr` into a `JournError` containing traceback information.
/// The call must include the `Python<'_>` instance as the first argument.
#[macro_export]
macro_rules! pyerr {
    ($py: ident, $py_err: ident) => {
        JournError::from(($py, $py_err))
    };
}

impl From<(Python<'_>, PyErr)> for JournError {
    fn from((py, py_err): (Python<'_>, PyErr)) -> Self {
        let mut maybe_cause = Some(py_err);
        let mut err_msg: Option<JournError> = None;
        while let Some(cause) = maybe_cause {
            match cause.traceback(py) {
                Some(tb) => {
                    err_msg = Some(match err_msg {
                        Some(err_msg) => err_msg.with_source(err!(
                            "{}\n  {}",
                            cause,
                            tb.format().unwrap_or("(Unable to get traceback)".to_string())
                        )),
                        None => err!(
                            "{}\n  {}",
                            cause,
                            tb.format().unwrap_or("(Unable to get traceback)".to_string())
                        ),
                    });
                }
                None => {
                    err_msg = Some(match err_msg {
                        Some(err_msg) => err_msg.with_source(cause.clone_ref(py)),
                        None => err!("{}", cause.clone_ref(py)),
                    })
                }
            }
            maybe_cause = cause.cause(py);
        }
        err_msg.unwrap()
    }
}
