/*
 * Copyright (c) 2022-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::error::{BlockContext, BlockContextError, JournError, JournResult};
use crate::parsing::text_block::TextBlock;
use crate::{err, pyerr};
use nom::bytes::complete::take_while1;
use nom::character::is_digit;
use pyo3::types::PyDict;
use pyo3::{intern, PyAny, PyErr, PyObject, PyResult, Python, ToPyObject};
use std::collections::HashMap;
use std::hash::Hash;
use std::path::Path;
use std::sync::{Arc, Condvar, LazyLock, Mutex};
use std::thread;

#[derive(Debug)]
pub struct PythonEnvironment;

pub trait PyObjectArgKey: ToPyObject + Hash {}

/// Like Py03's FromPyObject except that it is not parametised by a lifetime and thus can only extract
/// fully owned objects. E.g. String, but not &str. This permits our eval() to return a generic return type, which
/// is not possible using FromPyObject since it may return a type whose lifetime is bound to the lifetime of the GIL which is dropped
/// at the end of eval().
pub trait FromPyObjectOwned: Sized {
    fn extract(ob: &PyAny) -> PyResult<Self>;
}
macro_rules! from_py_object_owned {
    ($type:ty) => {
        impl FromPyObjectOwned for $type {
            fn extract(ob: &PyAny) -> PyResult<Self> {
                ob.extract()
            }
        }
    };
}
from_py_object_owned!(PyObject);
from_py_object_owned!(Option<String>);
from_py_object_owned!(String);
from_py_object_owned!(bool);
impl<T> FromPyObjectOwned for Vec<T>
where
    T: FromPyObjectOwned,
{
    fn extract(ob: &PyAny) -> PyResult<Self> {
        let l = ob.extract::<Vec<&PyAny>>()?;
        let mut ret = Vec::with_capacity(l.len());
        for o in l {
            ret.push(FromPyObjectOwned::extract(o)?)
        }
        Ok(ret)
    }
}

static PYTHON_STARTING: Mutex<bool> = Mutex::new(false);
static PYTHON_STARTED: LazyLock<Arc<(Mutex<bool>, Condvar)>> =
    LazyLock::new(|| Arc::new((Mutex::new(false), Condvar::new())));

impl PythonEnvironment {
    /// Wait for the PythonEnvironment to come up, if indeed it is not already up.
    pub fn wait_for() {
        let starting = PYTHON_STARTING.lock().unwrap();
        if *starting {
            drop(starting);
            // Pause the current thread, waiting for it to start up.
            let (lock, cvar) = &**PYTHON_STARTED;
            let mut started = lock.lock().unwrap();
            while !*started {
                started = cvar.wait(started).unwrap()
            }
        }
    }

    pub fn startup() {
        let mut starting_lock = PYTHON_STARTING.lock().unwrap();
        if !*starting_lock {
            *starting_lock = true;
            drop(starting_lock);
            thread::spawn(|| {
                #[cfg(feature = "python-embedded")]
                Self::startup_embedded();
                #[cfg(not(feature = "python-embedded"))]
                Self::startup_external();

                Python::with_gil(|py| {
                    let startup_code = "
                    # Import datetime in to the system namespace. This is called from `valuer`.\n\
                    from datetime import datetime\n\

                    # Import Decimal. This is called in mod_ledger.\n\
                    from decimal import Decimal\n\

                    # Add the current directory to the path so that we can import modules from the current directory (journ.so).\n\
                    # The command line interpreter normally does this anyway.\n\
                    import sys\n\
                    sys.path.append('.')";

                    py.run(startup_code, None, None).unwrap();
                });

                let (lock, cvar) = &**PYTHON_STARTED;
                *lock.lock().unwrap() = true;
                cvar.notify_all();
                *PYTHON_STARTING.lock().unwrap() = false;
                debug!("Python: initialised");
            });
        }
    }

    /// When building with this feature enabled, it is necessary to set the PYO3_CONFIG_FILE environment
    /// variable to the absolute location of the pyo3-build-config-file.txt file located in she
    /// resources/embedded_python directory.
    #[cfg(feature = "python-embedded")]
    fn startup_embedded() {
        use pyembed::{MainPythonInterpreter, OxidizedPythonInterpreterConfig};

        debug!("Embedded Python: initialising");

        // This way the ledger module will get initialised when the module is next imported.
        // This has to be done before the interpreter is created.
        pyo3::append_to_inittab!(ledger);

        // Create the interpreter.
        let config = OxidizedPythonInterpreterConfig {
            // Use no arguments, otherwise they get copied from the current process which
            // causes python to immediately barf.
            argv: Some(vec![]),
            // Need this to be true to get the embedded resources to work.
            oxidized_importer: true,
            // This includes what should have been built for the local build machine using
            // the pyoxidizer command.
            packed_resources: vec![pyembed::PackedResourcesSource::Memory(include_bytes!(
                r#"../../resources/embedded_python/packed-resources"#
            ))],
            ..OxidizedPythonInterpreterConfig::default()
        };
        let interpreter = MainPythonInterpreter::new(config)
            .expect("Unable to initialise the embedded python interpreter");

        // The interpreter is not `Send` nor `Sync`. We can't keep it in a static variable,
        // so we'll just forget it to ensure that we can still use it.
        Box::leak(Box::new(interpreter));
    }

    #[cfg(not(feature = "python-embedded"))]
    fn startup_external() {
        use crate::python::mod_ledger::ledger;
        use pyo3::prepare_freethreaded_python;

        debug!("External Python: initialising");

        // This way the ledger module will get initialised when the module is next imported.
        // This has to be done before the interpreter is initialised.
        pyo3::append_to_inittab!(ledger);

        prepare_freethreaded_python();
    }

    pub fn run_code(code: &str, ji: u32) -> JournResult<()> {
        Self::wait_for();
        Python::with_gil(|py| {
            py.run(code, Some(PythonEnvironment::journal_dict(py, ji, None)), None)
                .map_err(|e| context_err(e, py))
        })
    }

    pub fn eval<'a, T>(
        expr: &str,
        args: Option<HashMap<String, Box<dyn ToPyObject + 'a>>>,
        ji: Option<u32>,
    ) -> JournResult<T>
    where
        T: FromPyObjectOwned,
    {
        Python::with_gil(|py: Python| {
            let locals = match args {
                Some(args) => {
                    let locals = PyDict::new(py);
                    for (k, v) in args {
                        locals.set_item(k.to_object(py), v.to_object(py)).unwrap();
                    }
                    Some(locals)
                }
                None => None,
            };
            if let Some(ji) = ji {
                PythonEnvironment::set_active_journal(py, ji);
            }
            py.eval(expr, ji.map(|ji| PythonEnvironment::journal_dict(py, ji, None)), locals)
                .map_err(|e| {
                    err!("Error evaluating python expression: '{}'", expr)
                        .with_source(pyerr!(py, e))
                })
                .and_then(|res| {
                    T::extract(res).map_err(|e| err!(e; "Unable to convert python object"))
                })
        })
    }

    pub fn run<'a>(
        code: &str,
        ji: Option<u32>,
        args: Option<HashMap<String, Box<dyn ToPyObject + 'a>>>,
        file: Option<&str>,
    ) -> JournResult<()> {
        Self::run_then(code, ji, args, file, |_py, _locals| Ok(()))
    }

    pub fn run_then<'a, F, R>(
        code: &str,
        ji: Option<u32>,
        args: Option<HashMap<String, Box<dyn ToPyObject + 'a>>>,
        file: Option<&str>,
        func: F,
    ) -> JournResult<R>
    where
        F: Fn(Python, Option<&PyDict>) -> JournResult<R>,
    {
        Python::with_gil(|py: Python| {
            let locals = args.map(|args| {
                let locals = PyDict::new(py);
                for (k, v) in args {
                    locals.set_item(k.to_object(py), v.to_object(py)).unwrap();
                }
                locals
            });
            if let Some(ji) = ji {
                PythonEnvironment::set_active_journal(py, ji);
            }

            // We allow different locals to be passed in, but caution with this: using `None` for
            // the locals will make sure it is the same as the globals which is fine. But when it is `Some` can
            // lead to issues if not calling a function. If executing script logic for example, the imports at the top
            // will go in to the locals map, but the function will access them from the globals.
            py.run(code, ji.map(|ji| PythonEnvironment::journal_dict(py, ji, file)), locals)
                .map_err(|e| {
                    let mut bc = BlockContext::from(&TextBlock::from(code));

                    // Try to highlight the line number in the error message.
                    let mut lowest_cause = e.clone_ref(py);
                    while let Some(cause) = lowest_cause.cause(py) {
                        lowest_cause = cause;
                    }
                    let py_err_str = match lowest_cause.traceback(py) {
                        Some(tb) => tb.format().unwrap(),
                        None => lowest_cause.to_string(),
                    };
                    // Check the filenames match and only highlight the line if they do.
                    if let Some(begin) = py_err_str.rfind("File \"") {
                        let py_err_filename = py_err_str[begin..]
                            .find("\",")
                            .map(|end| &py_err_str[begin + "File \"".len()..begin + end]);
                        if let (Some(filename), Some(py_err_filename)) = (file, py_err_filename) {
                            let canonical_bc_filename = Path::new(filename).canonicalize();
                            let canonical_py_err_filename =
                                Path::new(py_err_filename).canonicalize();

                            if let (Ok(can_bc_filename), Ok(can_py_err_filename)) =
                                (canonical_bc_filename, canonical_py_err_filename)
                            {
                                if can_bc_filename == can_py_err_filename {
                                    py_err_str.rfind("line ").map(|pos| {
                                        if let Ok((_, line_num)) =
                                            take_while1::<_, &str, JournError>(|c: char| {
                                                c.is_ascii_digit()
                                            })(
                                                &py_err_str[pos + "line ".len()..]
                                            )
                                        {
                                            let line_num = line_num.parse::<usize>().unwrap();
                                            bc.clear_highlights();
                                            bc.highlight_line(line_num);
                                            bc.shrink(5);
                                            bc.set_line(Some(line_num));
                                        }
                                    });
                                }
                            }
                        }
                    }

                    let bce = BlockContextError::new(bc, "Error executing python code".to_string());
                    JournError::new(bce).with_source(pyerr!(py, e))
                })?;
            func(py, locals)
        })
    }

    pub(super) fn set_active_journal(py: Python, journal_incarnation: u32) {
        py.run(&format!("__active_journal={}", journal_incarnation), None, None).unwrap();
    }

    pub(super) fn journal_dict<'a, 'py>(
        py: Python<'py>,
        journal_incarnation: u32,
        file: Option<&'a str>,
    ) -> &'py PyDict {
        let mod_main = py.import("__main__").unwrap();
        let main_dict = mod_main.dict();
        let journal_dict = match main_dict
            .get_item(intern!(py, "__journals"))
            .ok()
            .flatten()
            .map(|j| j.downcast::<PyDict>().unwrap())
        {
            Some(journals) => match journals
                .get_item(journal_incarnation)
                .ok()
                .flatten()
                .map(|j| j.downcast::<PyDict>().unwrap())
            {
                Some(journal) => journal,
                None => {
                    journals.set_item(journal_incarnation, PyDict::new(py)).unwrap();
                    journals
                        .get_item(journal_incarnation)
                        .ok()
                        .flatten()
                        .map(|j| j.downcast::<PyDict>().unwrap())
                        .unwrap()
                }
            },
            None => {
                main_dict.set_item(intern!(py, "__journals"), PyDict::new(py)).unwrap();
                let journals = main_dict
                    .get_item(intern!(py, "__journals"))
                    .unwrap()
                    .unwrap()
                    .downcast::<PyDict>()
                    .unwrap();
                journals.set_item(journal_incarnation, PyDict::new(py)).unwrap();

                // This dictionary will be used as the globals for Python execution. We need to make
                // sure that builtins is defined otherwise some things don't work.
                let journal_dict = journals
                    .get_item(journal_incarnation)
                    .ok()
                    .flatten()
                    .map(|j| j.downcast::<PyDict>().unwrap())
                    .unwrap();
                let builtins = py.import(intern!(py, "builtins")).unwrap();
                journal_dict.set_item(intern!(py, "__builtins__"), builtins).unwrap();

                let sys = py.import(intern!(py, "sys")).unwrap();
                journal_dict.set_item(intern!(py, "sys"), sys).unwrap();
                journal_dict
            }
        };

        // The file is a special variable that is used to store the file name of the executing code.
        match file {
            Some(file) => journal_dict.set_item(intern!(py, "__file__"), file).unwrap(),
            None => {
                if let Ok(Some(_)) = journal_dict.get_item(intern!(py, "__file__")) {
                    journal_dict.del_item(intern!(py, "__file__")).unwrap()
                }
            }
        }
        journal_dict
    }
}

fn context_err(e: PyErr, py: Python) -> JournError {
    err!("Error in Python context environment: {}", e.value(py))
}

#[cfg(test)]
mod tests {
    use crate::python::environment::PythonEnvironment;

    #[test]
    pub fn test_add_code() {
        PythonEnvironment::startup();
        let code = "def abc():\n return \"Hello\"".to_string();
        PythonEnvironment::run_code(&code, 1).unwrap();
        let s = PythonEnvironment::eval::<String>("abc()", None, None).unwrap();
        assert_eq!(s, "Hello");
    }
}
