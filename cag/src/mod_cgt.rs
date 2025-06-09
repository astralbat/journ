/*
 * Copyright (c) 2022-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::deal::Deal;
use journ_core::err;
use journ_core::error::JournResult;
use journ_core::python::conversion::DateTimeWrapper;
use pyo3::prelude::PyModule;
use pyo3::types::{PyAnyMethods, PyDict, PyDictMethods, PyModuleMethods};
use pyo3::{Bound, PyResult, Python, pyclass, pymethods, pymodule, wrap_pymodule};
use pyo3::{IntoPyObject, PyObject};
use std::convert::Infallible;

/*
pub struct PythonCgtModule;
impl PythonCgtModule {
    pub fn set_matcher(matcher: &Matcher) -> JournResult<()> {
        Python::with_gil(|py| {
            let cgt_mod: &PyModule = py
                .eval("sys.modules[\"cgt\"]", None, None)
                .expect("'cgt' module not loaded")
                .downcast::<PyModule>()
                .unwrap();
            cgt_mod.dict().set_item("__matcher", matcher as *const Matcher as usize).unwrap();
            Ok(())
        })
    }
}
*/
#[pymodule]
fn cgt<'py>(_py: Python, _m: Bound<'py, PyModule>) -> PyResult<()> {
    //m.add_function(wrap_pyfunction!(pair_unpaired_buys_with_sells, m).unwrap()).unwrap();
    Ok(())
}

pub fn register() -> JournResult<()> {
    //PythonEnvironment::wait_for();
    Python::with_gil(|py| {
        match py.import("sys") {
            Ok(mod_sys) => match mod_sys.dict().get_item("modules").unwrap() {
                Some(dict) => match dict.downcast_into::<PyDict>() {
                    Ok(dict) => match dict.set_item("cgt".to_string(), wrap_pymodule!(cgt)(py)) {
                        Ok(()) => Ok(()),
                        Err(_) => Err(err!("Failed to set module as an item on sys.modules")),
                    },
                    Err(_) => Err(err!("Failed to downcast sys.modules as PyDict")),
                },
                None => Err(err!("Failed to get python sys.modules dictionary")),
            },
            Err(_) => Err(err!("Unable to import python sys module")),
        }
        .and_then(|_| {
            py.run(c"import cgt", None, None).map_err(|e| err!("Unable to import module: {}", e))
        })
    })
}

/// Represents the 'Deal' class in the python environment.
#[derive(Clone)]
#[pyclass(unsendable)]
pub struct PyDeal {
    #[pyo3(get)]
    date_from: DateTimeWrapper,
}

#[pymethods]
impl PyDeal {}

impl<'h> From<&Deal<'h>> for PyDeal {
    fn from(deal: &Deal<'h>) -> Self {
        PyDeal { date_from: DateTimeWrapper(deal.datetime().start().datetime()) }
    }
}

/*
#[pyclass]
#[derive(Clone)]
struct PyAdjustment {
    date_from: JDateTime,
}

impl ToPyObject for DealingEvent {
    fn to_object(&self, py: Python) -> PyObject {
        match self {
            DealingEvent::Deal(deal) => deal.to_object(py),
            DealingEvent::PoolAdjustment(pa) => pa.to_object(py),
        }
    }
}*/
/*
impl FromPyObjectOwned for DealingEvent {
    fn extract(ob: &PyAny) -> PyResult<Self> {
        match ob.get_type().name().unwrap() {
            "PyDeal" => <Deal as FromPyObjectOwned>::extract(ob).map(|d| DealingEvent::Deal(d)),
            "PyAdjustment" => {
                <PoolAdjustment as FromPyObjectOwned>::extract(ob).map(|pa| DealingEvent::PoolAdjustment(pa))
            }
            other => panic!("Unknown conversion type: {}", other),
        }
    }
}*/

impl<'py, 'h> IntoPyObject<'py> for Deal<'h> {
    type Target = PyDeal;
    type Output = Bound<'py, PyDeal>;
    type Error = Infallible;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        Ok(Bound::new(py, PyDeal::from(&self)).expect("Unexpected error converting Deal to PyDeal"))
    }
}

/*
impl ToPyObject for Deal {
    fn to_object(&self, py: Python) -> PyObject {
        let deal_lock = self.entry().lock().unwrap();
        let datetime_range = deal_lock.date_and_time().datetime_range();

        let properties = self
            .properties()
            .clone()
            .into_iter()
            .map(|p| (p.key().to_string(), p.value().map(str::to_string)))
            .collect();

        PyDeal { date_from: datetime_range.0.clone(), deal_ptr: self as *const Deal as *mut Deal }.into_py(py)
    }
}*/
/*
impl FromPyObjectOwned for Deal {
    fn extract(ob: &PyAny) -> PyResult<Self> {
        let deal_ptr: usize = ob.get_item("deal_ptr")?.extract()?;
        let deal: Deal = deal_ptr as *mut Deal;
        unsafe { Ok(deal_ptr as *mut Deal as &mut Deal) }
    }
}*/
/*
impl ToPyObject for PoolAdjustment {
    fn to_object(&self, py: Python) -> PyObject {
        PyAdjustment { date_from: self.datetime().0.clone() }.into_py(py)
    }
}*/
/*
/// Creates a pairing between buys and sells which are selected by the `compare` function. Only pairs that have not been previously
/// paired by this call are selected.
///
/// The `compare` argument is a function invoked with a candidate buy and sell to pair (ordered by date, not type). The function should return
/// an integer:
/// * zero: to indicate that this buy and sell are indeed a pair.
/// * less than zero: to indicate that these are not a pair, but should continue searching against the pair's first element.
/// * greater than zero: to indicate that these are not a pair and should no longer consider the pair's first element.
/// # Unsafe
/// This function dereferences the __matcher address and modifies the dealing events contained within.
#[pyfunction]
#[pyo3(pass_module)]
unsafe fn pair_unpaired_buys_with_sells(mod_cgt: &PyModule, compare: PyObject) -> PyResult<Vec<(PyDeal, PyDeal)>> {
    // Set by matcher
    let matcher = &mut *(mod_cgt.getattr("__matcher").unwrap().extract::<usize>()? as *mut Matcher);
    let events = matcher.events_mut();
    let mut unmatched = vec![];
    let mut pair_indices = vec![];
    let mut amount_balances = HashMap::new();

    // So we can pop elements in chronological order.
    events.reverse();
    'outer: while let Some(event_i) = events.pop() {
        update_amount_balance(&mut amount_balances, &event_i);

        if let DealingEvent::Deal(mut deal_i) = event_i {
            let mut deal_i_adj = Box::new(AdjustedTransaction::new(
                Box::new(deal_i.clone()),
                PoolAdjustment::identity(Arc::clone(deal_i.entry()), deal_i.unit()),
            ));
            for j in (0..events.len()).rev() {
                match &mut events[j] {
                    DealingEvent::Deal(deal_j) => {
                        match compare
                            .call1(mod_cgt.py(), (PyDeal::from(&mut deal_i), PyDeal::from(deal_j)))?
                            .extract::<i64>(mod_cgt.py())?
                        {
                            // Compare result 0 means these deals should be paired.
                            n if n == 0 => {
                                if let DealingEvent::Deal(deal_j) = events.remove(j) {
                                    // Decide which deal: i or j needs to be split
                                    if deal_i_adj.amount().abs() > deal_j.amount().abs() {
                                        let (deal_i_match, deal_i_change) =
                                            deal_i_adj.split(deal_j.amount().abs(), MatchMethod::Fifo);
                                        let deal_i_match = deal_i_match.inner_deals().remove(0);
                                        let deal_i_change = deal_i_change.inner_deals().remove(0);
                                        events.push(DealingEvent::Deal(deal_i_change));
                                        // Append the matched part of the split into new events.
                                        unmatched.push(DealingEvent::Deal(deal_i_match));
                                        unmatched.push(DealingEvent::Deal(deal_j));
                                    } else {
                                        let (deal_j_match, deal_j_change) = deal_j.split(deal_i_adj.amount().abs());
                                        unmatched.push(DealingEvent::Deal(deal_i));
                                        unmatched.push(DealingEvent::Deal(deal_j_match));
                                        if !deal_j_change.is_empty() {
                                            events.insert(j, DealingEvent::Deal(deal_j_change));
                                        }
                                    }
                                    pair_indices.push((unmatched.len() - 2, unmatched.len() - 1));
                                }
                                continue 'outer;
                            }
                            // The compare function result > 0 means that it will never yield 0 for events >= j, so move
                            // on to the next event_i.
                            n if n > 0 => continue 'outer,
                            _ => {}
                        }
                    }
                    DealingEvent::PoolAdjustment(_) => {
                        // When making adjustments, we are adjusting our assets. It therefore does not
                        // make sense to adjust something we are selling.
                        if deal_i.is_acquisition() {
                            if let DealingEvent::PoolAdjustment(pa) = events.remove(j) {
                                let adjusted_amount = match pa.amount_adjustment() {
                                    ArithmeticFunction::Add(adj_amount) => {
                                        let bal = amount_balances.balance(adj_amount.unit());
                                        *adj_amount / bal * deal_i_adj.amount()
                                    }
                                    ArithmeticFunction::Scale(factor) => deal_i_adj.amount() * *factor,
                                };
                                let adjusted_consideration = match pa.consideration_adjustment() {
                                    Some(ArithmeticFunction::Add(adj_amount)) => {
                                        let bal = amount_balances.balance(adj_amount.unit());
                                        Some(*adj_amount / bal * deal_i_adj.consideration())
                                    }
                                    Some(ArithmeticFunction::Scale(factor)) => {
                                        Some(deal_i_adj.consideration() * *factor)
                                    }
                                    None => None,
                                };
                                deal_i_adj = Box::new(AdjustedTransaction::new(
                                    deal_i_adj,
                                    PoolAdjustment::new(
                                        0,
                                        Arc::clone(deal_i.entry()),
                                        ArithmeticFunction::Add(adjusted_amount),
                                        adjusted_consideration.map(|m| ArithmeticFunction::Add(m)),
                                    ),
                                ));
                                unmatched.push(DealingEvent::PoolAdjustment(pa));
                            }
                        }
                    }
                }
            }
            unmatched.push(DealingEvent::Deal(deal_i));
        }
    }
    // Change events to be new_events.
    *events = unmatched;

    // Creating raw deal pointers only at the end is appreciably safer.
    let pairs = pair_indices
        .into_iter()
        .map(|pair| {
            (
                if let DealingEvent::Deal(d_0) = &mut events[pair.0] {
                    PyDeal::from(d_0)
                } else {
                    unreachable!("Invalid pair index")
                },
                if let DealingEvent::Deal(d_1) = &mut events[pair.1] {
                    PyDeal::from(d_1)
                } else {
                    unreachable!("Invalid pair index")
                },
            )
        })
        .collect();

    // Require sorting
    events.sort();

    Ok(pairs)
}*/
/*
/// Updates `balances` with the change in balance brought about by the dealing event.
/// By calling for all events in chronological order, we can maintain the actual asset balance for CGT purposes.
fn update_amount_balance(balances: &mut HashMap<&'static Unit, Amount>, event: &DealingEvent) {
    match event {
        DealingEvent::Deal(deal) => {
            match balances.entry(deal.unit()) {
                Entry::Occupied(mut oe) => oe.insert(*oe.get() + deal.amount()),
                Entry::Vacant(ve) => *ve.insert(deal.amount()),
            };
            let curr_bal = balances.balance(deal.unit());
            balances.insert(deal.unit(), curr_bal + deal.amount());
        }
        DealingEvent::PoolAdjustment(adj) => match balances.entry(adj.unit()) {
            Entry::Occupied(mut oe) => match adj.amount_adjustment() {
                ArithmeticFunction::Add(amount) => {
                    oe.insert(*oe.get() + amount);
                }
                ArithmeticFunction::Scale(factor) => {
                    oe.insert(*oe.get() * factor);
                }
            },
            Entry::Vacant(ve) => match adj.amount_adjustment() {
                ArithmeticFunction::Add(amount) => {
                    ve.insert(amount);
                }
                ArithmeticFunction::Scale(_) => {}
            },
        },
    }
}*/
