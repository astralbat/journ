/*
 * Copyright (c) 2019-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::{Amount, Quantity};
use crate::configuration::Configuration;
use crate::date_and_time::JDateTime;
use crate::error::{JournError, JournResult};
use crate::journal_entry::JournalEntry;
use crate::money_util::UnitAmountMap;
use crate::parsing;
use crate::price::Price;
use crate::price_db::PriceDatabase;
use crate::python::environment::PythonEnvironment;
use crate::python::lambda::Lambda;
use crate::python::mod_ledger::PyPrice;
use crate::unit::Unit;
use crate::valued_amount::{Valuation, ValuedAmount};
use crate::{err, parse};
use chrono::DateTime;
use chrono_tz::Tz;
use itertools::Itertools;
use linked_hash_set::LinkedHashSet;
use nalgebra::{DMatrix, DVector, OMatrix};
use pyo3::{PyObject, Python, ToPyObject};
use rust_decimal::prelude::{One, Zero};
use rust_decimal::Decimal;
use smallvec::{smallvec, SmallVec};
use std::borrow::Cow;
use std::cell::{OnceCell, RefCell};
use std::cmp::Ordering;
use std::collections::{BTreeSet, HashSet, VecDeque};
use std::convert::{Infallible, TryFrom};
use std::iter;
use std::ops::{ControlFlow, FromResidual, Neg, Range, RangeTo, Try};
use std::sync::Arc;

/// A trait for valuing amounts in different units.
pub trait Valuer<'h> {
    /// Values the specified `amount` in the `quote_unit`.
    ///
    /// Returns `Ok(Some(valued_amount))` if the value could be determined, `Ok(None)` if the value could not be determined,
    /// and `Err(e)` if an error occurred while determining the value.
    ///
    /// Returned values should never be rounded. It is the responsibility of the caller to round the value if necessary.
    ///
    /// It is allowed for the function to return an `Amount` in a unit other than the `quote_unit`, but implementors should
    /// note this behaviour up front.
    fn value(
        &mut self,
        amount: Amount<'h>,
        quote_unit: &'h Unit<'h>,
    ) -> JournResult<Option<Amount<'h>>>;
}

impl<'h, F> Valuer<'h> for F
where
    F: FnMut(Amount<'h>, &'h Unit<'h>) -> JournResult<Option<Amount<'h>>>,
{
    fn value(&mut self, amount: Amount<'h>, unit: &'h Unit<'h>) -> JournResult<Option<Amount<'h>>> {
        self(amount, unit)
    }
}

/// An entry valuer that looks to its own postings for valuations.
/// This is a simple valuer that does not analyse as a system of linear equations.
/// See [`LinearSystemValuer`] for that.
pub struct EntryValuer<'h, 'e> {
    entry: &'e JournalEntry<'h>,
}
impl<'h, 'e> EntryValuer<'h, 'e> {
    pub fn new(entry: &'e JournalEntry<'h>) -> Self {
        Self { entry }
    }
}

impl<'h, 'e> From<&'e JournalEntry<'h>> for EntryValuer<'h, 'e> {
    fn from(entry: &'e JournalEntry<'h>) -> Self {
        Self::new(entry)
    }
}

impl<'h> Valuer<'h> for EntryValuer<'h, '_> {
    fn value(
        &mut self,
        amount: Amount<'h>,
        quote_unit: &'h Unit<'h>,
    ) -> JournResult<Option<Amount<'h>>> {
        // Look for a direct unit valuation
        for pst in self.entry.postings() {
            if pst.unit() == amount.unit() {
                if let Ok(Some(amount)) =
                    pst.valued_amount().unit_valuer().value(amount, quote_unit)
                {
                    return Ok(Some(amount));
                }
            }
        }
        // Next, look for a total valuation
        for pst in self.entry.postings() {
            let base_amount = pst.valued_amount().value_in(amount.unit());
            let quote_amount = pst.valued_amount().value_in(quote_unit);
            if let (Some(base_amount), Some(quote_amount)) = (base_amount, quote_amount) {
                return Ok(Some(
                    quote_amount.abs() / base_amount.abs().quantity() * amount.quantity(),
                ));
            }
        }
        Ok(None)
    }
}

/// Valuer that derives valuations from a set of valued amounts in a specific quote unit.
pub struct LinearSystemValuer<'h> {
    data: Vec<f64>,
    units: SmallVec<[&'h Unit<'h>; 4]>,
    row_count: usize,
    zero_sum_row: Option<usize>,
}
impl<'h> LinearSystemValuer<'h> {
    /// Creates a new valuer from a set of valued amounts in a specific quote unit.
    /// `valued_amounts` should be a collection of X = Y known values.
    pub fn new(
        valued_amounts: impl Iterator<Item = (Amount<'h>, Amount<'h>)>,
    ) -> LinearSystemValuer<'h> {
        /*
        let mut units = smallvec!();
        let mut valued_amounts_count = 0;
        for (unit_a, unit_b) in valued_amounts.clone() {

            if !units.contains(&unit_a.unit()) {
                units.push(unit_a.unit());
            }
            if !units.contains(&unit_b.unit()) {
                units.push(unit_b.unit());
            }
            valued_amounts_count += 1;
        }*/

        // Add valuations, with equations rearranged to be Amount - Value = 0.
        let mut data = Vec::with_capacity(8);
        //data.extend((0..units.len()).map(|_| 0.0));
        let mut vav =
            LinearSystemValuer { data, units: smallvec!(), row_count: 0, zero_sum_row: None };

        for (amount, val) in valued_amounts {
            vav.add_value((amount, val));
        }
        vav
    }

    fn ensure_has_unit(&mut self, unit: &'h Unit<'h>) {
        if !self.units.contains(&unit) {
            let units_len = self.units.len();
            self.units.push(unit);

            let mut i = units_len;
            while i <= self.data.len() {
                self.data.insert(i, 0.0);
                i += units_len + 1;
                if units_len == 0 {
                    break;
                }
            }
        }
    }

    pub fn add_value(&mut self, value: (Amount<'h>, Amount<'h>)) {
        assert!(value.0.is_positive(), "Amount must be positive");
        assert!(value.1.is_positive(), "Value must be positive");

        self.ensure_has_unit(value.0.unit());
        self.ensure_has_unit(value.1.unit());

        let last_row = (self.row_count + 1) * self.units.len() - self.units.len();
        let amount_col = self.unit_col(value.0.unit());
        let val_col = self.unit_col(value.1.unit());
        // Make sure the last row is zeroed out. Gets used during value.
        self.data[last_row..last_row + self.units.len()].iter_mut().for_each(|c| *c = 0.0);
        // Make the value negative so that the equation is Amount + Value = 0.
        self.data[last_row + amount_col] = f64::try_from(value.0.quantity()).unwrap();
        self.data[last_row + val_col] = f64::try_from(value.1.quantity() * dec!(-1)).unwrap();
        // Keep the last row available for the Valuer impl.
        self.data.extend((0..self.units.len()).map(|_| 0.0));
        self.row_count += 1;
    }

    /// Adds a zero sum constraint to the valuer. This extra information can be useful in solving the linear system.
    /// the `amounts` should either sum to zero or the total considered value of them are zero if they are in more
    /// than one kind of unit.
    pub fn add_zero_sum(&mut self, amounts: impl Iterator<Item = Amount<'h>>) {
        // Pre-total the amounts in Decimal to make more accurate
        let mut total_amounts = vec![];
        for amount in amounts {
            total_amounts += amount
        }
        // All amounts are zero - there is no useful zero sum information to add
        // and attempting to do so will cause the system to be unsolvable.
        if total_amounts.iter().all(|a| a.is_zero()) {
            return;
        }

        for amount in total_amounts {
            self.ensure_has_unit(amount.unit());

            for (j, i) in self.row_indices(self.row_count).enumerate() {
                if j == self.unit_col(amount.unit()) {
                    self.data[i] = f64::try_from(amount.quantity()).unwrap();
                }
            }
        }

        // Keep the last row available for the Valuer impl.
        self.data.extend((0..self.units.len()).map(|_| 0.0));
        self.zero_sum_row = Some(self.row_count);
        self.row_count += 1;
    }

    fn unit_col(&self, unit: &'h Unit<'h>) -> usize {
        self.units.iter().position(|u| *u == unit).unwrap()
    }

    fn row_indices(&mut self, i: usize) -> Range<usize> {
        i * self.units.len()..(i + 1) * self.units.len()
    }
}

impl<'h> From<&JournalEntry<'h>> for LinearSystemValuer<'h> {
    fn from(entry: &JournalEntry<'h>) -> Self {
        // Iterator of all the valuations we are using.
        let total_valuations = move || {
            entry.balanced_postings().flat_map(move |p| {
                p.valuations().filter_map(move |v| {
                    match v {
                        Valuation::Total(m, _) => {
                            // Zero valuations create a contradiction for the base unit whose solution we
                            // have set to 1.0.
                            if m.is_zero() {
                                None
                            } else {
                                Some((p.amount().abs(), m.abs()))
                            }
                        }
                        Valuation::Unit(price) => {
                            Some((p.unit().with_quantity(Decimal::one()), price.abs()))
                        }
                    }
                })
            })
        };

        let mut vav = LinearSystemValuer::new(total_valuations());
        vav.add_zero_sum(entry.balanced_postings().map(|p| p.amount()));
        vav
    }
}

impl<'h> Valuer<'h> for LinearSystemValuer<'h> {
    fn value(
        &mut self,
        amount: Amount<'h>,
        quote_unit: &'h Unit<'h>,
    ) -> JournResult<Option<Amount<'h>>> {
        // If the base unit is not in the system, we cannot value it.
        if self.units.iter().all(|u| *u != amount.unit()) {
            return Ok(None);
        }

        // Ensure the quote unit is part of the system
        self.ensure_has_unit(quote_unit);

        let base_unit = amount.unit();
        // The last row is special in that 1.0 is set against the column of the base_curr and
        // 0 for all others. This matches the 1.0 in the b vector and defines the system's solution to be in terms of
        // the base unit.
        for (j, i) in self.row_indices(self.row_count).enumerate() {
            self.data[i] = if self.unit_col(base_unit) == j { 1.0 } else { 0.0 };
        }

        let mut A = DMatrix::from_row_slice(self.row_count + 1, self.units.len(), &self.data);
        let mut b = DVector::from_fn(A.nrows(), |i, _| if i == self.row_count { 1.0 } else { 0.0 });

        // Compute a tolerance based on f64 machine accuracy.
        // We use this to check whether values ~0 are considered zero.
        let singular_values = A.clone().svd(false, false).singular_values;
        let epsilon = 2.22 * 10.0f64.powf(-16.0);
        let tol = epsilon * A.ncols().max(A.nrows()) as f64 * singular_values[0];

        // Reorder the columns of A so that the units of interest are first. This ensures they are retained
        // when we retain only those columns that are linearly independent.
        A.swap_columns(self.unit_col(base_unit), 0);
        A.swap_columns(self.unit_col(quote_unit), 1);

        // If the system is not full rank, we'll have to remove some columns below.
        // This means the zero sum row is no longer valid and will have to be removed.
        if A.rank(tol) < A.ncols() {
            if let Some(zsr) = self.zero_sum_row {
                A = A.remove_row(zsr);
                b = b.remove_row(0);
            }
        }

        // Decide on which columns of A are going to be included.
        // We only want those that are linearly independent i.e. they contribute to the rank of A.
        // If we don't do this the system will be underdetermined and we won't be able to solve it uniquely.
        let mut span_matrix = DMatrix::zeros(A.nrows(), 0);
        span_matrix = span_matrix.insert_column(0, 0.0);
        span_matrix.column_mut(0).copy_from(&A.column(0));
        for col_index in 1..A.ncols() {
            let candidate_column = A.column(col_index).clone_owned();

            // Create the augmented span matrix by adding the candidate column
            let augmented_matrix = {
                let mut augmented = span_matrix.clone();
                let augmented_cols = augmented.ncols();
                augmented = augmented.insert_column(augmented_cols, 0.0);
                augmented.column_mut(augmented_cols).copy_from(&candidate_column);
                augmented
            };

            // Compute the ranks
            let current_rank = span_matrix.rank(tol);
            let augmented_rank = augmented_matrix.rank(tol);

            // If the rank increases, the candidate column is linearly independent
            if augmented_rank > current_rank {
                span_matrix = augmented_matrix;
            } else if col_index == 1 {
                // If the quote column is not linearly independent, we can't solve the system.
                return Ok(None);
            }
        }

        // We don't have our base/quote units included.
        if span_matrix.ncols() < 2 {
            return Ok(None);
        }

        trace!("A = {}", span_matrix);
        trace!("b = {}", b);

        // Now we are ready to solve
        let svd = span_matrix.clone().svd(true, true);
        let x = svd.solve(&b, tol).unwrap();
        trace!("x = {}", x);

        let tol = 1e-10;
        //let base_rate_adj = 1.0 / x.column(0)[0];
        let rate = x.column(0)[1];
        // A rate of approximately zero means there is no solution. The power should not be set too high - as high as experience allows.
        //let zero_check = (rate * 10.0f64.powf(9.0)).round();
        if rate > tol {
            return Ok(Some(
                (quote_unit.with_quantity(Decimal::try_from(1.0 / rate).unwrap_or_else(|e| {
                    panic!("Unable to convert {}, to Decimal: {}", 1.0 / rate, e)
                })) * amount.quantity())
                .abs(),
            ));
        }

        Ok(None)
    }
}

pub struct LambdaValuer<'h, 'p, 'l> {
    config: Configuration<'h>,
    price_db: &'p PriceDatabase<'h>,
    lambda: &'l Lambda,
    datetime: JDateTime<'h>,
}
impl<'h, 'p, 'l> LambdaValuer<'h, 'p, 'l> {
    pub fn new(
        config: Configuration<'h>,
        price_db: &'p PriceDatabase<'h>,
        datetime: JDateTime<'h>,
        lambda: &'l Lambda,
    ) -> Self {
        Self { config, price_db, lambda, datetime }
    }

    fn extract_price_from_python_result(
        config: &mut Configuration<'h>,
        base_unit: &'h Unit<'h>,
        quote_unit: &'h Unit<'h>,
        datetime: JDateTime<'h>,
        result: PyObject,
    ) -> JournResult<Vec<Arc<Price<'h>>>> {
        Python::with_gil(|py| {
            if let Ok(list) = result.extract::<Vec<_>>(py) {
                let mut main_list = vec![];
                for l in list {
                    main_list.append(&mut LambdaValuer::extract_price_from_python_result(
                        config, base_unit, quote_unit, datetime, l,
                    )?);
                }
                Ok(main_list)
            } else if let Ok(s) = result.extract::<String>(py) {
                // String can be an Amount (Unit & Decimal), or just a Decimal. In the latter case, we assume the
                // quote_unit as the unit.
                let amount_str = config.allocator().alloc(s).as_str();
                let price = match parse!(amount_str, parsing::amount::amount, config) {
                    (Ok((_, amount)), _config) => amount,
                    (Err(_), config) => {
                        let dec = parse!(amount_str, parsing::amount::parsed_decimal, config).0?.1;
                        quote_unit.with_quantity(dec)
                    }
                };
                Ok(vec![Arc::new(Price::new(datetime, base_unit, price, None))])
            } else if let Ok(py_price) = result.extract::<PyPrice>(py) {
                Ok(vec![Arc::new(
                    py_price
                        .as_price(py, config)
                        .map_err(|e| err!(e; "Unable to convert PyPrice"))?,
                )])
            } else if let Ok(None) = result.extract::<Option<String>>(py) {
                Err(err!("No price found for: '{}' -> '{}'", base_unit, quote_unit))
            } else {
                Err(err!("Unexpected python result: {}", result))
            }
        })
    }
}

impl<'h, 'p, 'l> Valuer<'h> for LambdaValuer<'h, 'p, 'l> {
    /// Values the specified `amount` in the `quote_unit` by running the python lambda function.
    /// The result of the function is converted to an amount which may not be in the same `quote_unit` as requested.
    /// The function could be run multiple times in this case to value via another unit.
    fn value(
        &mut self,
        amount: Amount<'h>,
        quote_unit: &'h Unit<'h>,
    ) -> JournResult<Option<Amount<'h>>> {
        let py_datetime: PyObject = match PythonEnvironment::eval(
            &format!(
                "datetime.strptime(\"{}\", \"%Y-%m-%dT%H:%M:%S%z\")",
                self.datetime.format("%FT%T%z")
            ),
            None,
            None,
        ) {
            Ok(r) => r,
            Err(e) => panic!("Unable to prepare datetime for valuation call: {}", e),
        };

        let args: Vec<Box<dyn ToPyObject>> = vec![
            Box::new(amount.unit().to_string()),
            Box::new(quote_unit.to_string()),
            Box::new(py_datetime),
        ];

        let result =
            self.lambda.eval(args, self.config.version().node_id().journal_incarnation())?;
        let db_config =
            amount.unit().prices().and_then(|p_db| p_db.node()).map(|file| file.config());
        let date_format = self.config.as_herd_ref().date_format();
        let time_format = self.config.as_herd_ref().time_format();
        let mut prices = match LambdaValuer::extract_price_from_python_result(
            &mut self.config,
            amount.unit(),
            quote_unit,
            self.datetime,
            /*
            JDateTime::from_datetime(
                self.datetime,
                Some(
                    db_config
                        .as_ref()
                        .map(|c| c.as_herd_ref().date_format())
                        .unwrap_or(date_format),
                ),
                Some(
                    db_config
                        .as_ref()
                        .map(|c| c.as_herd_ref().time_format())
                        .unwrap_or(time_format),
                ),
            ),*/
            result,
        ) {
            Ok(r) => r,
            Err(e) => {
                return Err(
                    err!(err!(e; "Cannot determine price from expression: '{}'", self.lambda.expression()); "No price found for '{}' -> '{}'", amount.unit(), quote_unit),
                )
            }
        };
        let got_inverse = prices.first().map(|p| p.base_unit() == quote_unit).unwrap_or(false);
        if prices.len() == 1 {
            self.price_db.put(prices[0].clone());
            debug!(
                "{} Retrieved Price for '{}': {}",
                self.datetime,
                prices[0].base_unit(),
                prices[0].price()
            );
            // The price evaluation function might have given us the inverse of what we've asked for and we'll
            // need to correct that here.
            let price =
                if got_inverse { Arc::new(prices[0].inverse()) } else { prices.pop().unwrap() };
            Ok(Some(price.quote_unit().with_quantity(amount.quantity() * price.price().quantity())))
        } else {
            // More than one result provided. We put all prices in to the price database and initialise
            // a new price database with just the results provided, returning the price that's deemed closest.
            let result_database = PriceDatabase::default();
            let prices_len = prices.len();
            for price in prices {
                self.price_db.put(Arc::clone(&price));
                result_database.put(price);
            }
            debug!("{} Inserted {} prices in to the price db", self.datetime, prices_len);
            let (bc, qc) =
                if got_inverse { (quote_unit, amount.unit()) } else { (amount.unit(), quote_unit) };
            match result_database.get_closest(self.datetime.datetime(), -1, bc, qc) {
                Some(price) => {
                    let price = if got_inverse { Arc::new(price.inverse()) } else { price };
                    debug!(
                        "{} Retrieved Price for '{}': {}",
                        self.datetime,
                        price.base_unit(),
                        price.price()
                    );
                    Ok(Some(
                        price
                            .quote_unit()
                            .with_quantity(amount.quantity() * price.price().quantity()),
                    ))
                }
                None => Err(err!("No price found for '{}' -> '{}'", amount.unit(), quote_unit)),
            }
        }
    }
}

/*
struct RecursiveValuer<'h> {
    prices_retrieved: RefCell<Vec<Arc<Price<'h>>>>,
}

impl<'h> RecursiveValuer<'h> {
    fn check_circular_price_lookup(&self, price: Arc<Price<'h>>) -> JournResult<()> {
        if self.prices_retrieved.borrow().iter().any(|p| p.quote_unit() == price.quote_unit()) {
            return Err(err!(
                "Circular loop detected while looking up price for currencies: {}",
                self.prices_retrieved.borrow().iter().map(|p| p.quote_unit()).join(",")
            ));
        }
        self.prices_retrieved.borrow_mut().push(Arc::clone(&price));
        Ok(())
    }

    fn get_value_internal(
        &self,
        config: &mut Configuration<'h>,
        price_db: &Arc<PriceDatabase<'h>>,
        base_unit: &'h Unit<'h>,
        quote_unit: &'h Unit<'h>,
        datetime: DateTime<Tz>,
    ) -> JournResult<Arc<Price<'h>>> {
        let price1 =
            self.eval_conversion_function(config, price_db, base_unit, quote_unit, datetime)?;
        let price = if price1.quote_unit() != quote_unit {
            self.check_circular_price_lookup(Arc::clone(&price1))?;
            let price2 = self.get_value_internal(
                config,
                price_db,
                price1.quote_unit(),
                quote_unit,
                datetime,
            )?;
            let combined_sources = if price1.sources().chain(price2.sources()).next().is_some() {
                let sources_string =
                    price1.sources().chain(price2.sources()).collect::<Vec<_>>().join(",");
                Some(config.allocator().alloc(sources_string).as_str())
            } else {
                None
            };
            Arc::new(Price::new(
                price1.datetime(),
                price1.base_unit(),
                price2.price() * price1.price().quantity(),
                combined_sources,
            ))
        } else {
            price1
        };
        Ok(price)
    }

    fn eval_conversion_function(
        &self,
        config: &mut Configuration<'h>,
        price_db: &PriceDatabase<'h>,
        base_unit: &'h Unit<'h>,
        quote_unit: &'h Unit<'h>,
        datetime: DateTime<Tz>,
    ) -> JournResult<Arc<Price<'h>>> {
        if let Some(expr) = base_unit.conversion_expression() {
            let py_datetime: PyObject = match PythonEnvironment::eval(
                &format!(
                    "datetime.strptime(\"{}\", \"%Y-%m-%dT%H:%M:%S%z\")",
                    datetime.format("%FT%T%z")
                ),
                None,
                None,
            ) {
                Ok(r) => r,
                Err(e) => panic!("Unable to prepare datetime for valuation call: {}", e),
            };

            let args: Vec<Box<dyn ToPyObject>> = vec![
                Box::new(base_unit.to_string()),
                Box::new(quote_unit.to_string()),
                Box::new(py_datetime),
            ];

            let result = expr.eval(args, config.version().node_id().journal_incarnation())?;
            let db_config =
                base_unit.prices().and_then(|p_db| p_db.node()).map(|file| file.config());
            let mut prices = match RecursiveValuer::extract_price_from_python_result(
                config,
                base_unit,
                quote_unit,
                JDateTime::from_datetime(
                    datetime,
                    Some(
                        db_config
                            .as_ref()
                            .map(|c| c.as_herd_ref().date_format())
                            .unwrap_or(config.as_herd_ref().date_format()),
                    ),
                    Some(
                        db_config
                            .as_ref()
                            .map(|c| c.as_herd_ref().time_format())
                            .unwrap_or(config.as_herd_ref().time_format()),
                    ),
                ),
                result,
            ) {
                Ok(r) => r,
                Err(e) => {
                    return Err(
                        err!(err!(e; "Cannot determine price from expression: '{}'", expr.expression()); "No price found for '{}' -> '{}'", base_unit, quote_unit),
                    )
                }
            };
            let got_inverse = prices.first().map(|p| p.base_unit() == quote_unit).unwrap_or(false);
            if prices.len() == 1 {
                price_db.put(prices[0].clone());
                debug!("Retrieved Price for '{}': {}", prices[0].base_unit(), prices[0].price());
                // The price evaluation function might have given us the inverse of what we've asked for and we'll
                // need to correct that here.
                if got_inverse {
                    Ok(Arc::new(prices[0].inverse()))
                } else {
                    Ok(prices.pop().unwrap())
                }
            } else {
                // More than one result provided. We put all prices in to the price database and initialise
                // a new price database with just the results provided, returning the price that's deemed closest.
                let result_database = PriceDatabase::default();
                let prices_len = prices.len();
                for price in prices {
                    price_db.put(Arc::clone(&price));
                    result_database.put(price);
                }
                debug!("Inserted {} prices in to the price db", prices_len);
                let (bc, qc) =
                    if got_inverse { (quote_unit, base_unit) } else { (base_unit, quote_unit) };
                match result_database.get_closest(datetime, -1, bc, qc) {
                    Some(price) => {
                        let price = if got_inverse { Arc::new(price.inverse()) } else { price };
                        debug!("Retrieved Price for '{}': {}", price.base_unit(), price.price());
                        Ok(price)
                    }
                    None => Err(err!("No price found for '{}' -> '{}'", base_unit, quote_unit)),
                }
            }
        } else {
            Err(err!("No price expression found for unit: {}", base_unit))
        }
    }

    fn extract_price_from_python_result(
        config: &mut Configuration<'h>,
        base_unit: &'h Unit<'h>,
        quote_unit: &'h Unit<'h>,
        datetime: JDateTime<'h>,
        result: PyObject,
    ) -> JournResult<Vec<Arc<Price<'h>>>> {
        Python::with_gil(|py| {
            if let Ok(list) = result.extract::<Vec<_>>(py) {
                let mut main_list = vec![];
                for l in list {
                    main_list.append(&mut RecursiveValuer::extract_price_from_python_result(
                        config, base_unit, quote_unit, datetime, l,
                    )?);
                }
                Ok(main_list)
            } else if let Ok(s) = result.extract::<String>(py) {
                // String can be an Amount (Unit & Decimal), or just a Decimal. In the latter case, we assume the
                // quote_unit as the unit.
                let amount_str = config.allocator().alloc(s).as_str();
                let price = match parse!(amount_str, parsing::amount::amount, config) {
                    (Ok((_, amount)), _config) => amount,
                    (Err(_), config) => {
                        let dec = parse!(amount_str, parsing::amount::parsed_decimal, config).0?.1;
                        quote_unit.with_quantity(dec)
                    }
                };
                Ok(vec![Arc::new(Price::new(datetime, base_unit, price, None))])
            } else if let Ok(py_price) = result.extract::<PyPrice>(py) {
                Ok(vec![Arc::new(
                    py_price
                        .as_price(py, config)
                        .map_err(|e| err!(e; "Unable to convert PyPrice"))?,
                )])
            } else if let Ok(None) = result.extract::<Option<String>>(py) {
                Err(err!("No price found for: '{}' -> '{}'", base_unit, quote_unit))
            } else {
                Err(err!("Unexpected python result: {}", result))
            }
        })
    }
}

impl Default for RecursiveValuer<'_> {
    fn default() -> Self {
        Self { prices_retrieved: RefCell::new(Vec::with_capacity(1)) }
    }
}

pub struct AlternativeBaseUnitValuer<'h, 'a> {
    alternative_units: Vec<&'h Unit<'h>>,
    inner: &'a dyn Valuer<'h>,
}
impl<'h> Valuer<'h> for AlternativeBaseUnitValuer<'h, '_> {
    fn value(
        &mut self,
        amount: Amount<'h>,
        quote_unit: &'h Unit<'h>,
    ) -> JournResult<Option<Amount<'h>>> {
        let base_unit = amount.unit();
        let unit_to_value = match self
            .alternative_units
            .iter()
            .copied()
            .sorted_by(|a, b| match (a.conversion_ranking(), b.conversion_ranking()) {
                (Some(a_v), Some(b_v)) => a_v.cmp(&b_v),
                (Some(_), None) => Ordering::Less,
                (None, Some(_)) => Ordering::Greater,
                (None, None) => Ordering::Equal,
            })
            .filter(|c| c.conversion_ranking().is_some())
            .next()
        {
            Some(unit) if unit != quote_unit => unit,
            _ => base_unit,
        };
        if unit_to_value != base_unit {
            info!("Attempting to value {} via {}", base_unit, unit_to_value);
        }
        self.alternative_units.retain(|u| *u != unit_to_value);
        if let Some(value) = self.inner.value(unit_to_value.with_quantity(1), quote_unit)? {
            return Ok(Some(value * amount.quantity()));
        }
        Ok(None)
    }
}*/

pub struct SystemValuer<'h, 'e> {
    entry_valuer: Option<EntryValuer<'h, 'e>>,
    config: Configuration<'h>,
    linear_system_valuer: LinearSystemValuer<'h>,
    datetime: JDateTime<'h>,
    alternative_base_units: VecDeque<&'h Unit<'h>>,
    tried_base_units: HashSet<&'h Unit<'h>>,
}
impl<'h> SystemValuer<'h, '_> {
    pub fn on_date(config: Configuration<'h>, datetime: JDateTime<'h>) -> Self {
        Self {
            entry_valuer: None,
            config,
            linear_system_valuer: LinearSystemValuer::new(iter::empty()),
            datetime,
            alternative_base_units: VecDeque::new(),
            tried_base_units: HashSet::new(),
        }
    }
}
impl<'h, 'e> From<&'e JournalEntry<'h>> for SystemValuer<'h, 'e> {
    fn from(entry: &'e JournalEntry<'h>) -> Self {
        Self {
            entry_valuer: Some(EntryValuer::from(entry)),
            linear_system_valuer: LinearSystemValuer::from(entry),
            config: entry.config().clone(),
            datetime: entry.date_and_time().average(),
            alternative_base_units: entry
                .postings()
                .flat_map(|p| p.valued_amount().units())
                .filter(|u| u.conversion_ranking().is_some())
                .sorted_by_key(|u| u.conversion_ranking())
                .collect(),
            tried_base_units: HashSet::new(),
        }
    }
}
impl<'h> Valuer<'h> for SystemValuer<'h, '_> {
    fn value(
        &mut self,
        amount: Amount<'h>,
        quote_unit: &'h Unit<'h>,
    ) -> JournResult<Option<Amount<'h>>> {
        // Try the entry valuer first.
        if let Some(entry_valuer) = &mut self.entry_valuer {
            if let Some(val) = entry_valuer.value(amount, quote_unit)? {
                info!("{} Valued via entry: {} = {:?}", self.datetime, amount, val);
                return Ok(Some(val));
            }
        }
        // Next, the linear system valuer.
        if let Some(value) = self.linear_system_valuer.value(amount, quote_unit)? {
            info!("{} Valued via derivation: {} = {:?}", self.datetime, amount, value);
            return Ok(Some(value));
        }

        // Finally, enter a loop where we try first, in terms of alternative base units first if we have any.
        let base_unit = amount.unit();
        self.alternative_base_units.push_back(base_unit);
        let value = loop {
            match self.alternative_base_units.pop_front() {
                Some(unit_to_value) => {
                    if unit_to_value != base_unit {
                        info!(
                            "{} Attempting to value {} via {}",
                            self.datetime, base_unit, unit_to_value
                        );
                        self.tried_base_units.insert(unit_to_value);
                    }

                    let price_db = base_unit
                        .prices()
                        .map(Arc::clone)
                        .unwrap_or_else(|| Arc::new(PriceDatabase::default()));
                    match unit_to_value.conversion_expression() {
                        Some(expr) => {
                            let mut lambda_valuer = LambdaValuer::new(
                                self.config.clone(),
                                &price_db,
                                self.datetime,
                                expr,
                            );
                            match lambda_valuer.value(unit_to_value.with_quantity(1), quote_unit)? {
                                Some(val)
                                    if unit_to_value == base_unit && val.unit() == quote_unit =>
                                {
                                    info!(
                                        "{} Valued via lookup: {} = {:?}",
                                        self.datetime, amount, val
                                    );
                                    break Ok(Some(val));
                                }
                                None if unit_to_value == base_unit => break Ok(None),
                                // Received a valuation not in our base unit. Add this to the system and try to solve
                                // if for the actual base unit.
                                Some(val) => {
                                    self.linear_system_valuer
                                        .add_value((unit_to_value.with_quantity(1), val));
                                    if let Some(val) =
                                        self.linear_system_valuer.value(amount, quote_unit)?
                                    {
                                        info!(
                                            "{} Valued via derivation: {} = {:?}",
                                            self.datetime, amount, val
                                        );
                                        break Ok(Some(val));
                                    }
                                    // When the python function returns a value in a different unit, it also indicates a request
                                    // for indirection. We'll try to value that unit in the quote unit.
                                    // We need to note the units we've previously tried so we don't get stuck in a loop.
                                    if val.unit() != quote_unit
                                        && !self.tried_base_units.contains(&val.unit())
                                    {
                                        self.alternative_base_units.push_front(val.unit());
                                    }
                                }
                                None => {}
                            }
                        }
                        None if unit_to_value == base_unit => {
                            break Err(err!("No price expression found for unit: {}", base_unit));
                        }
                        None => continue,
                    }
                }
                None => break Err(err!("Unable to value: {}", amount)),
            }
        }?;
        Ok(value)
    }
}

/*
/// Gets a valuation of `base_curr`/`quote_curr` by evaluating the price lookup function of the base unit.
pub fn get_value<'h>(
    config: &mut Configuration<'h>,
    base_curr: &'h Unit<'h>,
    quote_curr: &'h Unit<'h>,
    datetime: DateTime<Tz>,
) -> JournResult<Arc<Price<'h>>> {
    let valuer = RecursiveValuer::default();
    let price_db =
        base_curr.prices().map(Arc::clone).unwrap_or_else(|| Arc::new(PriceDatabase::default()));
    valuer.prices_retrieved.borrow_mut().clear();
    valuer.get_value_internal(config, &price_db, base_curr, quote_curr, datetime)
}

 */

/*
/// Attempts to evaluate the price of `base_curr`/`quote_curr` by first attempting to derive it on the entry,
/// and failing that, by evaluating the price lookup function on `base_curr`.
///
/// When it comes to evaluating the price lookup function, if another unit on the entry should 'out rank'
/// the `base_curr`, it will be used in place of the `base_curr` instead (as desired by the user).
/// The evaluation process repeats until either the price can be inferred on the entry, or the price was evaluated using `base_curr`
/// (reluctantly when 'out ranked').
///
/// Returns at least one price discovered where the last one will always be in terms of the `base_curr`.
///
/// # Note
/// This will not update the entry itself, use [value_entry()] instead.
fn value_with_entry<'h>(
    entry: &mut JournalEntry<'h>,
    base_unit: &'h Unit<'h>,
    quote_unit: &'h Unit<'h>,
) -> JournResult<SmallVec<[Arc<Price<'h>>; 2]>> {
    let mut prices = SmallVec::<[Arc<Price>; 2]>::new();
    match entry.valuation(base_unit.with_quantity(1), quote_unit)? {
        Some(price) => {
            prices.push(Arc::new(Price::new(
                entry.date_and_time().average(),
                base_unit,
                price,
                None,
            )));
        }
        None => {
            let mut valued_units = vec![];
            // Create a temporary config that will require to be mutable in case of previously unknown units for example.
            // It is not important to update the entry with this.
            let mut writeable_config = entry.config().clone();
            //let mut writeable_entry = OnceCell::new();
            loop {
                let curr_to_value = match entry
                    .postings()
                    .flat_map(|p| p.valued_amount().units())
                    .sorted_by(|a, b| match (a.conversion_ranking(), b.conversion_ranking()) {
                        (Some(a_v), Some(b_v)) => a_v.cmp(&b_v),
                        (Some(_), None) => Ordering::Less,
                        (None, Some(_)) => Ordering::Greater,
                        (None, None) => Ordering::Equal,
                    })
                    .find(|c| !valued_units.contains(c))
                    .filter(|c| c.conversion_ranking().is_some())
                {
                    Some(curr) if curr != quote_unit => curr,
                    _ => base_unit,
                };
                if curr_to_value != base_unit {
                    info!("Attempting to value {} via {}", base_unit, curr_to_value);
                }

                let average_time = entry.date_and_time().average();
                let price = get_value(
                    &mut writeable_config,
                    curr_to_value,
                    quote_unit,
                    average_time.datetime(),
                )
                .map_err(
                    |e| err!(e; entry.err(format!("Unable to value {base_unit} in {quote_unit}"))),
                )?;
                valued_units.push(curr_to_value);

                info!(
                    "Looked up price of {}/{} = {} on {}",
                    price.base_unit(),
                    price.quote_unit(),
                    price.price(),
                    price.datetime()
                );
                prices.push(price);
                let price = prices.last().unwrap();

                // Stop when the price is in the unit we want.
                if price.base_unit() == base_unit {
                    break;
                }

                // Add the price the entry and check whether it can now be derived on the entry.
                //let we = entry.get_mut_or_init(|| entry.clone());
                for pst in
                    entry.postings_mut().filter(|p| p.amount_in(price.quote_unit()).is_none())
                {
                    if let Some(amount) = pst.amount_in(price.base_unit()) {
                        let val = Valuation::new_total(
                            (price.price() * amount.quantity().abs()).rounded(),
                            false,
                        );
                        info!("Setting {}{}", pst.amount(), val);
                        pst.set_valuation(val);
                    }
                }
                // Try to derive the price on the entry.
                if let Some(price) = entry.valuation(base_unit.with_quantity(1), quote_unit)? {
                    prices.push(Arc::new(Price::new(
                        entry.date_and_time().average(),
                        base_unit,
                        price,
                        None,
                    )));
                    break;
                }
            }
        }
    }
    Ok(prices)
}

 */

/*
/// Values an entry to ensure all postings valued in `base_unit` can also valued in `quote_unit`.
pub fn value_entry<'h>(
    entry: &mut JournalEntry<'h>,
    base_unit: &'h Unit<'h>,
    quote_unit: &'h Unit<'h>,
) -> JournResult<SmallVec<[Arc<Price<'h>>; 2]>> {
    // Try to derive first, seeing as we're writing to the entry. This may save calling the price lookup function later.
    entry.derive_valuations()?;

    let prices = value_with_entry(entry, base_unit, quote_unit)?;

    // Ensure the value is rounded as totalling up later might deviate otherwise
    // If we are quoting all postings on the entry, we need to make sure they balance. To do this, we set
    // the last posting to the remaining amount.
    let base_unit_price = prices.last().unwrap();
    let mut partially_quoted = false;
    let unquoted_posting_count =
        entry.postings().filter(|p| p.amount_in(quote_unit).is_none()).count();
    let mut balanced_amount = quote_unit.with_quantity(0);
    for (i, pst) in entry.postings_mut().enumerate() {
        match pst.amount_in(quote_unit) {
            Some(val) => balanced_amount += val,
            None => {
                if let Some(amount) = pst.amount_in(base_unit_price.base_unit()) {
                    let total = if !partially_quoted && i == unquoted_posting_count - 1 {
                        balanced_amount * -1
                    } else {
                        (base_unit_price.price() * amount.quantity()).rounded()
                    };
                    balanced_amount += total;
                    pst.set_valuation(Valuation::new_total(total.abs(), false));
                } else {
                    partially_quoted = true;
                }
            }
        }
    }
    for price in &prices {
        for source in price.sources() {
            if !entry.has_metadata_tag_value("ValueSource", source) {
                entry.append_metadata("ValueSource", source);
            }
        }
    }
    Ok(prices)
}*/

pub enum ValueResult<'h, O> {
    Ok(O),
    Err(JournError),
    ValuationNeeded(&'h Unit<'h>, &'h Unit<'h>),
}
pub enum ValueResidual<'h> {
    Err(JournError),
    ValuationNeeded(&'h Unit<'h>, &'h Unit<'h>),
}

impl<'h, O> Try for ValueResult<'h, O> {
    type Output = O;
    type Residual = ValueResidual<'h>;

    fn from_output(output: Self::Output) -> Self {
        ValueResult::Ok(output)
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            ValueResult::Ok(output) => ControlFlow::Continue(output),
            ValueResult::Err(e) => ControlFlow::Break(ValueResidual::Err(e)),
            ValueResult::ValuationNeeded(u1, u2) => {
                ControlFlow::Break(ValueResidual::ValuationNeeded(u1, u2))
            }
        }
    }
}
impl<'h, O> FromResidual<ValueResidual<'h>> for ValueResult<'h, O> {
    fn from_residual(residual: ValueResidual<'h>) -> Self {
        match residual {
            ValueResidual::Err(e) => ValueResult::Err(e),
            ValueResidual::ValuationNeeded(u1, u2) => ValueResult::ValuationNeeded(u1, u2),
        }
    }
}

impl<'h, O> FromResidual<Result<Infallible, JournError>> for ValueResult<'h, O> {
    fn from_residual(residual: Result<Infallible, JournError>) -> Self {
        match residual {
            Err(e) => ValueResult::Err(e),
            Ok(infallible) => match infallible {}, // This will never happen
        }
    }
}

/// Executes a function that may return a `ValuationNeeded` result during its processing.
/// This will cause the valuation to be performed on the entry before retrying.
pub fn exec_optimistic<'h, F, O>(entry: &mut Cow<JournalEntry<'h>>, f: F) -> JournResult<O>
where
    F: Fn(&JournalEntry<'h>) -> ValueResult<'h, O>,
{
    loop {
        match f(entry.as_ref()) {
            ValueResult::Ok(o) => return Ok(o),
            ValueResult::Err(e) => return Err(e),
            ValueResult::ValuationNeeded(base, quote) => {
                match SystemValuer::from(entry.as_ref()).value(base.with_quantity(1), quote)? {
                    Some(price) => {
                        let mut entry = entry.to_mut();
                        for pst in entry.postings_mut() {
                            if let Some(amount) = pst.amount_in(base) {
                                // When the amount is small, use unit valuations for increased accuracy. This matters when using the LinearSystemValuer.
                                // We round in case the entry gets written out later.
                                let val = if amount.abs() < 1 {
                                    Valuation::new_unit(price.rounded())
                                } else {
                                    Valuation::new_total(
                                        (price * amount.quantity().abs()).rounded(),
                                        false,
                                    )
                                };
                                pst.set_valuation(val);
                            }
                        }
                    }
                    None => {
                        return Err(err!("Unable to value {} in {} on entry", base, quote));
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use nalgebra::{DMatrix, DVector};

    #[test]
    fn test_linear() {
        // Case 1
        #[rustfmt::skip]
            let m = DMatrix::from_row_slice(3, 2, &[
            1.0, 0.0,
            10.0, -2.0,
            0.0, 0.0,
        ]);
        let b = DVector::from_row_slice(&[1.0, 0.0, 0.0]);
        let decomp = m.svd(true, true);
        let solved = decomp.solve(&b, 0.0).unwrap();
        println!("Case 1: {}", solved);

        // Case 2
        #[rustfmt::skip]
            let m = DMatrix::from_row_slice(2, 2, &[
            1.0, 0.0,
            10.0, -5.0,
        ]);
        let b = DVector::from_row_slice(&[1.0, 0.0]);
        let decomp = m.svd(true, true);
        let solved = decomp.solve(&b, 0.0).unwrap();
        println!("Case 2: {}", solved);

        // Case 3
        #[rustfmt::skip]
            let m = DMatrix::from_row_slice(2, 2, &[
            1.0, 0.0,
            12.0, -4.0
        ]);
        let b = DVector::from_row_slice(&[1.0, 0.0]);
        let decomp = m.svd(true, true);
        let solved = decomp.solve(&b, 0.0).unwrap();
        println!("Case 3: {}", solved);

        // Case 4
        #[rustfmt::skip]
            let m = DMatrix::from_row_slice(3, 3, &[
            1.0, 0.0, 0.0,
            0.0, -5.0, 2.0,
            10.0, 0.0, -2.0
        ]);
        let b = DVector::from_row_slice(&[1.0, 0.0, 0.0]);
        let decomp = m.svd(true, true);
        let solved = decomp.solve(&b, 0.0).unwrap();
        println!("Case 4: {}", solved);

        // Case 5
        #[rustfmt::skip]
            let m = DMatrix::from_row_slice(4, 4, &[
            1.0, 0.0, 0.0, 0.0,
            8.0, 0.0, 0.0, -12.0,
            0.0, 0.0, 16.0, -4.0,
            8.0, -4.0, 16.0, -8.0
        ]);
        let b = DVector::from_row_slice(&[1.0, 0.0, 0.0, 0.0]);
        let decomp = m.svd(true, true);
        let solved = decomp.solve(&b, 0.0).unwrap();
        println!("Case 5: {}", solved);

        // Case 6
        #[rustfmt::skip]
        let m = DMatrix::from_row_slice(5, 5, &[
            0.0, 1.0, 0.0, 0.0, 0.0,
            8.0, 0.0, 0.0, -12.0, 0.0,
            0.0, 0.0, 16.0, -4.0, 0.0,
            0.0, 0.0, -32.0, 0.0, 4.0,
            8.0, -4.0, 16.0, 0.0, -4.0,
        ]);
        let b = DVector::from_row_slice(&[1.0, 0.0, 0.0, 0.0, 0.0]);
        let solved = m.svd(true, true).solve(&b, 0.0).unwrap();
        println!("Case 6: {}", solved);

        // Case 7 (incomplete system)
        #[rustfmt::skip]
            let m = DMatrix::from_row_slice(5, 5, &[
            0.0, 1.0, 0.0, 0.0, 0.0,
            8.0, -16.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 16.0, -4.0, 0.0,
            0.0, 0.0, 16.0, 0.0, -16.0,
            0.0, 0.0, 0.0, 0.0, 0.0
        ]);
        let b = DVector::from_row_slice(&[1.0, 0.0, 0.0, 0.0, 0.0]);
        let solved = m.svd(true, true).solve(&b, 0.0).unwrap();
        println!("Case 7: {}", solved);

        // Case 8 (slightly different rates)
        #[rustfmt::skip]
            let m = DMatrix::from_row_slice(3, 2, &[
            0.0, 1.0,
            1.0, -1.111,
            10.0, -11.12,
        ]);
        let b = DVector::from_row_slice(&[1.0, 0.0, 0.0]);
        let solved = m.svd(true, true).solve(&b, 0.0).unwrap();
        println!("Case 8: {}", solved);
    }
}
