/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::Amount;
use crate::configuration::Configuration;
use crate::datetime::JDateTime;
use crate::journal_context::JContext;
use crate::parsing::parse_with_config_mut;
use crate::price::Price;
use crate::price_db::PriceDatabase;
use crate::python::conversion::{DateTimeWrapper, DeferredArg};
use crate::python::environment::PythonEnvironment;
use crate::python::mod_ledger::PyPrice;
use crate::unit::Unit;
use crate::valuer::{
    LinearSystemValuer, PriceDatabaseValuer, Valuation, ValuationError, ValuationResult, Valuer,
};
use crate::{err, parsing};
use pyo3::{Py, PyAny, Python};
use rust_decimal::Decimal;
use smallvec::SmallVec;
use std::sync::Arc;

pub struct LambdaValuer<'h> {
    config: Configuration<'h>,
    datetime: JDateTime,
    linear_system_valuer: Option<LinearSystemValuer<'h>>,
    /// Units currently being valued. This is used to detect cycles in the valuation process and prevent infinite recursion.
    eval_stack: SmallVec<[(&'h Unit<'h>, &'h Unit<'h>); 4]>,
}
impl<'h> LambdaValuer<'h> {
    pub fn new(config: Configuration<'h>, datetime: JDateTime) -> Self {
        Self { config, datetime, linear_system_valuer: None, eval_stack: SmallVec::new() }
    }

    /// Use the requested `LinearSystemValuer` that can be appended to and evaluated when
    /// the lambda function is unable to provide a valuation. This is useful when the lambda
    /// function is used to provide a valuation for a unit that is not directly convertible to
    /// the requested quote unit, but can be converted via another unit.
    pub fn set_linear_system_valuer(&mut self, valuer: LinearSystemValuer<'h>) {
        self.linear_system_valuer = Some(valuer);
    }

    pub fn into_linear_system_valuer(self) -> Option<LinearSystemValuer<'h>> {
        self.linear_system_valuer
    }

    fn extract_price_from_python_result(
        config: &mut Configuration<'h>,
        base_unit: &'h Unit<'h>,
        quote_unit: &'h Unit<'h>,
        datetime: JDateTime,
        result: Py<PyAny>,
    ) -> Result<Vec<Arc<Price<'h>>>, ValuationError> {
        Python::attach(|py| {
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
                let amount_str = JContext::get().allocator().alloc(s).as_str();
                let price = match parse_with_config_mut(amount_str, parsing::amount::amount, config)
                {
                    Ok((_, amount)) => amount,
                    Err(_) => {
                        let dec =
                            parse_with_config_mut(amount_str, parsing::decimal::decimal, config)
                                .map_err(ValuationError::EvalFailure)?
                                .1;
                        quote_unit.with_quantity(dec)
                    }
                };
                Ok(vec![Arc::new(Price::new(datetime, base_unit, price, None))])
            } else if let Ok(py_price) = result.extract::<PyPrice>(py) {
                Ok(vec![Arc::new(py_price.as_price(py, config).map_err(|e| {
                    ValuationError::EvalFailure(err!(e; "Unable to convert PyPrice"))
                })?)])
            } else if let Ok(val) = result.extract::<u128>(py) {
                Ok(vec![Arc::new(Price::new(
                    datetime,
                    base_unit,
                    quote_unit.with_quantity(Decimal::from(val)),
                    None,
                ))])
            } else if let Ok(None) = result.extract::<Option<String>>(py) {
                Err(ValuationError::Undetermined(err!("None returned")))
            } else {
                Err(ValuationError::EvalFailure(err!("Unexpected python result: {}", result)))
            }
        })
    }

    fn value_impl(&mut self, quote_unit: &'h Unit<'h>, amount: Amount<'h>) -> ValuationResult<'h> {
        // Check the price database first
        if let Ok(lookup_value) = PriceDatabaseValuer::new(self.datetime).value(quote_unit, amount)
        {
            debug!("{} Valued via price DB: {:?}", self.datetime, lookup_value);
            return Ok(lookup_value);
        }
        let price_db = amount.unit().prices().unwrap_or_else(|| JContext::get().price_database());

        let lambda = amount.unit().conversion_expression().ok_or_else(|| {
            ValuationError::Undetermined(err!(
                "No valuation expression found for '{}'",
                amount.unit()
            ))
        })?;

        let py_datetime = match PythonEnvironment::eval::<DateTimeWrapper>(
            &format!(
                "datetime.strptime(\"{}\", \"%Y-%m-%dT%H:%M:%S%z\")",
                self.datetime.datetime().format("%FT%T%z")
            ),
            None,
        ) {
            Ok(r) => r,
            Err(e) => panic!("Unable to prepare datetime for valuation call: {}", e),
        };

        let args: Vec<Box<dyn DeferredArg>> = vec![
            Box::new(quote_unit.to_string()),
            Box::new(amount.unit().to_string()),
            Box::new(py_datetime),
        ];

        let no_price_found_err = || {
            err!(
                "No price found from: '{}'",
                lambda.expression_with_args(vec![
                    quote_unit.to_string(),
                    amount.unit().to_string(),
                    py_datetime.0.format("%FT%T%z").to_string()
                ]),
            )
        };

        let result = lambda.eval(args).map_err(|e| {
            ValuationError::EvalFailure(
                err!(
                    "Eval failure with: {}",
                    lambda.expression_with_args(vec![
                        quote_unit.to_string(),
                        amount.unit().to_string(),
                        py_datetime.0.format("%FT%T%z").to_string()
                    ])
                )
                .with_source(e),
            )
        })?;
        let mut prices = match LambdaValuer::extract_price_from_python_result(
            &mut self.config,
            amount.unit(),
            quote_unit,
            self.datetime,
            result,
        ) {
            Ok(r) => r,
            Err(ValuationError::EvalFailure(e)) => {
                return Err(ValuationError::EvalFailure(
                    err!(err!(e; "Cannot determine price from expression: '{}'", lambda.expression()); "No price found for '{}' -> '{}'", amount.unit(), quote_unit),
                ));
            }
            Err(ValuationError::Undetermined(_e)) => {
                return Err(ValuationError::Undetermined(no_price_found_err()));
            }
        };
        let got_inverse = prices.first().map(|p| p.base_unit() == quote_unit).unwrap_or(false);
        match prices.len() {
            // No results, no valuation possible.
            0 => Err(ValuationError::Undetermined(err!(
                no_price_found_err().with_source(err!("No results returned"))
            ))),
            1 => {
                price_db.put(prices[0].clone());
                debug!(
                    "{:?} Retrieved Price for '{}': {:?}",
                    self.datetime,
                    prices[0].base_unit(),
                    prices[0].price()
                );
                // The price evaluation function might have given us the inverse of what we've asked for and we'll
                // need to correct that here.
                let price =
                    if got_inverse { Arc::new(prices[0].inverse()) } else { prices.pop().unwrap() };
                let mut valuation = Valuation::binary(
                    price.quote_unit().with_quantity(amount.quantity() * price.price().quantity()),
                    amount,
                );
                price.sources().for_each(|s| {
                    valuation.add_source(s);
                });
                Ok(valuation)
            }
            // More than one result provided. We put all prices in to the price database and initialise
            // a new price database with just the results provided, returning the price that's deemed closest.
            _ => {
                let result_database = PriceDatabase::default();
                let prices_len = prices.len();
                for price in prices {
                    price_db.put(Arc::clone(&price));
                    result_database.put(price);
                }
                debug!("{:?} Inserted {} prices in to the price db", self.datetime, prices_len);
                let (bc, qc) = if got_inverse {
                    (quote_unit, amount.unit())
                } else {
                    (amount.unit(), quote_unit)
                };
                match result_database.get_closest(self.datetime, usize::MAX, bc, qc) {
                    Some(price) => {
                        let price = if got_inverse { Arc::new(price.inverse()) } else { price };
                        debug!(
                            "{:?} Retrieved Price for '{}': {:?}",
                            self.datetime,
                            price.base_unit(),
                            price.price()
                        );
                        let mut valuation = Valuation::binary(
                            price
                                .quote_unit()
                                .with_quantity(amount.quantity() * price.price().quantity()),
                            amount,
                        );
                        price.sources().for_each(|s| {
                            valuation.add_source(s);
                        });
                        Ok(valuation)
                    }
                    None => unreachable!("There's at least one price in the result database"),
                }
            }
        }
    }
}

impl<'h> Valuer<'h> for LambdaValuer<'h> {
    /// Values the specified `amount` in the `quote_unit` by running the python lambda function
    /// of the `amount`'s unit. If the lambda function is not available, the valuer will attempt
    /// to use the `quote_unit`'s instead.
    ///
    /// # Lambda Function Return Values
    /// The lambda function can return a single price, a list of prices, or a string representing an amount.
    /// If the lambda function returns a list of prices, the valuer will use the closest price to the
    /// current datetime.
    ///
    /// Should the lambda function return a price that is not in terms of _base/quote_, therefore,
    /// including a third unit, the valuer will attempt to value via the third unit in a recursive
    /// fashion.
    fn value(&mut self, quote_unit: &'h Unit<'h>, amount: Amount<'h>) -> ValuationResult<'h> {
        if self.eval_stack.contains(&(amount.unit(), quote_unit)) {
            return Err(ValuationError::EvalFailure(err!(
                "Cycle detected in valuation: '{}' -> '{}'",
                amount.unit(),
                quote_unit
            )));
        }
        self.eval_stack.push((amount.unit(), quote_unit));

        let res = 'eval: {
            let mut value = {
                match self.value_impl(quote_unit, amount) {
                    Ok(result) => Ok(result),
                    Err(ValuationError::EvalFailure(e)) => Err(ValuationError::EvalFailure(e)),
                    Err(ValuationError::Undetermined(_e)) => {
                        // Try the inverse
                        self.value_impl(amount.unit(), quote_unit.with_quantity(1))
                    }
                }
            }?;

            match Valuer::value(&mut value, quote_unit, amount) {
                Ok(val) => Ok(val),
                Err(_) => {
                    if let Some(ref mut linear_system_valuer) = self.linear_system_valuer {
                        let mut values_iter = value.values();
                        let first_second_vals =
                            (values_iter.next().unwrap(), values_iter.next().unwrap());
                        if !linear_system_valuer.has_value(first_second_vals) {
                            linear_system_valuer.add_value(first_second_vals);
                        }
                        if let Ok(val) = linear_system_valuer.value(quote_unit, amount) {
                            break 'eval Ok(val);
                        }
                    }

                    self.value_via(quote_unit, amount, value)
                }
            }
        };

        self.eval_stack.pop();
        res
    }
}
