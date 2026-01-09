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
use crate::price::Price;
use crate::price_db::PriceDatabase;
use crate::python::conversion::{DateTimeWrapper, DeferredArg};
use crate::python::environment::PythonEnvironment;
use crate::python::lambda::Lambda;
use crate::python::mod_ledger::PyPrice;
use crate::unit::Unit;
use crate::valuer::{Valuation, ValuationError, ValuationResult, Valuer};
use crate::{err, parse, parsing};
use pyo3::{PyObject, Python};
use std::sync::Arc;

pub struct LambdaValuer<'h, 'p, 'l> {
    config: Configuration<'h>,
    price_db: &'p PriceDatabase<'h>,
    lambda: &'l Lambda,
    datetime: JDateTime,
}
impl<'h, 'p, 'l> LambdaValuer<'h, 'p, 'l> {
    pub fn new(
        config: Configuration<'h>,
        price_db: &'p PriceDatabase<'h>,
        datetime: JDateTime,
        lambda: &'l Lambda,
    ) -> Self {
        Self { config, price_db, lambda, datetime }
    }

    fn extract_price_from_python_result(
        config: &mut Configuration<'h>,
        base_unit: &'h Unit<'h>,
        quote_unit: &'h Unit<'h>,
        datetime: JDateTime,
        result: PyObject,
    ) -> Result<Vec<Arc<Price<'h>>>, ValuationError> {
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
                    Ok((_, amount)) => amount,
                    Err(_) => {
                        let dec = parse!(amount_str, parsing::amount::parsed_decimal, config)
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
            } else if let Ok(None) = result.extract::<Option<String>>(py) {
                Err(ValuationError::Undetermined(err!("None returned")))
            } else {
                Err(ValuationError::EvalFailure(err!("Unexpected python result: {}", result)))
            }
        })
    }
}

impl<'h, 'p, 'l> Valuer<'h> for LambdaValuer<'h, 'p, 'l> {
    /// Values the specified `amount` in the `quote_unit` by running the python lambda function.
    /// The result of the function is converted to an amount which may not be in the same `quote_unit` as requested.
    /// The function could be run multiple times in this case to value via another unit.
    fn value(&mut self, quote_unit: &'h Unit<'h>, amount: Amount<'h>) -> ValuationResult<'h> {
        let py_datetime = match PythonEnvironment::eval::<DateTimeWrapper>(
            &format!(
                "datetime.strptime(\"{}\", \"%Y-%m-%dT%H:%M:%S%z\")",
                self.datetime.datetime().format("%FT%T%z")
            ),
            None,
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
                self.lambda.expression_with_args(vec![
                    quote_unit.to_string(),
                    amount.unit().to_string(),
                    py_datetime.0.format("%FT%T%z").to_string()
                ]),
            )
        };

        let result = self
            .lambda
            .eval(args, self.config.version().node_id().journal_incarnation())
            .map_err(|e| {
                ValuationError::EvalFailure(
                    err!(
                        "Eval failure with: {}",
                        self.lambda.expression_with_args(vec![
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
                    err!(err!(e; "Cannot determine price from expression: '{}'", self.lambda.expression()); "No price found for '{}' -> '{}'", amount.unit(), quote_unit),
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
                let mut valuation = Valuation::new(
                    price.quote_unit().with_quantity(amount.quantity() * price.price().quantity()),
                );
                price.sources().for_each(|s| {
                    valuation.add_source(s);
                });
                Ok(valuation)
            }
            // More than one result provided. We put all prices in to the price database and initialize
            // a new price database with just the results provided, returning the price that's deemed closest.
            _ => {
                let result_database = PriceDatabase::default();
                let prices_len = prices.len();
                for price in prices {
                    self.price_db.put(Arc::clone(&price));
                    result_database.put(price);
                }
                debug!("{} Inserted {} prices in to the price db", self.datetime, prices_len);
                let (bc, qc) = if got_inverse {
                    (quote_unit, amount.unit())
                } else {
                    (amount.unit(), quote_unit)
                };
                match result_database.get_closest(self.datetime, usize::MAX, bc, qc) {
                    Some(price) => {
                        let price = if got_inverse { Arc::new(price.inverse()) } else { price };
                        debug!(
                            "{} Retrieved Price for '{}': {}",
                            self.datetime,
                            price.base_unit(),
                            price.price()
                        );
                        let mut valuation = Valuation::new(
                            price
                                .quote_unit()
                                .with_quantity(amount.quantity() * price.price().quantity()),
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
