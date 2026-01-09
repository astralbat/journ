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
use crate::err;
use crate::journal_entry::JournalEntry;
use crate::price_db::PriceDatabase;
use crate::unit::Unit;
use crate::valuer::{
    EntryValuer, LambdaValuer, LinearSystemValuer, Valuation, ValuationError, ValuationResult,
    Valuer,
};
use itertools::Itertools;
use std::collections::{HashSet, VecDeque};
use std::iter;
use std::sync::Arc;

pub struct SystemValuer<'h, 'e> {
    entry_valuer: Option<EntryValuer<'h, 'e>>,
    config: Configuration<'h>,
    linear_system_valuer: LinearSystemValuer<'h>,
    datetime: JDateTime,
    alternative_base_units: VecDeque<&'h Unit<'h>>,
    tried_base_units: HashSet<&'h Unit<'h>>,
}
impl<'h> SystemValuer<'h, '_> {
    pub fn on_date(config: Configuration<'h>, datetime: JDateTime) -> Self {
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
    fn value(&mut self, quote_unit: &'h Unit<'h>, amount: Amount<'h>) -> ValuationResult<'h> {
        // We don't need to value anything
        if amount.unit() == quote_unit {
            return Ok(Valuation::new(amount));
        }

        // Try the entry valuer first.
        match self.entry_valuer.as_mut().map(|ev| ev.value(quote_unit, amount)) {
            Some(Ok(entry_valuation)) => {
                info!("{} Valued via entry: {} = {:?}", self.datetime, amount, entry_valuation);
                return Ok(entry_valuation);
            }
            Some(Err(ValuationError::EvalFailure(e))) => {
                return Err(ValuationError::EvalFailure(err!(e; "Entry valuer failed")));
            }
            _ => {}
        }

        // Next, the linear system valuer.
        match self.linear_system_valuer.value(quote_unit, amount) {
            Ok(linear_valuation) => {
                info!(
                    "{} Valued via derivation: {} = {:?}",
                    self.datetime, amount, linear_valuation
                );
                return Ok(linear_valuation);
            }
            Err(ValuationError::EvalFailure(e)) => {
                return Err(ValuationError::EvalFailure(err!(e; "Linear system valuer failed")));
            }
            _ => {}
        }

        // Finally, enter a loop where we try first, in terms of alternative base units first if we have any.
        let base_unit = amount.unit();
        self.alternative_base_units.push_back(base_unit);
        let mut intermediate_valuation = None;
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
                            match lambda_valuer
                                .value(quote_unit, unit_to_value.with_quantity(1))
                                .map(|mut v| {
                                    intermediate_valuation.take().map(|iv| v.set_via(iv));
                                    v
                                }) {
                                Ok(lambda_val)
                                    if unit_to_value == base_unit
                                        && lambda_val.unit() == quote_unit =>
                                {
                                    let value = *lambda_val * amount.quantity();
                                    info!(
                                        "{} Valued via lookup: {} = {:?}",
                                        self.datetime, amount, value
                                    );
                                    break Ok(lambda_val.with_value(value));
                                }
                                // Received a valuation not in our base unit. Add this to the system and try to solve
                                // it for the actual base unit.
                                Ok(lambda_val) => {
                                    info!(
                                        "{} Valued via lookup: {} = {:?}",
                                        self.datetime,
                                        unit_to_value.with_quantity(1),
                                        *lambda_val
                                    );
                                    self.linear_system_valuer
                                        .add_value((unit_to_value.with_quantity(1), *lambda_val));

                                    match self.linear_system_valuer.value(quote_unit, amount) {
                                        Ok(mut linear_system_val) => {
                                            linear_system_val.set_via(lambda_val);
                                            info!(
                                                "{} Valued via derivation: {} = {:?}",
                                                self.datetime, amount, *linear_system_val
                                            );
                                            break Ok(linear_system_val);
                                        }
                                        _ => {
                                            // When the python function returns a value in a different unit, it also indicates a request
                                            // for indirection. We'll try to value that unit in the quote unit.
                                            // We need to note the units we've previously tried so we don't get stuck in a loop.
                                            if lambda_val.unit() != quote_unit
                                                && !self
                                                    .tried_base_units
                                                    .contains(&lambda_val.unit())
                                            {
                                                self.alternative_base_units
                                                    .push_front(lambda_val.unit());
                                            }
                                            intermediate_valuation = Some(lambda_val);
                                        }
                                    }
                                }
                                Err(ValuationError::Undetermined(reason))
                                    if unit_to_value == base_unit =>
                                {
                                    break Err(ValuationError::Undetermined(reason));
                                }
                                Err(ValuationError::EvalFailure(e)) => {
                                    break Err(ValuationError::EvalFailure(e));
                                }
                                _ => {}
                            }
                        }
                        None if unit_to_value == base_unit => {
                            break Err(ValuationError::Undetermined(err!(
                                "No price expression found for unit: {}",
                                base_unit
                            )));
                        }
                        None => continue,
                    }
                }
                None => {
                    break Err(ValuationError::Undetermined(err!("Unable to value: {}", amount)));
                }
            }
        }?;
        Ok(value)
    }
}
