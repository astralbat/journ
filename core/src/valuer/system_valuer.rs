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
use std::cell::OnceCell;
use std::collections::{HashSet, VecDeque};
use std::iter;
use std::sync::Arc;

/// The primary means of valuation.
///
/// The valuer is lazily initialized for efficiency.
pub struct SystemValuer<'h, 'e> {
    inner: OnceCell<SystemValuerInner<'h, 'e>>,
    kind: Option<SystemValuerKind<'h, 'e>>,
}

impl<'h> SystemValuer<'h, '_> {
    pub fn on_date(config: Configuration<'h>, datetime: JDateTime) -> Self {
        Self { inner: OnceCell::new(), kind: Some(SystemValuerKind::OnDate(config, datetime)) }
    }
}

impl<'h, 'e> From<&'e JournalEntry<'h>> for SystemValuer<'h, 'e> {
    fn from(entry: &'e JournalEntry<'h>) -> Self {
        SystemValuer { inner: OnceCell::new(), kind: Some(SystemValuerKind::Entry(entry)) }
    }
}

impl<'h, 'e> Valuer<'h> for SystemValuer<'h, 'e> {
    fn value(&mut self, quote_unit: &'h Unit<'h>, amount: Amount<'h>) -> ValuationResult<'h> {
        self.inner
            .get_mut_or_init(|| match self.kind.take().unwrap() {
                SystemValuerKind::Entry(entry) => SystemValuerInner::from(entry),
                SystemValuerKind::OnDate(config, date) => SystemValuerInner::on_date(config, date),
            })
            .value(quote_unit, amount)
    }
}

enum SystemValuerKind<'h, 'e> {
    Entry(&'e JournalEntry<'h>),
    OnDate(Configuration<'h>, JDateTime),
}

struct SystemValuerInner<'h, 'e> {
    entry_valuer: Option<EntryValuer<'h, 'e>>,
    config: Configuration<'h>,
    linear_system_valuer: LinearSystemValuer<'h>,
    datetime: JDateTime,
    tried_base_units: HashSet<&'h Unit<'h>>,
}
impl<'h> SystemValuerInner<'h, '_> {
    pub fn on_date(config: Configuration<'h>, datetime: JDateTime) -> Self {
        Self {
            entry_valuer: None,
            config,
            linear_system_valuer: LinearSystemValuer::new(iter::empty()),
            datetime,
            tried_base_units: HashSet::new(),
        }
    }
}
impl<'h, 'e> From<&'e JournalEntry<'h>> for SystemValuerInner<'h, 'e> {
    /// Creates the `SystemValuer` from the entry.
    ///
    /// It should be assumed that if all the `entry`'s balanced postings are
    /// valued in any particular unit; the resulting valuations are themselves
    /// balanced (according to rounding tolerances).
    fn from(entry: &'e JournalEntry<'h>) -> Self {
        Self {
            entry_valuer: Some(EntryValuer::from(entry)),
            linear_system_valuer: LinearSystemValuer::from(entry),
            config: entry.config().clone(),
            datetime: entry.date_and_time().average(),
            tried_base_units: HashSet::new(),
        }
    }
}
impl<'h> Valuer<'h> for SystemValuerInner<'h, '_> {
    fn value(&mut self, quote_unit: &'h Unit<'h>, amount: Amount<'h>) -> ValuationResult<'h> {
        // We don't need to value anything
        if amount.unit() == quote_unit {
            return Ok(Valuation::basis(amount));
        } else if amount.is_zero() {
            return Ok(Valuation::from_amount(quote_unit.with_quantity(0), amount));
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

        // We can't work out the value from the entry. So we try via other units on the entry
        // that are higher ranked, iff we can value `amount` in terms of the higher ranked amount.
        let mut via_opt: Option<_> = self
            .entry_valuer
            .as_ref()
            .map(|ev| {
                ev.entry()
                    .postings()
                    .flat_map(|p| p.valued_amount().amounts())
                    .filter(|a| {
                        match (a.unit().conversion_ranking(), amount.unit().conversion_ranking()) {
                            (Some(a), Some(b)) => a < b,
                            (None, _) => false,
                            (_, None) => true,
                        }
                    })
                    .sorted_by_key(|a| a.unit())
                    .dedup()
                    .sorted_by_key(|a| a.unit().conversion_ranking().unwrap())
                    .filter_map(|a| {
                        a.unit().conversion_ranking()?;
                        self.linear_system_valuer.value(a.unit(), amount).ok()
                    })
                    .next()
            })
            .unwrap_or_default();

        loop {
            match via_opt.take() {
                Some(via) => {
                    info!(
                        "{} Attempting to value {} via {}",
                        self.datetime,
                        amount.unit(),
                        via.unit()
                    );
                    let price_db = via
                        .unit()
                        .prices()
                        .map(Arc::clone)
                        .unwrap_or_else(|| Arc::new(PriceDatabase::default()));
                    match via.unit().conversion_expression() {
                        Some(expr) => {
                            let mut lambda_valuer = LambdaValuer::new(
                                self.config.clone(),
                                &price_db,
                                self.datetime,
                                expr,
                            );
                            match lambda_valuer.value(quote_unit, *via) {
                                Ok(mut lambda_val) if lambda_val.unit() == quote_unit => {
                                    lambda_val.set_via(via);
                                    break Ok(lambda_val);
                                }
                                Ok(mut lambda_val) => {
                                    if !self.linear_system_valuer.has_value((*via, *lambda_val)) {
                                        self.linear_system_valuer.add_value((*via, *lambda_val));
                                    }

                                    match self.linear_system_valuer.value(quote_unit, amount) {
                                        Ok(mut lsv) => {
                                            lsv.set_via(via);
                                            break Ok(lsv);
                                        }
                                        Err(_) => {
                                            lambda_val.set_via(via);
                                            via_opt = Some(lambda_val);
                                        }
                                    }
                                }
                                Err(e) => break Err(e),
                            }
                        }
                        // No price expression, so try direct
                        None => continue,
                    }
                }
                None => {
                    let price_db = amount
                        .unit()
                        .prices()
                        .map(Arc::clone)
                        .unwrap_or_else(|| Arc::new(PriceDatabase::default()));
                    match amount.unit().conversion_expression() {
                        Some(expr) => {
                            let mut lambda_valuer = LambdaValuer::new(
                                self.config.clone(),
                                &price_db,
                                self.datetime,
                                expr,
                            );
                            match lambda_valuer.value(quote_unit, amount) {
                                Ok(lambda_val) if lambda_val.unit() == quote_unit => {
                                    break Ok(lambda_val);
                                }
                                Ok(lambda_val) => {
                                    via_opt = Some(lambda_val);
                                }
                                Err(e) => break Err(e),
                            }
                        }
                        None => {
                            break Err(ValuationError::Undetermined(err!(
                                "No price expression found for unit: {}",
                                amount.unit()
                            )));
                        }
                    }
                }
            }
        }

        /*
            // Finally, enter a loop where we try first, in terms of alternative base units first if we have any.
        let base_unit = amount.unit();
        alternative_base_amounts.push_back(Valuation::basis(amount));
        let mut intermediate_valuation = None;
        let value = loop {
            match alternative_base_amounts.pop_front() {
                Some(amount_to_value) => {
                    if amount_to_value.unit() != base_unit {
                        info!(
                            "{} Attempting to value {} via {}",
                            self.datetime,
                            base_unit,
                            amount_to_value.unit()
                        );
                        self.tried_base_units.insert(amount_to_value.unit());
                    }

                    let price_db = base_unit
                        .prices()
                        .map(Arc::clone)
                        .unwrap_or_else(|| Arc::new(PriceDatabase::default()));
                    match amount_to_value.unit().conversion_expression() {
                        Some(expr) => {
                            let mut lambda_valuer = LambdaValuer::new(
                                self.config.clone(),
                                &price_db,
                                self.datetime,
                                expr,
                            );
                            match lambda_valuer.value(quote_unit, *amount_to_value).map(|mut v| {
                                intermediate_valuation.take().map(|iv| v.set_via(iv));
                                v
                            }) {
                                Ok(lambda_val)
                                    if amount_to_value.unit() == base_unit
                                        && lambda_val.unit() == quote_unit =>
                                {
                                    info!(
                                        "{} Valued via lookup: {} = {:?}",
                                        self.datetime, amount, *lambda_val
                                    );
                                    break Ok(lambda_val);
                                }
                                // Received a valuation not in our base/quote units. Add this to the system and try to solve it.
                                // We do this even if the valuation is in our quote unit due to the linear system
                                // providing greater consistency for the entry (valuations should be balanced).
                                Ok(lambda_val) => {
                                    info!(
                                        "{} Valued via lookup: {} = {:?}",
                                        self.datetime, amount_to_value, *lambda_val
                                    );
                                    if !self
                                        .linear_system_valuer
                                        .has_value((*amount_to_value, *lambda_val))
                                    {
                                        self.linear_system_valuer
                                            .add_value((*amount_to_value, *lambda_val));
                                    }

                                    match self.linear_system_valuer.value(quote_unit, amount) {
                                        Ok(mut linear_system_val) => {
                                            linear_system_val.set_via(amount_to_value)
                                            if linear_system_val.unit() != lambda_val.unit() {
                                                linear_system_val.set_via(lambda_val);
                                            } else {
                                                linear_system_val.set_via(amount_to_value);
                                            }
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
                                                alternative_base_amounts
                                                    .push_front(lambda_val.value());
                                            }
                                            intermediate_valuation = Some(lambda_val);
                                        }
                                    }
                                }
                                Err(ValuationError::Undetermined(reason))
                                    if amount_to_value.unit() == base_unit =>
                                {
                                    break Err(ValuationError::Undetermined(reason));
                                }
                                Err(ValuationError::EvalFailure(e)) => {
                                    break Err(ValuationError::EvalFailure(e));
                                }
                                _ => {}
                            }
                        }
                        None if amount_to_value.unit() == base_unit => {
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
        Ok(value)*/
    }
}
