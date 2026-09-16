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
use crate::unit::Unit;
use crate::valuer::{
    EntryValuer, LambdaValuer, LinearSystemValuer, Valuation, ValuationError, ValuationResult,
    Valuer,
};
use itertools::Itertools;
use std::cell::OnceCell;
use std::{iter, mem};

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
}
impl<'h> SystemValuerInner<'h, '_> {
    pub fn on_date(config: Configuration<'h>, datetime: JDateTime) -> Self {
        Self {
            entry_valuer: None,
            config,
            linear_system_valuer: LinearSystemValuer::new(iter::empty()),
            datetime,
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
            datetime: entry.datetime_range().average(),
        }
    }
}
impl<'h> Valuer<'h> for SystemValuerInner<'h, '_> {
    fn value(&mut self, quote_unit: &'h Unit<'h>, amount: Amount<'h>) -> ValuationResult<'h> {
        // We don't need to value anything
        if amount.unit() == quote_unit {
            return Ok(Valuation::unary(amount));
        } else if amount.is_zero() {
            return Ok(Valuation::binary(quote_unit.with_quantity(0), amount));
        }

        // Try the entry valuer first.
        match self.entry_valuer.as_mut().map(|ev| ev.value(quote_unit, amount)) {
            Some(Ok(entry_valuation)) => {
                debug!("{} Valued via entry: {:?}", self.datetime, entry_valuation);
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
                info!("{} Valued via derivation: {:?}", self.datetime, linear_valuation);
                return Ok(linear_valuation);
            }
            Err(ValuationError::EvalFailure(e)) => {
                return Err(ValuationError::EvalFailure(err!(e; "Linear system valuer failed")));
            }
            _ => {}
        }

        // Next, the lambda valuer. This is the last resort, as it is the most expensive.
        // We can't work out the value from the entry. So we try via other units on the entry
        // that are higher ranked, iff we can value `amount` in terms of the higher ranked amount.
        let via_opt: Option<_> = self
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

        let mut lambda_valuer = LambdaValuer::new(self.config.clone(), self.datetime);
        lambda_valuer.set_linear_system_valuer(mem::take(&mut self.linear_system_valuer));
        let res = match via_opt {
            Some(via) if via.unit() != quote_unit && via.unit() != amount.unit() => {
                lambda_valuer.value_via(quote_unit, amount, via)
            }
            _ => lambda_valuer.value(quote_unit, amount),
        };
        self.linear_system_valuer = lambda_valuer.into_linear_system_valuer().unwrap();
        if let Ok(val) = &res {
            info!("{} Valued via value(): {:?}", self.datetime, val);
        }
        res

        /*
        let mut val_chain = via_opt;

        //while let Some(via) = val_chain.take() {
        let amount_to_val = val_chain.as_ref().map(Valuation::value).unwrap_or(amount);
            match LambdaValuer::new(self.config.clone(), self.datetime).value(quote_unit, amount_to_val) {
                Ok(mut lambda_val) => {
                    //lambda_val.set_via(via);
                    //val_chain = Some(lambda_val);
                    if !lambda_val
                        .values()
                        .any(|v| v.unit() == quote_unit || v.unit() == amount_to_val.unit())
                    {
                        return Err(ValuationError::Undetermined(err!(
                            "No valuation path from to {} via {}",
                            quote_unit,
                            via.values().map(|v| v.unit()).join(" -> ")
                        )));
                    }

                    let lambda_val_amount = lambda_val.via_values().next().unwrap();
                    if !self.linear_system_valuer.has_value((*lambda_val, lambda_val_amount)) {
                        self.linear_system_valuer.add_value((*lambda_val, lambda_val_amount));
                    }
                    match self.linear_system_valuer.value(quote_unit, amount) {
                        Ok(mut lsv) => {
                            lsv.set_via(via);
                            val_chain = Some(lsv);
                            break;
                        }
                        Err(_) => {
                            if via.via().any(|v| v.unit() == lambda_val.unit()) {
                                return Err(ValuationError::Undetermined(err!(
                                    "Infinite valuation loop: already valued via {}",
                                    via.unit()
                                )));
                            }
                            //lambda_val.set_via(via);
                            val_chain = Some(lambda_val);
                        }
                    }
                }
                // No price available, so try direct
                Err(ValuationError::Undetermined(_)) => continue,
                // A more serious error, so break out
                Err(ValuationError::EvalFailure(e)) => {
                    return Err(ValuationError::EvalFailure(e));
                }
            }
        //}
        let valuation = val_chain.unwrap();
        info!(
            "{} Valued with valuation function {} via {}",
            self.datetime,
            quote_unit,
            valuation.values().map(|v| v.unit()).join(" -> ")
        );
        Ok(valuation)

         */

        /*
        loop {
            match via_opt.take() {
                Some(via) => {
                    let qu =
                        if !via.contains_unit(quote_unit) { quote_unit } else { amount.unit() };
                    info!(
                        "{} Attempting to value {} via {}",
                        self.datetime,
                        qu,
                        via.values().map(|v| v.unit()).join(" -> ")
                    );
                    match LambdaValuer::new(self.config.clone(), self.datetime).value(qu, *via) {
                        Ok(mut lambda_val) => {
                            match Valuer::value(&mut lambda_val, quote_unit, amount) {
                                Ok(mut lambda_val) => {
                                    lambda_val.set_via(via);
                                    break Ok(lambda_val);
                                }
                                Err(_) => {
                                    if !self.linear_system_valuer.has_value((*via, *lambda_val)) {
                                        self.linear_system_valuer.add_value((*via, *lambda_val));
                                    }

                                    match self.linear_system_valuer.value(quote_unit, amount) {
                                        Ok(mut lsv) => {
                                            lsv.set_via(via);
                                            break Ok(lsv);
                                        }
                                        Err(_) => {
                                            if via.via().any(|v| v.unit() == lambda_val.unit()) {
                                                return Err(ValuationError::Undetermined(err!(
                                                    "Infinite valuation loop: already valued via {}",
                                                    via.unit()
                                                )));
                                            }
                                            lambda_val.set_via(via);
                                            via_opt = Some(lambda_val);
                                        }
                                    }
                                }
                            }
                        }
                        // No price available, so try direct
                        Err(ValuationError::Undetermined(_)) => continue,
                        // A more serious error, so break out
                        Err(ValuationError::EvalFailure(e)) => {
                            break Err(ValuationError::EvalFailure(e));
                        }
                    }
                }
                // via is None
                None => {
                    match LambdaValuer::new(self.config.clone(), self.datetime)
                        .value(quote_unit, amount)
                    {
                        Ok(mut lambda_val) => {
                            match Valuer::value(&mut lambda_val, quote_unit, amount) {
                                Ok(lambda_val) => {
                                    break Ok(lambda_val);
                                }
                                Err(e) => {
                                    via_opt = Some(lambda_val);
                                }
                            }
                        }
                        Err(e) => break Err(e),
                    }
                }
            }
        }*/
    }
}
