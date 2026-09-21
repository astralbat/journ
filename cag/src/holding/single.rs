/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjusted_value::AdjustedValue;
use crate::adjustment::Adjustment;
use crate::cag_configuration::CagConfiguration;
use crate::deal::Deal;
use crate::holding::deal_holding::DEAL_HOLDING_ID_COUNTER;
use crate::holding::{DealHolding, DealHoldingSummary};
use crate::module_init::MODULE_NAME;
use chrono::DateTime;
use chrono_tz::Tz;
use journ_core::amount::{Amount, Quantity};
use journ_core::configuration::Configuration;
use journ_core::datetime::{DateTimePrecision, JDateTime};
use journ_core::err;
use journ_core::error::{JournError, JournResult};
use journ_core::ext::NumExt;
use journ_core::unit::Unit;
use journ_core::valuer::{SystemValuer, Valuer};
use std::rc::Rc;

//#[derive(Debug, PartialEq, Eq)]
#[derive(Debug)]
pub struct SingleDealHolding<'h> {
    id: usize,
    deal: Deal<'h>,
    split_parent: Option<Rc<DealHoldingSummary<'h>>>,
}

impl<'h> SingleDealHolding<'h> {
    pub fn new(deal: Deal<'h>) -> Self {
        let id = DEAL_HOLDING_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        Self { deal, id, split_parent: None }
    }

    pub fn deal(&self) -> &Deal<'h> {
        &self.deal
    }

    pub fn into_deal(self) -> Deal<'h> {
        self.deal
    }

    pub fn adjusted_value(&self) -> AdjustedValue<'h> {
        self.deal.adjusted_value()
    }

    pub fn amount(&self) -> Amount<'h> {
        self.deal.amount()
    }

    pub fn value(&self) -> Amount<'h> {
        self.deal.value()
    }

    pub fn consideration(&self) -> Amount<'h> {
        self.deal.consideration()
    }

    pub fn expenses(&self) -> Amount<'h> {
        self.deal.expenses()
    }

    pub fn split_parent(&self) -> Option<&Rc<DealHoldingSummary<'h>>> {
        self.split_parent.as_ref()
    }

    pub fn id(&self) -> usize {
        self.id
    }

    /// Ensure that the deal can be valued in the specified `unit`.
    /// If not, attempt to perform a valuation on the underlying entry which will first try to derive the valuation,
    /// and fallback to using the price lookup functionality.
    fn value_with_system_valuer(&mut self, unit: &'h Unit<'h>) -> JournResult<()> {
        let mut valuer = SystemValuer::from(self.deal.entry());
        let cgt_config =
            self.deal.entry().config().module_config::<CagConfiguration>(MODULE_NAME).unwrap();
        let round_vals = cgt_config.round_deal_values();

        self.deal
            .value_with(&mut valuer, unit, round_vals)
            .map_err(|e| err!("Unable to value deal group: {}", self.deal.amount()).with_source(e))
    }

    fn value_with_date_valuer(
        &mut self,
        unit: &'h Unit<'h>,
        config: &Configuration<'h>,
        value_date: DateTime<Tz>,
    ) -> JournResult<()> {
        let cgt_config =
            self.deal().entry().config().module_config::<CagConfiguration>(MODULE_NAME).unwrap();
        let round_vals = cgt_config.round_deal_values();

        let date_valuer = |config: Configuration<'h>, value_date: DateTime<Tz>| {
            move |quote_unit: &'h Unit<'h>, base_amount: Amount<'h>| match SystemValuer::on_date(
                config.clone(),
                JDateTime::new(value_date, DateTimePrecision::Second),
            )
            .value(quote_unit, base_amount)
            {
                Ok(val) => Ok(val),
                Err(e) => Err(e),
            }
        };
        self.deal
            .value_with(&mut date_valuer(config.clone(), value_date), unit, round_vals)
            .map_err(|e| {
                err!("Unable to value deal group: {}", self.deal.amount()).with_source(e)
            })?;
        Ok(())
    }

    pub fn set_value_on_date(
        &mut self,
        uoa: &'h Unit<'h>,
        config: &Configuration<'h>,
        value_date: DateTime<Tz>,
    ) -> Result<(), JournError> {
        self.value_with_date_valuer(uoa, config, value_date)
    }

    pub fn ensure_valued(&mut self, uoa: &'h Unit<'h>) -> Result<(), JournError> {
        self.value_with_system_valuer(uoa)
    }

    pub fn split_max(mut self, amount: Quantity) -> Result<(Self, Option<Self>), Self> {
        if amount.is_sign_positive() != self.amount().quantity().is_sign_positive() {
            return Err(self);
        }
        if self.amount().is_zero() {
            return Err(self);
        }
        // Whole split, return self and None for the right side.
        if amount == self.amount().quantity() {
            return Ok((self, None));
        }

        let split_amount =
            self.amount().unit().with_quantity(amount.min_abs(self.amount().quantity()));
        let dh = DealHolding::Single(self);
        let split_parent = Rc::new(DealHoldingSummary::from(&dh));
        self = dh.into_single().unwrap();

        let (left_deal, right_deal) = self.deal.split(split_amount.quantity());
        let left = Self {
            id: DEAL_HOLDING_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
            deal: left_deal,
            split_parent: Some(Rc::clone(&split_parent)),
        };
        let right = right_deal.map(|deal| Self {
            // Old id is retained so it can still match an extraction event.
            id: self.id,
            deal,
            split_parent: Some(Rc::clone(&split_parent)),
        });
        Ok((left, right))
    }

    /// The adjustment is applied to both the valued amount and any expenses.
    pub fn add_adjustment(&mut self, adj: Adjustment<'h>) -> JournResult<()> {
        self.deal.add_adjustment(adj)
    }
}
