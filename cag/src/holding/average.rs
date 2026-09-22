/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjusted_value::AdjustedValue;
use crate::adjustment::{Adjustment, AmountAdjustment};
use crate::cag_configuration::CagConfiguration;
use crate::deal::Deal;
use crate::holding::DealHolding::Average;
use crate::holding::deal_holding::DEAL_HOLDING_ID_COUNTER;
use crate::holding::{DealHolding, DealHoldingSummary, SequenceDealHolding, Split};
use crate::module_init::MODULE_NAME;
use chrono::DateTime;
use chrono_tz::Tz;
use journ_core::amount::{Amount, Quantity};
use journ_core::configuration::Configuration;
use journ_core::datetime::JDateTimeRange;
use journ_core::error::{JournError, JournResult};
use journ_core::ext::NumExt;
use journ_core::unit::Unit;
use journ_core::valuer::{SystemValuer, ValuationError, Valuer};
use std::fmt;
use std::fmt::Debug;
use std::rc::Rc;

//#[derive(PartialEq, Eq)]
pub struct AverageDealHolding<'h> {
    pub(super) id: usize,
    deals: Vec<Deal<'h>>,
    adjusted_value: AdjustedValue<'h>,
    split_parent: Option<Rc<DealHoldingSummary<'h>>>,
}

impl<'h> AverageDealHolding<'h> {
    pub fn new(deal: Deal<'h>) -> Self {
        let id = DEAL_HOLDING_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let mut holding = Self {
            id,
            adjusted_value: AdjustedValue::zero(deal.unit(), deal.unit_of_account()),
            deals: vec![deal],
            split_parent: None,
        };
        holding.adjusted_value = holding.calculate_adj_value();
        holding
    }

    pub fn id(&self) -> usize {
        self.id
    }

    pub fn datetime(&self) -> JDateTimeRange {
        JDateTimeRange::new(
            self.deals.first().unwrap().datetime().start(),
            Some(self.deals.last().unwrap().datetime().end()),
        )
    }

    pub fn into_deal_iter(self) -> impl Iterator<Item = Deal<'h>> {
        self.deals.into_iter()
    }

    pub fn deal_iter(&self) -> impl Iterator<Item = &Deal<'h>> {
        self.deals.iter()
    }

    pub fn calculate_adj_value(&self) -> AdjustedValue<'h> {
        self.deal_iter().map(|deal| deal.adjusted_value()).sum::<AdjustedValue<'h>>()
    }

    pub fn set_value_on_date(
        &mut self,
        uoa: &'h Unit<'h>,
        config: &Configuration<'h>,
        date: DateTime<Tz>,
    ) -> Result<(), JournError> {
        for deal in self.deals.iter_mut() {
            deal.set_value_on_date(uoa, config, date)?;
        }
        self.adjusted_value = self.calculate_adj_value();
        Ok(())
    }

    pub fn ensure_valued(&mut self, uoa: &'h Unit<'h>) -> Result<(), JournError> {
        for deal in self.deals.iter_mut() {
            deal.ensure_valued(uoa)?;
        }
        self.adjusted_value = self.calculate_adj_value();
        Ok(())
    }

    pub fn adjusted_value(&self) -> AdjustedValue<'h> {
        self.adjusted_value
    }

    pub fn amount(&self) -> Amount<'h> {
        self.adjusted_value.amount()
    }

    pub fn value(&self) -> Amount<'h> {
        self.adjusted_value.value()
    }

    pub fn consideration(&self) -> Amount<'h> {
        self.adjusted_value.consideration()
    }

    pub fn expenses(&self) -> Amount<'h> {
        self.adjusted_value.expenses()
    }

    pub fn taxable_gain(&self) -> Option<Amount<'h>> {
        self.deals.iter().fold(None, |acc, d| match (acc, d.taxable_gain()) {
            (Some(a), Some(b)) => Some(a + b),
            (Some(a), None) | (None, Some(a)) => Some(a),
            (None, None) => None,
        })
    }

    pub fn split_parent(&self) -> Option<&Rc<DealHoldingSummary<'h>>> {
        self.split_parent.as_ref()
    }

    pub fn value_with<V: Valuer<'h>>(
        &mut self,
        valuer: &mut V,
        quote_unit: &'h Unit<'h>,
        round_vals: bool,
    ) -> JournResult<()> {
        for deal in self.deals.iter_mut() {
            deal.value_with(valuer, quote_unit, round_vals)?;
        }
        self.adjusted_value = self.calculate_adj_value();
        Ok(())
    }

    pub fn value_in_or_value_with<V: Valuer<'h>>(
        &mut self,
        in_unit: &'h Unit<'h>,
        valuer: &mut V,
        round_vals: bool,
    ) -> JournResult<()> {
        for deal in self.deals.iter_mut() {
            deal.value_with(valuer, in_unit, round_vals)?;
        }
        self.adjusted_value = self.calculate_adj_value();
        Ok(())
    }

    /// Values all deal's in the holding using the system valuer, created from each respective deal's entry.
    pub fn value_with_system_valuer(&mut self, unit: &'h Unit<'h>) -> Result<(), ValuationError> {
        for deal in self.deals.iter_mut() {
            let mut valuer = SystemValuer::from(deal.entry());
            let cgt_config =
                deal.entry().config().module_config::<CagConfiguration>(MODULE_NAME).unwrap();
            let round_vals = cgt_config.round_deal_values();

            deal.value_with(&mut valuer, unit, round_vals)?;
        }
        Ok(())
    }

    /// Splits the holding at the `amount` threshold, returning the left side and an optional remainder.
    ///
    /// All split amounts are rounded to the maximum scale allowed by their respective units.
    ///
    /// # Arguments
    /// - `amount`: The amount to split the holding at. If the amount is greater than the holding's amount, it will
    ///   be automatically reduced to be the holding's amount. If the amount is zero or has a different sign than the holding's amount, the split will fail.
    ///
    /// # Returns
    /// - `Ok((left, Some(right)))` if the holding was split into two
    /// - `Err(self)` if the holding could not be split (e.g. amount is zero or has a different sign)
    pub fn split_max(mut self, amount: Quantity) -> Result<(Self, Option<Self>), Self> {
        if amount.is_sign_positive() != self.adjusted_value.amount().quantity().is_sign_positive() {
            return Err(self);
        }
        if self.adjusted_value.amount().is_zero() {
            return Err(self);
        }

        let split_amount = self
            .adjusted_value
            .amount()
            .unit()
            .with_quantity(amount.min_abs(self.adjusted_value.amount().quantity()));

        let self_holding = DealHolding::Average(self);
        let summary = Rc::new(DealHoldingSummary::from(&self_holding));
        self = self_holding.into_average().unwrap();

        // The expected value of the split. We round according to our method's contract.
        let (adj_value_left, adj_value_right) = self.adjusted_value.split(split_amount.quantity());
        let split_ratio = split_amount.quantity() / self.adjusted_value.amount().quantity();
        let tg_togo = self
            .deals
            .iter()
            .fold(None, |acc, d| match (acc, d.taxable_gain()) {
                (Some(a), Some(b)) => Some(a + b),
                (Some(a), None) | (None, Some(a)) => Some(a),
                (None, None) => None,
            })
            .map(|tg| tg * split_ratio);

        let mut left_deals = Vec::with_capacity(self.deals.len());
        let mut right_deals = Vec::with_capacity(self.deals.len());

        let mut split = Split::new(
            [
                adj_value_left.amount(),
                adj_value_left.value(),
                adj_value_left.expenses(),
                tg_togo.unwrap_or(Amount::nil()),
            ],
            [
                self.adjusted_value.amount(),
                self.adjusted_value.value(),
                self.adjusted_value.expenses(),
                self.taxable_gain().unwrap_or(Amount::nil()),
            ],
        );
        split.set_round(&[true, true, true, true]);
        // Continuously adjusted amounts left to split.
        //let mut adj_value_rem = self.adjusted_value;
        //let mut adj_value_togo = adj_value_left;
        //let mut split_ratio =
        //    adj_value_togo.amount().quantity() / adj_value_rem.amount().quantity();

        for deal in self.deals.into_iter() {
            let (left, right) = deal.split_with_split(&mut split);

            //split_ratio = adj_value_togo.amount().quantity() / adj_value_rem.amount().quantity();

            //let (left, right) = if i == last_i {
            // The last deal gets the remainder of the split amount and adjusted value
            //    deal.split_all(adj_value_togo, tg_togo)
            //} else {
            // This may cause the left side to go slightly negative in expenses/value components due to rounding.
            //adj_value_rem -= deal.adjusted_value();
            //let (left, right) = deal.split_percent(split_ratio);
            //};

            //adj_value_togo -= left.adjusted_value();
            //tg_togo = tg_togo
            //    .map(|tg| if let Some(tg_left) = left.taxable_gain() { tg - tg_left } else { tg });

            left_deals.push(left);
            if let Some(right) = right {
                right_deals.push(right);
            }
        }
        let left = AverageDealHolding {
            id: DEAL_HOLDING_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
            adjusted_value: adj_value_left,
            deals: left_deals,
            split_parent: Some(Rc::clone(&summary)),
        };

        let right: Option<AverageDealHolding> = if right_deals.is_empty() {
            None
        } else {
            Some(AverageDealHolding {
                // Old id is retained so it can still match an extraction event.
                id: self.id,
                adjusted_value: adj_value_right.unwrap(),
                deals: right_deals,
                split_parent: Some(summary),
            })
        };

        // Sanity check that the split was accurate
        //debug_assert_eq!(adj_value_togo.amount().quantity(), Quantity::zero());
        //debug_assert_eq!(tg_togo.unwrap_or(Amount::nil()).quantity(), Quantity::zero());
        debug_assert_eq!(left.calculate_adj_value(), left.adjusted_value());
        debug_assert_eq!(
            right.as_ref().map(|r| r.calculate_adj_value()),
            right.as_ref().map(|r| r.adjusted_value())
        );
        Ok((left, right))
    }

    pub fn add_deal(&mut self, deal: Deal<'h>) {
        self.deals.push(deal);
        self.adjusted_value = self.calculate_adj_value();
    }

    /// The adjustment is applied to both the valued amount and any expenses.
    pub fn add_adjustment(&mut self, adj: Adjustment<'h>) -> JournResult<()> {
        let self_total = self.adjusted_value;

        // Test the adjustment on the whole holding first. Any error returned to the user would make more sense at this stage.
        adj.apply(&mut self.adjusted_value.clone())?;

        // Keep track of a remainder after applying each mini adjustment. We'll apply this to the last element.
        let mut rem_adj = adj.clone();
        let mut rem_amount_adjustments = rem_adj.amount_adjustments().to_vec();
        let deals_len = self.deals.len();
        for (i, deal) in self.deals.iter_mut().enumerate() {
            if i == deals_len - 1 {
                rem_adj.set_amount_adjustments(rem_amount_adjustments);
                deal.add_adjustment(rem_adj);
                break;
            } else {
                // Create the mini adjustment for this holding
                let mut mini_adj_amount_adjustments = vec![];
                for amount_adj in adj.amount_adjustments().iter().cloned() {
                    if matches!(amount_adj, AmountAdjustment::Scale(_)) {
                        mini_adj_amount_adjustments.push(amount_adj.clone());
                        continue;
                    }

                    // Calculate amount adjustments only for those in common
                    let (deal_amount, amount_total) =
                        if amount_adj.amount().unit() == self_total.amount().unit() {
                            (deal.amount(), self_total.amount())
                        } else if amount_adj.amount().unit() == self_total.value().unit() {
                            (deal.value(), self_total.value())
                        } else {
                            continue;
                        };

                    let (adj_amount, _) = amount_adj.amount().split_percent(
                        deal_amount.quantity() / amount_total.quantity(),
                        Some(amount_adj.amount().scale().max(amount_adj.amount().max_scale())),
                    );
                    let (left, _right) = amount_adj.split(adj_amount.quantity());
                    mini_adj_amount_adjustments.push(left.clone());
                    rem_amount_adjustments.push(left.inverse());
                }
                let mut mini_adj = adj.clone();
                mini_adj.set_amount_adjustments(mini_adj_amount_adjustments);
                // Apply the mini adjustment to the holding
                deal.add_adjustment(mini_adj)?;
            }
        }

        self.adjusted_value = self.calculate_adj_value();
        Ok(())
    }
}

impl Debug for AverageDealHolding<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Avg({:?})", self.adjusted_value)
    }
}

impl<'h> From<SequenceDealHolding<'h>> for AverageDealHolding<'h> {
    fn from(seq: SequenceDealHolding<'h>) -> Self {
        let balance = seq.adjusted_value();
        let split_parent = seq.split_parent().map(Rc::clone);

        Self {
            id: seq.id(),
            adjusted_value: balance,
            deals: seq.into_deal_iter().collect(),
            split_parent,
        }
    }
}

impl<'h> FromIterator<Deal<'h>> for AverageDealHolding<'h> {
    fn from_iter<I: IntoIterator<Item = Deal<'h>>>(iter: I) -> Self {
        let mut iter = iter.into_iter();
        let first = iter.next().unwrap();
        let mut holding = AverageDealHolding::new(first);
        for deal in iter {
            holding.deals.push(deal);
        }
        holding.adjusted_value = holding.calculate_adj_value();
        holding
    }
}

impl<'h> From<AverageDealHolding<'h>> for DealHolding<'h> {
    fn from(avg: AverageDealHolding<'h>) -> Self {
        Average(avg)
    }
}
