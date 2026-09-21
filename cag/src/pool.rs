/*
 * Copyright (c) 2023-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjusted_value::AdjustedValue;
use crate::adjustment::Adjustment;
use crate::cag_configuration::MatchMethod;
use crate::deal::Deal;
use crate::holding::{DealHolding, DealHoldingSummary, SingleDealHolding};
use crate::pool_event::{MatchDetails, PoolEvent, PoolEventKind};
use chrono::DateTime;
use chrono_tz::Tz;
use journ_core::alloc::HerdAllocator;
use journ_core::configuration::Configuration;
use journ_core::datetime::JDateTimeRange;
use journ_core::err;
use journ_core::error::JournResult;
use journ_core::unit::Unit;
use log::{debug, info, warn};
use rust_decimal_macros::dec;
use std::collections::BTreeMap;
use std::ops::Neg;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct HoldingKey<'h> {
    pub(super) unit: &'h Unit<'h>,
    pub(super) positive: bool,
}
impl<'h> HoldingKey<'h> {
    fn new(unit: &'h Unit<'h>, positive: bool) -> Self {
        HoldingKey { unit, positive }
    }
}

impl<'h> From<&DealHolding<'h>> for HoldingKey<'h> {
    fn from(holding: &DealHolding<'h>) -> Self {
        HoldingKey::new(holding.unit(), holding.amount().is_positive())
    }
}

impl Neg for HoldingKey<'_> {
    type Output = Self;

    fn neg(self) -> Self::Output {
        HoldingKey::new(self.unit, !self.positive)
    }
}

/// A pool is the top-level structure responsible for holding and matching `Deals`.
/// It only makes sense that a pool deals with a single unit (and its aliases) as quantities
/// should be matched together based on their unit.
///
/// Deals are typically pushed and matched in date order, and though this is not a requirement for
/// this structure, the pool will always reorder its events to be in date order.
///
/// The `unit_of_account` is the initial unit set but may be changed by pushing or matching
/// a deal in a different consideration unit. This will internally convert all `DealHoldings`
/// within to be based in a different consideration unit.
///
/// Multiple pools in a list form a cascade whereby events are first pushed to the pool at the top
/// and are then automatically aged out if `max_age` has been set. `Deals` and `Matches` aged out this
/// way will continue to fall on to the next successive pool until they reach a pool with `max_age` set
/// to `None`. This is useful in certain jurisdictions where deals matched closer in time may be treated
/// differently.
pub struct Pool<'h> {
    /// Unique and fixed identifier for the pool. Starts at 1 and increments.
    id: usize,
    /// The name of the pool can be changed by new configuration.
    name: &'h str,
    /// The unit gains and losses are measured in.
    unit_of_account: &'h Unit<'h>,
    /// A holding for each unit.
    holdings: BTreeMap<HoldingKey<'h>, DealHolding<'h>>,
    /// The match methods to use when the holding's balance is >= 0 and < 0 respectively.
    methods: (MatchMethod, MatchMethod),
    allocator: &'h HerdAllocator<'h>,
}

impl<'h> Pool<'h> {
    pub fn new(
        id: usize,
        name: &'h str,
        unit_of_account: &'h Unit<'h>,
        allocator: &'h HerdAllocator<'h>,
    ) -> Pool<'h> {
        Pool {
            id,
            name,
            unit_of_account,
            holdings: Default::default(),
            methods: Default::default(),
            allocator,
        }
    }

    pub fn id(&self) -> usize {
        self.id
    }

    pub(super) fn holdings(&self) -> &BTreeMap<HoldingKey<'h>, DealHolding<'h>> {
        &self.holdings
    }

    pub fn name(&self) -> &'h str {
        self.name
    }

    pub fn set_name(&mut self, name: &'h str) {
        self.name = name;
    }

    /// The pool's unit of account. The only time when this will be None is when
    /// the pool does not have any deals
    pub fn unit_of_account(&self) -> &'h Unit<'h> {
        self.unit_of_account
    }

    /// Gets whether the pool is completely empty for the given `unit`;
    /// there are no deals in its holding.
    pub fn key(&self, unit: &'h Unit<'h>, check_pos_first: bool) -> Option<HoldingKey<'h>> {
        if self.holdings.contains_key(&HoldingKey::new(unit, check_pos_first)) {
            Some(HoldingKey::new(unit, true))
        } else if self.holdings.contains_key(&HoldingKey::new(unit, !check_pos_first)) {
            Some(HoldingKey::new(unit, false))
        } else {
            None
        }
    }

    /// Gets the match methods for the pool. The first is used for matching against positive holdings,
    /// the second is used for matching against negative holdings.
    pub fn methods(&self) -> (MatchMethod, MatchMethod) {
        self.methods
    }

    pub fn set_methods(&mut self, methods: (MatchMethod, MatchMethod)) {
        self.methods = methods
    }

    /// Set a new unit of account to use henceforth.
    ///
    /// This will cause the pool's holding to check that it can be valued in the new unit of account and
    /// will return an `Err` if that is unsuccessful.
    pub fn change_unit_of_account(
        &mut self,
        uoa: &'h Unit<'h>,
        config_and_value_date: Option<(&Configuration<'h>, DateTime<Tz>)>,
    ) -> JournResult<()> {
        if uoa == self.unit_of_account {
            return Ok(());
        }
        for (_unit, holding) in &mut self.holdings.iter_mut() {
            let err = |e| err!("Unable to alter pool unit of account: {}", e);
            match config_and_value_date {
                Some((cfg, date)) => holding.set_value_on_date(uoa, cfg, date),
                None => holding.ensure_valued(uoa),
            }
            .map_err(err)?;
        }
        self.unit_of_account = uoa;
        Ok(())
    }

    /// Gets the holding extracted by its id along with the balance of the pool before and after the extraction. If the holding is not found, returns None.
    pub fn extract(
        &mut self,
        holding_id: usize,
        deal_unit: &'h Unit<'h>,
    ) -> Option<(AdjustedValue<'h>, DealHolding<'h>, AdjustedValue<'h>)> {
        // Need to check positive and negative sides as the holding may have flipped during adjustment.
        let holding_key = HoldingKey::new(deal_unit, true);
        for key in [holding_key, -holding_key] {
            match self.holdings.remove(&key) {
                Some(holding) => {
                    let balance = holding.adjusted_value();

                    match holding.extract(holding_id) {
                        Ok((extracted, remainder)) => {
                            if let Some(remainder) = remainder {
                                self.holdings.insert(key, remainder);
                            } else {
                                self.holdings.remove(&key);
                            }
                            return Some((
                                balance,
                                extracted,
                                self.holdings.get(&key).map(|h| h.adjusted_value()).unwrap_or_else(
                                    || AdjustedValue::zero(deal_unit, self.unit_of_account),
                                ),
                            ));
                        }
                        Err(holding) => {
                            self.holdings.insert(key, holding);
                            continue;
                        }
                    }
                }
                None => continue,
            }
        }
        None
    }

    pub fn push(
        &mut self,
        mut holding: DealHolding<'h>,
        event_datetime: JDateTimeRange,
        from_pool: Option<&'h str>,
    ) -> JournResult<(PoolEvent<'h>, usize)> {
        // Ensure that incoming deals can be valued in the pool's unit of account
        holding.ensure_valued(self.unit_of_account)?;

        let holding_key: HoldingKey = (&holding).into();
        let bal_before = self
            .holdings
            .get(&holding_key)
            .map(|h| h.adjusted_value())
            .unwrap_or_else(|| AdjustedValue::zero(holding.unit(), self.unit_of_account));
        let holding_snapshot = DealHoldingSummary::from(&holding);
        let extract_id = self.push_internal(holding);
        let bal_after = self.holdings.get(&holding_key).unwrap().adjusted_value();

        let event = PoolEvent::new(
            self.name,
            event_datetime,
            match from_pool {
                Some(pool) => PoolEventKind::MovedTo(holding_snapshot, pool),
                None => PoolEventKind::PooledDeal(holding_snapshot),
            },
            bal_before,
            bal_after,
        );
        debug!("{}", event);
        Ok((event, extract_id))
    }

    /// Tries to match the `originator` against the pool, returning the match event and any unmatched remainder.
    /// If the `originator` cannot be matched, it is returned unchanged.
    pub fn try_match(
        &mut self,
        mut originator: DealHolding<'h>,
        event_datetime: JDateTimeRange,
    ) -> JournResult<Result<(Vec<PoolEvent<'h>>, Option<DealHolding<'h>>), DealHolding<'h>>> {
        // Ensure the unit of account is set so that adj_deal.amount() is successful.
        originator.ensure_valued(self.unit_of_account)?;

        let mut events = vec![];
        let mut originator_remaining = Some(originator);
        loop {
            let res = self.try_match_single(originator_remaining.take().unwrap(), event_datetime);
            match res {
                Ok((Some(event), remainder)) => {
                    events.push(event);
                    match remainder {
                        Some(remainder) => originator_remaining = Some(remainder),
                        None => break,
                    }
                }
                Err(originator) => {
                    originator_remaining = Some(originator);
                    break;
                }
                _ => break,
            }
        }
        match (originator_remaining, events.is_empty()) {
            (Some(originator), true) => Ok(Err(originator)),
            (Some(originator), false) => Ok(Ok((events, Some(originator)))),
            _ => Ok(Ok((events, None))),
        }
    }

    /// Matches the `originator` against the existing holding, returning a single event and change.
    fn try_match_single(
        &mut self,
        originator: DealHolding<'h>,
        event_datetime: JDateTimeRange,
    ) -> Result<(Option<PoolEvent<'h>>, Option<DealHolding<'h>>), DealHolding<'h>> {
        let unit = originator.unit();
        let target_key = HoldingKey::new(unit, !originator.amount().is_positive());
        match self.holdings.remove(&target_key) {
            Some(target) => {
                let bal_before = target.adjusted_value();
                let match_method =
                    if bal_before.amount().is_negative() { self.methods.1 } else { self.methods.0 };
                match target.split_max(originator.amount().quantity() * dec!(-1), match_method) {
                    Ok((target_matched, target_remaining)) => {
                        let (originator_matched, originator_part_remaining) = originator
                            .split_max(target_matched.amount().quantity() * dec!(-1), match_method)
                            .unwrap();
                        let event = PoolEvent::new(
                            self.name,
                            event_datetime,
                            PoolEventKind::Match(MatchDetails::new(
                                match_method,
                                target_matched,
                                originator_matched,
                            )),
                            bal_before,
                            target_remaining
                                .as_ref()
                                .map(|dh| dh.adjusted_value())
                                .unwrap_or_else(|| AdjustedValue::zero(unit, self.unit_of_account)),
                        );
                        info!("{}", event);
                        if let Some(tr) = target_remaining {
                            self.holdings.insert(target_key, tr);
                        }
                        Ok((Some(event), originator_part_remaining))
                    }
                    Err(target) => {
                        self.holdings.insert(target_key, *target);
                        Err(originator)
                    }
                }
            }
            None => Err(originator),
        }
    }

    fn push_internal(&mut self, holding: DealHolding<'h>) -> usize {
        let holding_key = HoldingKey::new(holding.unit(), holding.amount().is_positive());
        let holding_id = holding.id();
        let method = if holding_key.positive { self.methods.0 } else { self.methods.1 };

        let new_holding = match self.holdings.remove(&holding_key) {
            Some(existing) => existing.push(holding, method),
            None => holding,
        };

        let id = if method == MatchMethod::Average { new_holding.id() } else { holding_id };
        self.holdings.insert(holding_key, new_holding);
        id
    }

    /// Pushes an adjustment on to the pool. This will only fail if the adjustment is a scalar
    /// adjustment, and it is being pushed on to an empty holding.
    pub(crate) fn push_adjustment(
        &mut self,
        mut adj: Adjustment<'h>,
    ) -> JournResult<PoolEvent<'h>> {
        let unit = adj.unit();

        // Search for the positive or negative holding depending on the adjustment being made. This is to ensure that later,
        // if the adjustment causes the holding to flip from positive to negative or vice versa, we can move it to the other side of the map
        // without overwriting the other side's holding.
        let ideal_holding_key =
            HoldingKey::new(unit, adj.amount_adjustments()[0].amount().is_positive());
        // Actually adjust the holding that exists.
        let actual_holding_key = if self.holdings.contains_key(&ideal_holding_key) {
            ideal_holding_key
        } else {
            HoldingKey::new(unit, !ideal_holding_key.positive)
        };

        let bal_before = self
            .holdings
            .get(&actual_holding_key)
            .map(|h| h.adjusted_value())
            .unwrap_or_else(|| AdjustedValue::zero(unit, self.unit_of_account));

        adj.convert_set_to_add(&bal_before);

        let adj_holding_key = match self.holdings.get_mut(&actual_holding_key) {
            Some(holding) => {
                // Check that the adjustment is after, in time to the sequence
                if adj.datetime().start() < holding.datetime().end() {
                    // This is a warning and occurs when there is a time overlap in the entries
                    warn!(
                        "Datetime of adjustment may be before that of the latest deal: {} < {}",
                        adj.datetime().start().datetime(),
                        holding.datetime().end().datetime(),
                    );
                }

                holding.add_adjustment(adj.clone())?;
                if holding.amount().is_positive() != bal_before.amount().is_positive() {
                    // The adjustment has caused the holding to flip from positive to negative or vice versa.
                    // We need to move the holding to the other side of the map.
                    let new_holding_key = -actual_holding_key;
                    let holding = self.holdings.remove(&actual_holding_key).unwrap();
                    let replaced_holding = self.holdings.insert(new_holding_key, holding);
                    debug_assert!(replaced_holding.is_none(), "holding overwritten");
                    new_holding_key
                } else {
                    actual_holding_key
                }
            }
            None => {
                // If we are pushing an adjustment on to an empty holding, this only makes
                // sense if the adjustment is additive. Scalar adjustments are essentially a noop,
                // and so we can ignore them.
                if adj.is_scalar() {
                    return Err(err!("Cannot push scalar adjustment on to empty holding"))?;
                }
                self.holdings.insert(
                    ideal_holding_key,
                    DealHolding::Single(SingleDealHolding::new(Deal::zero(
                        unit,
                        adj.entry(),
                        self.unit_of_account,
                    ))),
                );
                self.holdings.get_mut(&ideal_holding_key).unwrap().add_adjustment(adj.clone())?;
                ideal_holding_key
            }
        };

        let bal_after = self
            .holdings
            .get(&adj_holding_key)
            .map(|dh| dh.adjusted_value())
            .unwrap_or_else(|| AdjustedValue::zero(unit, self.unit_of_account));
        debug!(
            "{:?} Adjusted {:?} on pool {}, bal now = {:?}",
            adj.datetime(),
            adj,
            self.name,
            bal_after,
        );
        Ok(PoolEvent::new(
            self.name,
            adj.datetime(),
            PoolEventKind::Adjustment(adj),
            bal_before,
            bal_after,
        ))
    }

    /*
    pub fn balance(&self, unit: &'h Unit<'h>) -> AdjustedValue<'h> {
        match self.holdings.get(unit) {
            Some(holding) => holding.adjusted_value(),
            None => AdjustedValue::zero(unit, self.unit_of_account),
        }
    }*/
}

#[cfg(test)]
mod tests {
    use crate::deal;
    use crate::dealing_event::DealingEvent;
    use crate::pool::Pool;
    use journ_core::journal::Journal;
    use journ_core::*;

    fn journ<'h>() -> Journal<'h> {
        journ!(
            "cgt\n \
        tax-unit £"
        )
    }

    #[test]
    fn test_push() {
        let journ = journ();
        let acq1 = deal!(&*journ.root().config(), "2000-01-01", "$10 @@ £8");
        let mut pool = Pool::new(1, "pool1", unit!("$"));
        pool.match_and_push(DealingEvent::Deal(acq1)).unwrap();
        assert_eq!(pool.balance(unit!("$")).unwrap().amount.to_string(), "$10")
    }

    /*
    #[test]
    fn test_adjusted_pool_split() {
        let journ = journ();

        // Cost per share doubled from £1 to £2
        let mut adj_holding = AdjustedDealHolding::new(
            DealHolding::Single(deal!(&*journ.root().config(), "2000-01-01", "100 ACME @@ £100")),
            adjustment!(&*journ.root().config(), "2000-01-02", "-50 ACME"),
        );
        let (m1, _) = adj_holding
            .split_max(
                &deal!(&*journ.root().config(), "2000-01-03", "-10 ACME @@ £20"),
                dec!(10),
                MatchMethod::Fifo,
                None,
            )
            .unwrap();
        let pounds = journ.config().get_unit("£").unwrap();
        assert_eq!(m1.unwrap().balance(pounds).map(|r| r.amount), Ok(amount!("20 ACME")));
    }*/
}
