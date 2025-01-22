/*
 * Copyright (c) 2023-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjustment::Adjustment;
use crate::cgt_configuration::MatchMethod;
use crate::deal::{Deal, DealId};
use crate::deal_group::{DealGroup, DealGroupCriteria};
use crate::deal_holding::DealHolding;
use crate::pool_event::{MatchDetails, PoolEvent, PoolEventKind};
use chrono::DateTime;
use chrono_tz::Tz;
use journ_core::alloc::HerdAllocator;
use journ_core::amount::{Amount, AmountExpr};
use journ_core::configuration::Configuration;
use journ_core::date_and_time::JDateTimeRange;
use journ_core::err;
use journ_core::error::JournResult;
use journ_core::reporting::table;
use journ_core::unit::Unit;
use journ_core::valued_amount::{Valuation, ValuedAmount};
use log::{debug, info, warn};
use rust_decimal::Decimal;
use rust_decimal_macros::dec;
use std::collections::BTreeMap;
use std::fmt;
use std::ops::{Add, AddAssign, Mul, Sub};
use yaml_rust::Yaml;

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
    holdings: BTreeMap<&'h Unit<'h>, DealHolding<'h>>,
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

    pub fn holdings(&self) -> &BTreeMap<&'h Unit<'h>, DealHolding<'h>> {
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

    /// Gets the current `MatchMethod` according to whether the pool's balance is negative or positive.
    pub fn current_method(&self, unit: &'h Unit<'h>) -> MatchMethod {
        self.holdings.get(unit).map_or(self.methods.0, |h| {
            if h.total().amount().is_negative() {
                self.methods.1
            } else {
                self.methods.0
            }
        })
    }

    /// Gets whether the pool is completely empty for the given `unit`;
    /// there are no deals in its holding.
    pub fn is_empty(&self, unit: &'h Unit<'h>) -> bool {
        !self.holdings.contains_key(unit)
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
            if let Err(e) = holding.set_unit_of_account(uoa, config_and_value_date) {
                return Err(err!("Unable to alter pool unit of account: {}", e));
            }
        }
        self.unit_of_account = uoa;
        Ok(())
    }

    pub fn extract(
        &mut self,
        group_id: DealId<'h>,
        deal_unit: &'h Unit<'h>,
    ) -> Option<DealGroup<'h>> {
        let holding = self.holdings.remove(deal_unit)?;

        match holding.extract(group_id) {
            Ok((extracted, remainder)) => {
                if let Some(remainder) = remainder {
                    self.holdings.insert(deal_unit, remainder);
                } else {
                    self.holdings.remove(deal_unit);
                }
                Some(extracted)
            }
            Err(holding) => {
                self.holdings.insert(deal_unit, holding);
                None
            }
        }
    }

    pub fn push(
        &mut self,
        mut group: DealGroup<'h>,
        event_datetime: JDateTimeRange<'h>,
    ) -> JournResult<PoolEvent<'h>> {
        // Ensure that incoming deals can be valued in the pool's unit of account
        group.set_unit_of_account(self.unit_of_account, None)?;

        let deal_unit = group.unit();
        let bal_before = self
            .holdings
            .get(deal_unit)
            .map(|h| h.total().clone())
            .unwrap_or_else(|| PoolBalance::zero(deal_unit, self.allocator));

        self.push_internal(group.clone());

        let event = PoolEvent::new(
            self.name,
            event_datetime,
            PoolEventKind::PooledDeal(DealHolding::Group(group)),
            bal_before,
            self.holdings
                .get(deal_unit)
                .map(|dh| dh.total().clone())
                .unwrap_or_else(|| PoolBalance::zero(deal_unit, self.allocator)),
        );
        debug!("{}", event);
        Ok(event)
    }

    /// Tries to match the `originator` against the pool, returning the match event and any unmatched remainder.
    /// If the deal cannot be matched, the `originator` is returned.
    pub fn try_match(
        &mut self,
        mut originator: DealGroup<'h>,
        event_datetime: JDateTimeRange<'h>,
    ) -> JournResult<Result<(Vec<PoolEvent<'h>>, Option<DealGroup<'h>>), DealGroup<'h>>> {
        // Ensure the unit of account is set so that adj_deal.amount() is successful.
        originator.set_unit_of_account(self.unit_of_account, None)?;

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

    /// Matches a single dealgroup.
    fn try_match_single(
        &mut self,
        originator: DealGroup<'h>,
        event_datetime: JDateTimeRange<'h>,
    ) -> Result<(Option<PoolEvent<'h>>, Option<DealGroup<'h>>), DealGroup<'h>> {
        let unit = originator.unit();
        match self.holdings.remove(unit) {
            Some(target) => {
                let bal_before = target.total().clone();
                let match_method =
                    if bal_before.amount().is_negative() { self.methods.1 } else { self.methods.0 };
                match target
                    .split_max(originator.total().amount().quantity() * dec!(-1), match_method)
                {
                    Ok((target_matched, target_remaining)) => {
                        let (originator_matched, originator_part_remaining) = originator
                            .split_max(target_matched.total().amount().quantity() * dec!(-1))
                            .unwrap();
                        let originator_matched = DealHolding::Group(originator_matched);

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
                                .map(|dh| dh.total().clone())
                                .unwrap_or_else(|| PoolBalance::zero(unit, self.allocator)),
                        );
                        info!("{}", event);
                        if let Some(tr) = target_remaining {
                            self.holdings.insert(unit, tr);
                        }
                        Ok((Some(event), originator_part_remaining))
                    }
                    Err(target) => {
                        self.holdings.insert(unit, target);
                        Err(originator)
                    }
                }
            }
            None => Err(originator),
        }
    }

    fn push_internal(&mut self, group: DealGroup<'h>) {
        let deal_unit = group.unit();

        let new_holding = match self.holdings.remove(deal_unit) {
            Some(mut holding) => {
                holding.push_group(group);
                holding
            }
            None => DealHolding::Group(group),
        };
        self.holdings.insert(deal_unit, new_holding);
    }

    /// Pushes an adjustment on to the pool. This will only fail if the adjustment is a scalar
    /// adjustment and it is being pushed on to an empty holding.
    pub(crate) fn push_adjustment(
        &mut self,
        mut adj: Adjustment<'h>,
    ) -> JournResult<PoolEvent<'h>> {
        let unit = adj.unit();
        let bal_before = self
            .holdings
            .get(unit)
            .map(|h| h.total().clone())
            .unwrap_or_else(|| PoolBalance::zero(unit, self.allocator));

        adj.convert_set_to_add(bal_before.valued_amount());

        match self.holdings.get_mut(unit) {
            Some(holding) => {
                // Check that the adjustment is after, in time to the sequence
                if adj.datetime().start() < holding.datetime().end() {
                    // This is a warning and occurs when there is a time overlap in the entries
                    warn!(
                        "Datetime of adjustment may be before that of the latest deal: {} < {}",
                        adj.datetime().start(),
                        holding.datetime().end(),
                    );
                }

                holding.add_adjustment(adj.clone())
            }
            None => {
                // If we are pushing an adjustment on to an empty holding, this only makes
                // sense if the adjustment is additive. Scalar adjustments are essentially a noop,
                // and so we can ignore them.
                if adj.is_scalar() {
                    return Err(err!("Cannot push scalar adjustment on to empty holding"))?;
                }
                self.holdings.insert(
                    unit,
                    DealHolding::Group(DealGroup::new(
                        DealGroupCriteria::Single,
                        Deal::zero(unit, adj.entry()),
                    )),
                );
                self.holdings.get_mut(unit).unwrap().add_adjustment(adj.clone())
            }
        }?;

        let bal_after = self
            .holdings
            .get(unit)
            .map(|dh| dh.total().clone())
            .unwrap_or_else(|| PoolBalance::zero(unit, self.allocator));
        debug!(
            "{} Adjusted {:?} on pool {}, bal now = {:?}",
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

    pub fn balance(&self, unit: &'h Unit<'h>) -> PoolBalance<'h> {
        match self.holdings.get(unit) {
            Some(holding) => holding.total().clone(),
            None => PoolBalance::zero(unit, self.allocator),
        }
    }
}

/// A snapshot of a pool in time, summarising the total amount held in the pool and its cost which
/// is not necessarily the same as its value as the cost can include deal expenses.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PoolBalance<'h>(ValuedAmount<'h>);

impl<'h> PoolBalance<'h> {
    pub fn new(va: ValuedAmount<'h>) -> Self {
        assert!(!va.is_nil());

        PoolBalance(va)
    }

    pub fn zero(unit: &'h Unit<'h>, herd_allocator: &'h HerdAllocator<'h>) -> Self {
        PoolBalance(ValuedAmount::new_in(AmountExpr::from(unit.with_quantity(0)), herd_allocator))
    }

    pub fn is_zero(&self) -> bool {
        self.0.is_zero()
    }

    /// The pool's amount.
    pub fn amount(&self) -> Amount<'h> {
        self.0.amount()
    }

    pub fn valued_amount(&self) -> &ValuedAmount<'h> {
        &self.0
    }

    pub fn into_valued_amount(self) -> ValuedAmount<'h> {
        self.0
    }

    /// Gets the first valuation; the primary valuation.
    pub fn first_valuation(&self) -> Option<Valuation<'h>> {
        self.0.valuations().next().cloned()
    }

    /// Gets valuations only, excluding the main amount
    pub fn valuations(&self) -> impl Iterator<Item = Amount<'h>> + '_ {
        self.0.totalled_valuations().into_iter()
    }
}

impl fmt::Display for PoolBalance<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Debug for PoolBalance<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl<'a> From<&'a PoolBalance<'_>> for table::Cell<'a> {
    fn from(value: &'a PoolBalance) -> Self {
        (&value.0).into()
    }
}

impl From<PoolBalance<'_>> for table::Cell<'_> {
    fn from(value: PoolBalance) -> Self {
        value.0.into()
    }
}

impl From<&PoolBalance<'_>> for Yaml {
    fn from(value: &PoolBalance<'_>) -> Self {
        Yaml::from(&value.0)
    }
}

impl<'h> Add<&PoolBalance<'h>> for &PoolBalance<'h> {
    type Output = PoolBalance<'h>;

    fn add(self, rhs: &PoolBalance<'h>) -> Self::Output {
        PoolBalance((&self.0 + &rhs.0).unwrap())
    }
}

impl<'h> Sub<&PoolBalance<'h>> for &PoolBalance<'h> {
    type Output = PoolBalance<'h>;

    fn sub(self, rhs: &PoolBalance<'h>) -> Self::Output {
        PoolBalance((&self.0 - &rhs.0).unwrap())
    }
}

impl AddAssign<&Self> for PoolBalance<'_> {
    fn add_assign(&mut self, rhs: &Self) {
        assert_eq!(rhs.amount().unit(), self.amount().unit());

        self.0 = (&self.0 + &rhs.0).unwrap();
    }
}

impl<'h> Mul<Decimal> for &PoolBalance<'h> {
    type Output = PoolBalance<'h>;

    fn mul(self, rhs: Decimal) -> Self::Output {
        PoolBalance(&self.0 * rhs)
    }
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
