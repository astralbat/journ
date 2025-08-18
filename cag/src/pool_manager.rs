/*
 * Copyright (c) 2023-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjustment::Adjustment;
use crate::cgt_configuration::{CgtConfiguration, MatchMethod};
use crate::deal::DealId;
use crate::deal_group::DealGroup;
use crate::deal_holding::DealHolding;
use crate::module_init::MODULE_NAME;
use crate::pool::Pool;
use crate::pool_event::{PoolEvent, PoolEventKind};
use crate::ruleset::{ActionRule, AgeUnit, Condition, Rule};
use chrono::{LocalResult, TimeZone};
use journ_core::alloc::HerdAllocator;
use journ_core::configuration::{Configuration, Filter};
use journ_core::date_and_time::{JDateTime, JDateTimeRange};
use journ_core::err;
use journ_core::error::{BlockContextError, JournError, JournResult};
use journ_core::journal_entry::JournalEntry;
use journ_core::parsing::text_block::TextBlock;
use journ_core::unit::Unit;
use log::debug;
use std::collections::HashMap;

pub struct PoolManager<'h, 'u> {
    /// The default unit of account to use for all pools when none is specified by the pool configuration.
    default_unit_of_account: &'h Unit<'h>,
    config: Option<Configuration<'h>>,
    unit_filter: Box<dyn Filter<'u, Unit<'h>> + 'h>,
    /// Pools are expected to be in the order they are declared in the configuration.
    pools: Vec<Pool<'h>, &'h HerdAllocator<'h>>,
    pool_schedulers: HashMap<usize, PoolScheduler<'h>>,
}

impl<'h, 'u> PoolManager<'h, 'u> {
    pub fn new(default_unit_of_account: &'h Unit<'h>, allocator: &'h HerdAllocator<'h>) -> Self {
        // The starting implied pool is named "Pool".
        PoolManager {
            config: None,
            default_unit_of_account,
            unit_filter: Box::new(|_: &Unit<'_>| true),
            pools: Vec::new_in(allocator),
            pool_schedulers: HashMap::new(),
        }
    }

    pub fn pools(&self) -> &[Pool<'h>] {
        &self.pools
    }

    pub fn set_unit_filter(&mut self, unit_filter: Box<dyn Filter<'u, Unit<'h>> + 'h>) {
        self.unit_filter = unit_filter;
    }

    /// Gets the pool with the specified `name` or creates a new one if it does not exist. The `name` must
    /// refer to an existing pool within the configuration.
    /// The `pool_uoa` is the unit of account for the new pool and must be specified if the pool does not exist.
    ///
    /// # Panics
    /// If the pool does not exist and `pool_uoa` is `None`.
    fn get_or_create_pool<'a>(
        config: &'a Configuration<'h>,
        pools: &'a mut Vec<Pool<'h>, &'h HerdAllocator<'h>>,
        name: &'h str,
        pool_uoa: &'h Unit<'h>,
    ) -> &'a mut Pool<'h> {
        let cgt_config = config.module_config::<CgtConfiguration>(MODULE_NAME).unwrap();
        let pool_config = cgt_config.find_pool(name).unwrap();
        let pos = pools.iter_mut().position(|p| p.id() == pool_config.id().unwrap());
        match pos {
            Some(pos) => &mut pools[pos],
            None => {
                let allocated_name = config.allocator().alloc(name.to_string());
                let pool = Pool::new(
                    pool_config.id().unwrap(),
                    allocated_name.as_str(),
                    pool_uoa,
                    config.allocator(),
                );
                pools.push(pool);
                pools.last_mut().unwrap()
            }
        }
    }

    /// Gets the unit of account for the would-be destination pool of the `deal`.
    /// If there are no pools configured, no default unit of account, then the deal's unit is returned.
    pub fn initial_unit_of_account(&self, deal_group: &DealGroup<'h>) -> &'h Unit<'h> {
        let uoa_from_pool_name = |pool_name| -> &'h Unit<'h> {
            self.pools
                .iter()
                .find(|p| p.name() == pool_name)
                .map(|p| p.unit_of_account())
                .unwrap_or(self.default_unit_of_account)
        };

        let mut rules = deal_group.next_rules();
        'rules: loop {
            for rule in rules {
                match rule {
                    Rule::Decision(decision) => {
                        if deal_group.matches(decision.condition(), deal_group.datetime().start()) {
                            rules = decision.rules();
                            continue 'rules;
                        }
                    }
                    Rule::Action(ActionRule::Pool(name, cond)) => {
                        if deal_group.matches(&cond, deal_group.datetime().start()) {
                            break 'rules uoa_from_pool_name(name);
                        }
                    }
                    Rule::Action(ActionRule::Match(name)) => {
                        break 'rules uoa_from_pool_name(name);
                    }
                }
            }
            break deal_group.unit();
        }
    }

    pub fn pool_scheduler_mut(&mut self, pool_id: usize) -> &mut PoolScheduler<'h> {
        self.pool_schedulers.entry(pool_id).or_insert(PoolScheduler::default())
    }

    pub fn progress_deals(
        &mut self,
        event_datetime: JDateTimeRange<'h>,
    ) -> JournResult<Vec<PoolEvent<'h>>> {
        let mut pool_events = vec![];

        // Loop tactic for satisfying the borrow checker.
        'outer: loop {
            for pool in self.pools.iter_mut() {
                for unit in pool.holdings().keys().copied() {
                    let scheduler = self.pool_schedulers.entry(pool.id()).or_default();
                    if let Some(pse) = scheduler.remove_next(
                        event_datetime.start(),
                        unit,
                        pool.current_method(unit),
                    ) {
                        let bal_before = pool.balance(pse.deal_unit);
                        if let Some(group) = pool.extract(pse.deal_id, pse.deal_unit) {
                            let pe = PoolEvent::new(
                                pool.name(),
                                JDateTimeRange::new(pse.datetime, None),
                                PoolEventKind::UnpooledDeal(DealHolding::Group(group.clone())),
                                bal_before,
                                pool.balance(pse.deal_unit),
                            );
                            debug!("{}", pe);
                            pool_events.push(pe);
                            self.apply_deal_rules(
                                group,
                                JDateTimeRange::new(pse.datetime, None),
                                &mut pool_events,
                            )?;
                        }
                        continue 'outer;
                    }
                }
            }
            break;
        }
        Ok(pool_events)
    }

    pub fn push_deal_group(&mut self, group: DealGroup<'h>) -> JournResult<Vec<PoolEvent<'h>>> {
        let datetime = group.datetime();
        let mut pool_events = vec![];
        pool_events.extend(self.progress_deals(datetime)?);
        self.apply_deal_rules(group, datetime, &mut pool_events)?;
        Ok(pool_events)
    }

    pub fn push_adjustments<I: IntoIterator<Item = Adjustment<'h>>>(
        &mut self,
        adjustments: I,
    ) -> JournResult<Vec<PoolEvent<'h>, &'h HerdAllocator<'h>>> {
        let entry_err = |e: JournError, entry: &'h JournalEntry<'h>| {
            let entry_text = entry.to_string();
            let tb =
                entry.text_block().cloned().unwrap_or_else(|| TextBlock::from(entry_text.as_str()));
            err!(e; BlockContextError::from((&tb, "Unable to process entry")))
        };

        let mut pool_events = Vec::new_in(*self.pools.allocator());
        for adj in adjustments {
            let entry = adj.entry();
            pool_events.extend(self.progress_deals(adj.datetime())?);
            pool_events.extend(self.add_pool_adjustment(adj).map_err(|e| entry_err(e, entry))?);
        }
        Ok(pool_events)
    }

    /*
    /// Push a group of events sharing the same date and time.
    pub fn push_events(
        &mut self,
        events: impl Iterator<Item = DealingEvent<'h>>,
    ) -> JournResult<Vec<PoolEvent<'h>>> {
        let mut pool_events = vec![];

        for (i, event) in events.enumerate() {
            if i == 0 {
                pool_events.extend(self.progress_deals(event.datetime())?);
            }

            match event {
                DealingEvent::Deal(deal) => {
                    trace!("Including deal: {}", deal);
                    let datetime = deal.datetime();
                    self.apply_deal_rules(deal.into(), datetime, &mut pool_events).unwrap();
                }
                DealingEvent::PoolAdjustment(pa) => {
                    pool_events.extend(self.add_pool_adjustment(pa)?)
                }
            }
        }
        Ok(pool_events)
    }*/

    /// Applies the rules of the `deal` using the current pool configuration.
    fn apply_deal_rules(
        &mut self,
        mut group: DealGroup<'h>,
        event_datetime: JDateTimeRange<'h>,
        events: &mut Vec<PoolEvent<'h>>,
    ) -> JournResult<()> {
        loop {
            match group.advance_rule() {
                Some(Rule::Action(action)) => match action {
                    ActionRule::Match(pool_name) => {
                        let pool = PoolManager::get_or_create_pool(
                            self.config.as_ref().unwrap(),
                            &mut self.pools,
                            pool_name,
                            self.default_unit_of_account,
                        );
                        match pool.try_match(group, event_datetime)? {
                            Ok((match_events, remainder)) => {
                                events.extend(match_events);
                                match remainder {
                                    Some(rem) => group = rem,
                                    None => break,
                                }
                            }
                            Err(d) => group = d,
                        }
                    }
                    // Add to the pool while the condition is true
                    ActionRule::Pool(pool_name, until_condition) => {
                        // The deal already matches the condition. Do not pool it.
                        if group.matches(until_condition, event_datetime.start()) {
                            continue;
                        }

                        let pool = PoolManager::get_or_create_pool(
                            self.config.as_ref().unwrap(),
                            &mut self.pools,
                            pool_name,
                            self.default_unit_of_account,
                        );
                        let pool_id = pool.id();
                        let deal_id = group.id();

                        // Stop processing rules when we pool (terminal action).
                        return match until_condition {
                            Condition::False => {
                                events.push(pool.push(group, event_datetime)?);
                                Ok(())
                            }
                            Condition::Age(age, age_unit) => {
                                let datetime = group.datetime();
                                let deal_unit = group.unit();
                                events.push(pool.push(group, event_datetime)?);

                                let future_date = match age_unit {
                                    AgeUnit::Days => {
                                        datetime.start().datetime()
                                            + chrono::Duration::days(*age as i64)
                                    }
                                    AgeUnit::CalDays => {
                                        // Try to use the configured timezone for calculating when a calendar day is.
                                        // The entry's timezone is not necessarily the same as the one implied by the
                                        // tax regime.
                                        let tz = self
                                            .cgt_config()
                                            .unwrap()
                                            .timezone()
                                            .unwrap_or(datetime.start().timezone());

                                        let base_datetime =
                                            tz.from_utc_datetime(&datetime.start().naive_utc());
                                        let mut target_date = (base_datetime.naive_local().date()
                                            + chrono::Duration::days(*age as i64))
                                        .and_hms_opt(0, 0, 0)
                                        .unwrap();

                                        // The desired target time may not exist within the `tz`. This may be
                                        // due to daylight savings time or something else. We'll keep adding
                                        // 30 minutes until we find a valid time in that case.
                                        loop {
                                            match tz.from_local_datetime(&target_date) {
                                                LocalResult::None => {
                                                    target_date += chrono::Duration::minutes(30)
                                                }
                                                LocalResult::Single(dt) => {
                                                    break dt;
                                                }
                                                LocalResult::Ambiguous(_, later) => {
                                                    break later;
                                                }
                                            }
                                        }
                                    }
                                };
                                self.pool_scheduler_mut(pool_id).insert(PoolSchedulerEntry {
                                    pool_id,
                                    deal_id,
                                    datetime: JDateTime::from_datetime(
                                        future_date,
                                        Some(event_datetime.date_format()),
                                        event_datetime.time_format(),
                                    ),
                                    deal_unit,
                                });
                                Ok(())
                            }
                            _ => Err(err!(until_condition.to_string();
                                "Unsupported condition for Pool action"
                            )),
                        };
                    }
                },
                Some(Rule::Decision(decision)) => {
                    // Decide on the path to take
                    if group.matches(decision.condition(), event_datetime.start()) {
                        group.set_next_rules(decision.rules());
                    }
                }
                None => break,
            }
        }
        Ok(())
    }

    /// Adds a pool adjustment to either the pool specified by the adjustment,
    /// if any, or all the pools managed if not.
    fn add_pool_adjustment(&mut self, adj: Adjustment<'h>) -> JournResult<Vec<PoolEvent<'h>>> {
        let mut events = vec![];

        if let Some(pool) = adj.pool() {
            if self.cgt_config().unwrap().find_pool(pool).is_none() {
                return Err(err!("Pool not found: '{}'", pool));
            }
            let pool = PoolManager::get_or_create_pool(
                self.config.as_ref().unwrap(),
                &mut self.pools,
                pool,
                adj.unit(),
            );
            if !pool.is_empty(adj.unit()) || !adj.is_scalar() {
                events.push(pool.push_adjustment(adj.clone())?);
            }
        } else {
            for pool in self.pools.iter_mut() {
                if !pool.is_empty(adj.unit()) || !adj.is_scalar() {
                    events.push(pool.push_adjustment(adj.clone())?);
                }
            }
        }
        Ok(events)
    }

    /// Check if the `curr_config` is different to `new_config` and if so, update `curr_config` by merging in the
    /// changes from `new_config`.
    /// Pools of the same name will be kept and simply updated with the new configuration.
    /// New pools will be added in the same order as that configured while pools no longer referenced will be dropped,
    /// adding their holdings contained within back to the top of the cascade to be reevaluated. This ensures that
    /// `Deals` and `Matches` can never be lost or forgotten.
    pub fn update_configuration(&mut self, new_config: &Configuration<'h>) -> JournResult<()> {
        // Detect configuration change
        if self.config.as_ref() != Some(new_config) {
            self.config = Some(new_config.clone());
            let new_config = self.config.as_ref().unwrap();
            let new_cgt_config = new_config.module_config::<CgtConfiguration>(MODULE_NAME).unwrap();
            let default_uoa = new_cgt_config
                .unit_of_account_change()
                .and_then(|uoac| new_config.get_unit(uoac.unit_of_account()))
                .unwrap_or(self.default_unit_of_account);

            for new_pool_config in new_cgt_config.pools() {
                let pool_uoac = new_pool_config
                    .unit_of_account_change()
                    .or(new_cgt_config.unit_of_account_change());
                let pool_uoa = pool_uoac
                    .map(|uoac| new_config.get_unit(uoac.unit_of_account()).unwrap())
                    .unwrap_or(default_uoa);

                let pool = PoolManager::get_or_create_pool(
                    self.config.as_ref().unwrap(),
                    &mut self.pools,
                    new_pool_config.name(),
                    pool_uoa,
                );
                pool.set_methods(new_pool_config.methods());
                if let Some(new_name) = new_pool_config.new_name() {
                    pool.set_name(new_name);
                }
                if let Some(uoac) = pool_uoac {
                    pool.change_unit_of_account(
                        pool_uoa,
                        uoac.value_date().map(|d| (new_config, d)),
                    )?;
                }
            }
        }
        Ok(())
    }

    fn cgt_config(&self) -> Option<&CgtConfiguration> {
        self.config.as_ref().map(|c| c.module_config::<CgtConfiguration>(MODULE_NAME).unwrap())
    }
}

/// A scheduler to act on inner deals at a future date.
#[derive(Debug, Default)]
pub struct PoolScheduler<'h> {
    entries: Vec<PoolSchedulerEntry<'h>>,
}

impl<'h> PoolScheduler<'h> {
    /// Inserts the entry at the appropriate position in the scheduler.
    pub fn insert(&mut self, entry: PoolSchedulerEntry<'h>) {
        self.entries.push(entry);
    }

    /// Takes the next entry from the scheduler for the given `unit` that is older than or equal the given threshold (<= `threshold_date`).
    ///
    /// The entries are read according to the `match_method`
    pub fn remove_next(
        &mut self,
        threshold_date: JDateTime<'h>,
        unit: &'h Unit<'h>,
        match_method: MatchMethod,
    ) -> Option<PoolSchedulerEntry<'h>> {
        let indices = match match_method {
            MatchMethod::Lifo => (self.entries.len() - 1)..0,
            _ => 0..self.entries.len(),
        };
        for i in indices {
            let entry = &self.entries[i];
            if entry.datetime <= threshold_date && entry.deal_unit == unit {
                return Some(self.entries.remove(i));
            }
        }
        None
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PoolSchedulerEntry<'h> {
    /// The scheduled date and time. This should be in the desired timezone
    /// of fired events.
    datetime: JDateTime<'h>,
    pool_id: usize,
    deal_unit: &'h Unit<'h>,
    deal_id: DealId<'h>,
}

impl Ord for PoolSchedulerEntry<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.datetime.cmp(&other.datetime).then_with(|| self.deal_id.cmp(&other.deal_id))
    }
}

impl PartialOrd for PoolSchedulerEntry<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[cfg(test)]
mod tests {
    use crate::mod_cgt;
    use journ_core::journ;
    use journ_core::journal::Journal;

    fn journ<'h>() -> Journal<'h> {
        let j = journ!(
            r#"
         cgt
          tax-unit £
          period-ends 04-06"#
        );
        mod_cgt::register().unwrap();
        j
    }
}
