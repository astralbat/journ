/*
 * Copyright (c) 2023-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjusted_value::AdjustedValue;
use crate::adjustment::Adjustment;
use crate::cag_configuration::CagConfiguration;
use crate::holding::DealHolding;
use crate::module_init::MODULE_NAME;
use crate::pool::Pool;
use crate::pool_event::{PoolEvent, PoolEventKind};
use crate::ruleset::{ActionRule, AgeUnit, Condition, Rule};
use chrono::{LocalResult, TimeZone};
use journ_core::alloc::HerdAllocator;
use journ_core::configuration::{Configuration, Filter};
use journ_core::datetime::{DateTimePrecision, JDateTime, JDateTimeRange};
use journ_core::err;
use journ_core::error::{BlockContext, BlockContextError, JournError, JournResult};
use journ_core::journal_context::JContext;
use journ_core::journal_entry::JournalEntry;
use journ_core::parsing::text_block::TextBlockBuf;
use journ_core::unit::Unit;
use log::debug;
use std::collections::HashMap;

pub struct PoolManager<'h> {
    /// The unit of account to use. This can change over time with configuration updates.
    unit_of_account: &'h Unit<'h>,
    // Keep config to compare for changes. Would be better to implement a config change listener in core.
    config: Option<Configuration<'h>>,
    unit_filter: Box<dyn Filter<Unit<'h>> + 'h>,
    /// Pools are expected to be in the order they are declared in the configuration.
    pools: Vec<Pool<'h>, &'h HerdAllocator<'h>>,
    pool_schedulers: HashMap<usize, PoolScheduler<'h>>,
}

impl<'h> PoolManager<'h> {
    pub fn new(unit_of_account: &'h Unit<'h>, allocator: &'h HerdAllocator<'h>) -> Self {
        // The starting implied pool is named "Pool".
        PoolManager {
            config: None,
            unit_of_account,
            unit_filter: Box::new(|_: &Unit<'_>| true),
            pools: Vec::new_in(allocator),
            pool_schedulers: HashMap::new(),
        }
    }

    pub fn unit_of_account(&self) -> &'h Unit<'h> {
        self.unit_of_account
    }

    pub fn set_unit_filter(&mut self, unit_filter: Box<dyn Filter<Unit<'h>> + 'h>) {
        self.unit_filter = unit_filter;
    }

    fn get_pool(&self, pool_id: usize) -> Option<&Pool<'h>> {
        self.pools.iter().find(|p| p.id() == pool_id)
    }

    fn get_pool_by_name(&self, pool_name: &str) -> Option<&Pool<'h>> {
        self.pools.iter().find(|p| p.name() == pool_name)
    }

    fn get_pool_by_name_mut(&mut self, pool_name: &str) -> Option<&mut Pool<'h>> {
        self.pools.iter_mut().find(|p| p.name() == pool_name)
    }

    fn get_pool_mut(&mut self, pool_id: usize) -> Option<&mut Pool<'h>> {
        self.pools.iter_mut().find(|p| p.id() == pool_id)
    }

    /// Gets the pool with the specified `name` or creates a new one if it does not exist. The `name` must
    /// refer to an existing pool within the configuration.
    /// The `pool_uoa` is the unit of account for the new pool and must be specified if the pool does not exist.
    ///
    /// # Panics
    /// If the pool does not exist and `pool_uoa` is `None`.
    fn get_or_create_pool<'a>(
        pools: &'a mut Vec<Pool<'h>, &'h HerdAllocator<'h>>,
        name: &'h str,
        pool_uoa: &'h Unit<'h>,
    ) -> &'a mut Pool<'h> {
        let config = JContext::get().config();
        let cgt_config = config.module_config::<CagConfiguration>(MODULE_NAME).unwrap();
        let pool_config = cgt_config.find_pool(name).unwrap();
        let pos = pools.iter_mut().position(|p| p.id() == pool_config.id().unwrap());
        match pos {
            Some(pos) => &mut pools[pos],
            None => {
                let allocated_name = config.allocator().alloc(name.to_string());
                let pool = Pool::new(pool_config.id().unwrap(), allocated_name.as_str(), pool_uoa);
                pools.push(pool);
                pools.last_mut().unwrap()
            }
        }
    }

    pub fn pool_scheduler_mut(&mut self, pool_id: usize) -> &mut PoolScheduler<'h> {
        self.pool_schedulers.entry(pool_id).or_default()
    }

    pub fn progress_deals(
        &mut self,
        event_datetime: JDateTimeRange,
    ) -> JournResult<Vec<PoolEvent<'h>>> {
        let mut pool_events = vec![];

        // Loop tactic for satisfying the borrow checker.
        'outer: loop {
            for pool in self.pools.iter_mut() {
                let scheduler = self.pool_schedulers.entry(pool.id()).or_default();
                if let Some(pse) = scheduler.remove_next(event_datetime.start()) {
                    if let Some((bal_before, extracted, bal_after)) =
                        pool.extract(pse.holding_id, pse.deal_unit)
                    {
                        self.apply_deal_rules(
                            pse.rule,
                            extracted,
                            JDateTimeRange::new(pse.datetime, None),
                            Some((
                                self.pools.iter().find(|p| p.id() == pse.pool_id).unwrap().name(),
                                bal_before,
                                bal_after,
                            )),
                            &mut pool_events,
                        )?;
                    }
                    continue 'outer;
                }
            }
            break;
        }
        Ok(pool_events)
    }

    pub fn push_deal_holding(
        &mut self,
        holding: DealHolding<'h>,
    ) -> JournResult<Vec<PoolEvent<'h>>> {
        assert_ne!(holding.amount(), 0, "Cannot push deals of 0");

        let datetime = holding.datetime();
        let mut pool_events = vec![];
        pool_events.extend(self.progress_deals(datetime)?);

        let rules = JContext::get()
            .config()
            .module_config::<CagConfiguration>(MODULE_NAME)
            .unwrap()
            .ruleset();
        self.apply_deal_rules(rules, holding, datetime, None, &mut pool_events)?;
        Ok(pool_events)
    }

    pub fn push_adjustments<I: IntoIterator<Item = Adjustment<'h>>>(
        &mut self,
        adjustments: I,
    ) -> JournResult<Vec<PoolEvent<'h>>> {
        let entry_err = |e: JournError, entry: &'h JournalEntry<'h>| {
            let context = match entry.text_block() {
                Some(block) => BlockContext::from(block),
                None => {
                    let mut buf = TextBlockBuf::new();
                    buf.write(entry, Some(entry.config()));
                    BlockContext::from(&buf.as_text_block())
                }
            };
            err!(e; BlockContextError::new(context, "Unable to process entry"))
        };

        let mut pool_events = Vec::new();
        for adj in adjustments {
            let entry = adj.entry();
            pool_events.extend(self.progress_deals(adj.datetime())?);
            pool_events.extend(self.add_pool_adjustment(adj).map_err(|e| entry_err(e, entry))?);
        }
        Ok(pool_events)
    }

    /// Applies the rules of the `deal` using the current pool configuration.
    fn apply_deal_rules(
        &mut self,
        mut rules: &'h [Rule],
        mut holding: DealHolding<'h>,
        event_datetime: JDateTimeRange,
        from_pool: Option<(&'h str, AdjustedValue<'h>, AdjustedValue<'h>)>,
        events: &mut Vec<PoolEvent<'h>>,
    ) -> JournResult<()> {
        //let advanced_rule = group.advance_rule();
        let adjusted_value = holding.adjusted_value();
        //let deal_holding_group = HoldingHandle::new(DealHolding::Group(group));

        while let Some(rule) = rules.first() {
            rules = &rules[1..];
            match rule {
                Rule::Action(action) => match action {
                    ActionRule::Match(pool_name) => {
                        if let Some(pool) = self.get_pool_by_name_mut(pool_name) {
                            match pool.try_match(holding, event_datetime)? {
                                Ok((match_events, remainder)) => {
                                    if let Some((_from_pool, bal_before, _)) = from_pool {
                                        let _bal_after = match remainder.as_ref() {
                                            Some(remainder) => {
                                                bal_before
                                                    - (adjusted_value - remainder.adjusted_value())
                                            }
                                            None => bal_before - adjusted_value,
                                        };
                                        /*
                                        let pe = PoolEvent::new(
                                            from_pool,
                                            event_datetime,
                                            PoolEventKind::MatchedFrom(
                                                deal_holding_group.clone(),
                                                pool_name,
                                            ),
                                            bal_before.clone(),
                                            bal_after.clone(),
                                        );
                                        debug!("{}", pe);
                                        events.push(pe);
                                         */
                                        //if remainder.is_some() {
                                        //    bal_before = bal_after;
                                        //}
                                    }

                                    events.extend(match_events);
                                    match remainder {
                                        Some(rem) => holding = rem,
                                        None => break,
                                    }
                                }
                                Err(d) => holding = d,
                            }
                        }
                    }
                    // Add to the pool while the condition is true
                    ActionRule::Pool(pool_name, until_condition) => {
                        // The deal already matches the condition. Do not pool it.
                        if holding.matches(until_condition, event_datetime.start()) {
                            continue;
                        }

                        let pool = PoolManager::get_or_create_pool(
                            &mut self.pools,
                            pool_name,
                            self.unit_of_account,
                        );
                        let pool_id = pool.id();

                        // Stop processing rules when we pool (terminal action).
                        let pool_push_event = match until_condition {
                            Condition::False => {
                                let (pool_push_event, _extract_id) = pool.push(
                                    holding,
                                    event_datetime,
                                    from_pool.as_ref().map(|fp| fp.0),
                                )?;
                                Ok(pool_push_event)
                            }
                            Condition::Age(age, age_unit) => {
                                let datetime = holding.datetime();
                                let deal_unit = holding.unit();
                                let (pool_push_event, extract_id) = pool.push(
                                    holding,
                                    event_datetime,
                                    from_pool.as_ref().map(|fp| fp.0),
                                )?;

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
                                if !rules.is_empty() {
                                    self.pool_scheduler_mut(pool_id).insert(PoolSchedulerEntry {
                                        rule: rules,
                                        pool_id,
                                        holding_id: extract_id,
                                        datetime: JDateTime::new(
                                            future_date,
                                            DateTimePrecision::Second,
                                        ),
                                        deal_unit,
                                    });
                                }
                                Ok(pool_push_event)
                            }
                            _ => Err(err!(until_condition.to_string();
                                "Unsupported condition for Pool action"
                            )),
                        }?;
                        if let Some((from_pool, bal_before, bal_after)) = from_pool {
                            let moved_to_event = PoolEvent::new(
                                from_pool,
                                event_datetime,
                                PoolEventKind::MovedFrom(
                                    match pool_push_event.kind() {
                                        PoolEventKind::PooledDeal(hh)
                                        | PoolEventKind::MovedTo(hh, _) => hh.clone(),
                                        _ => unreachable!(),
                                    },
                                    pool_name,
                                ),
                                bal_before,
                                bal_after,
                            );
                            debug!("{}", moved_to_event);
                            events.push(moved_to_event);
                        }
                        events.push(pool_push_event);
                        return Ok(());
                    }
                },
                Rule::Decision(decision) => {
                    // Decide on the path to take
                    if holding.matches(decision.condition(), event_datetime.start()) {
                        rules = decision.rules();
                        continue;
                    }
                }
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
            let pool = PoolManager::get_or_create_pool(&mut self.pools, pool, adj.unit());
            if pool.key(adj.unit(), true).is_some() || !adj.is_scalar() {
                events.push(pool.push_adjustment(adj.clone())?);
            }
        } else {
            for pool in self.pools.iter_mut() {
                if pool.key(adj.unit(), true).is_some() || !adj.is_scalar() {
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
    pub fn update_configuration(&mut self) -> JournResult<()> {
        // Detect configuration change
        let new_config = JContext::get().config();
        if self.config.as_ref() != Some(&*new_config) {
            self.config = Some(new_config.clone());
            //let new_config = self.config.as_ref().unwrap();
            let new_cgt_config = new_config.module_config::<CagConfiguration>(MODULE_NAME).unwrap();
            let default_uoa = new_cgt_config
                .unit_of_account_change()
                .and_then(|uoac| new_config.get_unit(uoac.unit_of_account()))
                .unwrap_or(self.unit_of_account);

            let uoac = new_cgt_config.unit_of_account_change();
            let uoa = uoac
                .map(|uoac| new_config.get_unit(uoac.unit_of_account()).unwrap())
                .unwrap_or(default_uoa);
            for new_pool_config in new_cgt_config.pools() {
                let pool =
                    PoolManager::get_or_create_pool(&mut self.pools, new_pool_config.name(), uoa);
                pool.set_methods(new_pool_config.methods());
                if let Some(new_name) = new_pool_config.new_name() {
                    pool.set_name(new_name);
                }
                if let Some(uoac) = uoac {
                    pool.change_unit_of_account(uoa, uoac.value_date().map(|d| (&*new_config, d)))?;
                }
            }
        }
        Ok(())
    }

    fn cgt_config(&self) -> Option<&CagConfiguration> {
        self.config.as_ref().map(|c| c.module_config::<CagConfiguration>(MODULE_NAME).unwrap())
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
    pub fn remove_next(
        &mut self,
        threshold_date: JDateTime,
        //unit: &'h Unit<'h>,
    ) -> Option<PoolSchedulerEntry<'h>> {
        for i in 0..self.entries.len() {
            let entry = &self.entries[i];
            if entry.datetime <= threshold_date {
                //&& entry.deal_unit == unit {
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
    datetime: JDateTime,
    /// The next rule sequence to apply to the deal holding when it is fired.
    rule: &'h [Rule],
    pool_id: usize,
    deal_unit: &'h Unit<'h>,
    /// The identifier of the holding to extract from the pool when the event is fired.
    holding_id: usize,
}

impl Ord for PoolSchedulerEntry<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.datetime.cmp(&other.datetime).then_with(|| self.holding_id.cmp(&other.holding_id))
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
