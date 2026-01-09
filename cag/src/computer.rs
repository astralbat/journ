/*
 * Copyright (c) 2017-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::capital_gains::CapitalGains;
use crate::cg_flows::CgFlows;
use crate::cgt_configuration::CagConfiguration;
use crate::cgt_journal_entry::CapitalGainsMetadataAccess;
use crate::deal::Deal;
use crate::deal_group::{DealGroup, DealGroupCriteria, PushError};
use crate::dealing_event::DealingEvent;
use crate::expenses::EntryExpenses;
use crate::mod_cgt;
use crate::module_init::MODULE_NAME;
use crate::pool_event::PoolEvent;
use crate::pool_manager::PoolManager;
use crate::report::cag_command::CagCommand;
use chrono::Utc;
use journ_core::configuration::{Configuration, Filter};
use journ_core::datetime::JDateTimeRange;
use journ_core::error::JournResult;
use journ_core::error::{BlockContextError, JournError};
use journ_core::journal::Journal;
use journ_core::journal_entry::JournalEntry;
use journ_core::journal_entry_flow::Flow;
use journ_core::parsing::text_block::TextBlock;
use journ_core::report::command::arguments::{Cmd, Command};
use journ_core::unit::Unit;
use journ_core::valuer::ValueResult;
use journ_core::{err, match_map, valuer};
use log::info;
use regex::Regex;
use smallvec::{SmallVec, smallvec};
use std::borrow::Cow;
use std::collections::{HashSet, VecDeque};
use std::ops::RangeBounds;

lazy_static! {
    static ref CGT_ACCOUNT_PATTERN: Regex = Regex::new(r"CGT:.*").unwrap();
    static ref CGT_S104_ACCOUNT_PATTERN: Regex =
        Regex::new(r"CGT:S104:([^:]+):(?:Amount|AllowableCost)").unwrap();
    static ref DESC_CGT_YEAR_END_CARRIED_FORWARD: String =
        "CGT Year End Carried Forward".to_string();
}

pub struct CapitalGainsComputer {
    #[allow(dead_code)]
    warnings: Vec<JournError>,
}

impl<'h> CapitalGainsComputer {
    pub fn new() -> Self {
        Self { warnings: vec![] }
    }

    pub fn compute_gains(&mut self, journal: &Journal<'h>) -> Result<CapitalGains<'h>, JournError> {
        let cmd = Cmd::cast::<CagCommand>();
        mod_cgt::register()?;

        // Use the account filter to decide which currencies we're going to calculate on.
        let mut included_units: Vec<&'h Unit<'h>> = {
            let mut units = HashSet::new();
            for entry in journal.entry_range(cmd.begin_and_end_cmd().begin_end_range()) {
                for pst in entry.postings() {
                    if cmd.account_filter().is_included(pst.account()) {
                        // Prefer the journal config for the unit over the posting's.
                        units.insert(
                            journal
                                .config()
                                .get_unit(pst.amount().unit().code())
                                .unwrap_or_else(|| pst.amount().unit()),
                        );
                    }
                }
            }
            let mut units: Vec<&Unit> = units.into_iter().collect();
            units.sort_by(|c1, c2| c1.display_name().cmp(c2.display_name()));
            units
        };
        // Refine with the unit filter
        included_units.retain(|c| cmd.unit_filter().is_included(*c));
        info!("Included units for CAG: {:?}", included_units);

        let initial_unit_of_account = CapitalGainsComputer::initial_unit_of_account(journal)?;

        // TODO
        // We have a singleton PoolManager, but different entries in different files may have their
        // own view on capitalgains configuration, requiring in effect, a tree of pool managers.
        let mut pool_manager = PoolManager::new(initial_unit_of_account, journal.allocator());

        let unit_filter = Box::new(move |unit: &Unit<'h>| included_units.contains(&unit));
        pool_manager.set_unit_filter(unit_filter.clone());
        let mut pool_events = vec![];
        let mut queue = DealingEventQueue::new();

        // We scan all entries with capital gains as not doing so will change
        // the result completely.
        for entry in journal.entry_range(..) {
            let entry_err = |e: JournError| {
                let entry_text = entry.to_string();
                let tb = entry
                    .text_block()
                    .cloned()
                    .unwrap_or_else(|| TextBlock::from(entry_text.as_str()));
                err!(e; BlockContextError::from((&tb, "Unable to process entry")))
            };

            pool_manager.update_configuration(entry.config()).map_err(entry_err)?;

            let events = self
                .scan_entry(entry, &unit_filter, pool_manager.unit_of_account())
                .map_err(entry_err)?;
            pool_events.extend(queue.add_events(events, &mut pool_manager)?);
        }
        pool_events.extend(queue.flush_all(&mut pool_manager)?);

        pool_events.extend(pool_manager.progress_deals(JDateTimeRange::new(
            cmd.datetime_fmt_cmd().datetime_from_utc(&Utc::now().naive_utc()),
            None,
        ))?);

        // Now report on only those events in the full date range asked for.
        let from_to_range = cmd.begin_and_end_cmd().begin_end_range();
        pool_events.retain(|e| from_to_range.contains(&e.deal_datetime().start()));

        let cg = CapitalGains::new(
            Configuration::clone(journal.config()),
            pool_events,
            pool_manager.pools().iter().map(|p| (p.name(), p.balances().collect())).collect(),
        );
        Ok(cg)
    }

    /// Scan the `entry` for `DealingEvents`. These may be implied from the Postings mentioning Accounts with assets or explicitly
    /// provided via Metadata. The latter overrides the former.
    ///
    /// The `assumed_uoa` is used as the quote unit for valuations. It is based on the first pool the deal _could_ be pushed to.
    fn scan_entry<'u>(
        &mut self,
        entry: &'h JournalEntry<'h>,
        unit_filter: impl Filter<Unit<'h>> + Copy,
        unit_of_account: &'h Unit<'h>,
    ) -> Result<Vec<DealingEvent<'h>>, JournError>
    where
        'h: 'u,
    {
        let cg_metadata = entry.cg_metadata(unit_of_account)?;

        // Assume entries that are adjustments to have no deals.
        // This allows adjustments entries to manage Asset accounts as part of that reorganisation.
        let mut reorgs = cg_metadata.adjustments().peekable();
        if reorgs.peek().is_some() {
            return Ok(reorgs
                .filter(|adj| unit_filter.is_included(adj.unit()))
                .map(|adj| DealingEvent::PoolAdjustment(adj.clone()))
                .collect());
        }

        let mut writeable_entry = Cow::Borrowed(entry);

        let round_values = entry
            .config()
            .module_config::<CagConfiguration>(MODULE_NAME)
            .unwrap()
            .round_deal_values();
        valuer::exec_optimistic(&mut writeable_entry, round_values, |valued_entry| {
            let mut deals = Self::scan_net_equity_flows(valued_entry, entry, unit_of_account)?;

            // Ensure included deals are valued in the uoa
            for deal in deals.iter_mut().filter(|d| unit_filter.is_included(d.unit())) {
                if let Err(e) = deal.ensure_valued(unit_of_account, round_values) {
                    return ValueResult::Err(e.into());
                }
            }

            // There are no other net equity flows of interest - either because they have been
            // explicitly specified in metadata, they are deemed to be a UOA, or haven't been included
            // in the unit_filter.
            if deals.iter().all(|d| d.is_required()) {
                return ValueResult::Ok(
                    deals
                        .into_iter()
                        .filter(|d| unit_filter.is_included(d.unit()))
                        .map(DealingEvent::Deal)
                        .collect(),
                );
            }

            if deals.iter().any(|d| unit_filter.is_included(d.unit())) {
                let implied_deals = deals.iter().filter(|d| !d.is_required());
                let implied_deals_no_uoa = implied_deals.filter(|d| d.unit() != unit_of_account);
                let expenses_division = EntryExpenses::scan_and_divide(
                    valued_entry,
                    unit_of_account,
                    implied_deals_no_uoa.clone().map(|d| d.valued_amount()),
                )?;

                for (i, deal) in deals
                    .iter_mut()
                    .filter(|d| !d.is_required())
                    .filter(|d| d.unit() != unit_of_account)
                    .enumerate()
                    .filter(|(_, d)| unit_filter.is_included(d.unit()))
                {
                    let mut expenses = expenses_division.get_expenses(i);
                    expenses = expenses.without_unit(deal.unit());

                    deal.add_expenses(expenses);
                }
            }
            ValueResult::Ok(
                deals
                    .into_iter()
                    .filter(|d| d.unit() != unit_of_account || d.is_required())
                    .filter(|d| unit_filter.is_included(d.unit()))
                    .map(DealingEvent::Deal)
                    .collect(),
            )
        })
        .map(Ok)?
    }

    /// Scans the `valued_entry/existing_entry` for deals both explicit and implicit.
    ///
    /// Explicit deals override implicit ones for any particular unit.
    ///
    /// Only deals for included units according to `unit_filter` are returned.
    fn scan_net_equity_flows<'a, 'u>(
        valued_entry: &'a JournalEntry<'h>,
        existing_entry: &'h JournalEntry<'h>,
        unit_of_account: &'h Unit<'h>,
    ) -> JournResult<SmallVec<[Deal<'h>; 4]>> {
        let mut deals = smallvec![];

        let cg_flows = CgFlows::new(valued_entry.flows());
        let cg_metadata = existing_entry.cg_metadata(unit_of_account)?;

        // All the units we're dealing with
        #[allow(clippy::mutable_key_type)]
        let units = cg_flows
            .iter()
            .map(Flow::unit)
            .chain(cg_metadata.all_deals().map(Deal::unit))
            //.filter(|u| unit_filter.is_included(*u))
            .collect::<HashSet<_>>();

        for unit in units {
            // Explicit deals override implicit for a particular unit
            let explicit_deals = cg_metadata.deals(unit);
            if !explicit_deals.is_empty() {
                for deal in explicit_deals.into_iter().filter(|d| !d.total().is_zero()) {
                    deals.push(deal);
                }
                continue;
            }

            // Add implicit deals
            let unit_flows = cg_flows.by_unit(unit);
            deals.append(&mut unit_flows.create_deals(existing_entry, unit_of_account)?);
        }

        Ok(deals)
    }

    /*
    fn scan_net_equity_flows<'a, 'u>(
        valued_entry: &'a JournalEntry<'h>,
        existing_entry: &'h JournalEntry<'h>,
        unit_filter: impl Filter<Unit<'h>>,
        initial_uoa: impl Fn(&DealGroup<'h>) -> &'h Unit<'h>,
    ) -> ValueResult<'h, NetEquityFlowsAndMetadataEvents<'h>> {
        let mut net_equity_flows = valued_entry
            .flows()
            .net_equity()
            .filter(|flow| !flow.account_root().map(|f| f.has_tag("CAG-Exempt")).unwrap_or(false))
            .collect::<SmallVec<[_; 4]>>();
        net_equity_flows.reduce_down_by_account_type();
        let cg_metadata = existing_entry.cg_metadata()?;

        // These are the units we're dealing in. This still includes the UOA as we can't be sure what it is
        // until the deal is pushed to a pool.
        #[allow(clippy::mutable_key_type)]
        let units = net_equity_flows
            .iter()
            .map(|f| f.unit())
            .chain(cg_metadata.all_deals().filter(|d| d.is_required()).map(|d| d.unit()))
            .filter(|u| unit_filter.is_included(*u))
            .collect::<HashSet<_>>();

        let mut units_to_scan = smallvec![];
        let mut first_uoa = None;
        let mut metadata_events = vec![];
        for unit in units.iter().copied() {
            // If there are any explicit deals, we don't need to scan for implicit ones. Save time and any
            // valuations we don't need right now.
            let metadata_deals = cg_metadata.deals(unit);
            if metadata_deals.iter().any(|d| d.is_required()) {
                for deal in metadata_deals
                    .iter()
                    .filter(|d| d.is_required())
                    .filter(|d| !d.total().is_zero())
                {
                    metadata_events.push(DealingEvent::Deal(deal.clone()));
                }
                continue;
            }

            let net_equity_flow = net_equity_flows
                .iter()
                .find(|flow| flow.valued_amount().amount().unit() == unit)
                .unwrap();
            let dummy_deal = Deal::zero(net_equity_flow.unit(), existing_entry);
            // Don't include this flow if it's would-be UOA is the same as its unit. E.g. a € transaction
            // can't be added to a pool whose unit of account is also €.
            let initial_uoa = initial_uoa(&DealGroup::new(DealGroupCriteria::Single, dummy_deal));
            if initial_uoa != unit {
                units_to_scan.push(net_equity_flow.unit());
                first_uoa = Some(initial_uoa);
            }
        }

        if units_to_scan.is_empty() {
            return ValueResult::Ok(NetEquityFlowsAndMetadataEvents {
                neq_flows: None,
                metadata_events,
            });
        }

        let uoa = first_uoa.unwrap();
        let net_equity_flows_without_uoa =
            || net_equity_flows.iter().filter(|flow| flow.unit() != uoa).map(Flow::valued_amount);

        // First check that the net equity flows are valued in the uoa
        for val in net_equity_flows_without_uoa().filter(|f| units.contains(f.unit())) {
            if val.value_in(uoa).is_none() {
                return ValueResult::ValuationNeeded(val.unit(), uoa);
            }
        }
        ValueResult::Ok(NetEquityFlowsAndMetadataEvents {
            neq_flows: Some(NetEquityFlows {
                flows: net_equity_flows.into_iter().map(Flow::into_valued_amount).collect(),
                included_units: units_to_scan,
                uoa,
            }),
            metadata_events,
        })
    }*/

    /// Gets the initial unit of account for the first entry to be processed.
    fn initial_unit_of_account(journal: &Journal<'h>) -> JournResult<&'h Unit<'h>> {
        let mut unit_of_account = None;

        let entries = journal.entry_range(..);
        if let Some(entry) = entries.clone().next() {
            let cag_config = CagConfiguration::get(entry.config());
            if let Some(cag_config) = cag_config
                && let Some(uoac) = cag_config.unit_of_account_change()
            {
                unit_of_account = journal.config().get_unit(uoac.unit_of_account());
            }
        }
        if unit_of_account.is_none() {
            'outer: for entry in entries {
                for pst in entry.postings() {
                    if let Some(val) = pst.valuations().next() {
                        unit_of_account = Some(val.unit());
                        break 'outer;
                    }
                }
            }
        }
        unit_of_account.ok_or_else(|| err!(
            "No default unit of account detected. Set one explicitly with:\ncapitalgains unitofaccount <unit>"
        ))
    }
}

struct DealingEventQueue<'h> {
    events: VecDeque<DealingEvent<'h>>,
}
impl<'h> DealingEventQueue<'h> {
    pub fn new() -> Self {
        Self { events: VecDeque::new() }
    }

    pub fn add_events<I: IntoIterator<Item = DealingEvent<'h>>>(
        &mut self,
        events: I,
        pool_manager: &mut PoolManager<'h>,
    ) -> JournResult<Vec<PoolEvent<'h>>> {
        let mut pool_events = vec![];

        for event in events {
            match event {
                DealingEvent::PoolAdjustment(pa) => {
                    self.events.push_back(DealingEvent::PoolAdjustment(pa));
                }
                DealingEvent::Deal(mut deal) => {
                    let deal_unit = deal.unit();
                    let flush_to_offset = 'outer: {
                        let mut flush_to_offset = None;
                        // For all candidate groups
                        for (i, group) in self.events.iter_mut().enumerate().filter_map(|(i, e)| match_map!(e, DealingEvent::Group(g) if g.unit() == deal_unit => (i, g))) {
                            match group.try_push(deal) {
                                Err(PushError::CriteriaMismatch(rejected_deal)) => {
                                    // All groups to i inclusive are complete and can be flushed.
                                    if group.is_complete(rejected_deal.datetime()) {
                                        flush_to_offset = Some(i);
                                    }
                                    deal = rejected_deal;
                                }
                                Err(PushError::EvalError(e)) => {
                                    return Err(e);
                                }
                                Ok(_) => {
                                    break 'outer flush_to_offset;
                                }
                            }
                        }
                        self.push_back_deal(deal);
                        flush_to_offset
                    };
                    if let Some(offset) = flush_to_offset {
                        pool_events.extend(self.flush_to_offset(offset, pool_manager)?);
                    }
                }
                _ => unreachable!("Unexpected event type"),
            }
        }
        Ok(pool_events)
    }

    fn push_back_deal(&mut self, deal: Deal<'h>) {
        let cmd = Cmd::cast::<CagCommand>();
        let criteria = if cmd.group_deals_by_date() {
            DealGroupCriteria::same_day_same_sign(&deal)
        } else {
            DealGroupCriteria::Single
        };
        self.events.push_back(DealingEvent::Group(DealGroup::new(criteria, deal)));
    }

    pub fn flush_to_offset(
        &mut self,
        offset: usize,
        pool_manager: &mut PoolManager<'h>,
    ) -> JournResult<Vec<PoolEvent<'h>>> {
        let mut pool_events = vec![];
        for event in self.events.drain(..=offset) {
            match event {
                DealingEvent::Group(group) => {
                    pool_events.extend(pool_manager.push_deal_group(group)?);
                }
                DealingEvent::PoolAdjustment(pa) => {
                    pool_events.extend(pool_manager.push_adjustments([pa])?);
                }
                _ => panic!("Unexpected event type"),
            }
        }
        Ok(pool_events)
    }

    pub fn flush_all(
        mut self,
        pool_manager: &mut PoolManager<'h>,
    ) -> JournResult<Vec<PoolEvent<'h>>> {
        if self.events.is_empty() {
            return Ok(vec![]);
        }
        self.flush_to_offset(self.events.len() - 1, pool_manager)
    }
}

/*
#[derive(Debug)]
struct DealsByDateAggregation<'h> {
    date: Option<NaiveDate>,
    deals: HashMap<&'h Unit<'h>, Vec<DealingEvent<'h>>>,
}
impl<'h> DealsByDateAggregation<'h> {
    pub fn new() -> Self {
        Self { date: None, deals: HashMap::new() }
    }

    fn try_add(&mut self, event_to_add: DealingEvent<'h>) -> Result<(), Self> {
        let event_to_add_date = event_to_add
            .datetime()
            .start()
            .datetime()
            .with_timezone(&Arguments::get().datetime_args.timezone)
            .date_naive();
        match self.date {
            None => {
                self.date = Some(event_to_add_date);
                self.deals.insert(event_to_add.unit(), vec![event_to_add]);
                Ok(())
            }
            Some(date) => {
                if date == event_to_add_date {
                    let mut added = false;
                    'top: for (unit, events) in self.deals.iter_mut() {
                        for event in events {
                            match (event, &event_to_add) {
                                (DealingEvent::Deal(mut deal), DealingEvent::Deal(deal_to_add)) => {
                                    if *unit == deal_to_add.unit() {
                                        if let Some(added_deal) = deal + deal_to_add {
                                            deal = added_deal;
                                            added = true;
                                            break 'top;
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    if !added {
                        self.deals.entry(event_to_add.unit()).or_default().push(event_to_add);
                    }
                    Ok(())
                // Start a new aggregation
                } else {
                    let mut new = Self::new();
                    new.try_add(event_to_add).unwrap();
                    Err(new)
                }
            }
        }
    }

    pub fn into_inner(self) -> Vec<DealingEvent<'h>> {
        self.deals.into_iter().collect()
    }
}*/

#[cfg(test)]
mod tests {
    use crate::cgt_configuration::CagCommand;
    use crate::cgt_journal_entry::CapitalGainsMetadataAccess;
    use crate::computer::CapitalGainsComputer;
    use crate::deal;
    use crate::dealing_event::DealingEvent;
    use indoc::indoc;
    use journ_core::directive::{Directive, DirectiveKind};
    use journ_core::{entry, entry_dir, match_map, unit, val};
    use std::assert_eq;

    #[test]
    fn test_scan_entry() {
        let entry = match_map!(entry_dir!(indoc! {r#"
            Account Expenses:Exp
                +CGT-AllowableExpenses
            2000-01-01
                Assets:Buy  €25
                Expenses:Exp  €5
                Assets:Sell  -€30
        "#}), DirectiveKind::Entry(e) => e)
        .unwrap();

        let mut comp = CapitalGainsComputer::new(CagCommand::default());
        let events = comp.scan_entry(entry, None).unwrap();
        assert_eq!(events, vec![DealingEvent::Deal(deal!("€25 ++ €5"))])
    }
}
