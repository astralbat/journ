/*
 * Copyright (c) 2017-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::capital_gains::CapitalGains;
use crate::cgt_configuration::{CagCommand, CgtConfiguration};
use crate::cgt_journal_entry::CapitalGainsMetadataAccess;
use crate::deal::Deal;
use crate::deal_group::{DealGroup, DealGroupCriteria};
use crate::dealing_event::DealingEvent;
use crate::expenses::EntryExpenses;
use crate::mod_cgt;
use crate::module_init::MODULE_NAME;
use crate::pool_event::PoolEvent;
use crate::pool_manager::PoolManager;
use chrono::Utc;
use journ_core::arguments::Arguments;
use journ_core::configuration::{Configuration, Filter};
use journ_core::date_and_time::JDateTimeRange;
use journ_core::error::JournResult;
use journ_core::error::{BlockContextError, JournError};
use journ_core::journal::Journal;
use journ_core::journal_entry::JournalEntry;
use journ_core::journal_entry_flow::Flow;
use journ_core::parsing::text_block::TextBlock;
use journ_core::unit::Unit;
use journ_core::valued_amount::ValuedAmount;
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
    args: &'static Arguments,
}

impl CapitalGainsComputer {
    pub fn new(args: &'static Arguments) -> Self {
        Self { args, warnings: vec![] }
    }

    pub fn compute_gains<'h>(
        &mut self,
        journal: &Journal<'h>,
    ) -> Result<CapitalGains<'h>, JournError> {
        let cmd = self.args.cast_cmd::<CagCommand>().unwrap();
        mod_cgt::register()?;

        // Use the account filter to decide which currencies we're going to calculate on.
        let mut included_units: Vec<&'h Unit<'h>> = {
            let mut units = HashSet::new();
            for entry in journal.entry_range(self.args.begin_end_range()) {
                for pst in entry.postings() {
                    if cmd.account_filter().is_included(pst.account()) {
                        units.insert(pst.amount().unit());
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

        // Find default unit of account
        let mut default_unit_of_account = None;
        'outer: for entry in journal.entry_range(self.args.begin_end_range()) {
            for flow in entry.flows().net_equity() {
                if let Some(val) = flow.valued_amount().valuations().next() {
                    default_unit_of_account = Some(val.unit());
                    break 'outer;
                }
            }
            if let Some(config) = entry.config().module_config::<CgtConfiguration>(MODULE_NAME) {
                if let Some(uoac) = config.unit_of_account_change() {
                    default_unit_of_account =
                        Some(entry.config().get_unit(uoac.unit_of_account()).unwrap());
                    break 'outer;
                }
            }
        }
        if default_unit_of_account.is_none() {
            return Err(err!(
                "No default unit of account detected. Set one explicitly with:\ncapitalgains unitofaccount <unit>"
            ));
        }

        let mut pool_manager =
            PoolManager::new(default_unit_of_account.unwrap(), journal.allocator());
        let unit_filter = Box::new(move |unit: &Unit<'h>| included_units.contains(&unit));
        pool_manager.set_unit_filter(unit_filter.clone());
        let mut pool_events = vec![];
        let mut queue = DealingEventQueue::new();
        for entry in journal.entry_range(self.args.begin_end_range()) {
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
                .scan_entry(entry, &unit_filter, |deal_group: &DealGroup<'h>| {
                    pool_manager.initial_unit_of_account(deal_group)
                })
                .map_err(entry_err)?;
            pool_events.extend(queue.add_events(events, &mut pool_manager)?);
        }
        pool_events.extend(queue.flush_all(&mut pool_manager)?);

        pool_events.extend(pool_manager.progress_deals(JDateTimeRange::new(
            cmd.datetime_args.datetime_from_utc(&Utc::now().naive_utc()),
            None,
        ))?);

        // Now report on only those events in the full date range asked for.
        let from_to_range = cmd.from_to_range();
        pool_events.retain(|e| from_to_range.contains(&e.deal_datetime().start().datetime()));

        let cg = CapitalGains::new(
            Configuration::clone(&journal.config()),
            pool_events,
            pool_manager.pools().iter().map(|p| (p.name(), p.balances().collect())).collect(),
        );
        Ok(cg)
    }

    /// Scan the `entry` for `DealingEvents`. These may be implied from the Postings mentioning Accounts with assets or explicitly
    /// provided via Metadata. The latter overrides the former.
    ///
    /// The `assumed_uoa` is used as the quote unit for valuations. It is based on the first pool the deal _could_ be pushed to.
    fn scan_entry<'h>(
        &mut self,
        entry: &'h JournalEntry<'h>,
        unit_filter: impl Filter<Unit<'h>> + Copy,
        initial_uoa: impl Fn(&DealGroup<'h>) -> &'h Unit<'h> + Copy,
    ) -> Result<Vec<DealingEvent<'h>>, JournError> {
        let cg_metadata = entry.cg_metadata()?;

        // Assume entries that are adjustments to have no deals.
        // This allows adjustments entries to manage Asset accounts as part of that reorganisation.
        let mut reorgs = cg_metadata.adjustments().peekable();
        if reorgs.peek().is_some() {
            return Ok(reorgs
                .filter(|adj| unit_filter.is_included(adj.unit()))
                .map(|adj| DealingEvent::PoolAdjustment(adj.clone()))
                .collect());
        }

        //let cgt_config =
        //    entry.config().as_herd_ref().module_config::<CgtConfiguration>(MODULE_NAME).unwrap();
        //let mut deals = vec![];
        let mut writeable_entry = Cow::Borrowed(entry);
        //let mut expense_vals_sum;
        //let mut included_net_equity_vals;
        //let mut net_equity_flows;
        //let equity_normalised_weights;
        //let mut uoa;
        //let mut net_equity_flows_without_uoa;

        valuer::exec_optimistic(&mut writeable_entry, |valued_entry| {
            let mut neq_flows_and_metadata =
                Self::scan_net_equity_flows(valued_entry, entry, unit_filter, initial_uoa)?;

            // There are no other net equity flows of interest - either because they have been
            // explicitly specified in metadata, they are deemed to be a UOA, or haven't been included
            // in the unit_filter.
            if neq_flows_and_metadata.neq_flows.is_none() {
                return ValueResult::Ok(neq_flows_and_metadata.take_metadata_events());
            }

            let neq_flows = neq_flows_and_metadata.neq_flows.unwrap();
            let expenses_division = EntryExpenses::scan_and_divide(
                &valued_entry,
                neq_flows.uoa,
                neq_flows.all_flows_no_uoa(),
            )?;

            let mut deals = neq_flows_and_metadata.metadata_events;
            for (i, net_equity_flow) in neq_flows.included_flows() {
                /*
                let mut expenses = if expense_vals_sum.amount() > 0 {
                    expense_vals_sum.split_weighted(equity_normalised_weights.as_ref().unwrap(), i)
                } else {
                    ValuedAmount::nil()
                };*/
                let mut expenses = expenses_division.get_expenses(i);
                expenses = expenses.without_unit(net_equity_flow.unit());

                let deal = Deal::new(None, entry, net_equity_flow.clone(), expenses, None, false);
                deals.push(DealingEvent::Deal(deal));
            }
            ValueResult::Ok(deals)
        })
        .map(Ok)?

        /*
        'restart: loop {
            //writeable_entry.derive_valuations()?;
            let flows = Some(writeable_entry.flows());
            net_equity_flows = flows.as_ref().unwrap().net_equity().collect::<Vec<_>>();

            // These are the units we're dealing in. This still includes the UOA as we can't be sure what it is
            // until the deal is pushed to a pool.
            let units = net_equity_flows
                .iter()
                .map(|f| f.unit())
                .chain(cg_metadata.all_deals().filter(|d| d.is_required()).map(|d| d.unit()))
                .filter(|u| unit_filter.is_included(*u))
                .collect::<HashSet<_>>();

            included_net_equity_vals = vec![];
            let mut first_uoa = None;
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
                        deals.push(DealingEvent::Deal(deal.clone()));
                    }
                    continue;
                }

                let net_equity_flow = net_equity_flows
                    .iter()
                    .find(|flow| flow.valued_amount().amount().unit() == unit)
                    .unwrap();
                let dummy_deal = Deal::zero(net_equity_flow.unit(), entry);
                // Don't include this flow if it's would-be UOA is the same as its unit. E.g. a € transaction
                // can't be added to a pool whose unit of account is also €.
                let initial_uoa =
                    initial_uoa(&DealGroup::new(DealGroupCriteria::Single, dummy_deal));
                if initial_uoa != unit {
                    included_net_equity_vals.push(net_equity_flow.valued_amount());
                    first_uoa = Some(initial_uoa);
                }
            }
            if included_net_equity_vals.is_empty() {
                return Ok(deals);
            }

            uoa = first_uoa.unwrap();
            net_equity_flows_without_uoa = || {
                net_equity_flows.iter().filter(|flow| flow.unit() != uoa).map(Flow::valued_amount)
            };

            // First check that the net equity flows are valued in the uoa
            for val in net_equity_flows_without_uoa().filter(|f| units.contains(f.unit())) {
                if val.value_in(uoa).is_none() {
                    valuer::value_with_entry_and_write(&mut writeable_entry, val.unit(), uoa)?;
                    continue 'restart;
                }
            }
            break;
        }*/

        /*b
        let expenses = EntryExpenses::scan(&mut writeable_entry, uoa)?;
        let expenses_division = expenses.divide_between(net_equity_flows_without_uoa())?;*/

        /*
            expense_vals_sum = Self::expenses_sum(&mut writeable_entry, uoa)?;

            // If we have some expenses, we are going to need to divide them amongst the deals we intend to include.
            // We do this by normalising the group of `included_net_equity_vals`. If we can't normalise them, it means they
            // do not share a common unit, and so we force a valuation using `first_uoa`.
            equity_normalised_weights = if expense_vals_sum.amount() > 0 {
                let nil_amount = ValuedAmount::nil();
                let has_disposal = net_equity_flows_without_uoa().any(|f| f.amount().is_negative());
                let has_acquisition =
                    net_equity_flows_without_uoa().any(|f| f.amount().is_positive());
                let equity_flows =
                    net_equity_flows_without_uoa().map(|f| match cgt_config.assign_expenses() {
                        AssignExpenses::PreferAcquisition if has_acquisition => {
                            if f.amount().is_positive() {
                                f
                            } else {
                                &nil_amount
                            }
                        }
                        AssignExpenses::PreferDisposal if has_disposal => {
                            if f.amount().is_negative() {
                                f
                            } else {
                                &nil_amount
                            }
                        }
                        _ => f,
                    });
                let weights = ValuedAmount::into_normalized_weights(equity_flows);
                if weights.is_none() {
                    for val in net_equity_flows_without_uoa() {
                        if val.value_in(uoa).is_none() {
                            valuer::value_with_entry_and_write(
                                &mut writeable_entry,
                                val.amount().unit(),
                                uoa,
                            )?;
                        }
                    }
                    continue 'restart;
                } else {
                    weights
                }
            } else {
                None
            };
            break;
        }*/

        /*
        // Now we have the normalised weights, we can create the deals.
        // Filter after enumeration to ensure the `ith` element maps to `equity_normalised_weights[i]`.
        for (i, net_equity_flow) in net_equity_flows_without_uoa()
            .enumerate()
            .filter(|(_, f)| included_net_equity_vals.iter().any(|v| v.unit() == f.unit()))
        {
            /*
            let mut expenses = if expense_vals_sum.amount() > 0 {
                expense_vals_sum.split_weighted(equity_normalised_weights.as_ref().unwrap(), i)
            } else {
                ValuedAmount::nil()
            };*/
            let mut expenses = expenses_division.get_expenses(i);
            expenses = expenses.without_unit(net_equity_flow.unit());

            let deal = Deal::new(None, entry, net_equity_flow.clone(), expenses, None, false);
            deals.push(DealingEvent::Deal(deal));
        }
        Ok(deals)*/
    }

    fn scan_net_equity_flows<'a, 'h>(
        valued_entry: &'a JournalEntry<'h>,
        existing_entry: &'h JournalEntry<'h>,
        unit_filter: impl Filter<Unit<'h>>,
        initial_uoa: impl Fn(&DealGroup<'h>) -> &'h Unit<'h>,
    ) -> ValueResult<'h, NetEquityFlowsAndMetadataEvents<'h>> {
        let flows = Some(valued_entry.flows());
        let net_equity_flows = flows.as_ref().unwrap().net_equity().collect::<Vec<_>>();
        let cg_metadata = existing_entry.cg_metadata()?;

        // These are the units we're dealing in. This still includes the UOA as we can't be sure what it is
        // until the deal is pushed to a pool.
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
                flows: flows.unwrap().net_equity().map(Flow::into_valued_amount).collect(),
                included_units: units_to_scan,
                uoa,
            }),
            metadata_events,
        })
    }

    /*
    /// Get the sum of all expenses in the entry. Expenses are those postings whose accounts are tagged with
    /// `CGT-Expense`. This will initially attempt to sum those `ValuedAmounts` which may fail if they do
    /// not share a common unit. In this case, we force a valuation using `value_unit`.
    ///
    /// The expenses returned will be valued in the `value_unit`, or at least one other unit. Otherwise,
    /// a valuation in the `value_unit` will be forced.
    fn expenses_sum<'a, 'h>(
        entry: &'a mut JournalEntry<'h>,
        value_unit: &'h Unit<'h>,
    ) -> JournResult<ValuedAmount<'h>> {
        const ALLOWABLE_EXPENSE_TAG: &str = "CAG-DealExpense";

        'restart: loop {
            let expense_vals_sum = entry
                .postings()
                .filter(|p| p.account().has_tag(ALLOWABLE_EXPENSE_TAG))
                .map(Posting::valued_amount)
                .sum::<Option<ValuedAmount<'h>>>();
            match expense_vals_sum {
                Some(val) if val.is_nil() => break 'restart Ok(val),
                Some(val) if val.units().count() == 1 && val.value_in(value_unit).is_none() => {
                    valuer::value_with_entry_and_write(entry, val.unit(), value_unit)?;
                    continue 'restart;
                }
                Some(val) => break 'restart Ok(val),
                None => {
                    let expense_vals: Vec<_> = entry
                        .postings()
                        .filter(|p| p.account().has_tag(ALLOWABLE_EXPENSE_TAG))
                        .map(Posting::valued_amount)
                        .cloned()
                        .collect();

                    for val in expense_vals {
                        let base_unit = val.unit();
                        if val.value_in(value_unit).is_none() {
                            valuer::value_with_entry_and_write(entry, base_unit, value_unit)?;
                        }
                    }
                    continue 'restart;
                }
            }
        }
    }*/
}

struct NetEquityFlows<'h> {
    flows: SmallVec<[ValuedAmount<'h>; 4]>,
    included_units: SmallVec<[&'h Unit<'h>; 4]>,
    uoa: &'h Unit<'h>,
}

impl<'h> NetEquityFlows<'h> {
    pub fn all_flows_no_uoa(&self) -> impl Iterator<Item = &ValuedAmount<'h>> + Clone + '_ {
        self.flows.iter().filter(move |f| f.unit() != self.uoa)
    }

    pub fn included_flows(&self) -> impl Iterator<Item = (usize, &ValuedAmount<'h>)> + '_ {
        self.all_flows_no_uoa()
            .enumerate()
            .filter(move |(_, f)| self.included_units.iter().any(|u| *u == f.unit()))
    }
}

struct NetEquityFlowsAndMetadataEvents<'h> {
    neq_flows: Option<NetEquityFlows<'h>>,
    metadata_events: Vec<DealingEvent<'h>>,
}

impl<'h> NetEquityFlowsAndMetadataEvents<'h> {
    pub fn take_metadata_events(&mut self) -> Vec<DealingEvent<'h>> {
        std::mem::take(&mut self.metadata_events)
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
                            if let Err(rejected_deal) = group.try_push(deal) {
                                // All groups to i inclusive are complete and can be flushed.
                                if group.is_complete(rejected_deal.datetime()) {
                                    flush_to_offset = Some(i);
                                }
                                deal = rejected_deal;
                            } else {
                                break 'outer flush_to_offset;
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
        let cmd = Arguments::get().cast_cmd::<CagCommand>().unwrap();
        let criteria = if cmd.group_deals_by_date {
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
