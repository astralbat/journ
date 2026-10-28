/*
 * Copyright (c) 2017-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjusted_value::AdjustedValue;
use crate::cag_configuration::CagConfiguration;
use crate::capital_gains::CapitalGains;
use crate::deal::Deal;
use crate::dealing_event::DealingEvent;
use crate::expenses::EntryExpenses;
use crate::holding::{DealHolding, SingleDealHolding};
use crate::metadata::{CAG_INCLUDE, CAG_ZERO_PROCEEDS, CapitalGainsMetadataAccess};
use crate::mod_cgt;
use crate::module_init::MODULE_NAME;
use crate::pool_manager::PoolManager;
use crate::report::cag_command::CagCommand;
use chrono::Utc;
use journ_core::configuration::Filter;
use journ_core::datetime::JDateTimeRange;
use journ_core::err;
use journ_core::error::BlockContext;
use journ_core::error::JournResult;
use journ_core::error::{BlockContextError, JournError};
use journ_core::journal::Journal;
use journ_core::journal_context::JContext;
use journ_core::journal_entry::JournalEntry;
use journ_core::journal_entry_flow::{Flows, LinkedFlow};
use journ_core::parsing::text_block::TextBlockBuf;
use journ_core::report::command::arguments::Command;
use journ_core::report::expr::{LinkedFlowContext, ScalarExpr};
use journ_core::unit::Unit;
use journ_core::valued_amount::{PostingValuation, ValuedAmount};
use journ_core::valuer::{SystemValuer, Valuer};
use log::info;
use smallvec::{SmallVec, smallvec};
use std::collections::HashSet;
use std::ops::RangeBounds;

#[derive(Default)]
pub struct CapitalGainsComputer {
    #[allow(dead_code)]
    warnings: Vec<JournError>,
}

impl<'h> CapitalGainsComputer {
    pub fn compute_gains(&mut self, journal: &Journal<'h>) -> Result<CapitalGains<'h>, JournError> {
        let cmd = JContext::get().cast_cmd::<CagCommand>();
        mod_cgt::register()?;

        // Use the account filter to decide which units we're going to calculate on.
        let account_filter = cmd.account_filter();
        let mut included_units: Vec<&'h Unit<'h>> = {
            #[allow(clippy::mutable_key_type)] // Unit only hashes/compares on its code.
            let mut units = HashSet::new();
            for entry in journal.entry_range(cmd.begin_and_end_cmd().begin_end_range()) {
                for pst in entry.postings() {
                    if account_filter.is_included(pst.account()) {
                        // Prefer the journal config for the unit over the posting's.
                        units.insert(
                            JContext::get()
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

        // We scan all entries with capital gains as not doing so will change
        // the result completely.
        for entry in journal.entry_range(..) {
            let entry_err = |e: JournError| {
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

            // Set configuration in context so that it may be used throughout our processing.
            JContext::get().set_config(entry.config().clone());

            pool_manager.update_configuration().map_err(entry_err)?;

            let events = self
                .scan_entry(entry, &unit_filter, pool_manager.unit_of_account())
                .map_err(entry_err)?;
            for event in events {
                let events = match event {
                    DealingEvent::Deal(deal) => {
                        if deal.amount().is_zero() {
                            continue;
                        }
                        pool_manager
                            .push_deal_holding(DealHolding::Single(SingleDealHolding::new(deal)))?
                    }
                    DealingEvent::PoolAdjustment(pa) => pool_manager.push_adjustments([pa])?,
                };
                pool_events.extend(events);
            }
        }

        pool_events.extend(pool_manager.progress_deals(JDateTimeRange::new(
            cmd.datetime_fmt_cmd().datetime_from_utc(&Utc::now().naive_utc()),
            None,
        ))?);

        // Now report on only those events in the full date range asked for.
        let from_to_range = cmd.begin_and_end_cmd().begin_end_range();
        pool_events.retain(|e| from_to_range.contains(&e.deal_datetime()));

        let cg = CapitalGains::new(pool_events);
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

        let round_values = entry
            .config()
            .module_config::<CagConfiguration>(MODULE_NAME)
            .unwrap()
            .round_deal_values();
        let mut valuer = SystemValuer::from(entry);
        let (mut implicit_flows, explicit_deals) = Self::scan_net_equity_flows(
            &mut valuer,
            entry,
            unit_of_account,
            &unit_filter,
            round_values,
        )?;

        let mut all_deals = explicit_deals;
        if implicit_flows.iter().any(|f| unit_filter.is_included(f.unit())) {
            // To divide expenses, we need at least one common unit across all flows. If there isn't one, we'll now value
            // them all in the unit of account. This may include units not in the unit filter.
            if ValuedAmount::common_units(implicit_flows.iter().map(|f| f.valued_amount()))
                .next()
                .is_none()
            {
                for flow in implicit_flows.iter_mut().filter(|f| f.unit() != unit_of_account) {
                    flow.valued_amount_mut().set_value_in_or_value_with(
                        unit_of_account,
                        &mut valuer,
                        round_values,
                    )?;
                }
            }

            let expenses_division = EntryExpenses::scan_and_divide(
                &mut valuer,
                entry,
                unit_of_account,
                implicit_flows.iter().map(|f| f.valued_amount()),
            )?;

            for (i, mut flow) in implicit_flows
                .into_iter()
                .enumerate()
                .filter(|(_, d)| unit_filter.is_included(d.unit()))
            {
                let mut expenses = expenses_division.get_expenses(i);
                if round_values {
                    expenses = expenses.rounded();
                }

                // Set the consideration to zero if the flow or its linked flow has the CAG-Zero-Proceeds metadata key.
                if flow.account_root().unwrap().has_metadata_key(&CAG_ZERO_PROCEEDS)
                    || flow.linked().account_root().unwrap().has_metadata_key(&CAG_ZERO_PROCEEDS)
                {
                    flow.valued_amount_mut().set_valuation(PostingValuation::new_total(
                        unit_of_account.with_quantity(0),
                        false,
                    ));
                }

                let mut consideration = flow
                    .valued_amount()
                    .value_in_or_value_with(unit_of_account, &mut valuer)?
                    .abs();
                if round_values {
                    consideration = consideration.rounded();
                }

                all_deals.push(Deal::new(
                    entry,
                    AdjustedValue::new(
                        flow.valued_amount().amount(),
                        if flow.valued_amount().amount().is_positive() {
                            consideration + expenses
                        } else {
                            consideration - expenses
                        },
                        expenses,
                    ),
                    Some(flow),
                    None,
                ));
            }
        }
        Ok(all_deals.into_iter().map(DealingEvent::Deal).collect())
        //})
        //.map(Ok)?
    }

    /// Scans the `valued_entry/existing_entry` for deals both explicit and implicit.
    ///
    /// Explicit deals override implicit ones for any particular unit.
    ///
    /// All deals need to be scanned here, not only just for the units we're interested
    /// in reporting on. This is so that expenses can be allocated across all deals accurately.
    fn scan_net_equity_flows<'a>(
        valuer: &mut impl Valuer<'h>,
        entry: &'h JournalEntry<'h>,
        uoa: &'h Unit<'h>,
        unit_filter: &impl Filter<Unit<'h>>,
        round_values: bool,
    ) -> JournResult<(SmallVec<[LinkedFlow<'h>; 4]>, SmallVec<[Deal<'h>; 4]>)> {
        // Add explicit deals
        let cg_metadata = entry.cg_metadata()?;
        let mut explicit_deals: SmallVec<[Deal<'h>; 4]> = smallvec![];
        for (mut valued_amount, expenses, taxable_gain, _md_position) in
            cg_metadata.into_deal_metadata()
        {
            if unit_filter.is_included(valued_amount.unit()) {
                if round_values {
                    valued_amount.make_all_valuations_total();
                    valued_amount.round_total_valuations();
                }

                let mut consideration = valued_amount.value_in_or_value_with(uoa, valuer)?.abs();
                if round_values {
                    consideration = consideration.rounded();
                }
                let expenses = expenses.value_in_or_value_with(uoa, valuer)?.abs();

                let deal = Deal::new(
                    entry,
                    AdjustedValue::new(
                        valued_amount.amount(),
                        if valued_amount.amount().is_positive() {
                            consideration + expenses
                        } else {
                            consideration - expenses
                        },
                        expenses,
                    ),
                    None,
                    taxable_gain
                        .map(|tg| tg.value_in_or_value_with(uoa, valuer).map(|v| v.abs()))
                        .transpose()?,
                );
                explicit_deals.push(deal);
            }
        }

        // If the entry doesn't contain any units we're interested in (aside from explicit deals/adjustments), we
        // can save some effort.
        if entry.units().iter().all(|unit| {
            !unit_filter.is_included(unit)
                || explicit_deals.iter().map(|deal| deal.unit()).any(|d_unit| d_unit == *unit)
        }) {
            return Ok((smallvec![], explicit_deals));
        }

        let flows = entry.flows();
        let linked_flows = flows.linked(valuer)?;

        let mut included_flows: SmallVec<[LinkedFlow<'h>; 4]> = smallvec![];
        let config = entry.config().module_config::<CagConfiguration>(MODULE_NAME).unwrap();
        for (debit, credit) in linked_flows.into_iter() {
            for i in 0..2 {
                let (flow, linked) = if i == 0 { (&debit, &credit) } else { (&credit, &debit) };

                // Already included explicitly, so skip this flow.
                if explicit_deals.iter().any(|deal| deal.unit() == flow.unit()) {
                    continue;
                }
                // Skip flows in the unit of account as they are not relevant to capital gains.
                if flow.unit() == uoa {
                    continue;
                }

                let mut context = LinkedFlowContext::new(flow, linked);

                let include = {
                    let include_override =
                        flow.account_root().unwrap().metadata_by_key(CAG_INCLUDE);
                    if !include_override.is_empty() {
                        let mut include = true;
                        for md in include_override {
                            include = include
                                && md
                                    .value()
                                    .map(|v| {
                                        v.parse().and_then(|expr: ScalarExpr| {
                                            Ok(expr.eval(&mut context)?.as_lenient_bool())
                                        })
                                    })
                                    .unwrap_or(Ok(true))?;
                        }
                        include
                    } else {
                        config.include_flows().unwrap().eval(&mut context)?.as_lenient_bool()
                    }
                };

                if include {
                    included_flows.push(LinkedFlow::new(flow.clone(), linked.clone()));
                }
            }
        }

        Ok((included_flows, explicit_deals))
    }

    /// Gets the initial unit of account for the first entry to be processed.
    fn initial_unit_of_account(journal: &Journal<'h>) -> JournResult<&'h Unit<'h>> {
        let mut unit_of_account = None;

        let entries = journal.entry_range(..);
        if let Some(entry) = entries.clone().next() {
            let cag_config = CagConfiguration::get(entry.config());
            if let Some(cag_config) = cag_config
                && let Some(uoac) = cag_config.unit_of_account_change()
            {
                unit_of_account = JContext::get().config().get_unit(uoac.unit_of_account());
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

#[cfg(test)]
mod tests {
    use crate::computer::CapitalGainsComputer;
    use crate::deal;
    use crate::dealing_event::DealingEvent;
    use crate::metadata::CapitalGainsMetadataAccess;
    use crate::report::cag_command::CagCommand;
    use indoc::indoc;
    use journ_core::configuration::{AccountFilter, UnitFilter};
    use journ_core::directive::{Directive, DirectiveKind};
    use journ_core::{entry, entry_dir, match_map, unit, val};
    use std::{assert_eq, iter};

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

        let mut comp = CapitalGainsComputer::default();
        let events = comp.scan_entry(entry, UnitFilter::new(unit!("€")), None).unwrap();
        assert_eq!(events, vec![DealingEvent::Deal(deal!("€25 ++ €5"))])
    }
}
