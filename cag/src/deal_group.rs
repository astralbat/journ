/*
 * Copyright (c) 2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjustment::Adjustment;
use crate::cgt_configuration::{CagCommand, CgtConfiguration};
use crate::deal::{Deal, DealId};
use crate::deal_holding::AverageDealHolding;
use crate::module_init::MODULE_NAME;
use crate::pool::PoolBalance;
use crate::ruleset::{AgeUnit, Condition, DealKind, Rule};
use chrono::{DateTime, Duration, NaiveDate};
use chrono_tz::Tz;
use journ_core::alloc::HerdAllocator;
use journ_core::amount::{Amount, Quantity};
use journ_core::arguments::Arguments;
use journ_core::configuration::Configuration;
use journ_core::date_and_time::{JDateTime, JDateTimeRange};
use journ_core::err;
use journ_core::error::{JournError, JournResult};
use journ_core::journal_entry::JournalEntry;
use journ_core::unit::Unit;
use journ_core::valued_amount::ValuedAmount;
use journ_core::valuer::{SystemValuer, Valuer};
use linked_hash_set::LinkedHashSet;
use std::fmt;
use yaml_rust::Yaml;

/// A logical grouping of deals that are processed as one. This will usually be used to group together deals made on the same day.
///
/// A `DealGroup` can never be empty; there will always be at least one deal in the group.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DealGroup<'h> {
    //holding: Box<DealHolding<'h>, &'h HerdAllocator<'h>>,
    id: DealId<'h>,
    /// The group will always have a unit of account: explicit or implied. Initially, this will be implied bo be the first valuation on the `holding`,
    /// or if none, the primary unit of the `holding`. When added to a pool, it should be changed to the pool's accounting unit.
    unit_of_account: Option<&'h Unit<'h>>,
    criteria: DealGroupCriteria,
    next_rule: &'h [Rule],
    deals: Vec<Deal<'h>>,
    holding: AverageDealHolding<'h>,
    //holding: Box<DealHolding<'h>, &'h HerdAllocator<'h>>,
    //valued_amount: ValuedAmount<'h>,
    //allowable_expenses: ValuedAmount<'h>,
    taxable_gain: Option<ValuedAmount<'h>>,
    //balance: PoolBalance<'h>,
    split_parent: Option<Box<DealGroup<'h>>>,
}
impl<'h> DealGroup<'h> {
    /// Creates a new group with the given initial deal and criteria.
    pub fn new(criteria: DealGroupCriteria, deal: Deal<'h>) -> Self {
        let cgt_config =
            deal.entry().config().module_config::<CgtConfiguration>(MODULE_NAME).unwrap();
        let next_rule = cgt_config.ruleset();
        let id = deal.id();
        let holding = AverageDealHolding::from(&deal);

        DealGroup {
            unit_of_account: None,
            id,
            criteria,
            next_rule,
            holding,
            taxable_gain: deal.taxable_gain().map(|tg| tg.clone()),
            deals: vec![deal],
            split_parent: None,
        }
    }

    pub fn id(&self) -> DealId<'h> {
        self.id
    }

    pub fn allocator(&self) -> &'h HerdAllocator<'h> {
        self.deals[0].allocator()
    }

    pub fn unit(&self) -> &'h Unit<'h> {
        self.deals[0].unit()
    }

    pub fn total(&self) -> &PoolBalance<'h> {
        self.holding.total()
    }

    pub fn total_before_expenses(&self) -> &ValuedAmount<'h> {
        self.holding.total_before_expenses()
    }

    pub fn expenses(&self) -> &ValuedAmount<'h> {
        self.holding.expenses()
    }

    /*
    pub fn adjustments(&self) -> &[Adjustment<'h>] {
        self.holding.adjustments()
    }*/

    pub fn taxable_gain(&self) -> Option<&ValuedAmount<'h>> {
        self.taxable_gain.as_ref()
    }

    /// The corresponding journal entries for deals in this group. There will
    /// always be at least one entry.
    pub fn entries(&self) -> impl Iterator<Item = &'h JournalEntry<'h>> + '_ {
        self.deals.iter().map(|d| d.entry())
    }

    /*
    pub fn first_deal(&self) -> &Deal<'h> {
        self.holding.root_parent().first_deal()
    }*/

    pub fn last_deal(&self) -> &Deal<'h> {
        self.deals.last().unwrap()
    }

    pub fn deal_iter(&self) -> Box<dyn Iterator<Item = &Deal<'h>> + '_> {
        Box::new(self.deals.iter())
    }

    /// Splits this deal in to two parts at the amount threshold.
    /// To be successful, the sign of the `amount` should be the same as the deal.
    /// An error is returned if the operation does not result in any split.
    pub(crate) fn split_max(self, amount: Quantity) -> Result<(Self, Option<Self>), Self> {
        let balance_before = self.total().amount().quantity();
        let split_parent = self.clone();

        match self.holding.split_max(amount) {
            Ok((left, rem)) => {
                let split_percent = left.total().amount().quantity() / balance_before;

                // Split taxable gain
                let (left_tg, right_tg) = self
                    .taxable_gain
                    .clone()
                    .map(|tg| {
                        let tg_amount = (tg.amount() * split_percent).rounded().quantity();
                        tg.split(tg_amount)
                    })
                    .map(|(tg1, tg2)| (Some(tg1), Some(tg2)))
                    .unwrap_or((None, None));

                let left = Self {
                    holding: left,
                    deals: self.deals.clone(),
                    taxable_gain: left_tg,
                    split_parent: Some(Box::new(split_parent.clone())),
                    ..self
                };
                if let Some(rem) = rem {
                    Ok((
                        left,
                        Some(Self {
                            holding: rem,
                            deals: self.deals,
                            taxable_gain: right_tg,
                            split_parent: Some(Box::new(split_parent)),
                            ..self
                        }),
                    ))
                } else {
                    Ok((left, None))
                }
            }
            Err(holding) => {
                let taxable_gain = self.taxable_gain;
                let deals = self.deals;
                let sp = self.split_parent;
                Err(Self { holding, deals, taxable_gain, split_parent: sp, ..self })
            }
        }
    }

    /*
    pub fn split_max(self, max: Quantity) -> Result<(DealGroup<'h>, Option<Self>), Self> {
        let allocator = self.allocator();
        match self.holding.split_max(max, MatchMethod::Average) {
            Ok((left_bal, right_bal)) => {
                let criteria = self.criteria;
                let next_rule = self.next_rule;
                let entries = self.entries;
                let id = self.id.clone();

                let left = DealGroup {
                    id,
                    holding: Box::new_in(left_bal, allocator),
                    criteria,
                    next_rule,
                    entries: entries.clone(),
                };
                let right = right_bal.map(|right_bal| DealGroup {
                    id,
                    holding: Box::new_in(right_bal, allocator),
                    criteria,
                    next_rule,
                    entries,
                });
                Ok((left, right))
            }
            Err(holding) => Err(DealGroup {
                id: self.id,
                holding: Box::new_in(holding, allocator),
                criteria: self.criteria,
                next_rule: self.next_rule,
                entries: self.entries,
            }),
        }

        /*
        let criteria = self.criteria;
        let next_rule = self.next_rule;
        let allocator = self.allocator();
        let split_parent = Some(Box::new_in(DealHolding::Group(self.clone()), allocator));
        let orig_parent = self.split_parent;

        let mut split_amount_rem = max;
        let mut left_deals = Vec::new_in(allocator);
        let mut right_deals = Vec::new_in(allocator);

        for deal in self.deals {
            if split_amount_rem.is_zero() {
                right_deals.push(deal);
                continue;
            }
            match deal.split_max(split_amount_rem) {
                Ok((left, right)) => {
                    split_amount_rem -= left.total().amount().quantity();
                    left_deals.push(left);
                    if let Some(right) = right {
                        right_deals.push(right);
                    }
                }
                Err(deal) => right_deals.push(deal),
            }
        }
        if left_deals.is_empty() {
            return Err(DealGroup {
                id: self.id,
                balance: Self::calc_balance(&right_deals, allocator),
                deals: right_deals,
                criteria,
                next_rule,
                split_parent: orig_parent,
            });
        }
        let left = DealGroup {
            id: self.id,
            balance: Self::calc_balance(&left_deals, allocator),
            deals: left_deals,
            criteria,
            next_rule,
            split_parent: split_parent.clone(),
        };
        let right = if right_deals.is_empty() {
            None
        } else {
            Some(DealGroup {
                id: self.id,
                balance: Self::calc_balance(&right_deals, allocator),
                deals: right_deals,
                criteria,
                next_rule,
                split_parent,
            })
        };
        Ok((left, right))*/
    }*/

    /// Attempts to push a deal into the group. If the deal does not match the criteria, it is returned.
    pub fn try_push(&mut self, deal: Deal<'h>) -> Result<(), Deal<'h>> {
        if !self.criteria.matches(&deal) {
            return Err(deal);
        }
        match self.holding.add_deal(&deal) {
            Ok(_) => {
                self.deals.push(deal);
                Ok(())
            }
            Err(_) => Err(deal),
        }
    }

    pub fn split_parent(&self) -> Option<&DealGroup<'h>> {
        self.split_parent.as_deref()
    }

    pub fn root_parent(&self) -> &DealGroup<'h> {
        match self.split_parent() {
            Some(parent) => parent.root_parent(),
            None => self,
        }
    }

    pub fn add_adjustment(&mut self, adj: Adjustment<'h>) -> JournResult<()> {
        self.holding.add_adjustment(adj)
    }

    pub fn entry_description(&self) -> LinkedHashSet<&'h str> {
        let mut descriptions = LinkedHashSet::new();
        for entry in self.entries() {
            descriptions.insert(entry.description());
        }
        descriptions
    }

    pub fn metadata(&self, tag: &str) -> LinkedHashSet<String> {
        let mut tag_vals = LinkedHashSet::new();
        for entry in self.entries() {
            tag_vals.extend(entry.metadata_tag_values(tag));
        }
        tag_vals
    }

    pub fn datetime(&self) -> JDateTimeRange<'h> {
        let start = self.deals[0].datetime().start();
        let end = self.deals.last().unwrap().datetime().end();
        JDateTimeRange::new(start, Some(end))
    }

    /// Advances to the next rule, returning the previous one.
    pub fn advance_rule(&mut self) -> Option<&'h Rule> {
        match self.next_rule {
            [head, tail @ ..] => {
                self.next_rule = tail;
                Some(head)
            }
            [] => None,
        }
    }

    pub fn next_rules(&self) -> &'h [Rule] {
        self.next_rule
    }

    pub fn set_next_rules(&mut self, rules: &'h [Rule]) {
        self.next_rule = rules;
    }

    /// Gets whether the group is complete after this `datetime`.
    pub fn is_complete(&self, datetime: JDateTimeRange<'h>) -> bool {
        self.criteria.is_complete(datetime)
    }

    /// Gets whether this deal matches the specified condition at the specified clock time.
    pub fn matches(&self, cond: &Condition, clock: JDateTime<'h>) -> bool {
        let datetime = self.datetime();

        match cond {
            Condition::True => true,
            Condition::False => false,
            Condition::Age(age, age_unit) => match age_unit {
                AgeUnit::Days => {
                    clock.naive_utc() - datetime.start().datetime().naive_utc()
                        >= Duration::days(*age as i64)
                }
                AgeUnit::CalDays => {
                    clock.utc_date() - datetime.start().utc_date() >= Duration::days(*age as i64)
                }
            },
            Condition::Kind(kind) => match kind {
                DealKind::Buy => self.is_acquisition(),
                DealKind::Sell => self.is_disposal(),
            },
            Condition::And(left, right) => self.matches(left, clock) && self.matches(right, clock),
        }
    }

    pub fn is_acquisition(&self) -> bool {
        self.total().amount().is_positive()
    }

    pub fn is_disposal(&self) -> bool {
        self.total().amount().is_negative()
    }

    /// Ensure that all of the group's components can be valued in the specified `unit`.
    /// If not, attempt to perform a valuation on the underlying entries which will first try to derive the valuation,
    /// and fallback to using the price lookup functionality.
    fn value_with_system_valuer(&mut self, unit: &'h Unit<'h>) -> JournResult<()> {
        let deals = &self.deals;
        let mut avg_entry_valuer = |base_amount: Amount<'h>, quote_unit: &'h Unit<'h>| {
            let mut total_amount = Amount::nil();
            let mut total_value = Amount::nil();
            for deal in deals {
                total_amount += deal.valued_amount().amount();
                match SystemValuer::from(deal.entry())
                    .value(deal.valued_amount().amount(), quote_unit)?
                {
                    Some(val) => total_value += val,
                    None => {
                        return Err(err!("No value for deal: {}", deal));
                    }
                }
            }
            Ok(Some(total_value / total_amount.quantity() * base_amount.quantity()))
        };

        self.holding.value_in_or_value_with(unit, &mut avg_entry_valuer)?;
        self.taxable_gain
            .as_mut()
            .map(|tg| tg.value_in_or_value_with(unit, &mut avg_entry_valuer).map(Some))
            .unwrap_or(Ok(None))?;
        Ok(())
    }

    fn value_with_date_valuer(
        &mut self,
        unit: &'h Unit<'h>,
        config: &Configuration<'h>,
        value_date: DateTime<Tz>,
    ) -> JournResult<()> {
        let date_valuer = |config: Configuration<'h>, value_date: DateTime<Tz>| {
            move |base_amount: Amount<'h>, quote_unit: &'h Unit<'h>| match SystemValuer::on_date(
                config.clone(),
                JDateTime::from_datetime(
                    value_date,
                    Some(config.date_format()),
                    Some(config.time_format()),
                ),
            )
            .value(base_amount, quote_unit)?
            {
                Some(val) => Ok(Some(val.rounded())),
                None => Err(err!("No value for amount: {}", base_amount)),
            }
        };
        self.holding.value_with(&mut date_valuer(config.clone(), value_date), unit)?;
        self.taxable_gain
            .as_mut()
            .map(|tg| tg.value_with(&mut date_valuer(config.clone(), value_date), unit));
        Ok(())
    }

    pub fn set_unit_of_account(
        &mut self,
        uoa: &'h Unit<'h>,
        config_and_value_date: Option<(&Configuration<'h>, DateTime<Tz>)>,
    ) -> Result<(), JournError> {
        match config_and_value_date {
            Some((config, value_date)) => {
                self.value_with_date_valuer(uoa, config, value_date)?;
            }
            None => {
                self.value_with_system_valuer(uoa)?;
            }
        }
        self.unit_of_account = Some(uoa);
        // Put in terms of the uoa. This makes more sense when printing.
        self.holding.set_in_terms_of_unit_of_account(uoa);
        Ok(())
    }
}

impl fmt::Display for DealGroup<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, deal) in self.deal_iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            write!(f, "{}", deal)?;
        }
        Ok(())
    }
}

impl From<&DealGroup<'_>> for Yaml {
    fn from(value: &DealGroup<'_>) -> Self {
        let mut map = yaml_rust::yaml::Hash::new();
        let expenses = value.expenses();
        let total = value.total().valued_amount().clone();
        let total_before_expenses = value.total_before_expenses();

        map.insert(Yaml::String("total".to_string()), (&total).into());
        map.insert(Yaml::String("expenses".to_string()), expenses.into());
        map.insert(Yaml::String("total_before_expenses".to_string()), total_before_expenses.into());
        map.insert(Yaml::String("datetime".to_string()), value.datetime().into());
        map.insert(
            Yaml::String("remainder".to_string()),
            Yaml::Boolean(value.root_parent().total().amount() != value.total().amount()),
        );
        Yaml::Hash(map)
    }
}

/// The criteria used to determine how deals are grouped together.
/// All deals within the group will satisfy the same criteria.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DealGroupCriteria {
    /// One deal, one group. This is the simplest grouping.
    Single,
    /// Group deals together if they occur on the same day (as determined by the reporting timezone),
    /// and have the same sign (i.e. all buys or all sells).
    SameDaySameSign(NaiveDate, bool),
}
impl DealGroupCriteria {
    pub fn same_day_same_sign(deal: &Deal) -> Self {
        let tz = Arguments::get().cast_cmd::<CagCommand>().unwrap().datetime_args.timezone;
        let deal_date = deal.datetime().start().datetime().with_timezone(&tz).date_naive();
        DealGroupCriteria::SameDaySameSign(deal_date, deal.total().amount().is_positive())
    }

    /// Gets whether the `deal` matches the criteria.
    pub fn matches(&self, deal: &Deal) -> bool {
        match self {
            DealGroupCriteria::SameDaySameSign(date, sign) => {
                let tz = Arguments::get().cast_cmd::<CagCommand>().unwrap().datetime_args.timezone;
                let deal_date = deal.datetime().start().datetime().with_timezone(&tz).date_naive();
                deal_date == *date && deal.total().amount().is_positive() == *sign
            }
            DealGroupCriteria::Single => false,
        }
    }

    pub fn is_complete(&self, datetime: JDateTimeRange) -> bool {
        match self {
            DealGroupCriteria::SameDaySameSign(date, _) => {
                let tz = Arguments::get().cast_cmd::<CagCommand>().unwrap().datetime_args.timezone;
                let date_rhs = datetime.start().datetime().with_timezone(&tz).date_naive();
                date_rhs > *date
            }
            DealGroupCriteria::Single => true,
        }
    }
}
