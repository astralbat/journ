/*
 * Copyright (c) 2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjustment::Adjustment;
use crate::cgt_configuration::CagConfiguration;
use crate::deal::{Deal, DealId};
use crate::deal_holding::AverageDealHolding;
use crate::module_init::MODULE_NAME;
use crate::pool::PoolBalance;
use crate::report::cag_command::CagCommand;
use crate::ruleset::{AgeUnit, Condition, DealKind, Rule};
use chrono::{DateTime, Duration, NaiveDate};
use chrono_tz::Tz;
use journ_core::alloc::HerdAllocator;
use journ_core::amount::{Amount, Quantity};
use journ_core::configuration::Configuration;
use journ_core::datetime::{DateTimePrecision, JDateTime, JDateTimeRange};
use journ_core::err;
use journ_core::error::{JournError, JournResult};
use journ_core::journal_entry::JournalEntry;
use journ_core::metadata::Metadata;
use journ_core::report::command::arguments::{Cmd, Command};
use journ_core::unit::Unit;
use journ_core::valued_amount::ValuedAmount;
use journ_core::valuer::{SystemValuer, Valuation, ValuationError, Valuer};
use linked_hash_set::LinkedHashSet;
use std::fmt;
pub enum PushError<'h> {
    CriteriaMismatch(Deal<'h>),
    EvalError(JournError),
}

/// A logical grouping of deals that are processed as one. This will usually be used to group together deals made on the same day.
///
/// A `DealGroup` can never be empty; there will always be at least one deal in the group.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DealGroup<'h> {
    id: DealId<'h>,
    /// The group will always have a unit of account: explicit or implied. Initially, this will be implied to be the first valuation on the `holding`,
    /// or if none, the primary unit of the `holding`. When added to a pool, it should be changed to the pool's accounting unit.
    unit_of_account: Option<&'h Unit<'h>>,
    criteria: DealGroupCriteria,
    next_rule: &'h [Rule],
    deals: Vec<Deal<'h>>,
    holding: AverageDealHolding<'h>,
    taxable_gain: Option<ValuedAmount<'h>>,
    split_parent: Option<Box<DealGroup<'h>>>,
}
impl<'h> DealGroup<'h> {
    /// Creates a new group with the given initial deal and criteria.
    pub fn new(criteria: DealGroupCriteria, deal: Deal<'h>) -> Self {
        let cgt_config =
            deal.entry().config().module_config::<CagConfiguration>(MODULE_NAME).unwrap();
        let next_rule = cgt_config.ruleset();
        let id = deal.id();
        let holding = AverageDealHolding::from(&deal);

        DealGroup {
            unit_of_account: None,
            id,
            criteria,
            next_rule,
            holding,
            taxable_gain: deal.taxable_gain().cloned(),
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

    pub fn unit_of_account(&self) -> &'h Unit<'h> {
        self.deals[0].unit_of_account()
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

    pub fn taxable_gain(&self) -> Option<&ValuedAmount<'h>> {
        self.taxable_gain.as_ref()
    }

    /// The corresponding journal entries for deals in this group. There will
    /// always be at least one entry.
    pub fn entries(&self) -> impl Iterator<Item = &'h JournalEntry<'h>> + '_ {
        self.deals.iter().map(|d| d.entry())
    }

    pub fn last_deal(&self) -> &Deal<'h> {
        self.deals.last().unwrap()
    }

    pub fn deal_iter(&self) -> Box<dyn Iterator<Item = &Deal<'h>> + '_> {
        Box::new(self.deals.iter())
    }

    /// Splits this deal in to two parts at the amount threshold.
    /// To be successful, the sign of the `amount` should be the same as the deal.
    /// An error is returned if the operation does not result in any split.
    pub(crate) fn split_max(self, amount: Quantity) -> Result<(Self, Option<Self>), Box<Self>> {
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
                Err(Box::new(Self { holding, deals, taxable_gain, split_parent: sp, ..self }))
            }
        }
    }

    /// Attempts to push a deal into the group. If the deal does not match the criteria, it is returned.
    #[allow(clippy::result_large_err)]
    pub fn try_push(&mut self, deal: Deal<'h>) -> Result<(), PushError<'h>> {
        if !self.criteria.matches(&deal) {
            return Err(PushError::CriteriaMismatch(deal));
        }
        match self.holding.add_deal(&deal) {
            Ok(_) => {
                // Update taxable gain as the sum of the current group's and the incoming deal's.
                self.taxable_gain = match (&self.taxable_gain, deal.taxable_gain()) {
                    (Some(a), Some(b)) => {
                        let sum = a + b;
                        if sum.is_none() {
                            let a_units =
                                a.units().map(ToString::to_string).collect::<Vec<_>>().join(", ");
                            let b_units =
                                b.units().map(ToString::to_string).collect::<Vec<_>>().join(", ");
                            let source = err!("[{}] vs [{}]", a_units, b_units);
                            return Err(PushError::EvalError(err!("Unable to combine taxable gain for deal group. Incompatible units").with_source(source)));
                        }
                        sum
                    }
                    (None, Some(_)) | (Some(_), None) => {
                        return Err(PushError::EvalError(err!(
                            "Cannot mix deals with and without taxable gain in the same deal group: {}",
                            deal
                        )));
                    }
                    (None, None) => None,
                };

                self.deals.push(deal);
                Ok(())
            }
            Err(_) => Err(PushError::CriteriaMismatch(deal)),
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

    pub fn metadata_by_key(&self, key: &str) -> LinkedHashSet<&Metadata<'h>> {
        let mut vals = LinkedHashSet::new();
        for md in self.deals.iter().flat_map(|d| d.metadata().iter().filter(|m| m.key() == key)) {
            vals.insert(md);
        }
        vals
    }

    pub fn datetime(&self) -> JDateTimeRange {
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
    pub fn is_complete(&self, datetime: JDateTimeRange) -> bool {
        self.criteria.is_complete(datetime)
    }

    /// Gets whether this deal matches the specified condition at the specified clock time.
    pub fn matches(&self, cond: &Condition, clock: JDateTime) -> bool {
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
        let cgt_config = self
            .entries()
            .next()
            .unwrap()
            .config()
            .module_config::<CagConfiguration>(MODULE_NAME)
            .unwrap();
        let round_vals = cgt_config.round_deal_values();

        let deals = &self.deals;
        let mut avg_entry_valuer = |quote_unit: &'h Unit<'h>, base_amount: Amount<'h>| {
            let mut total_amount = Amount::nil();
            let mut total_value: Option<Valuation> = None;
            for deal in deals {
                total_amount += deal.valued_amount().amount();
                match SystemValuer::from(deal.entry())
                    .value(quote_unit, deal.valued_amount().amount())
                {
                    Ok(val) => {
                        total_value = total_value.and_then(|tv| &tv + &val).or(Some(val));
                    }
                    Err(ValuationError::Undetermined(reason)) => {
                        return Err(ValuationError::EvalFailure(
                            err!("No value for deal: {}", deal,).with_source(reason),
                        ));
                    }
                    Err(e) => return Err(e),
                }
            }
            let mut total_value = total_value.unwrap();
            total_value
                .revalue(*total_value / total_amount.quantity() * base_amount.quantity())
                .unwrap();
            Ok(total_value)
        };

        if self.taxable_gain.is_none() {
            self.holding.value_in_or_value_with(unit, &mut avg_entry_valuer, round_vals).map_err(
                |e| err!("Unable to value deal group: {}", self.holding.amount()).with_source(e),
            )?;
        }
        Ok(())
    }

    fn value_with_date_valuer(
        &mut self,
        unit: &'h Unit<'h>,
        config: &Configuration<'h>,
        value_date: DateTime<Tz>,
    ) -> JournResult<()> {
        let cgt_config = self
            .entries()
            .next()
            .unwrap()
            .config()
            .module_config::<CagConfiguration>(MODULE_NAME)
            .unwrap();
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
        self.holding
            .value_with(&mut date_valuer(config.clone(), value_date), unit, round_vals)
            .map_err(|e| {
                err!("Unable to value deal group: {}", self.holding.amount()).with_source(e)
            })?;
        self.taxable_gain.as_mut().map(|tg| {
            tg.value_with(unit, &mut date_valuer(config.clone(), value_date), round_vals)
        });
        Ok(())
    }

    pub fn set_value_on_date(
        &mut self,
        uoa: &'h Unit<'h>,
        config: &Configuration<'h>,
        value_date: DateTime<Tz>,
    ) -> Result<(), JournError> {
        self.value_with_date_valuer(uoa, config, value_date)?;
        self.unit_of_account = Some(uoa);
        // Put in terms of the uoa. This makes more sense when printing.
        self.holding.set_in_terms_of_unit_of_account(uoa);
        Ok(())
    }

    pub fn ensure_valued(&mut self, uoa: &'h Unit<'h>) -> Result<(), JournError> {
        self.value_with_system_valuer(uoa)?;
        self.unit_of_account = Some(uoa);
        // Put in terms of the uoa. This makes more sense when printing and finding the cost later.
        self.holding.set_in_terms_of_unit_of_account(uoa);
        self.taxable_gain.as_mut().map(|tg| tg.order_valuation(uoa, 0));
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

/*
impl From<&DealGroup<'_>> for Yaml {
    fn from(value: &DealGroup<'_>) -> Self {
        let mut map = yaml_rust2::yaml::Hash::new();
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
}*/

/// The criteria used to determine how deals are grouped together.
/// All deals within the group will satisfy the same criteria.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DealGroupCriteria {
    /// One deal, one group. This is the simplest grouping.
    Single,
    /// Group deals together if they occur on the same day (as determined by the report timezone),
    /// and have the same sign (i.e. all buys or all sells).
    SameDaySameSign(NaiveDate, bool),
}
impl DealGroupCriteria {
    pub fn same_day_same_sign(deal: &Deal) -> Self {
        // It is important to make sure the timezone is consistent
        let tz = Cmd::cast::<CagCommand>().datetime_fmt_cmd().timezone_or_default();
        let deal_date = deal.datetime().start().datetime().with_timezone(&tz).date_naive();
        DealGroupCriteria::SameDaySameSign(deal_date, deal.total().amount().is_positive())
    }

    /// Gets whether the `deal` matches the criteria.
    pub fn matches(&self, deal: &Deal) -> bool {
        match self {
            DealGroupCriteria::SameDaySameSign(date, sign) => {
                let tz = Cmd::cast::<CagCommand>().datetime_fmt_cmd().timezone_or_default();
                let deal_date = deal.datetime().start().datetime().with_timezone(&tz).date_naive();
                deal_date == *date && deal.total().amount().is_positive() == *sign
            }
            DealGroupCriteria::Single => false,
        }
    }

    pub fn is_complete(&self, datetime: JDateTimeRange) -> bool {
        match self {
            DealGroupCriteria::SameDaySameSign(date, _) => {
                let tz = Cmd::cast::<CagCommand>().datetime_fmt_cmd().timezone_or_default();
                let date_rhs = datetime.start().datetime().with_timezone(&tz).date_naive();
                date_rhs > *date
            }
            DealGroupCriteria::Single => true,
        }
    }
}
