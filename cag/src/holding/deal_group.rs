/*
 * Copyright (c) 2024-2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjusted_value::AdjustedValue;
use crate::adjustment::Adjustment;
use crate::cgt_configuration::CagConfiguration;
use crate::deal::Deal;
use crate::holding::deal_holding::HoldingHandle;
use crate::holding::summary::DealHoldingSummary;
use crate::holding::{AverageDealHolding, DealHolding};
use crate::module_init::MODULE_NAME;
use crate::report::cag_command::CagCommand;
use crate::ruleset::{AgeUnit, Condition, DealKind, Rule};
use chrono::{DateTime, Duration, NaiveDate};
use chrono_tz::Tz;
use journ_core::amount::{Amount, Quantity};
use journ_core::configuration::Configuration;
use journ_core::datetime::{DateTimePrecision, JDateTime, JDateTimeRange};
use journ_core::err;
use journ_core::error::{JournError, JournResult};
use journ_core::journal_context::JContext;
use journ_core::journal_entry::JournalEntry;
use journ_core::metadata::Metadata;
use journ_core::report::command::arguments::Command;
use journ_core::unit::Unit;
use journ_core::valuer::{SystemValuer, Valuer};
use linked_hash_set::LinkedHashSet;
use std::cell::{Ref, RefCell};
use std::fmt;
use std::rc::Rc;

pub enum PushError<'h> {
    CriteriaMismatch(Deal<'h>),
    EvalError(JournError),
}

/// A logical grouping of deals that are processed as one. This will usually be used to group together deals made on the same day.
///
/// A `DealGroup` can never be empty; there will always be at least one deal in the group.
#[derive(Debug, PartialEq, Eq)]
pub struct DealGroup<'h> {
    id: usize,
    criteria: DealGroupCriteria,
    next_rule: &'h [Rule],
    holding: HoldingHandle<'h>,
    split_parent: Option<Rc<DealHoldingSummary<'h>>>,
}
impl<'h> DealGroup<'h> {
    /// Creates a new group with the given initial deal and criteria.
    pub fn new(criteria: DealGroupCriteria, deal: Deal<'h>) -> Self {
        let cgt_config =
            deal.entry().config().module_config::<CagConfiguration>(MODULE_NAME).unwrap();
        let next_rule = cgt_config.ruleset();
        let holding = AverageDealHolding::new(deal);

        static ID_COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        let id = ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        DealGroup { id, criteria, next_rule, holding, split_parent: None }
    }

    pub fn id(&self) -> usize {
        self.id
    }

    pub fn amount(&self) -> Amount<'h> {
        self.holding.borrow().amount()
    }

    pub fn unit(&self) -> &'h Unit<'h> {
        self.holding.borrow().amount().unit()
    }

    pub fn unit_of_account(&self) -> &'h Unit<'h> {
        self.holding.borrow().value().unit()
    }

    pub fn adjusted_value(&self) -> AdjustedValue<'h> {
        self.holding.borrow().adjusted_value()
    }

    pub fn value(&self) -> Amount<'h> {
        self.holding.borrow().value()
    }

    pub fn consideration(&self) -> Amount<'h> {
        self.holding.borrow().consideration()
    }

    pub fn expenses(&self) -> Amount<'h> {
        self.holding.borrow().expenses()
    }

    /// The corresponding journal entries for deals in this group. There will
    /// always be at least one entry.
    pub fn entries(&self) -> Box<dyn Iterator<Item = Ref<&'h JournalEntry<'h>>> + '_> {
        self.holding.borrow().entries().map(|entry| Ref::map(self.holding.borrow(), |_| entry))
    }

    pub fn with_entries<F, R>(&self, f: F) -> R
    where
        F: FnOnce(Box<dyn Iterator<Item = &'h JournalEntry<'h>> + '_>) -> R,
    {
        let holding = self.holding.borrow();
        f(Box::new(holding.entries().map(|entry| entry)))
    }

    pub fn into_deal_iter(self) -> impl Iterator<Item = Deal<'h>> + 'h {
        self.holding.take().into_deal_iter()
    }

    pub fn deal_iter(&self) -> impl Iterator<Item = &Deal<'h>> + '_ {
        self.holding.deal_iter()
    }

    /// Splits this deal in to two parts at the amount threshold.
    /// To be successful, the sign of the `amount` should be the same as the deal.
    /// An error is returned if the operation does not result in any split.
    pub(crate) fn split_max(self, amount: Quantity) -> Result<(Self, Option<Self>), Box<Self>> {
        let self_summary = Rc::new(DealHoldingSummary::new(
            self.split_parent.clone(),
            self.holding.datetime(),
            self.holding.adjusted_value(),
        ));

        match self.holding.split_max(amount) {
            Ok((left, rem)) => {
                let id = self.id;
                let criteria = self.criteria;
                let next_rule = self.next_rule;

                let left = Self {
                    id,
                    holding: left,
                    split_parent: Some(Rc::clone(&self_summary)),
                    criteria,
                    next_rule,
                };
                if let Some(rem) = rem {
                    Ok((
                        left,
                        Some(Self {
                            id,
                            holding: rem,
                            split_parent: Some(self_summary),
                            criteria,
                            next_rule,
                        }),
                    ))
                } else {
                    Ok((left, None))
                }
            }
            Err(holding) => {
                //let sp = self.split_parent;
                Err(Box::new(Self { holding, ..self }))
            }
        }
    }

    /// Attempts to push a deal into the group. If the deal does not match the criteria, it is returned.
    #[allow(clippy::result_large_err)]
    pub fn try_push(&mut self, deal: Deal<'h>) -> Result<(), PushError<'h>> {
        if !self.criteria.matches(&deal) {
            return Err(PushError::CriteriaMismatch(deal));
        }
        self.holding.borrow_mut().add_deal(deal);
        Ok(())
    }

    pub fn split_parent(&self) -> Option<&Rc<DealHoldingSummary<'h>>> {
        self.split_parent.as_ref()
    }

    pub fn add_adjustment(&mut self, adj: Adjustment<'h>) -> JournResult<()> {
        self.holding.borrow_mut().add_adjustment(adj)
    }

    pub fn entry_description(&self) -> LinkedHashSet<&'h str> {
        let mut descriptions = LinkedHashSet::new();
        self.with_entries(|entries| {
            for entry in entries {
                descriptions.insert(entry.description());
            }
        });
        descriptions
    }

    pub fn entry_metadata_by_key(&self, key: &str) -> LinkedHashSet<&Metadata<'h>> {
        let mut vals = LinkedHashSet::new();
        self.with_entries(|entries| {
            for entry in entries {
                for md in entry.metadata_by_key(key) {
                    vals.insert(md);
                }
            }
        });
        vals
    }

    pub fn datetime(&self) -> JDateTimeRange {
        self.holding.borrow().datetime()
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
        self.amount().is_positive()
    }

    pub fn is_disposal(&self) -> bool {
        self.amount().is_negative()
    }

    /// Ensure that all of the group's components can be valued in the specified `unit`.
    /// If not, attempt to perform a valuation on the underlying entries which will first try to derive the valuation,
    /// and fallback to using the price lookup functionality.
    fn value_with_system_valuer(&mut self, unit: &'h Unit<'h>) -> JournResult<()> {
        self.holding.value_with_system_valuer(unit).map_err(|e| {
            err!("Unable to value deal group: {}", self.holding.amount()).with_source(e)
        })
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
        Ok(())
    }

    pub fn set_value_on_date(
        &mut self,
        uoa: &'h Unit<'h>,
        config: &Configuration<'h>,
        value_date: DateTime<Tz>,
    ) -> Result<(), JournError> {
        self.value_with_date_valuer(uoa, config, value_date)
    }

    pub fn ensure_valued(&mut self, uoa: &'h Unit<'h>) -> Result<(), JournError> {
        self.value_with_system_valuer(uoa)
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
        let tz = JContext::get().cast_cmd::<CagCommand>().datetime_fmt_cmd().timezone_or_default();
        let deal_date = deal.datetime().start().datetime().with_timezone(&tz).date_naive();
        DealGroupCriteria::SameDaySameSign(deal_date, deal.amount().is_positive())
    }

    /// Gets whether the `deal` matches the criteria.
    pub fn matches(&self, deal: &Deal) -> bool {
        match self {
            DealGroupCriteria::SameDaySameSign(date, sign) => {
                let tz = JContext::get()
                    .cast_cmd::<CagCommand>()
                    .datetime_fmt_cmd()
                    .timezone_or_default();
                let deal_date = deal.datetime().start().datetime().with_timezone(&tz).date_naive();
                deal_date == *date && deal.amount().is_positive() == *sign
            }
            DealGroupCriteria::Single => false,
        }
    }

    pub fn is_complete(&self, datetime: JDateTimeRange) -> bool {
        match self {
            DealGroupCriteria::SameDaySameSign(date, _) => {
                let tz = JContext::get()
                    .cast_cmd::<CagCommand>()
                    .datetime_fmt_cmd()
                    .timezone_or_default();
                let date_rhs = datetime.start().datetime().with_timezone(&tz).date_naive();
                date_rhs > *date
            }
            DealGroupCriteria::Single => true,
        }
    }
}
