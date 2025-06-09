/*
 * Copyright (c) 2021-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::pool_event::{AggregatedPoolEvent, PoolEvent, PoolEventKind};
use crate::ruleset::{ActionRule, Rule, RuleSet};
use chrono::{DateTime, Duration};
use chrono_tz::Tz;
use journ_core::account::Account;
use journ_core::arguments::{Command, DateTimeArguments};
use journ_core::configuration::{create_unit_filter, AccountFilter, Filter};
use journ_core::directive::DirectiveKind;
use journ_core::err;
use journ_core::error::JournError;
use journ_core::ext::StrExt;
use journ_core::module::{
    ModuleConfiguration, ModuleConfigurationClone, ModuleConfigurationEq, ModuleDirectiveObj,
};
use journ_core::python::lambda::Lambda;
use journ_core::unit::Unit;
use std::any::Any;
use std::ops::{Bound, Deref, RangeBounds};
use std::str::FromStr;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::LazyLock;
use std::{cmp, fmt, mem};

pub static DEFAULT_POOL: &str = "Pool_1";

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MergeCgtError {
    MaxAgeNotAllowedFinalPool,
}

/// The configuration object for the capital gains tax module. This must have a static lifetime in order to
/// implement `ModuleConfiguration`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CgtConfiguration {
    round_deal_values: Option<bool>,
    assign_expenses: Option<AssignExpenses>,
    pools: Vec<PoolConfiguration>,
    comments: Vec<String>,
    ruleset: Option<RuleSet>,
    timezone: Option<Tz>,
    /// A change in the unit of account for all pools (unless overridden by pool configuration).
    unit_of_account_change: Option<UnitOfAccountChange>,
}

impl CgtConfiguration {
    pub fn empty() -> Self {
        Self {
            round_deal_values: None,
            assign_expenses: None,
            timezone: None,
            pools: vec![],
            comments: vec![],
            ruleset: None,
            unit_of_account_change: None,
        }
    }

    pub fn round_deal_values(&self) -> bool {
        self.round_deal_values.unwrap_or_default()
    }

    pub fn set_round_deal_values(&mut self, round_deal_values: bool) {
        self.round_deal_values = Some(round_deal_values)
    }

    pub fn assign_expenses(&self) -> AssignExpenses {
        self.assign_expenses.unwrap_or_default()
    }

    pub fn set_assign_expenses(&mut self, assign_expenses: AssignExpenses) {
        self.assign_expenses = Some(assign_expenses);
    }

    pub fn timezone(&self) -> Option<Tz> {
        self.timezone
    }

    pub fn set_timezone(&mut self, timezone: Tz) {
        self.timezone = Some(timezone)
    }

    pub fn set_comments(&mut self, comments: Vec<String>) {
        self.comments = comments
    }

    pub fn set_unit_of_account_change(
        &mut self,
        unit_of_account_change: Option<UnitOfAccountChange>,
    ) {
        self.unit_of_account_change = unit_of_account_change
    }

    pub fn unit_of_account_change(&self) -> Option<&UnitOfAccountChange> {
        self.unit_of_account_change.as_ref()
    }

    pub fn pools(&self) -> &Vec<PoolConfiguration> {
        &self.pools
    }

    /// Gets the pool configuration for the pool with the specified name. This is guaranteed to succeed
    /// if the pool name comes from the ruleset once the configuration is merged.
    pub fn find_pool(&self, name: &str) -> Option<&PoolConfiguration> {
        self.pools.iter().find(|p| p.name() == name)
    }

    pub fn set_pools(&mut self, pools: Vec<PoolConfiguration>) {
        self.pools = pools
    }

    pub fn set_pool(&mut self, pool: PoolConfiguration) {
        match self.pools.iter_mut().find(|p| *p == &pool) {
            Some(found) => *found = pool,
            None => self.pools.push(pool),
        }
    }

    pub fn ruleset(&self) -> &RuleSet {
        static DEFAULT_RULES: LazyLock<RuleSet> = LazyLock::new(RuleSet::default);
        self.ruleset.as_ref().unwrap_or(&DEFAULT_RULES)
    }

    pub fn set_ruleset(&mut self, ruleset: RuleSet) {
        self.ruleset = Some(ruleset)
    }

    /// Merge `other` into `self` and return the result. This will not modify `self` or `other`.
    pub fn merge(&self, other: &CgtConfiguration) -> Result<CgtConfiguration, MergeCgtError> {
        let mut new_config = self.clone();
        static POOL_ID: AtomicUsize = AtomicUsize::new(1);

        // Update the pools
        let mut self_pools = self.pools.clone();
        for pool in other.pools.iter() {
            match self_pools.iter_mut().find(|p| *p == pool) {
                Some(existing_pool) => {
                    existing_pool.set_unit_of_account_change(pool.unit_of_account_change.clone());
                    if let Some(new_name) = pool.new_name() {
                        existing_pool.set_new_name(new_name.to_string());
                    }
                    existing_pool.set_methods(pool.methods);
                }
                None => {
                    let mut p = pool.clone();
                    p.set_id(POOL_ID.fetch_add(1, Ordering::Relaxed));
                    self_pools.push(p)
                }
            }
        }

        // Check the rules for unknown pools and create them if necessary.
        if let Some(ruleset) = &other.ruleset {
            for rule in ruleset.iter() {
                if let Rule::Action(ActionRule::Pool(pool_name, _)) = rule {
                    if !self_pools.iter().any(|p| p.name() == pool_name) {
                        let mut pc = PoolConfiguration::new(pool_name.to_string());
                        pc.set_id(POOL_ID.fetch_add(1, Ordering::Relaxed));
                        self_pools.push(pc);
                    }
                }
            }
            new_config.set_ruleset(ruleset.clone());
        }

        new_config.set_pools(self_pools);
        if let Some(round_deals) = other.round_deal_values {
            new_config.set_round_deal_values(round_deals);
        }
        if let Some(assign_expenses) = other.assign_expenses {
            new_config.set_assign_expenses(assign_expenses);
        }
        if let Some(timezone) = other.timezone {
            new_config.set_timezone(timezone);
        }
        if let Some(uoac) = &other.unit_of_account_change {
            new_config.set_unit_of_account_change(Some(uoac.clone()));
        }
        Ok(new_config)
    }
}

impl Default for CgtConfiguration {
    fn default() -> Self {
        Self {
            round_deal_values: None,
            assign_expenses: None,
            timezone: None,
            pools: vec![PoolConfiguration::default()],
            comments: vec![],
            ruleset: None,
            unit_of_account_change: None,
        }
    }
}

impl<'h> PartialEq<DirectiveKind<'h>> for CgtConfiguration {
    fn eq(&self, other: &DirectiveKind<'h>) -> bool {
        if let DirectiveKind::Module(module_obj) = other {
            match module_obj.as_any().downcast_ref::<CgtConfiguration>() {
                Some(other) => self == other,
                None => false,
            }
        } else {
            false
        }
    }
}

impl ModuleDirectiveObj for CgtConfiguration {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl ModuleConfiguration for CgtConfiguration {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn merge(&mut self, _other: Box<dyn ModuleConfiguration>) {
        todo!();
        /*if let Some(other) = (other as &dyn Any).downcast_ref::<CgtConfiguration>() {
            self.merge(other);
        }*/
    }
}

impl ModuleConfigurationClone for CgtConfiguration {
    fn clone_box(&self) -> Box<dyn ModuleConfiguration> {
        Box::new(self.clone())
    }
}

impl ModuleConfigurationEq for CgtConfiguration {
    fn eq(&self, other: &dyn ModuleConfiguration) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<CgtConfiguration>() {
            self == other
        } else {
            false
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CapitalGainsGroupBy {
    EventDate,
    DealDate,
    Disposal,
    Pool,
    Event,
}

impl CapitalGainsGroupBy {
    /// Returns true if the two events should be aggregated together.
    pub fn can_group<'h>(
        &self,
        e1: &AggregatedPoolEvent<'h, '_>,
        e2: &AggregatedPoolEvent<'h, '_>,
    ) -> bool {
        match self {
            CapitalGainsGroupBy::EventDate => {
                e1.event_datetime().start().date() == e2.event_datetime().start().date()
            }
            CapitalGainsGroupBy::DealDate => {
                e1.deal_datetime().start().date() == e2.deal_datetime().start().date()
            }
            CapitalGainsGroupBy::Disposal => match (
                e1.kind().as_ref().map(|t| t.as_ref()),
                e2.kind().as_ref().map(|t| t.as_ref()),
            ) {
                (Some(PoolEventKind::Match(_)), Some(PoolEventKind::Match(_))) => {
                    e1.deal_datetime().start().date() == e2.deal_datetime().start().date()
                }
                _ => false,
            },
            CapitalGainsGroupBy::Pool => e1.pool_name() == e2.pool_name(),
            CapitalGainsGroupBy::Event => {
                e1.unit() == e2.unit()
                    && if let (Some(e1), Some(e2)) = (
                        e1.kind().as_ref().map(|k| k.as_ref()),
                        e2.kind().as_ref().map(|k| k.as_ref()),
                    ) {
                        mem::discriminant(e1) == mem::discriminant(e2)
                    } else {
                        false
                    }
            }
        }
    }
}

impl FromStr for CapitalGainsGroupBy {
    type Err = JournError;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value.to_lowercase().as_str() {
            "event-date" => Ok(CapitalGainsGroupBy::EventDate),
            "deal-date" => Ok(CapitalGainsGroupBy::DealDate),
            "pool" => Ok(CapitalGainsGroupBy::Pool),
            "event" => Ok(CapitalGainsGroupBy::Event),
            "disposal" => Ok(CapitalGainsGroupBy::Disposal),
            _ => Err(err!(
                "valid values are 'event-date', 'deal-date', 'event', 'disposal' and 'pool'"
            )),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum CapitalGainsOrderBy {
    EventDate,
    DealDate,
    Unit,
}

impl FromStr for CapitalGainsOrderBy {
    type Err = JournError;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value.to_lowercase().as_str() {
            "event-date" => Ok(CapitalGainsOrderBy::EventDate),
            "deal-date" => Ok(CapitalGainsOrderBy::DealDate),
            "unit" => Ok(CapitalGainsOrderBy::Unit),
            _ => Err(err!("valid values are 'event-date', 'deal-date' or 'unit'")),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CapitalGainsColumn {
    /// The datetime range of the event
    EventDate,
    /// The datetime range of the deal which is the datetimes of the journal entries whence they came.
    DealDate,
    /// The unit operated with
    Unit,
    /// A description of the event
    Event,
    /// The name of the pool operated on
    Pool,
    /// The amount being acquired before expenses if this event represents the original point of acquisition.
    Acquired,
    /// The total cost, including expenses of the amount being acquired if this event represents the original point of acquisition, or an adjustment being made.
    TotalCost,
    /// The total cost of the acquisition if this a match event.
    ActualCost,
    /// The proceeds of a sale before expenses are considered.
    Disposed,
    /// The proceeds of a sale after expenses are deducted.
    NetProceeds,
    /// The expenses of a sale
    Expenses,
    /// The gain made from the match, if any.
    Gain,
    /// The loss made from the match, if any.
    Loss,
    /// The balance of the pool before the event
    PoolBalBefore,
    /// The balance of the pool after the event
    PoolBalAfter,
    /// The concatenated description taken from the underlying journal entries tied to this event.
    Description,
    /// The concatenated metadata of the specified tag, taken from the underlying journal entries tied to this event.
    Metadata(String),
}

impl CapitalGainsColumn {
    pub fn metadata_tag(&self) -> Option<&str> {
        match self {
            CapitalGainsColumn::Metadata(tag) => Some(tag),
            _ => None,
        }
    }
}

impl FromStr for CapitalGainsColumn {
    type Err = JournError;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value.to_lowercase().as_str() {
            "event-date" => Ok(CapitalGainsColumn::EventDate),
            "deal-date" => Ok(CapitalGainsColumn::DealDate),
            "unit" => Ok(CapitalGainsColumn::Unit),
            "event" => Ok(CapitalGainsColumn::Event),
            "pool" => Ok(CapitalGainsColumn::Pool),
            "acquired" => Ok(CapitalGainsColumn::Acquired),
            "total-cost" => Ok(CapitalGainsColumn::TotalCost),
            "actual-cost" => Ok(CapitalGainsColumn::ActualCost),
            "disposed" => Ok(CapitalGainsColumn::Disposed),
            "net-proceeds" => Ok(CapitalGainsColumn::NetProceeds),
            "expenses" => Ok(CapitalGainsColumn::Expenses),
            "gain" => Ok(CapitalGainsColumn::Gain),
            "loss" => Ok(CapitalGainsColumn::Loss),
            "pool-bal-before" => Ok(CapitalGainsColumn::PoolBalBefore),
            "pool-bal-after" => Ok(CapitalGainsColumn::PoolBalAfter),
            "description" => Ok(CapitalGainsColumn::Description),
            value if value.starts_with('+') => {
                Ok(CapitalGainsColumn::Metadata(value[1..].to_string()))
            }
            _ => Err(err!("valid values are 'event-date', 'deal-date', 'unit', 'event', 'pool', 'acquired', 'total-cost', 'disposed', 'net-proceeds', 'gain', 'loss', 'pool-bal-before', 'pool-bal-after', 'description' or '+<METADATA_TAG>")),
        }
    }
}

#[derive(Default, Debug)]
pub struct CagCommand {
    pub datetime_args: DateTimeArguments,
    pub account_filter: Vec<String>,
    pub unit_filter: Vec<String>,
    pub event_filter: Vec<EventPattern>,
    pub group_by: Vec<CapitalGainsGroupBy>,
    pub group_deals_by_date: bool,
    pub show_group: bool,
    pub order_by: Vec<CapitalGainsOrderBy>,
    pub disposed_mode: bool,
    pub output_csv: bool,
    pub output_yaml: bool,
    pub show_time: bool,
    pub write_valuations: bool,
    pub write_metadata: bool,
    pub columns: Vec<CapitalGainsColumn>,
    pub from: Option<DateTime<Tz>>,
    pub to: Option<DateTime<Tz>>,
}

impl CagCommand {
    pub fn account_filter(&self) -> impl for<'h> Filter<Account<'h>> + '_ {
        AccountFilter(&self.account_filter)
    }

    pub fn set_account_filter(&mut self, account_filter: Vec<String>) {
        self.account_filter = account_filter;
    }

    pub fn unit_filter(&self) -> impl for<'h> Filter<Unit<'h>> + '_ {
        create_unit_filter(&self.unit_filter)
    }

    pub fn set_unit_filter(&mut self, units: Vec<String>) {
        self.unit_filter = units;
    }

    pub fn event_filter(&self) -> EventFilter {
        EventFilter(&self.event_filter)
    }

    pub fn set_event_filter(&mut self, event_filter: Vec<EventPattern>) {
        self.event_filter = event_filter;
    }

    pub fn output_yaml(&self) -> bool {
        self.output_yaml
    }

    pub fn set_output_yaml(&mut self, output_yaml: bool) {
        self.output_yaml = output_yaml;
    }

    pub fn from_to_range(&self) -> impl RangeBounds<DateTime<Tz>> {
        match (self.from, self.to) {
            (Some(from), Some(to)) => (Bound::Included(from), Bound::Excluded(to)),
            (Some(from), None) => (Bound::Included(from), Bound::Unbounded),
            (None, Some(to)) => (Bound::Unbounded, Bound::Excluded(to)),
            (None, None) => (Bound::Unbounded, Bound::Unbounded),
        }
    }
}

impl Command for CagCommand {}

/// A filter that only includes events that matches any of the strings within.
///
/// Strings may take the format:
/// * "pooled" to match all pooled events.
/// * "pooled <pool_name>" to match pooled events for a specific pool.
/// * "unpooled" to match all unpooled events.
/// * "unpooled <pool_name>" to match unpooled events for a specific pool.
/// * "matched" to match all matched events.
/// * "matched <pool_name>" to match matched events for a specific pool.
/// * "adjusted" to match all adjustment events.
/// * "adjusted <pool_name>" to match adjustment events for a specific pool.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EventPattern(String);

impl FromStr for EventPattern {
    type Err = JournError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lower = s.to_lowercase();

        let valid = lower == "pooled"
            || lower.starts_with("pooled ")
            || lower == "unpooled"
            || lower.starts_with("unpooled ")
            || lower == "matched"
            || lower.starts_with("matched ")
            || lower == "adjusted"
            || lower.starts_with("adjusted ");

        if valid {
            Ok(Self(lower))
        } else {
            Err(err!("must be one of 'pooled [pool]', 'unpooled [pool]', 'matched [pool]' or 'adjusted [pool]' where [pool] is an optional name of a pool"))
        }
    }
}

impl Deref for EventPattern {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug)]
pub struct EventFilter<'a>(&'a Vec<EventPattern>);

impl<'h> Filter<PoolEvent<'h>> for EventFilter<'_> {
    fn is_included(&self, event: &PoolEvent<'h>) -> bool {
        // No filter specified; always include.
        if self.0.is_empty() {
            return true;
        }
        let lower_name = event.pool_name().to_lowercase();

        match event.kind() {
            PoolEventKind::PooledDeal(..) => self.0.iter().any(|s| {
                s.deref() == "pooled"
                    || s.starts_with("pooled ") && s.split_whitespace().nth(1) == Some(&lower_name)
            }),
            PoolEventKind::UnpooledDeal(..) => self.0.iter().any(|s| {
                s.deref() == "unpooled"
                    || s.starts_with("unpooled ")
                        && s.split_whitespace().nth(1) == Some(&lower_name)
            }),
            PoolEventKind::Match(..) => self.0.iter().any(|s| {
                s.deref() == "matched"
                    || s.starts_with("matched ") && s.split_whitespace().nth(1) == Some(&lower_name)
            }),
            PoolEventKind::Adjustment(..) => self.0.iter().any(|s| {
                s.deref() == "adjusted"
                    || s.starts_with("adjusted ")
                        && s.split_whitespace().nth(1) == Some(&lower_name)
            }),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnitOfAccountChange {
    /// The code of the unit
    unit_of_account: String,
    value_date: Option<DateTime<Tz>>,
}

impl UnitOfAccountChange {
    pub fn new(new_unit: String, value_date: Option<DateTime<Tz>>) -> Self {
        Self { unit_of_account: new_unit, value_date }
    }
    pub fn value_date(&self) -> Option<DateTime<Tz>> {
        self.value_date
    }

    /// The code of the unit of account. The unit of this code will have been merged
    /// at the time of creation, and so is guaranteed to exist within the configuration.
    pub fn unit_of_account(&self) -> &str {
        &self.unit_of_account
    }
}

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum MatchMethod {
    /// First in, first out method. This is the default.
    #[default]
    Fifo,
    /// Last in, first out method.
    Lifo,
    /// Average match method.
    Average,
}

impl FromStr for MatchMethod {
    type Err = JournError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.cmp_ignore_case_ascii("average") == cmp::Ordering::Equal {
            Ok(MatchMethod::Average)
        } else if s.cmp_ignore_case_ascii("fifo") == cmp::Ordering::Equal {
            Ok(MatchMethod::Fifo)
        } else if s.cmp_ignore_case_ascii("lifo") == cmp::Ordering::Equal {
            Ok(MatchMethod::Lifo)
        } else {
            Err(err!("'average', 'fifo' or 'lifo' expected"))
        }
    }
}

impl fmt::Display for MatchMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MatchMethod::Fifo => write!(f, "FIFO"),
            MatchMethod::Lifo => write!(f, "LIFO"),
            MatchMethod::Average => write!(f, "Average"),
        }
    }
}

#[derive(Debug, Clone, Eq)]
pub struct PoolConfiguration {
    /// The config will gain a unique ID when it is merged to the configuration.
    id: Option<usize>,
    name: String,
    new_name: Option<String>,
    unit_of_account_change: Option<UnitOfAccountChange>,
    /// Methods to apply when the pool has a positive balance and a negative balance.
    methods: (MatchMethod, MatchMethod),
    match_condition: Option<Lambda>,
    max_age: Option<Duration>,
}

impl PoolConfiguration {
    pub fn new(name: String) -> Self {
        Self {
            id: None,
            name,
            new_name: None,
            methods: Default::default(),
            max_age: Default::default(),
            match_condition: None,
            unit_of_account_change: None,
        }
    }

    pub fn id(&self) -> Option<usize> {
        self.id
    }

    pub fn set_id(&mut self, id: usize) {
        self.id = Some(id)
    }

    /// The name of this pool for display purposes.
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn set_name(&mut self, name: String) {
        self.name = name
    }

    /// The new name of this pool from now on.
    pub fn new_name(&self) -> Option<&str> {
        self.new_name.as_deref()
    }

    pub fn set_new_name(&mut self, name: String) {
        self.new_name = Some(name)
    }

    pub fn unit_of_account_change(&self) -> Option<&UnitOfAccountChange> {
        self.unit_of_account_change.as_ref()
    }

    pub fn set_unit_of_account_change(
        &mut self,
        unit_of_account_change: Option<UnitOfAccountChange>,
    ) {
        self.unit_of_account_change = unit_of_account_change
    }

    pub fn methods(&self) -> (MatchMethod, MatchMethod) {
        self.methods
    }

    pub fn set_methods(&mut self, methods: (MatchMethod, MatchMethod)) {
        self.methods = methods
    }

    pub fn match_condition(&self) -> Option<&Lambda> {
        self.match_condition.as_ref()
    }

    pub fn set_match_condition(&mut self, condition: Option<Lambda>) {
        self.match_condition = condition
    }

    pub fn max_age(&self) -> Option<Duration> {
        self.max_age
    }

    pub fn set_max_age(&mut self, max_age: Option<Duration>) {
        assert!(
            max_age.is_none() || max_age.unwrap() > Duration::zero(),
            "maxAge must be positive"
        );

        self.max_age = max_age
    }
}

impl Default for PoolConfiguration {
    /// The default pool configuration is the pool with the name "Pool_1".
    fn default() -> Self {
        Self {
            id: Some(0),
            name: DEFAULT_POOL.to_string(),
            new_name: None,
            methods: Default::default(),
            max_age: Default::default(),
            match_condition: None,
            unit_of_account_change: None,
        }
    }
}

/// Two PoolConfigurations are considered equal when their names are the same.
impl PartialEq for PoolConfiguration {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

/// Guidance on how expenses should be divided amongst equity flows within a `JournalEntry`.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub enum AssignExpenses {
    /// Prefer assignment to acquisitions.
    PreferAcquisition,
    /// Prefer assignment to disposals.
    PreferDisposal,
    /// Assign expenses evenly to acquisitions and disposals.
    #[default]
    Balanced,
}

impl FromStr for AssignExpenses {
    type Err = JournError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "preferacquisition" => Ok(AssignExpenses::PreferAcquisition),
            "preferdisposal" => Ok(AssignExpenses::PreferDisposal),
            "balanced" => Ok(AssignExpenses::Balanced),
            _ => Err(err!("'preferAcquisition', 'preferDisposal' or 'balanced' expected")),
        }
    }
}
