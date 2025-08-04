/*
 * Copyright (c) 2017-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::account::Account;
use crate::alloc::HerdAllocator;
use crate::arguments::Arguments;
use crate::date_and_time::{DEFAULT_DATE_FORMAT, DEFAULT_TIME_FORMAT, DateFormat, TimeFormat};
use crate::ext::StrExt;
use crate::journal_node::{JournalNode, NodeId};
use crate::module::MODULES;
use crate::module::{ModuleConfiguration, ModuleConfigurationEq};
use crate::parsing::DerefMutAndDebug;
use crate::price_db::PriceDatabase;
use crate::unit::{NumberFormat, RoundingStrategy, Unit};
use chrono_tz::Tz;
use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::string::ToString;
use std::sync::{Arc, MutexGuard};

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct ConfigurationVersion<'h> {
    node_id: &'h NodeId<'h>,
    version: u32,
}

impl<'h> ConfigurationVersion<'h> {
    fn initial(node_id: &'h NodeId<'h>) -> Self {
        Self { node_id, version: 0 }
    }

    /// Gets the next version from the current one.
    fn next(self) -> Self {
        Self { node_id: self.node_id, version: self.version + 1 }
    }

    pub fn node_id(&self) -> &'h NodeId<'h> {
        self.node_id
    }
}

#[derive(Debug)]
struct BaseConfiguration<'h> {
    /// The version sequence of the `Configuration`. The version changes whenever the `BaseConfiguration`
    /// changes _and_ it is in use elsewhere (`Configuration::clone`).
    parent: Option<Arc<BaseConfiguration<'h>>>,
    args: &'h Arguments,
    version: ConfigurationVersion<'h>,
    date_format: &'h DateFormat<'h>,
    time_format: &'h TimeFormat<'h>,
    timezone: Tz,
    number_format: NumberFormat,
    default_unit: &'h Unit<'h>,
    units: HashMap<String, &'h Unit<'h>>,
    accounts: HashMap<String, Arc<Account<'h>>>,
    /// Maps module names to their configuration.
    module_config: HashMap<&'static str, &'h dyn ModuleConfiguration>,
}

impl<'h> BaseConfiguration<'h> {
    fn new(
        parent: Option<Arc<BaseConfiguration<'h>>>,
        args: &'h Arguments,
        allocator: &'h HerdAllocator<'h>,
        node_id: &'h NodeId<'h>,
    ) -> Self {
        let mut bc = Self {
            parent,
            args,
            number_format: Default::default(),
            date_format: DEFAULT_DATE_FORMAT.deref(),
            time_format: DEFAULT_TIME_FORMAT.deref(),
            timezone: Tz::UTC,
            default_unit: allocator.alloc(Unit::none()),
            units: Default::default(),
            accounts: Default::default(),
            module_config: Default::default(),
            version: ConfigurationVersion::initial(node_id),
        };
        for module in MODULES.lock().unwrap().iter() {
            if let Some(default_config) = module.default_config() {
                bc.module_config.insert(module.name(), default_config);
            }
        }
        bc
    }

    /// Gets a mutable reference to the configuration so that it can be updated.
    /// This works to ensure that the version is updated when the configuration is mutated,
    /// _and_ there are other references to it (`Configuration::clone`).
    fn get_mut(self: &mut Arc<Self>) -> &mut Self {
        if Arc::strong_count(self) > 1 {
            let new_base = Self {
                parent: Some(self.clone()),
                args: self.args,
                version: ConfigurationVersion::next(self.version),
                date_format: self.date_format,
                time_format: self.time_format,
                timezone: self.timezone,
                number_format: self.number_format,
                default_unit: self.default_unit,
                units: HashMap::new(),
                accounts: HashMap::new(),
                module_config: HashMap::new(),
            };
            *self = Arc::new(new_base);
            Arc::get_mut(self).unwrap()
        } else {
            Arc::get_mut(self).unwrap()
        }
    }

    pub fn get_unit(&self, code: &str) -> Option<&'h Unit<'h>> {
        self.units.get(code).copied().or_else(|| {
            // If the unit is not found, check the parent configuration if it exists.
            self.parent.as_ref().and_then(|parent| parent.get_unit(code))
        })
    }

    pub fn get_account(&self, full_name: &str) -> Option<&Arc<Account<'h>>> {
        self.accounts.get(full_name).or_else(|| {
            // If the account is not found, check the parent configuration if it exists.
            self.parent.as_ref().and_then(|parent| parent.get_account(full_name))
        })
    }

    pub fn get_module_config<T: ModuleConfiguration>(
        &self,
        module_name: &'static str,
    ) -> Option<&'h T> {
        self.module_config.get(module_name).map(|c| c.as_any().downcast_ref().unwrap()).or_else(
            || {
                // If the module configuration is not found, check the parent configuration if it exists.
                self.parent.as_ref().and_then(|parent| parent.get_module_config(module_name))
            },
        )
    }
}

impl PartialEq for BaseConfiguration<'_> {
    fn eq(&self, other: &Self) -> bool {
        let eq = self.number_format == other.number_format
            && self.default_unit == other.default_unit
            && self.units == other.units
            && self.accounts == other.accounts
            && self.module_config.len() == other.module_config.len();
        if !eq {
            return false;
        }

        // Use the ModuleConfigurationEq trait to compare the module configurations.
        // The `PartialEq<Self>` trait is not object-safe.
        for (k, v) in self.module_config.iter() {
            match other.module_config.get(k) {
                Some(other_v) => {
                    if (*v as &dyn ModuleConfigurationEq).eq(&**other_v) {
                        return false;
                    }
                }
                None => return false,
            }
        }
        true
    }
}

pub trait Filter<T: ?Sized> {
    fn is_included(&self, item: &T) -> bool;
}

impl<F, T: ?Sized> Filter<T> for F
where
    for<'c> F: Fn(&'c T) -> bool,
{
    fn is_included(&self, item: &T) -> bool {
        self(item)
    }
}

pub struct AlwaysIncluded;

impl<T: ?Sized> Filter<T> for AlwaysIncluded {
    fn is_included(&self, _item: &T) -> bool {
        true
    }
}

pub enum MergeCgtError {
    MaxAgeNotAllowedFinalPool,
}

/// The configuration of `JournalFiles`.
///
/// Configuration is modified via directives that are specified amongst the data of the `JournalFiles` themselves. Configuration is
/// parsed alongside `JournalEntries` and specifies how entries are parsed as well as containing all the `Accounts` and
/// `Units` encountered so far.
///
/// When `JournalFiles` are 'branched' with the `branch` directive, so too is the configuration. That is, the new
/// child branch is seeded with a [Clone::clone()] of the parent's `Configuration`. This does not happen with the `include`
/// directive where a reference to the same configuration is used.
#[derive(Clone)]
pub struct Configuration<'h> {
    herd_allocator: &'h HerdAllocator<'h>,
    base_config: Arc<BaseConfiguration<'h>>,
}

impl<'h> Configuration<'h> {
    /// Initializes the configuration from the arguments passed.  This is a more useful and natural way to initialize
    /// the starting `Configuration` values, rather than simply relying on defaults. As an example, a date format argument
    /// specifies the output date format used for reporting, but this should also be the initial date format to expect
    /// for `JournalEntries`.
    ///
    /// This should attempt to behave using the principle of 'least surprise' to the user.
    pub fn from_args(
        args: &'h Arguments,
        allocator: &'h HerdAllocator<'h>,
        node_id: &'h NodeId<'h>,
    ) -> Self {
        Self {
            base_config: Arc::new(BaseConfiguration::new(None, args, allocator, node_id)),
            herd_allocator: allocator,
        }
    }

    /// Creates a new `Configuration` that is a branch of the current one. The new configuration
    /// is essentially the same as the current one.
    pub fn branch(&self) -> Self {
        Configuration {
            base_config: Arc::new(BaseConfiguration {
                parent: Some(self.base_config.clone()),
                args: self.base_config.args,
                version: self.base_config.version.next(),
                date_format: self.base_config.date_format,
                time_format: self.base_config.time_format,
                timezone: self.base_config.timezone,
                number_format: self.base_config.number_format,
                default_unit: self.base_config.default_unit,
                units: HashMap::new(),
                accounts: HashMap::new(),
                module_config: HashMap::new(),
            }),
            herd_allocator: self.herd_allocator,
        }
    }

    pub fn args(&self) -> &'h Arguments {
        self.base_config.args
    }

    pub fn version(&self) -> ConfigurationVersion {
        self.base_config.version
    }

    /// Gets the date format in use.
    pub fn date_format(&self) -> &'h DateFormat<'h> {
        &self.base_config.date_format
    }

    /// Sets the date format for reading subsequent entries. The date format can be overridden
    /// multiple times to accommodate different entry styles.
    pub fn set_date_format(&mut self, df: &'h DateFormat<'h>) {
        let base_config = self.get_mut();
        base_config.date_format = df;
    }

    /// Gets the time format in use.
    pub fn time_format(&self) -> &'h TimeFormat<'h> {
        &self.base_config.time_format
    }

    /// Sets the time format for reading subsequent entries. The time format can be overridden
    /// multiple times to accommodate different entry styles.
    pub fn set_time_format(&mut self, tf: &'h TimeFormat<'h>) {
        let base_config = self.get_mut();
        base_config.time_format = tf;
    }

    /// Gets the time zone in use. The default is UTC unless overridden within the journal file.
    pub fn timezone(&self) -> Tz {
        self.base_config.timezone
    }

    pub fn set_time_zone(&mut self, tz: Tz) {
        let base_config = self.get_mut();
        base_config.timezone = tz;
    }

    pub fn number_format(&self) -> NumberFormat {
        self.base_config.number_format
    }

    pub fn default_unit(&self) -> &'h Unit<'h> {
        self.base_config.default_unit
    }

    pub fn merge_default_unit(
        &mut self,
        def_unit: &Unit<'h>,
        allocator: &'h HerdAllocator<'h>,
    ) -> &'h Unit<'h> {
        self.get_mut().default_unit =
            allocator.alloc(Configuration::merge_units(def_unit, self.base_config.default_unit));
        self.base_config.default_unit
    }

    pub fn get_unit(&self, code: &str) -> Option<&'h Unit<'h>> {
        self.base_config.get_unit(code)
    }

    /// An `Iterator` of all `Units` kept by this `Configuration`.
    pub fn units(&self) -> impl Iterator<Item = &'h Unit<'h>> + '_ {
        self.base_config.units.values().copied()
    }

    /// Inserts the unit if it doesn't already exist. If it exists, then a new unit is created
    /// that is merged with the existing one with the new currencies parameters overriding the existing
    /// where they have been specified.
    /// Returns the unit inserted or merged.
    pub fn merge_unit(
        &mut self,
        unit: &Unit<'h>,
        allocator: &'h HerdAllocator<'h>,
    ) -> &'h Unit<'h> {
        let mut_base = self.get_mut();

        // See if we already have a unit with the same code as any of the unit's aliases and merge with that.
        let mut merged_unit = None;
        for alias in unit.aliases() {
            if let Some(entry) = mut_base.get_unit(alias) {
                merged_unit = Some(&*allocator.alloc(Self::merge_units(unit, entry)));
                break;
            }
        }
        // Otherwise, merge with the default unit
        if merged_unit.is_none() {
            merged_unit = Some(allocator.alloc(Self::merge_units(unit, mut_base.default_unit)));
        }

        // Insert an entry for each alias in the merged unit, now having merged aliases.
        for alias in merged_unit.unwrap().aliases() {
            mut_base.units.insert(alias.to_string(), merged_unit.unwrap());
        }
        merged_unit.unwrap()
    }

    /// Takes two units and merges them to create a third unit.
    /// Values are taken from the primary unit and falling back to the secondary.
    fn merge_units(primary: &Unit<'h>, secondary: &Unit<'h>) -> Unit<'h> {
        let mut builder = if primary.code() != "" {
            Unit::new(primary.code())
        } else if secondary.code() != "" {
            Unit::new(secondary.code())
        } else {
            Unit::none()
        };
        builder.set_name(primary.name().cloned().or(secondary.name().cloned()));
        builder.set_conversion_ranking(
            primary.conversion_ranking().or(secondary.conversion_ranking()),
        );
        builder.set_conversion_expression(
            primary.conversion_expression().cloned().or(secondary.conversion_expression().cloned()),
        );
        builder.set_prices(primary.prices().cloned().or(secondary.prices().cloned()));

        // Combine aliases, but not with the default unit
        let mut combined_aliases = Vec::with_capacity(primary.aliases().count());
        for alias in secondary.aliases().chain(primary.aliases()).map(|a| a.to_string()) {
            if !combined_aliases.contains(&alias) {
                combined_aliases.push(alias);
            }
        }
        builder.set_aliases(combined_aliases);

        // Set the format if one is set
        if primary.has_format() {
            builder.set_format(primary.format().clone());
        } else if secondary.has_format() {
            builder.set_format(secondary.format().clone());
        }

        // Set the rounding strategy if one is set
        if primary.rounding_strategy() != RoundingStrategy::default() {
            builder.set_rounding_strategy(primary.rounding_strategy());
        } else {
            builder.set_rounding_strategy(secondary.rounding_strategy());
        }

        builder.set_metadata(
            secondary.metadata().iter().chain(primary.metadata().iter()).cloned().collect(),
        );

        builder
    }

    /// Merges the specified `config` over the top of this configuration.
    /// Parent and version are left unchanged.
    pub fn merge_config(&mut self, config: &Configuration<'h>) {
        let base_config = self.get_mut();
        base_config.date_format = config.date_format();
        base_config.time_format = config.time_format();
        base_config.timezone = config.timezone();
        base_config.number_format = config.number_format();
        base_config.default_unit = config.default_unit();
        base_config.args = config.args();
        for (k, v) in config.base_config.units.iter() {
            base_config.units.insert(k.clone(), *v);
        }
        for (k, v) in config.base_config.accounts.iter() {
            base_config.accounts.insert(k.clone(), Arc::clone(v));
        }
        for (k, v) in config.base_config.module_config.iter() {
            base_config.module_config.insert(k, *v);
        }
    }

    /// The set of all unique price databases.
    pub fn price_databases(&self) -> Vec<&Arc<PriceDatabase<'h>>> {
        let mut dbs = vec![];
        if let Some(price_db) = self.base_config.default_unit.prices() {
            dbs.push(price_db);
        }
        for curr in self.base_config.units.values() {
            if let Some(curr_db) = curr.prices() {
                if !dbs.iter().any(|db| Arc::ptr_eq(db, curr_db)) {
                    dbs.push(curr_db);
                }
            }
        }
        dbs
    }

    fn insert_account(&mut self, account: Arc<Account<'h>>) -> Arc<Account<'h>> {
        let inserted = match self.get_mut().accounts.entry(account.name_exact().to_string()) {
            Entry::Vacant(ve) => Arc::clone(ve.insert(account)),
            Entry::Occupied(mut oe) => oe.insert(account),
        };
        inserted
    }

    pub fn merge_account(&mut self, account: Arc<Account<'h>>) -> Arc<Account<'h>> {
        let mut_base = self.get_mut();

        match mut_base.accounts.get(account.name_exact()) {
            Some(acc) => {
                let secondary = Arc::clone(acc);
                let merged = self.merge_accounts(&account, &secondary);
                let ret = Arc::clone(&merged);
                self.get_mut().accounts.insert(account.name_exact().to_string(), merged);
                ret
            }
            None => {
                let ret = Arc::clone(&account);
                mut_base.accounts.insert(account.name_exact().to_string(), account);
                ret
            }
        }
    }

    /// Merges two accounts, taking values from the primary and falling back to the secondary
    fn merge_accounts(
        &mut self,
        primary: &Arc<Account<'h>>,
        secondary: &Arc<Account<'h>>,
    ) -> Arc<Account<'h>> {
        let mut metadata = vec![];
        metadata.append(&mut primary.metadata().clone());
        metadata.append(&mut secondary.metadata().clone());

        let parent = Account::parent_str(primary.name_exact())
            .map(|parent| self.get_or_create_account(parent));
        Arc::new(Account::new(primary.name_exact().to_string(), parent, metadata))
    }

    pub fn get_or_create_account<'a, S: Into<Cow<'a, str>>>(
        &mut self,
        full_name: S,
    ) -> Arc<Account<'h>> {
        let full_name = full_name.into();
        match self.get_account(&full_name) {
            Some(acc) => Arc::clone(acc),
            None => {
                let parent = Account::parent_str(&full_name).map(|s| self.get_or_create_account(s));
                let acc = Arc::new(Account::new(full_name.into_owned(), parent, vec![]));
                self.insert_account(Arc::clone(&acc));
                acc
            }
        }
    }

    pub fn get_account(&self, full_name: &str) -> Option<&Arc<Account<'h>>> {
        self.base_config.get_account(full_name)
    }

    pub fn module_config<T: ModuleConfiguration>(
        &self,
        module_name: &'static str,
    ) -> Option<&'h T> {
        self.base_config.get_module_config(module_name)
    }

    pub fn set_module_config<T: ModuleConfiguration>(
        &mut self,
        module_name: &'static str,
        config: &'h T,
    ) {
        let bc = self.get_mut();
        bc.module_config.insert(module_name, config);
    }

    /*
    pub fn cgt_config(&self) -> &CgtConfiguration<'h> {
        &self.base_config.cgt_config
    }

    pub fn merge_cgt_config(&mut self, cgt_config: &CgtConfiguration<'h>) -> Option<MergeCgtError> {
        let existing = &mut self.base_config.get_mut().cgt_config;

        // Update the pools
        let mut new_pools = if cgt_config.clear_pool_config { Vec::new() } else { existing.pools.clone() };
        for pool in cgt_config.pools.iter() {
            match new_pools.iter_mut().find(|p| *p == pool) {
                Some(existing_pool) => {
                    existing_pool.set_unit_of_account_change(pool.unit_of_account_change);
                    existing_pool.set_max_age(pool.max_age());
                    existing_pool.set_name(pool.new_name);
                    existing_pool.set_method(pool.method);
                    existing_pool.set_match_condition(pool.match_condition.clone());
                }
                None => new_pools.push(pool.clone()),
            }
        }
        if new_pools.last().map(|p| p.max_age.is_some()).unwrap_or(false) {
            return Some(MergeCgtError::MaxAgeNotAllowedFinalPool);
        }

        // Don't let there be no pools. Create one as a last resort with a default name for the first pool.
        if new_pools.is_empty() {
            new_pools.push(PoolConfiguration::new("Pool_1"));
        }
        existing.set_pools(new_pools);
        if let Some(round_deals) = cgt_config.round_deals {
            existing.set_round_deals(round_deals);
        }
        None
    }*/

    pub fn allocator(&self) -> &'h HerdAllocator<'h> {
        self.herd_allocator
    }

    /// Allocates an object.
    pub fn alloc<T>(&self, t: T) -> &'h mut T {
        self.herd_allocator.alloc(t)
    }

    fn get_mut(&mut self) -> &mut BaseConfiguration<'h> {
        self.base_config.get_mut()
    }
}

impl PartialEq for Configuration<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.base_config == other.base_config
    }
}

impl Eq for Configuration<'_> {}

impl Debug for Configuration<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.base_config.fmt(f)?;
        Ok(())
    }
}

impl<'s, 'h> DerefMutAndDebug<'h, 's, Configuration<'h>> for &'s mut Configuration<'h> {}

impl<'s, 'h> DerefMutAndDebug<'h, 's, Configuration<'h>> for MutexGuard<'s, Configuration<'h>> {}

pub fn create_unit_filter<S: AsRef<str>>(units: &[S]) -> impl for<'t> Filter<Unit<'t>> + '_ {
    struct UnitFilter<'a, S: AsRef<str>>(&'a [S]);
    impl<S: AsRef<str>> Filter<Unit<'_>> for UnitFilter<'_, S> {
        fn is_included(&self, u: &Unit) -> bool {
            // When the filter is empty, all units are included.
            if self.0.is_empty() {
                return true;
            }

            for unit_str in self.0 {
                // If the unit starts with a + (and not quoted), then it is a tag
                // and not a unit code.
                if unit_str.as_ref().starts_with('+') {
                    if u.has_tag(unit_str.as_ref()[1..].trim_start()) {
                        return true;
                    }
                    continue;
                }

                let unit_str_unquoted = unit_str.as_ref().trim_quotes();
                for alias in u.aliases() {
                    if alias.eq_ignore_ascii_case(unit_str_unquoted) {
                        return true;
                    }
                }
            }
            false
        }
    }
    UnitFilter(units)
}

/// The standard account filter is a prefix filter.
pub struct AccountFilter<'a, S: AsRef<str>>(pub &'a [S]);

impl<S: AsRef<str>> Filter<Account<'_>> for AccountFilter<'_, S> {
    fn is_included(&self, item: &Account) -> bool {
        // When the filter is empty, all accounts are included.
        if self.0.is_empty() {
            return true;
        }

        for account_str in self.0 {
            if item.name_exact().starts_with(account_str.as_ref()) {
                return true;
            }
        }
        false
    }
}

/// The standard description filter is a substring filter.
pub struct DescriptionFilter<'a, S: AsRef<str>>(pub &'a [S]);

impl<S: AsRef<str>> Filter<str> for DescriptionFilter<'_, S> {
    fn is_included(&self, description: &str) -> bool {
        // When the filter is empty, all accounts are included.
        if self.0.is_empty() {
            return true;
        }

        for desc_part in self.0 {
            if description.contains(desc_part.as_ref()) {
                return true;
            }
        }
        false
    }
}

pub struct FileFilter<'a, S: AsRef<str>>(pub &'a [S]);

impl<'h, S: AsRef<str>> Filter<JournalNode<'h>> for FileFilter<'_, S> {
    fn is_included(&self, node: &JournalNode<'h>) -> bool {
        // When the filter is empty, all accounts are included.
        if self.0.is_empty() {
            return true;
        }

        if let Some(file_name) = node.nearest_filename() {
            for file_part in self.0 {
                if file_name.ends_with(file_part.as_ref()) {
                    return true;
                }
            }
        }
        false
    }
}
