/*
 * Copyright (c) 2019-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::account::Account;
use crate::alloc::HerdAllocator;
use crate::amount::Amount;
use crate::configuration::Configuration;
use crate::date_and_time::{DateAndTime, JDate};
use crate::error::{BlockContext, BlockContextError, JournError, JournResult};
use crate::ext::{RangeBoundsExt, StrExt};
use crate::journal_entry_flow::Flows;
use crate::journal_node::NodeId;
use crate::metadata::{Metadata, MetadataKey};
use crate::parsing::text_block::TextBlock;
use crate::posting::{Posting, PostingId};
use crate::unit::Unit;
use crate::valued_amount::Valuation;
use crate::{err, match_map};
use chrono::{
    DateTime, Datelike, Duration, NaiveDate, NaiveDateTime, NaiveTime, TimeZone, Timelike,
};
use chrono_tz::Tz;
use linked_hash_set::LinkedHashSet;
use rust_decimal::Decimal;
use rust_decimal::prelude::Zero;
use rust_decimal_macros::*;
use smallvec::{SmallVec, smallvec};
use std::cell::Cell;
use std::ops::{Add, Bound, Range, RangeBounds};
use std::{cmp, fmt};

/// An entry identifier that identifies the entry in space. This means that duplicate entries
/// may be defined within a journal file and that is a valid thing. The two entries are distinct
/// and will receive unique ids.
/// Sorting the entries by id will sort them in their file declaration order.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EntryId<'h> {
    node_id: &'h NodeId<'h>,
    id: u32,
}

impl<'h> EntryId<'h> {
    pub(crate) fn allocate(node_id: &'h NodeId<'h>) -> Self {
        thread_local! {
            static ENTRY_COUNTER: Cell<u32> = Cell::new(0);
        }
        let new_id = ENTRY_COUNTER.with(|k| {
            let prev_id = k.get();
            k.set(prev_id + 1);
            prev_id + 1
        });
        EntryId { node_id, id: new_id }
    }

    pub fn node_id(&self) -> &'h NodeId<'h> {
        self.node_id
    }

    /// Transforms the identifier on an entry from an identifier in space to one in time.
    /// Sorting on the returned `date_id` will allow sorting entries in date order, whilst still preserving
    /// their order in space for those entries which have the same date and time.
    pub fn as_date_id(entry: &JournalEntry<'h>, use_aux_date: bool) -> u64 {
        if use_aux_date {
            EntryId::id_first(
                &entry
                    .date_and_time
                    .aux_date_time()
                    .map(|adt| adt.datetime())
                    .unwrap_or_else(|| entry.date_and_time.datetime_from()),
            ) + entry.id().id as u64
        } else {
            EntryId::id_first(&entry.date_and_time.datetime_from()) + entry.id().id as u64
        }
    }

    /// Gets an id that points to the first entry at the particular date and time.
    pub(crate) fn id_first(date: &DateTime<Tz>) -> u64 {
        // This scheme still gives us room for a maximum 2^27 entries
        let utc_date = date.naive_utc();
        ((utc_date.num_days_from_ce() as u64) << 44_u64)
            + ((utc_date.num_seconds_from_midnight() as u64) << 27_u64)
    }

    pub(crate) fn id_last(date: &DateTime<Tz>) -> u64 {
        let utc_date = date.naive_utc();
        ((utc_date.num_days_from_ce() as u64) << 44_u64)
            + ((utc_date.num_seconds_from_midnight() as u64) << 27_u64)
            + (2u64.pow(27) - 1)
    }

    pub fn id_range<R>(range: R) -> Range<u64>
    where
        R: RangeBounds<DateTime<Tz>>,
    {
        let start = match range.start_bound() {
            Bound::Included(date) => Self::id_first(date),
            Bound::Excluded(date) => Self::id_first(&date.add(Duration::seconds(1))),
            Bound::Unbounded => Self::id_first(&Tz::UTC.from_utc_datetime(&NaiveDateTime::new(
                NaiveDate::from_ymd_opt(1, 1, 1).unwrap(),
                NaiveTime::from_hms_opt(0, 0, 0).unwrap(),
            ))),
        };
        let end = match range.end_bound() {
            Bound::Included(date) => Self::id_first(&date.add(Duration::seconds(1))),
            Bound::Excluded(date) => Self::id_first(date),
            // The max date where num_days_from_ce() fits in 20 bits
            Bound::Unbounded => Self::id_last(&Tz::UTC.from_utc_datetime(&NaiveDateTime::new(
                NaiveDate::from_ymd_opt(2871, 11, 25).unwrap(),
                NaiveTime::from_hms_opt(23, 59, 59).unwrap(),
            ))),
        };
        start..end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EntryObject<'h> {
    /// The boolean argument indicates whether the posting has been elided.
    Posting(Posting<'h>, bool),
    Metadata(Metadata<'h>),
    Comments(&'h str),
}

#[derive(Clone)]
pub struct JournalEntry<'h> {
    id: Option<EntryId<'h>>,
    /// The text block from which the entry came. This will be `None` if the entry was inserted into a node rather than parsed.
    text_block: Option<&'h TextBlock<'h>>,
    date_id: Option<u64>,
    date_and_time: DateAndTime<'h>,
    objects: Vec<EntryObject<'h>, &'h HerdAllocator<'h>>,
    description: &'h str,
    /// The state of the configuration at the time the entry was parsed.
    config: Configuration<'h>,
}

impl<'h> JournalEntry<'h> {
    pub fn new(
        node_id: &'h NodeId<'h>,
        config: Configuration<'h>,
        date_and_time: DateAndTime<'h>,
        description: &'h str,
        objects: Vec<EntryObject<'h>, &'h HerdAllocator<'h>>,
    ) -> Self {
        let mut je = Self {
            id: None,
            config,
            date_id: None,
            text_block: None,
            date_and_time,
            description,
            objects,
        };
        je.set_node_id(node_id);
        je
    }

    pub fn id(&self) -> EntryId<'h> {
        self.id.expect("Id to have been initialised")
    }

    fn set_node_id(&mut self, node_id: &'h NodeId<'h>) {
        self.id = Some(EntryId::allocate(node_id));
        let id = self.id.unwrap();
        for pst in self.postings_mut() {
            pst.set_entry_id(id);
        }

        self.date_id = Some(EntryId::as_date_id(self, self.config.args().aux_date()));
    }

    pub fn date_id(&self) -> u64 {
        self.date_id.unwrap()
    }

    pub fn date(&self) -> JDate<'h> {
        self.date_and_time.date_from()
    }

    pub fn date_and_time(&self) -> &DateAndTime<'h> {
        &self.date_and_time
    }

    pub fn aux_time(&self) -> Option<NaiveTime> {
        self.date_and_time.aux_time()
    }

    pub fn utc_average(&self) -> NaiveDateTime {
        self.date_and_time.utc_average()
    }

    pub fn text_block(&self) -> Option<&'h TextBlock<'h>> {
        self.text_block
    }

    pub fn set_text_block(&mut self, text_block: &'h TextBlock<'h>) {
        self.text_block = Some(text_block);
    }

    pub fn config(&self) -> &Configuration<'h> {
        &self.config
    }

    /// Gets the description trimmed
    pub fn description(&self) -> &'h str {
        self.description.trim()
    }

    pub fn objects(&self) -> &Vec<EntryObject<'h>, &'h HerdAllocator<'h>> {
        &self.objects
    }

    pub fn append_object(&mut self, mut object: EntryObject<'h>) {
        if let EntryObject::Posting(pst, ..) = &mut object {
            pst.set_entry_id(self.id());
        }
        self.objects.push(object);
    }

    pub fn find_posting(&self, posting_id: PostingId) -> Option<&Posting<'h>> {
        self.postings().find(|pst| pst.id() == posting_id)
    }

    pub fn find_posting_mut(&mut self, posting_id: PostingId) -> Option<&mut Posting<'h>> {
        self.postings_mut().find(|pst| pst.id() == posting_id)
    }

    pub fn append_posting(&mut self, pst: Posting<'h>) -> &Posting<'h> {
        self.append_object(EntryObject::Posting(pst, false));
        match_map!(self.objects.last().unwrap(), EntryObject::Posting(p, _) => p).unwrap()
    }

    pub fn postings(&self) -> impl DoubleEndedIterator<Item = &Posting<'h>> + '_ {
        self.objects
            .iter()
            .filter_map(|obj| if let EntryObject::Posting(pst, _) = obj { Some(pst) } else { None })
    }

    pub fn postings_mut(&mut self) -> impl Iterator<Item = &mut Posting<'h>> {
        self.objects
            .iter_mut()
            .filter_map(|obj| if let EntryObject::Posting(pst, _) = obj { Some(pst) } else { None })
    }

    pub fn balanced_postings(&self) -> impl Iterator<Item = &Posting<'h>> + Clone {
        self.objects
            .iter()
            .filter_map(|obj| if let EntryObject::Posting(pst, _) = obj { Some(pst) } else { None })
            .filter(|p| p.account().is_balanced())
    }

    pub fn real_postings(&self) -> impl Iterator<Item = &Posting<'h>> {
        self.objects
            .iter()
            .filter_map(|obj| if let EntryObject::Posting(pst, _) = obj { Some(pst) } else { None })
            .filter(|p| !p.account().is_virtual())
    }

    pub fn contains_account(&self, account: &Account<'h>) -> bool {
        self.objects
            .iter()
            .filter_map(|obj| if let EntryObject::Posting(pst, _) = obj { Some(pst) } else { None })
            .any(|p| **p.account() == *account)
    }

    /*
    /// Gets a map of all the exchange rates from `base_unit` that are available
    /// from examination of the entry data alone.
    /// This can only be done for postings in the balanced set as postings that don't balance
    /// may conflict.
    pub fn exchange_rates(&self, base_unit: &'h Unit<'h>) -> JournResult<UnitAmountMap<'h>> {
        let units = self.units();

        // Avoid units.len() - 1 problems below
        if units.len() == 0 {
            return Ok(UnitAmountMap::with_capacity(0));
        }

        let mut rates = UnitAmountMap::with_capacity(units.len() - 1);

        // First add all unit values that have been explicitly set for base_unit in the entry. These override others.
        rates.extend(self.balanced_postings().filter(|p| p.unit() == base_unit).flat_map(|p| {
            p.valuations().filter_map(|v| {
                if let Valuation::Unit(unit_val) = v {
                    Some(**unit_val)
                } else {
                    None
                }
            })
        }));
        if rates.len() == units.len() - 1 {
            return Ok(rates);
        }

        // The entry's postings and valuations can be thought of as a linear system that may or not be complete.
        // Using a linear algebra library, we are able to throw all of the posting amounts in to a matrix to work
        // out the exchange rates in terms of a particular unit (Ax = b).
        // This method seems to work even if the system is not complete. That is, there just isn't enough valuations
        // within the entry to determine all the exchange rates; for these currencies 0 is returned which will be interpreted
        // as the exchange rate not existing.
        // Also note that this currently works with f64 values which should be accurate enough for most cases. This could
        // be made to work with Decimal for improved accuracy, but a wrapper type would likely be needed that implements ComplexField.

        // Iterator of all the valuations we are using.
        let total_valuations = || {
            self.balanced_postings().flat_map(|p| {
                p.valuations().filter_map(move |v| {
                    if let Valuation::Total(m, _) = v {
                        // Zero valuations create a contradiction for the base unit whose solution we
                        // have set to 1.0.
                        if p.unit() == base_unit && m.is_zero() {
                            None
                        } else {
                            Some((
                                p.amount(),
                                if p.amount().is_positive() {
                                    m.amount() * dec!(-1)
                                } else {
                                    m.amount()
                                },
                            ))
                        }
                    } else {
                        None
                    }
                })
            })
        };
        // Gets the column position (0-based) of the specified unit
        let unit_col = |curr| units.iter().position(|c| *c == curr).unwrap();
        assert!(units.iter().any(|c| *c == base_unit), "base_curr not found in entry");

        // Initialise matrix data with 0's.
        let mut data = Vec::with_capacity((total_valuations().count() + 2) * units.len());
        (0..data.capacity()).for_each(|_| data.push(0.0f64));

        // The first row is special in that 1.0 is set against the column of the base_curr and
        // 0 for all others. This matches the 1.0 in the b vector and defines the system's solution to be in terms of
        // the base unit.
        for (j, _unit) in units.iter().enumerate() {
            if unit_col(base_unit) == j {
                data[j] = 1.0;
                break;
            }
        }

        // The second row is also special in that we enter the coefficients in the implied Debits = Credits rule
        // of the entry. We rearrange the equation to be Debits - Credits = 0. These are applied additively so that a
        // $5 on both sides will make the cell 0 again.
        #[allow(clippy::needless_range_loop)]
        for p in units.len()..units.len() * 2 {
            let j = p - units.len();
            for pst in self.balanced_postings() {
                if j == unit_col(pst.unit()) {
                    data[p] += f64::try_from(pst.amount().quantity()).unwrap();
                }
            }
        }

        // The remaining rows are the total valuations, with equations rearranged to be Amount - Value = 0.
        for (pos, (amount, val)) in total_valuations().enumerate() {
            let i = pos + 2;
            data[i * units.len() + unit_col(amount.unit())] =
                f64::try_from(amount.quantity()).unwrap();
            data[i * units.len() + unit_col(val.unit())] = f64::try_from(val.quantity()).unwrap();
        }

        let a = DMatrix::from_row_slice(total_valuations().count() + 2, units.len(), &data);
        let b = DVector::from_fn(a.nrows(), |i, _| if i == 0 { 1.0 } else { 0.0 });

        let mut entry_str = String::new();
        self.write(&mut entry_str, true).unwrap();
        trace!(
            "Exchange rates solution for base unit: \"{}\" and entry:\n{}",
            base_unit,
            entry_str
        );
        trace!("A = {}", a);
        trace!("b = {}", b);
        let svd = a.svd(true, true);
        // The rank tells us how many rates there are.
        let rank = svd.rank(1e-15);
        if rank == 0 {
            return Ok(rates);
        }
        let x = svd.solve(&b, 1e-15).unwrap();
        trace!("x = {}", x);

        // If the matrix A is not full rank, then it is an under-determined system and impossible to solve fully.
        // It will have an infinite number of solutions if it has any solutions at all. Presumably, we'll get
        // a least squares solution. This is a scalar for scaling the solution to make the exchange rate for the
        // base unit to be 1.0.
        let base_rate_adj = 1.0 / x.column(0)[unit_col(base_unit)];

        for i in 0..x.column(0).len() {
            if i != unit_col(base_unit) {
                let rate = x.column(0)[i] * base_rate_adj;
                // A rate of approximately zero means there is no solution. The power should not be set too high - as high as experience allows.
                let zero_check = (rate * 10.0f64.powf(11.0)).round();

                if zero_check != 0.0 {
                    rates.insert(units[i].with_quantity(
                        Decimal::try_from(1.0 / rate).unwrap_or_else(|e| {
                            panic!("Unable to convert {}, to Decimal: {}", 1.0 / rate, e)
                        }),
                    ))
                }
            }
        }

        trace!("rates = {:?}", rates);
        Ok(rates)
    }*/

    /// Gets a unique list of all units (within postings) in this entry in order of occurrence.
    pub fn units(&self) -> SmallVec<[&'h Unit<'h>; 2]> {
        let mut units = smallvec!();

        for unit in self.postings().flat_map(|pst| pst.valued_amount().units()) {
            if !units.contains(&unit) {
                units.push(unit);
            }
        }
        units
    }

    pub fn value_units(&self) -> SmallVec<[&'h Unit<'h>; 2]> {
        let mut units = smallvec!();

        for unit in self.postings().flat_map(|p| p.value_units()) {
            if !units.contains(&unit) {
                units.push(unit)
            }
        }
        units
    }

    /// Remove all postings that have the specified account
    /// The specified account must match exactly in its virtual'ness.
    pub fn remove_accounts_exact(entry: &mut Self, account: Account<'h>) {
        if JournalEntry::contains_account(entry, &account) {
            entry.objects.retain(|obj| {
                if let EntryObject::Posting(pst, _) = obj {
                    return pst.account().name_exact() != account.name_exact();
                }
                true
            });
        }
    }

    pub fn metadata<'a>(&'a self) -> impl DoubleEndedIterator<Item = &'a Metadata<'h>> {
        self.objects
            .iter()
            .filter_map(move |obj| if let EntryObject::Metadata(m) = obj { Some(m) } else { None })
    }

    pub fn metadata_tag_values(&self, key: &str) -> LinkedHashSet<String> {
        let mut vals = LinkedHashSet::new();
        for obj in self.objects.iter() {
            if let EntryObject::Metadata(m) = obj
                && m.key() == key
                && let Some(val) = m.value_unindented()
            {
                vals.insert_if_absent(val);
            }
        }
        vals
    }

    pub fn has_metadata_tag_value(&self, key: &str, value: &str) -> bool {
        for obj in self.objects.iter() {
            if let EntryObject::Metadata(m) = obj
                && m.key() == key
                && m.value() == Some(value)
            {
                return true;
            }
        }
        false
    }

    /// Clears all metadata values
    pub fn clear_metadata_tag_values(&mut self) {
        self.objects.retain(|obj| matches!(obj, EntryObject::Metadata(..)));
    }

    pub fn retain_metadata_tag_values<F>(&mut self, retain: F)
    where
        F: Fn(MetadataKey, Option<&str>) -> bool,
    {
        self.objects.retain(|obj| {
            if let EntryObject::Metadata(m) = obj {
                return retain(m.key(), m.value());
            }
            true
        });
    }

    /// Inserts the metadata at the specified position relating to other metadata items.
    /// If there are no other metadata items, the position _must_ be 0 and the metadata will be appended
    /// to the end of the entry.
    ///
    /// # Panics
    /// If the position is out of range.
    pub fn insert_metadata(&mut self, pos: usize, key: &'h str, value: &'h str) {
        let metadata_count =
            self.objects.iter().filter(|obj| matches!(obj, EntryObject::Metadata(..))).count();
        assert!(pos <= metadata_count, "Position out of range");

        // Find the prefixing spacing by examining other objects in the entry, preferably other metadata.
        let mut block_text = match pos.checked_sub(1).and_then(|p| self.metadata().nth(p)) {
            Some(md) => md.pretext().to_string(),
            None => {
                let mut leading_whitespace = match self.objects.last() {
                    Some(EntryObject::Posting(pst, ..)) => {
                        // FIXME: This should be the posting's block pretext.
                        "\n".to_string() + pst.leading_whitespace()
                    }
                    Some(EntryObject::Metadata(_md)) => unreachable!(),
                    // FIXME: This should be the comment's block pretext
                    Some(EntryObject::Comments(c)) => c.leading_whitespace().to_string(),
                    None => "  ".to_string(),
                };
                leading_whitespace.push('+');
                leading_whitespace
            }
        };
        block_text.push_str(key);
        block_text.push_str("  ");
        block_text.push_str(value);

        // Find the `self.objects` index: i where the metadata position is at pos.
        // If there are no other metadata items, the metadata will be appended
        let insert_pos = self
            .objects
            .iter()
            .enumerate()
            .filter(|(_, obj)| matches!(obj, EntryObject::Metadata(..)))
            .enumerate()
            .find_map(|(i, (md_pos, _))| if pos == md_pos { Some(i) } else { None })
            .unwrap_or(self.objects.len());

        let allocator = self.config.allocator();
        self.objects.insert(
            insert_pos,
            EntryObject::Metadata(Metadata::from(
                &*allocator.alloc(TextBlock::from(allocator.alloc(block_text).as_str())),
            )),
        )
    }

    pub fn append_metadata(&mut self, key: &'h str, value: &'h str) {
        self.insert_metadata(self.metadata().count(), key, value);
    }

    pub fn remove_metadata_tags_by_key(&mut self, key: &str) {
        self.objects.retain(|obj| {
            if let EntryObject::Metadata(m) = obj {
                return m.key() != key;
            }
            true
        });
    }

    pub fn remove_metadata_tags_by_key_and_value(&mut self, key: &str, val: &str) {
        self.objects.retain(|obj| {
            if let EntryObject::Metadata(m) = obj {
                return m.key() != key || m.value() != Some(val);
            }
            true
        });
    }

    pub fn matches_description_filter(&self, filter: &str) -> bool {
        self.description.contains(filter)
    }

    pub fn matches_account_filter(&self, filter: &str) -> bool {
        for pst in self.postings() {
            if pst.matches_account_filter(filter) {
                return true;
            }
        }
        false
    }

    pub fn flows(&self) -> Flows<'h> {
        Flows::create(self)
    }

    /// Checks and creates a new modified entry with derived entries and elided postings.
    pub fn check(&mut self) -> JournResult<()> {
        self.create_elided_postings()
            .and_then(|_| self.derive_posting_amount())
            .and_then(|_| self.check_amounts_balanced())
            .and_then(|_| self.check_valuations_balanced())
            .and_then(|_| self.check_valuations_consistent())
    }

    /// Create additional postings on the entry in situations with more than once unit
    /// # Example
    /// ```
    /// // A1  £3         -> A1  £3     
    /// // A2  $6         -> A2  $6
    /// // A4  $0         -> A4  $0
    /// // A3             -> A3  -£3
    /// //                -> A3  -$6
    /// ```
    pub(crate) fn create_elided_postings(&mut self) -> JournResult<()> {
        let mut summed_amounts: SmallVec<[Amount; 8]> = SmallVec::with_capacity(self.objects.len());
        self.postings()
            .filter(|p| !p.account().is_virtual_unbalanced())
            .filter(|p| !p.has_elided_amount())
            .map(|p| p.amount())
            .for_each(|pst_amount| summed_amounts += pst_amount);

        // Only one unit, no need to expand.
        if summed_amounts.iter().filter(|a| a.quantity() != Decimal::zero()).count() < 2 {
            return Ok(());
        }

        // There should only be one posting to expand, but in the case of more, this will
        // throw an error.
        let mut postings_to_expand_iter = self
            .objects
            .iter()
            .enumerate()
            .filter(|(_, obj)| {
                if let EntryObject::Posting(pst, _) = obj {
                    !pst.account().is_virtual_unbalanced()
                        && pst.balance_assertion().is_none()
                        && pst.has_elided_amount()
                } else {
                    false
                }
            })
            .map(|(pos, pst)| (pos, pst.clone()));

        if let Some((pos, posting_to_expand)) = postings_to_expand_iter.next() {
            if postings_to_expand_iter.next().is_some() {
                return Err(err!("Too many elided amounts"));
            }
            if let EntryObject::Posting(posting_to_expand, _) = posting_to_expand {
                self.objects.remove(pos);
                let mut first_pst = true;
                for amount in summed_amounts.into_iter() {
                    let mut pst = posting_to_expand.clone();
                    // Set amount separately so that we can make sure it is marked as elided.
                    pst.set_amount(amount * dec!(-1), true, self.config.allocator());
                    self.objects.insert(pos, EntryObject::Posting(pst, !first_pst));
                    first_pst = false;
                }
            }
        }

        Ok(())
    }

    /// One posting in a journal entry may have its amount omitted and so it can therefore
    /// be derived from the other posting amounts.
    pub(crate) fn derive_posting_amount(&mut self) -> JournResult<()> {
        if self.postings().any(|p| p.account().is_virtual_unbalanced() && p.has_elided_amount()) {
            return Err(err!("Illegal virtual unbalanced posting with an elided amount"));
        }

        let num_amounts = self
            .postings()
            .filter(|p| !p.account().is_virtual_unbalanced() && p.is_amount_set())
            .count();

        match self.postings().filter(|p| !p.account().is_virtual_unbalanced()).count() {
            // All postings have amounts, no need to derive
            n if num_amounts == n => Ok(()),
            // More than one posting without an amount
            n if num_amounts < n - 1 => Err(err!("Multiple postings without an amount")),
            _ => {
                let allocator = self.config.allocator();
                let mut summed_amounts: Vec<Amount> = Vec::with_capacity(self.objects.len());
                self.postings()
                    .filter(|p| !p.account().is_virtual_unbalanced() && p.is_amount_set())
                    .map(|p| p.amount())
                    .for_each(|pst_amount| summed_amounts += pst_amount);

                let mut unbalanced_amounts = summed_amounts
                    .iter()
                    .copied()
                    .filter(|a| !a.quantity().is_zero())
                    .collect::<Vec<_>>();
                if unbalanced_amounts.len() > 1 {
                    Err(err!("Cannot derive posting when more than one unit does not balance"))
                } else if unbalanced_amounts.is_empty() {
                    // Set the amount to zero
                    self.postings_mut()
                        .filter(|pst| !pst.is_amount_set())
                        .filter(|pst| !pst.account().is_virtual_unbalanced())
                        .for_each(|pst| {
                            pst.set_amount(summed_amounts.pop().unwrap(), true, allocator)
                        });
                    Ok(())
                } else {
                    self.postings_mut()
                        .filter(|pst| !pst.is_amount_set())
                        .filter(|pst| !pst.account().is_virtual_unbalanced())
                        .for_each(|pst| {
                            pst.set_amount(
                                unbalanced_amounts.pop().unwrap() * dec!(-1),
                                true,
                                allocator,
                            )
                        });
                    Ok(())
                }
            }
        }
    }

    fn check_amounts_balanced(&self) -> JournResult<()> {
        let mut bals: SmallVec<[Amount; 8]> = smallvec![];
        for pst in self.postings().filter(|pst| !pst.account().is_virtual_unbalanced()) {
            bals += pst.amount();
        }
        match bals.into_iter().filter(|b: &Amount| !b.is_zero()).count() {
            0 => Ok(()),
            1 => Err(err!("Unbalanced entry. Amounts should total to zero.")),
            // This is a trade between multiple currencies which is acceptable
            _ => Ok(()),
        }
    }

    /// Where a particular valuation unit is specified against all postings, then check that
    /// valuation on the combined debit side is equal to the combined valuation on the credit side.
    /// This will not check if valuations are inconsistently applied at varying prices e.g. $1 @@ €1; $1 @@ €2.
    fn check_valuations_balanced(&self) -> JournResult<()> {
        'next_unit: for unit in self.value_units() {
            let mut credits_total = unit.with_quantity(0);
            let mut debits_total = unit.with_quantity(0);
            for pst in self.postings().filter(|p| p.account().is_balanced()) {
                // We allow for there to be multiple valuations in the same unit. We just take the first one
                // for our needs and leave it up to the ValuedAmount to ensure they're consistent.
                if let Some(val) =
                    pst.valued_amount().valuations().find(|v| v.unit() == unit && !v.is_elided())
                {
                    match val {
                        Valuation::Total(value, _elided) => {
                            if pst.is_credit() {
                                credits_total += **value;
                            } else {
                                debits_total += **value;
                            }
                        }
                        Valuation::Unit(value) => {
                            let total = (**value * pst.amount().quantity().abs()).rounded();
                            if pst.is_credit() {
                                credits_total += total;
                            } else {
                                debits_total += total;
                            }
                        }
                    }
                } else {
                    continue 'next_unit;
                }
            }
            if credits_total.abs() != debits_total {
                error!("Error on entry:\n{:?}", self);
                return Err(
                    err!(err!("Credits: {}\n    Debits:  {}", credits_total, debits_total); "Unable to balance valuations in entry"),
                );
            }
        }
        Ok(())
    }

    /// Check that where posting valuations are provided, they are done so in a consistent way, using the same price for all the postings
    /// with the same amount unit and valuation unit.
    /// Due to expected rounding of the valuations, the real price here is eluded. Thus we place error bars on the valuations, allowing
    /// a range of permitted prices. All of these ranges must intersect, for if they don't, we have proven that no single price can exist
    /// to satisfy all of the postings.
    fn check_valuations_consistent(&self) -> JournResult<()> {
        struct PriceRangeEntry<'h> {
            base_unit: &'h Unit<'h>,
            quote_unit: &'h Unit<'h>,
            range: (Bound<Decimal>, Bound<Decimal>),
        }
        let mut price_range_entries = SmallVec::<[PriceRangeEntry; 2]>::new();
        for pst in self.postings() {
            if pst.amount() == 0 {
                for valuation in pst.valuations() {
                    if let Valuation::Total(value, _) = valuation
                        && !value.is_zero()
                    {
                        return Err(err!(err!(
                                "Value should be 0 for 0 posting amounts: {}",
                                pst
                            ); "Inconsistent valuation"));
                    }
                }
            } else {
                'next_curr: for unit in pst.value_units() {
                    let pst_value = pst.amount_in(unit).unwrap().abs();
                    let pst_value_range = pst_value.rounding_error();
                    // Calculate the min and max possible prices. Handle rounded values 0 or close to 0
                    // as we are treating valuations amounts and valuations using abs(), we know the bounds will never be negative.
                    let price_bound = |val_bound: Bound<Amount>| match val_bound {
                        Bound::Included(v) => Bound::Included(
                            cmp::max(v.quantity(), Decimal::zero()) / pst.amount().quantity().abs(),
                        ),
                        Bound::Excluded(v) => Bound::Excluded(
                            cmp::max(v.quantity(), Decimal::zero()) / pst.amount().quantity().abs(),
                        ),
                        _ => unreachable!(),
                    };
                    let pst_price_range =
                        (price_bound(pst_value_range.0), price_bound(pst_value_range.1));

                    if let Some(entry) = price_range_entries
                        .iter_mut()
                        .find(|e| e.base_unit == pst.unit() && e.quote_unit == pst_value.unit())
                    {
                        match entry.range.intersection(&pst_price_range) {
                            Some(intersection) => entry.range = intersection,
                            None => {
                                return Err(err!(BlockContextError::new(
                                    BlockContext::from(
                                        pst.block()
                                            .unwrap_or(&TextBlock::from(pst.to_string().as_str()))
                                    ),
                                    format!(
                                        "Posting valuation: {} @@ {} is not consistent with previous postings. Expected to be in range {}",
                                        pst.amount(),
                                        pst_value,
                                        entry.range.to_string()
                                    )
                                )));
                            }
                        }
                        continue 'next_curr;
                    }
                    price_range_entries.push(PriceRangeEntry {
                        base_unit: pst.unit(),
                        quote_unit: unit,
                        range: pst_price_range,
                    });
                }
            }
        }
        Ok(())
    }

    pub fn write<W: fmt::Write>(&self, w: &mut W, include_elided: bool) -> fmt::Result {
        write!(w, "{}{}", self.date_and_time, self.description)?;
        for obj in self.objects.iter() {
            match obj {
                EntryObject::Comments(s) => {
                    writeln!(w)?;
                    write!(w, "{s}")?
                }
                EntryObject::Metadata(m) => {
                    write!(w, "{m}")?;
                }
                EntryObject::Posting(p, elided) => {
                    if include_elided || !*elided {
                        writeln!(w)?;
                        p.write(w, include_elided)?
                    }
                }
            }
        }
        Ok(())
    }

    /// Creates an error that includes the text block of the entry.
    pub fn err(&self, msg: String) -> JournError {
        let context = match self.text_block {
            Some(tb) => BlockContext::from(tb),
            None => BlockContext::from(&TextBlock::from(self.to_string().as_str())),
        };
        err!(BlockContextError::new(context, msg))
    }

    #[cfg(test)]
    pub fn get_posting(&self, n: usize) -> &Posting<'h> {
        self.postings().nth(n).unwrap()
    }
}

impl PartialEq for JournalEntry<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for JournalEntry<'_> {}

impl PartialOrd for JournalEntry<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<::std::cmp::Ordering> {
        Some(self.date_id.cmp(&other.date_id))
    }
}

impl Ord for JournalEntry<'_> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl fmt::Display for JournalEntry<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.write(f, false)
    }
}

impl fmt::Debug for JournalEntry<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.write(f, true)
    }
}

/*
impl From<&JournalEntry<'_>> for BlockContext {
    fn from(entry: &JournalEntry) -> Self {
        match entry.text_block {
            Some(tb) => BlockContext::from(tb),
            None => BlockContext::from(&TextBlock::from(entry.to_string().as_str())),
        }
    }
}*/

#[cfg(test)]
mod tests {
    use crate::*;
    use indoc::indoc;

    #[test]
    fn test_add() {
        let mut journ = journ!("");

        // Zero items to start with
        assert_eq!(journ.entry_range(..).count(), 0);

        // Item added during session
        let basic_entry = || {
            parse_block1!(
                indoc! {r#"
                2010-12-31  Transaction 1
                Assets:Current:Checking
                Expenses:Groceries  £10
            "#},
                parsing::entry::entry
            )
            .unwrap()
        };
        journ.append_entry(basic_entry(), journ.root().id()).unwrap();
        let entries: Vec<_> = journ.entry_range(..).collect();
        assert_eq!(entries.len(), 1);
    }

    #[test]
    fn test_exchange_rates() {
        let je_1 = entry!(indoc! {r#"
            2000-01-01
              A1  10A @@ 2B
              A2  -10A
        "#});
        let curr_a = je_1.config().get_unit("A").unwrap();
        assert_eq!(
            je_1.exchange_rates(curr_a).into_iter().next().map(|v| v.rounded_dec_places(1)),
            Some(amount!("0.2B"))
        );
    }
}
