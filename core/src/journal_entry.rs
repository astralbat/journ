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
use crate::datetime::JDateTime;
use crate::datetime::JDateTimeRange;
use crate::error::{BlockContext, BlockContextError, JournError, JournResult};
use crate::ext::RangeBoundsExt;
use crate::journal_entry_flow::{Flow, Flows};
use crate::journal_node::JournalNode;
use crate::metadata::Metadata;
use crate::parsing::text_block::{BlockObject, TextBlock, TextBlockBuf};
use crate::posting::{Posting, PostingId};
use crate::tree_id::TreeId;
use crate::unit::Unit;
use crate::valuer::LinearSystemValuer;
use crate::{err, match_map};
use itertools::Itertools;
use rust_decimal::Decimal;
use rust_decimal::prelude::Zero;
use rust_decimal_macros::*;
use smallvec::{SmallVec, smallvec};
use std::fmt::Write;
use std::ops::{Bound, RangeBounds};
use std::{cmp, fmt};

pub type EntryId = TreeId;

/// An entry identifier that identifies entries in time order.
#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct EntryDateId {
    timestamp_start: i64,
    timestamp_end: i64,
    // Fall back to comparing the entry in node/parsed order
    id: TreeId,
}

impl EntryDateId {
    pub fn date_range<R: RangeBounds<JDateTime>>(
        range: R,
    ) -> (Bound<EntryDateId>, Bound<EntryDateId>) {
        let start = range.start_bound().map(|d| EntryDateId {
            timestamp_start: d.datetime().timestamp(),
            timestamp_end: d.datetime().timestamp(),
            id: TreeId::MIN,
        });
        let end = range.end_bound().map(|d| EntryDateId {
            timestamp_start: d.datetime().timestamp(),
            timestamp_end: d.datetime().timestamp(),
            id: TreeId::MAX_INLINE,
        });
        (start, end)
    }

    pub fn timestamp_start(&self) -> i64 {
        self.timestamp_start
    }

    pub fn timestamp_end(&self) -> i64 {
        self.timestamp_end
    }

    pub fn with_id(&self, id: TreeId) -> Self {
        Self { id, ..*self }
    }
}

impl<'h> From<&JournalEntry<'h>> for EntryDateId {
    fn from(entry: &JournalEntry<'h>) -> Self {
        EntryDateId {
            timestamp_start: entry.datetime_range.start().datetime().timestamp(),
            timestamp_end: entry.datetime_range.end().datetime().timestamp(),
            id: entry.id.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum EntryObject<'h> {
    /// The boolean argument indicates whether the posting has been elided.
    Posting(Posting<'h>, bool),
    Metadata(Metadata<'h>),
    Comment(&'h str),
}

pub struct JournalEntry<'h> {
    id: EntryId,
    /// The text block from which the entry came. This will be `None` if the entry was inserted into a node rather than parsed.
    text_block: Option<&'h TextBlock<'h>>,
    datetime_range: JDateTimeRange,
    objects: Vec<EntryObject<'h>, &'h HerdAllocator<'h>>,
    /// The description of the entry. Parsed entries will have trailing space trimmed,
    /// but any leading spaces beyond the required number may still be present.
    description: &'h str,
    /// The state of the configuration at the time the entry was parsed.
    config: Configuration<'h>,
}

impl<'h> JournalEntry<'h> {
    pub fn new(
        node: &JournalNode<'h>,
        config: Configuration<'h>,
        datetime_range: JDateTimeRange,
        description: &'h str,
        objects: Vec<EntryObject<'h>, &'h HerdAllocator<'h>>,
    ) -> Self {
        let id = EntryId::new_root();
        let mut je = Self { id, config, text_block: None, datetime_range, description, objects };
        je.attach(node);
        je
    }

    pub fn id(&self) -> &EntryId {
        &self.id
    }

    /// Attaches this entry to the node specified by `node`.
    pub(super) fn attach(&mut self, node: &JournalNode) {
        let id = node.id().next_id();
        for pst in self.postings_mut() {
            pst.attach(&id);
        }

        self.id = id;
    }

    /*
    fn detach(&mut self) {
        self.id = TreeId::new_root();
        for pst in self.postings_mut() {
            pst.detach();
        }
    }*/

    pub fn datetime_range(&self) -> JDateTimeRange {
        self.datetime_range
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
        self.description.trim_start()
    }

    pub fn objects(&self) -> &Vec<EntryObject<'h>, &'h HerdAllocator<'h>> {
        &self.objects
    }

    pub fn append_object(&mut self, mut object: EntryObject<'h>) {
        if let EntryObject::Posting(pst, ..) = &mut object {
            pst.attach(self.id());
        }
        self.objects.push(object);
    }

    pub fn find_posting(&self, posting_id: &PostingId) -> Option<&Posting<'h>> {
        self.postings().find(|pst| pst.id() == posting_id)
    }

    pub fn find_posting_mut(&mut self, posting_id: &PostingId) -> Option<&mut Posting<'h>> {
        self.postings_mut().find(|pst| pst.id() == posting_id)
    }

    pub fn append_posting(&mut self, pst: Posting<'h>) -> &Posting<'h> {
        self.append_object(EntryObject::Posting(pst, false));
        match_map!(self.objects.last().unwrap(), EntryObject::Posting(p, _) => p).unwrap()
    }

    pub fn postings(&self) -> impl DoubleEndedIterator<Item = &Posting<'h>> + Clone + '_ {
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

    /// Gets a unique list of all account units used in the postings of this entry.
    ///
    /// This excludes units used in valuations.
    pub fn units(&self) -> SmallVec<[&'h Unit<'h>; 4]> {
        let mut units = smallvec!();

        for unit in self.postings().map(|pst| pst.unit()) {
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

    pub fn metadata_by_key(&self, key: &str) -> SmallVec<[&Metadata<'h>; 4]> {
        let mut vals = SmallVec::new();
        for obj in self.objects.iter() {
            if let EntryObject::Metadata(m) = obj
                && m.key() == key
            {
                vals.push(m);
            }
        }
        vals
    }

    pub fn has_metadata_tag_value(&self, key: &str, value: &str) -> bool {
        for obj in self.objects.iter() {
            if let EntryObject::Metadata(m) = obj
                && m.key() == key
                && m.value().map(|v| v == value).unwrap_or(false)
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

    /*
    /// Inserts the metadata at the specified position relating to other metadata items.
    /// If there are no other metadata items, the position _must_ be 0 and the metadata will be appended
    /// to the end of the entry.
    ///
    /// # Panics
    /// If the position is out of range.
    pub fn insert_metadata(&mut self, pos: usize, metadata: Metadata<'h>) {
        let metadata_count =
            self.objects.iter().filter(|obj| matches!(obj, EntryObject::Metadata(..))).count();
        assert!(pos <= metadata_count, "Position out of range");

        // Find the prefixing spacing by examining other objects in the entry, preferably other metadata.
        let mut block_text = String::new();
        match pos.checked_sub(1).and_then(|p| self.metadata().nth(p)).and_then(|m| m.pretext()) {
            Some(pretext) => {
                block_text.push_str(pretext);
            }
            None => {
                match self.objects.last() {
                    Some(EntryObject::Posting(pst, ..)) => {
                        // FIXME: This should be the posting's block pretext.
                        block_text.push('\n');
                        block_text.push_str(pst.leading_whitespace());
                    }
                    Some(EntryObject::Metadata(_md)) => unreachable!(),
                    // FIXME: This should be the comment's block pretext
                    Some(EntryObject::Comments(c)) => {
                        block_text.push_str(c.leading_whitespace());
                    }
                    None => {
                        block_text.push_str("  ");
                    }
                }
                block_text.push('+');
            }
        };
        block_text.push_str(metadata.key().as_ref());
        if let Some(v) = metadata.value() {
            block_text.push_str("  ");
            block_text.push_str(v);
        }

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
        let block = allocator.alloc(TextBlock::from(allocator.alloc(block_text).as_str()));
        self.objects
            .insert(insert_pos, EntryObject::Metadata(Metadata::lazy(self.config.clone(), block)))
    }*/

    /*
    pub fn append_metadata(&mut self, metadata: Metadata<'h>) {
        self.insert_metadata(self.metadata().count(), metadata);
    }*/

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
                return m.key() != key || m.value().map(|v| v != val).unwrap_or(true);
            }
            true
        });
    }

    pub fn matches_description_filter(&self, filter: &str) -> bool {
        self.description.contains(filter)
    }

    pub fn flows(&self) -> impl Flows<'h> {
        self.filtered_flows(|_pst| Ok(true)).unwrap()
    }

    pub fn filtered_flows<'a, F>(&'a self, mut filter: F) -> JournResult<impl Flows<'h>>
    where
        F: FnMut(&'a Posting<'h>) -> JournResult<bool>,
    {
        let mut flows: SmallVec<[Flow<'h>; 4]> = smallvec![];
        for pst in self.postings() {
            if !filter(pst)? {
                continue;
            }
            let va = pst.valued_amount().clone();
            flows.push(Flow::new(Some(pst.account().clone()), va));
        }
        // A flow of net 0 isn't a flow.
        //flows.retain(|f| f.amount() != 0);
        Ok(flows)
    }

    /// Checks and creates a new modified entry with derived entries and elided postings.
    pub fn check(&mut self) -> JournResult<()> {
        self.create_elided_postings()
            .and_then(|_| self.derive_posting_amount())
            .and_then(|_| self.check_amounts_balanced())
            .and_then(|_| LinearSystemValuer::check(self))
        // Commented out whilst linear system valuer improvements are made. This valuer
        // could perhaps do the job better.
        //.and_then(|_| self.check_valuations_balanced())
        //.and_then(|_| self.check_valuations_consistent())
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
            1 => {
                // We have a transaction between multiple units and one of the amounts is explicitly zero. We allow this.
                if self.postings().any(|p| p.amount().is_zero()) && self.units().len() > 1 {
                    Ok(())
                } else {
                    Err(err!("Unbalanced entry. Amounts should total to zero."))
                }
            }
            // This is a transaction between multiple units which is acceptable
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
                if let Some(val) = pst.valued_amount().value_in(unit) {
                    if val > 0 {
                        debits_total += val;
                    } else {
                        credits_total += val;
                    }
                }
                /*
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
                    }*/
                else {
                    continue 'next_unit;
                }
            }
            let sum = credits_total.rounded() + debits_total.rounded();
            if !sum.is_zero() {
                return Err(
                    err!(err!("Credits: {}, Debits: {} ({} difference)", credits_total.format_precise(), debits_total.format_precise(), sum.format_precise()); "Unable to balance valuations in entry"),
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
                    if !valuation.value().is_zero() {
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
                                let block_context = match pst.block() {
                                    Some(block) => BlockContext::from(block),
                                    None => {
                                        let mut buf = TextBlockBuf::new();
                                        buf.write(self, Some(self.config()));
                                        BlockContext::from(&buf.as_text_block())
                                    }
                                };
                                return Err(err!(BlockContextError::new(
                                    block_context,
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

    /// Creates an error that includes the text block of the entry.
    pub fn err(&self, msg: String) -> JournError {
        let context = match self.text_block {
            Some(tb) => BlockContext::from(tb),
            None => BlockContext::from(&TextBlock::from(self.to_string().as_str())),
        };
        err!(BlockContextError::new(context, msg))
    }

    /// Gets whether the two entries are considered duplicates. During reporting, duplicate entries
    /// are by default, skipped.
    ///
    /// At present, two entries are considered the same when they have:
    /// * this date range contains the other date range
    /// * different parent nodes
    /// * overlapping postings, metadata where one is a subset of another (must be consitent with date).
    ///
    /// This should give a good compromise in being:
    /// * `false` for identical entries in the same file (repeated transactions on the same day).
    /// * `true` for identical entries in different files (allowing separate files for separate accounts,
    ///   and each file to be complete).
    ///
    /// Possible future behaviour could allow the user to configure duplicate behaviour:
    /// * No duplicate detection - all entries are unique
    /// * Exact (ignoring desc) for differing nodes (current behaviour)
    /// * Always exact everywhere (including desc).
    /// * Metadata Value equality (the two entries may need merging for postings/metadata).
    ///
    /// Also, if duplicate behaviour is being configured, this may need to be set before any entries
    /// are parsed to avoid contradictions in branches.
    pub fn is_super_duplicate_of(&self, other: &Self) -> bool {
        if self.id.parent() == other.id.parent() {
            return false;
        }
        if !self.datetime_range.contains(&other.datetime_range) {
            return false;
        }

        let self_objs: SmallVec<[&EntryObject; 2]> =
            self.objects.iter().filter(|obj| !matches!(obj, EntryObject::Comment(_))).collect();
        let other_objs: SmallVec<[&EntryObject; 2]> =
            other.objects.iter().filter(|obj| !matches!(obj, EntryObject::Comment(_))).collect();
        // If there are no postings in other, we don't say we are a super duplicate.
        if self_objs.len() < other_objs.len() || other_objs.is_empty() {
            return false;
        }
        other_objs.iter().all(|oth_obj| {
            for self_obj in &self_objs {
                let found = match (self_obj, oth_obj) {
                    (EntryObject::Metadata(s_m), EntryObject::Metadata(o_m)) => s_m == o_m,
                    (EntryObject::Posting(self_pst, _), EntryObject::Posting(oth_pst, _)) => {
                        self_pst.contains(oth_pst)
                    }
                    _ => false,
                };
                if found {
                    return true;
                }
            }
            false
        })
    }

    #[cfg(test)]
    pub fn get_posting(&self, n: usize) -> &Posting<'h> {
        self.postings().nth(n).unwrap()
    }
}

impl Clone for JournalEntry<'_> {
    fn clone(&self) -> Self {
        Self {
            // The id remains intact so that it can be used to replace self later.
            id: self.id.clone(),
            text_block: None,
            datetime_range: self.datetime_range,
            objects: self.objects.clone(),
            description: self.description,
            config: self.config.clone(),
        }
    }
}

impl Eq for JournalEntry<'_> {}

impl PartialOrd for JournalEntry<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for JournalEntry<'_> {
    /// Two entries are equal if they have the same internal ID, or they have the same date, description
    /// and objects.
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
        /*
        self.date_and_time == other.date_and_time &&
            self.id == other.id*/

        /*
        if self.date_and_time != other.date_and_time
            || self.description.trim() != other.description.trim()
        {
            return false;
        }

        let mut self_objs = self.objects.clone();
        let mut other_objs = other.objects.clone();
        self_objs.sort();
        other_objs.sort();
        self_objs == other_objs*/
    }
}

impl Ord for JournalEntry<'_> {
    /// Compares two entries in a compatible way with `PartialEq` and `Borrow<JDateTime>`.
    ///
    /// This implementation will order by entries by their start dates and then
    /// by declaration order, as determined by its `id()`.
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.datetime_range
            .start()
            .cmp(&other.datetime_range.start())
            .then_with(|| self.id.cmp(&other.id))
    }
}

/*
impl<'h> std::borrow::Borrow<(&JDateTime, EntryId)> for &JournalEntry<'h> {
    /// It is very important that this aligns with `JournalEntry::cmp()`.
    fn borrow(&self) -> &(&JDateTime, EntryId) {
        &(self.date_and_time.datetime_range_ref().start_ref(), self.id)
    }
}*/

impl fmt::Display for JournalEntry<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut buf = TextBlockBuf::new();
        buf.write(self, Some(self.config()));
        write!(f, "{}", buf)
    }
}

impl fmt::Debug for JournalEntry<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut buf = TextBlockBuf::new();
        buf.set_include_elided(true);
        buf.write(self, Some(self.config()));
        write!(f, "{}", buf)
    }
}

impl BlockObject for JournalEntry<'_> {
    fn write(&self, buf: &mut TextBlockBuf, config: Option<&Configuration>) {
        self.datetime_range.write_for_entry(buf, &self.config).unwrap();
        // Description is optional
        if !self.description.is_empty() {
            write!(buf, "  {}", self.description).unwrap();
        }
        for obj in self.objects.iter() {
            match obj {
                EntryObject::Comment(s) => {
                    writeln!(buf).unwrap();
                    write!(buf, "{s}").unwrap()
                }
                EntryObject::Metadata(m) => {
                    buf.write_child(m, config, true);
                }
                EntryObject::Posting(p, elided) => {
                    if buf.include_elided() || !*elided {
                        buf.write_child(p, config, true);
                    }
                }
            }
        }
    }
}

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
            entry!(indoc! {r#"
                2010-12-31  Transaction 1
                Assets:Current:Checking
                Expenses:Groceries  £10
            "#})
        };
        journ.append_entry(basic_entry(), journ.root().id()).unwrap();
        let entries: Vec<_> = journ.entry_range(..).collect();
        assert_eq!(entries.len(), 1);
    }
}
