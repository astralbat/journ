/*
 * Copyright (c) 2019-2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::alloc::HerdAllocator;
use crate::configuration::Configuration;
use crate::datetime::JDateTime;
use crate::directive::DirectiveKind;
use crate::err;
use crate::error::{BlockContext, BlockContextError, JournError, JournErrors, JournResult};
use crate::journal_context::JContext;
use crate::journal_entry::{EntryDateId, JournalEntry};
use crate::journal_entry_query::PostingQuery;
use crate::journal_node::{JournalNode, JournalNodeKind};
use crate::parsing::parser::JournalParseNode;
use crate::parsing::text_block::TextBlock;
use crate::posting::Posting;
use crate::python::mod_ledger::PythonLedgerModule;
use crate::report::balance::AccountBalances;
use crate::tree_id::TreeId;
use normalize_path::NormalizePath;
use std::collections::{BTreeMap, VecDeque};
use std::iter::Peekable;
use std::path::Path;
use std::range::RangeBounds;
use std::thread;

pub struct Journal<'h> {
    root: &'h JournalNode<'h>,
    entries: BTreeMap<EntryDateId, &'h JournalEntry<'h>>,
    combined_config: &'h Configuration<'h>,
}

impl<'h> Journal<'h> {
    pub fn parse(
        filename: Option<&'h Path>,
        mut text_block: TextBlock<'h>,
        allocator: &'h HerdAllocator<'h>,
    ) -> JournResult<JContext<'h>> {
        let context = JContext::new(allocator);
        let journal = context.with(|| {
            let node_id = TreeId::new_root().into();
            let config = Configuration::new();
            let node = allocator.alloc(JournalNode::new(
                None,
                node_id,
                filename,
                JournalNodeKind::Entry,
                allocator,
            ));
            text_block.set_node(node);
            let allocated_block = allocator.alloc(text_block);

            let node_copy = &*node;
            let node = thread::scope(move |scope| {
                let parse_node =
                    JournalParseNode::new_root(node_copy, allocated_block, config, scope);
                parse_node.parse()
            })?;

            let mut journal = Journal::new_in(node, allocator);
            debug!("Checking balance assertions");
            journal.check_balance_assertions()?;

            Ok::<_, JournError>(journal)
        })?;
        context.set_config(journal.config().clone());
        context.set_journal(journal);
        Ok(context)
    }

    pub fn new_in(root: &'h JournalNode<'h>, allocator: &'h HerdAllocator<'h>) -> Journal<'h> {
        // Create a sorted logical map of entries and set the price databases.
        // The price databases are set here rather than during parsing to ensure a deterministic order
        // without a race condition.
        let mut entries = BTreeMap::new();
        for (_seg, dir) in root.all_directives_iter() {
            match dir.kind() {
                DirectiveKind::Entry(e) => {
                    //if !Journal::_contains_entry(&entries, *e) {
                    entries.insert(EntryDateId::from(*e), *e);
                    //}
                }
                DirectiveKind::Unit(unit) => {
                    if let Some(db) = unit.prices() {
                        for alias in unit.aliases() {
                            PythonLedgerModule::set_price_database(alias, db);
                        }
                    }
                }
                DirectiveKind::Units(units) => {
                    if let Some(db) = units.default_unit().and_then(|d| d.prices()) {
                        PythonLedgerModule::set_default_price_database(db);
                    }
                }
                _ => {}
            }
        }

        // Create a combined configuration that follows all branch paths in order,
        // applying all configuration items in order.
        let combined_config = allocator.alloc(Configuration::new());
        let mut segment = Some(*root.segments().first().unwrap());
        while let Some(seg) = segment {
            combined_config.merge_config(seg.config());
            segment = seg.next_segment();
        }

        Journal { entries, root, combined_config }
    }

    pub fn allocator(&self) -> &'h HerdAllocator<'h> {
        self.root.allocator()
    }

    // TODO: This is quite slow when inserting/replacing many entries. Could be sped up
    // perhaps by keeping track of them in a separate data structure.
    fn check_balance_assertions(&mut self) -> JournResult<()> {
        let mut bals = AccountBalances::new(true, vec![]);
        let mut errs = vec![];

        for id_and_entry in self.entries.iter() {
            let entry = id_and_entry.1;
            for pst in entry.postings() {
                bals.update_balance(pst.account(), pst.valued_amount(), false);
                if let Some(asserted_balance) = pst.balance_assertion() {
                    let account_bal = bals.balance(pst.account(), asserted_balance.unit());
                    if asserted_balance != account_bal {
                        let err = err!(
                            "Asserted balance: {}, Actual balance: {}",
                            asserted_balance,
                            account_bal
                        );
                        if let Some(raw) = id_and_entry.1.text_block() {
                            errs.push(err!("{}", raw.location()).with_source(err));
                        } else {
                            errs.push(err);
                        }
                    }
                }
            }
        }
        match errs.len() {
            0 => Ok(()),
            _ => Err(err!(JournErrors::new("Balance errors".to_string(), errs))),
        }
    }

    pub fn nodes_recursive(&self) -> Vec<&JournalNode<'h>> {
        self.root.children_recursive()
    }

    pub fn postings<'a>(
        &'a self,
        query: PostingQuery,
    ) -> impl Iterator<Item = JournResult<(&'h JournalEntry<'h>, &'h Posting<'h>)>> + 'a {
        query.into_iter(&self.entries)
    }

    pub fn entry_range<'a, R>(
        &'a self,
        range: R,
    ) -> impl Iterator<Item = &'h JournalEntry<'h>> + Clone + 'a
    where
        R: RangeBounds<JDateTime>,
    {
        self.entry_range_filtered(range, |_| true)
    }

    pub fn entry_range_filtered<'a, R, F>(
        &'a self,
        range: R,
        filter: F,
    ) -> impl Iterator<Item = &'h JournalEntry<'h>> + Clone + 'a
    where
        R: RangeBounds<JDateTime>,
        F: Fn(&'h JournalEntry<'h>) -> bool + Clone + 'a,
    {
        // A buffering iterator that returns super_duplicates over sub_duplicates.

        // Below is an illustration of overlapping entry date ranges in sorted order.
        // It is only when we get to entry `e` that we can clear the list as none
        // of those before it overlap it.
        // [---- a ---- ]
        // [-------- b ---------]
        //    [ -- c -- ]
        //              [-- d --]
        //                      [ ---- e ---- ]
        #[derive(Clone)]
        struct DuplicateDetectIterator<
            'h,
            'a,
            I: Iterator<Item = (&'a EntryDateId, &'h JournalEntry<'h>)> + Clone + 'a,
        > {
            inner: Peekable<I>,
            entries_overlapping_timestamp: VecDeque<&'h JournalEntry<'h>>,
            mode: Mode,
        }

        #[derive(Clone, Copy, PartialEq, Eq)]
        enum Mode {
            Clearing,
            Accumulating,
            Dedup,
        }

        impl<'h, 'a, I> Iterator for DuplicateDetectIterator<'h, 'a, I>
        where
            I: Iterator<Item = (&'a EntryDateId, &'h JournalEntry<'h>)> + Clone + 'a,
        {
            type Item = &'h JournalEntry<'h>;

            fn next(&mut self) -> Option<Self::Item> {
                loop {
                    match self.mode {
                        Mode::Accumulating => {
                            if self.entries_overlapping_timestamp.is_empty() {
                                self.entries_overlapping_timestamp.push_back(self.inner.next()?.1);
                            }
                            if let Some(e) = self.inner.peek() {
                                let retain_any =
                                    self.entries_overlapping_timestamp.iter().any(|eot| {
                                        e.1.datetime_range().start() < eot.datetime_range().end()
                                    });
                                if retain_any {
                                    self.entries_overlapping_timestamp
                                        .push_back(self.inner.next().unwrap().1);
                                    continue;
                                }
                            }
                            self.mode = Mode::Dedup;
                            continue;
                        }
                        Mode::Dedup => {
                            let mut i = 0;
                            'i: while i < self.entries_overlapping_timestamp.len() - 1 {
                                let mut j = i + 1;
                                'j: while j < self.entries_overlapping_timestamp.len() {
                                    if self.entries_overlapping_timestamp[i].is_super_duplicate_of(
                                        self.entries_overlapping_timestamp[j],
                                    ) {
                                        self.entries_overlapping_timestamp.remove(j);
                                        continue 'j;
                                    } else if self.entries_overlapping_timestamp[j]
                                        .is_super_duplicate_of(
                                            self.entries_overlapping_timestamp[i],
                                        )
                                    {
                                        self.entries_overlapping_timestamp.remove(i);
                                        continue 'i;
                                    }
                                    j += 1;
                                }
                                i += 1;
                            }
                            self.mode = Mode::Clearing;
                            continue;
                        }
                        Mode::Clearing => {
                            if !self.entries_overlapping_timestamp.is_empty() {
                                break self.entries_overlapping_timestamp.pop_front();
                            } else {
                                self.mode = Mode::Accumulating;
                                continue;
                            }
                        }
                    }
                }
            }
        }

        /*
        let duplicate_filter = move |e: &(&EntryDateId, &&'h JournalEntry<'h>)| {
            let retain_any = entries_overlapping_timestamp
                .iter()
                .any(|eot| e.1.datetime_range().start() < eot.datetime_range().end());
            if !retain_any {
                entries_overlapping_timestamp.clear();
            }
            for ent in entries_overlapping_timestamp.iter() {
                if ent.is_super_duplicate_of(e.1) {
                    return false;
                }
            }
            entries_overlapping_timestamp.push(*e.1);
            true
        };*/

        let range = EntryDateId::date_range(range);
        DuplicateDetectIterator {
            inner: self
                .entries
                .range(range)
                .map(|e| (e.0, *e.1))
                .filter(move |e| filter(e.1))
                .peekable(),
            entries_overlapping_timestamp: VecDeque::new(),
            mode: Mode::Accumulating,
        }

        /*
        // The duplicate_filter must be after the provided filter.
        self.entries
            .range(range)
            .filter(move |e| filter(e.1))
            .filter(duplicate_filter)
            .map(|e| *e.1)*/
    }

    /// Finds all entries in `datetime_range`, and having a description equal to `description`,
    /// and a tree id starting with `base_node_id`.
    pub fn find_entries<'a, 'b, R>(
        &'a self,
        datetime_range: R,
        description: Option<&'b str>,
        base_node_id: Option<&'b TreeId>,
    ) -> impl Iterator<Item = &'h JournalEntry<'h>> + 'a
    where
        'b: 'a,
        R: RangeBounds<JDateTime>,
    {
        let filter = move |e: &JournalEntry<'h>| {
            description.map(|d| d == e.description()).unwrap_or(true)
                && base_node_id.map(|bid| e.id().starts_with(bid)).unwrap_or(true)
        };
        self.entry_range_filtered(datetime_range, filter)
    }

    pub fn entry(&self, entry_id: &TreeId) -> &'h JournalEntry<'h> {
        self.node(&entry_id.parent().unwrap()).entry(entry_id)
    }

    pub fn append_entry(
        &mut self,
        mut entry: JournalEntry<'h>,
        node_id: &TreeId,
    ) -> JournResult<&'h JournalEntry<'h>> {
        // Check before checking if we have the entry already. The check
        // fills in elided amounts/postings which will alter equality.
        entry.check()?;

        let entry = self.node(node_id).append_entry(entry);
        //if !Self::_contains_entry(&self.entries, &entry) {
        self.add_entries(&[entry])?;
        //}
        Ok(entry)
    }

    /*
    pub fn contains_entry(&self, entry: &JournalEntry<'h>) -> bool {
        Journal::_contains_entry(&self.entries, entry)
    }

    fn _contains_entry(
        map: &BTreeMap<EntryDateId, &'h JournalEntry<'h>>,
        entry: &JournalEntry<'h>,
    ) -> bool {
        let date_id = EntryDateId::from(entry);
        for found in map.range(date_id.with_id(TreeId::MIN)..=date_id.with_id(TreeId::MAX_INLINE)) {
            if found.1.is_duplicate_of(entry) {
                return true;
            }
        }
        false
    }*/

    pub fn insert_entry(
        &mut self,
        mut entry: JournalEntry<'h>,
        index: &TreeId,
    ) -> JournResult<&'h JournalEntry<'h>> {
        // Check before checking if we have the entry already. The check
        // fills in elided amounts/postings which will alter equality.
        entry.check()?;

        let entry = self.node(index).insert_entry(entry);
        //if !Self::_contains_entry(&self.entries, entry) {
        self.add_entries(&[entry])?;
        //}
        Ok(entry)
    }

    /// Replaces the existing entry specified by `entry.id()` with the provided `entry` if found.
    pub fn replace_entry(
        &mut self,
        entry: JournalEntry<'h>,
    ) -> JournResult<Option<&'h JournalEntry<'h>>> {
        self.replace_entries(vec![entry]).map(|v| v.into_iter().next())
    }

    pub fn remove_entry(&mut self, entry: &JournalEntry<'h>) -> bool {
        self.entries.remove(&EntryDateId::from(entry));
        self.node(&entry.id().parent().unwrap()).remove_entry(entry).is_some()
    }

    /// Replaces entries in the journal. The entries are all checked before being replaced but should
    /// the balance assertions fail, recovery is not possible and the journal will be in an inconsistent
    /// state. This situation may be manageable if such an error aborts the program.
    pub fn replace_entries(
        &mut self,
        mut entries: Vec<JournalEntry<'h>>,
    ) -> JournResult<Vec<&'h JournalEntry<'h>>> {
        for entry in entries.iter_mut() {
            entry.check().map_err(|e| {
                err!(e; BlockContextError::new(BlockContext::from(&TextBlock::from(entry.to_string().as_str())), "Entry check failed"))
            })?;
        }

        // Recovery in case of error is not possible past this point as we don't have anyway to commit/rollback the
        // insertions.
        let mut new_entry_parts = vec![];
        for entry in entries {
            if let Some((old_entry, new_entry)) =
                self.node(&entry.id().parent().unwrap()).replace_entry(entry)
            {
                // Make sure the old one is removed in case the date id has changed.
                self.entries.remove(&EntryDateId::from(old_entry));
                new_entry_parts.push(new_entry);
            }
        }

        self.add_entries(&new_entry_parts)?;
        Ok(new_entry_parts)
    }

    /// Adds entries to the `entries` map and checks the balance assertions. If the balance assertions
    /// check fails, all entries are removed to restore the state as it was before the call.
    fn add_entries(&mut self, entries: &[&'h JournalEntry<'h>]) -> JournResult<()> {
        for entry in entries.iter().copied() {
            self.entries.insert(EntryDateId::from(entry), entry);
        }

        // Perform this check after the entry has been inserted into the file. If the result is erroneous,
        // we will need to back up and remove it again.
        let r = self.check_balance_assertions();

        if let Err(e) = r {
            for entry in entries.iter().copied() {
                self.entries.remove(&EntryDateId::from(entry));
            }
            Err(e)
        } else {
            Ok(())
        }
    }

    pub fn root(&self) -> &'h JournalNode<'h> {
        self.root
    }

    /// Returns a combined configuration that includes all configuration items applied from each successive
    /// journal segment in a depth-first fashion. I.e. descending into all branches and includes.
    /// The configuration thus contains all accounts, units and other items at their last setting.
    pub fn config(&self) -> &'h Configuration<'h> {
        self.combined_config
    }

    pub fn node(&self, index: &TreeId) -> &JournalNode<'h> {
        // We should not be able to panic as it should be impossible for the caller to obtain an invalid
        // index.
        for node in self.nodes_recursive() {
            if node.id() == index {
                return node;
            }
        }
        panic!("Cannot find journal file by index")
    }

    /// Finds the first file ending with the specified filename components.
    pub fn find_node_by_filename(&self, search: &Path) -> Option<&JournalNode<'h>> {
        self.nodes_recursive().into_iter().find(|f| {
            f.canonical_filename().map(|f| f.ends_with(search.normalize())).unwrap_or(false)
        })
    }
}
