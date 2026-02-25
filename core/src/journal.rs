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
use crate::error::{BlockContext, BlockContextError, JournErrors, JournResult};
use crate::journal_entry::{EntryDateId, EntryId, JournalEntry};
use crate::journal_node::{JournalNode, JournalNodeKind, NodeId};
use crate::parsing::input::TextBlockInput;
use crate::parsing::parser::JournalParseNode;
use crate::parsing::text_block::TextBlock;
use crate::python::mod_ledger::PythonLedgerModule;
use crate::report::balance::AccountBalances;
use crate::report::command::arguments::Arguments;
use nom_locate::LocatedSpan;
use normalize_path::NormalizePath;
use std::collections::BTreeMap;
use std::ops::RangeBounds;
use std::path::Path;
use std::thread;

pub struct Journal<'h> {
    root: &'h JournalNode<'h>,
    entries: BTreeMap<EntryDateId<'h>, (Option<&'h TextBlock<'h>>, &'h JournalEntry<'h>)>,
    combined_config: &'h Configuration<'h>,
}

impl<'h> Journal<'h> {
    pub fn parse(
        args: &Arguments,
        filename: Option<&'h Path>,
        text_block: TextBlock<'h>,
        allocator: &'h HerdAllocator<'h>,
    ) -> JournResult<Journal<'h>> {
        let node_id = allocator.alloc(NodeId::new_root());
        let config = Configuration::new(allocator, node_id);
        let allocated_block = allocator.alloc(text_block);
        let node = allocator.alloc(JournalNode::new(
            None,
            node_id,
            filename,
            JournalNodeKind::Entry,
            TextBlockInput::new(
                LocatedSpan::new(allocated_block.text()),
                allocated_block,
                allocator,
            ),
            allocator,
        ));

        let node_copy = &*node;
        let node = thread::scope(move |scope| {
            let parse_node = JournalParseNode::new_root(node_copy, config, scope);
            parse_node.parse()
        })?;

        let using_aux_date = args.aux_date();
        let mut journal = Journal::new_in(node, allocator);
        // It would be wrong to check entries when they're sorted according to aux date
        // and maybe too expensive to have a separate sorted map just for this.
        if !using_aux_date {
            debug!("Checking balance assertions");
            journal.check_balance_assertions()?;
        }

        Ok(journal)
    }

    pub fn new_in(root: &'h JournalNode<'h>, allocator: &'h HerdAllocator<'h>) -> Journal<'h> {
        // Create a sorted logical map of entries and set the price databases.
        // The price databases are set here rather than during parsing to ensure a deterministic order
        // without a race condition.
        let mut entries: BTreeMap<
            EntryDateId<'h>,
            (Option<&'h TextBlock<'h>>, &'h JournalEntry<'h>),
        > = BTreeMap::new();
        for (_seg, dir) in root.all_directives_iter() {
            match dir.kind() {
                DirectiveKind::Entry(e) => {
                    entries.insert(EntryDateId::from(*e), (dir.parsed(), e));
                }
                DirectiveKind::Unit(unit) => {
                    if let Some(db) = unit.prices() {
                        for alias in unit.aliases() {
                            PythonLedgerModule::set_price_database(
                                alias,
                                db,
                                root.id().journal_incarnation(),
                            );
                        }
                    }
                }
                DirectiveKind::Units(units) => {
                    if let Some(db) = units.default_unit().and_then(|d| d.prices()) {
                        PythonLedgerModule::set_default_price_database(
                            db,
                            root.id().journal_incarnation(),
                        );
                    }
                }
                _ => {}
            }
        }

        // Create a combined configuration that follows all branch paths in order,
        // applying all configuration items in order.
        let combined_config = allocator.alloc(Configuration::new(allocator, root.id()));
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

        for (_, raw_and_entry) in self.entries.range(..) {
            let entry = raw_and_entry.1;
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
                        if let Some(raw) = raw_and_entry.0 {
                            errs.push(err!("{}", raw.location().unwrap()).with_source(err));
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

    pub fn entry_range<'a, R>(
        &'a self,
        range: R,
    ) -> impl DoubleEndedIterator<Item = &'h JournalEntry<'h>> + Clone + 'a
    where
        R: RangeBounds<JDateTime>,
    {
        let range = EntryDateId::date_range(range);
        self.entries.range(range).map(|e| e.1.1)
    }

    /// Searches for an entry whose start date and description match those specified
    pub fn contains_entry(&self, start_date: JDateTime, description: &str) -> bool {
        for entry in self.entry_range(&start_date..=&start_date) {
            if entry.description() == description {
                return true;
            }
        }
        false
    }

    /// Finds all entries in `datetime_range`, and having a description equal to `description`.
    pub fn find_entries<'a, 'b, R>(
        &'a self,
        datetime_range: R,
        description: &'b str,
    ) -> impl Iterator<Item = &'h JournalEntry<'h>> + 'a
    where
        'b: 'a,
        R: RangeBounds<JDateTime>,
    {
        self.entry_range(datetime_range).filter(move |e| e.description() == description)
    }

    pub fn entry(&self, entry_id: EntryId<'h>) -> &'h JournalEntry<'h> {
        self.node(entry_id.node_id()).entry(entry_id)
    }

    pub fn entry_by_date_id(&self, date_id: EntryDateId<'h>) -> Option<&'h JournalEntry<'h>> {
        self.entries.get(&date_id).map(|e| e.1)
    }

    pub fn append_entry(
        &mut self,
        mut entry: JournalEntry<'h>,
        index: &NodeId<'h>,
    ) -> JournResult<&'h JournalEntry<'h>> {
        entry.check()?;

        let entry = self.node(index).append_entry(entry);
        self.add_entries(&[entry])?;
        Ok(entry)
    }

    pub fn insert_entry(
        &mut self,
        mut entry: JournalEntry<'h>,
        index: &NodeId<'h>,
    ) -> JournResult<&'h JournalEntry<'h>> {
        entry.check()?;

        // Set the date Id before inserting as it's used for comparing.
        let entry = self.node(index).insert_entry(entry);
        self.add_entries(&[entry])?;
        Ok(entry)
    }

    /// Replaces entries in the journal. The entries are all checked before being replaced but should
    /// the balance assertions fail, recovery is not possible and the journal will be in an inconsistent
    /// state. This situation may be manageable if such an error aborts the program.
    ///
    /// # Panics
    /// If the entry does not exist
    pub fn replace_entries(
        &mut self,
        mut entries: Vec<JournalEntry<'h>>,
        allocator: &'h HerdAllocator<'h>,
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
            let (old_entry, new_entry) =
                self.node(entry.id().node_id()).replace_entry(entry, allocator);
            // Make sure the old one is removed in case the date id has changed.
            self.entries.remove(&EntryDateId::from(old_entry));
            new_entry_parts.push(new_entry);
        }

        self.add_entries(&new_entry_parts)?;
        Ok(new_entry_parts)
    }

    /// Adds entries to the `entries` map and checks the balance assertions. If the balance assertions
    /// check fails, all entries are removed to restore the state as it was before the call.
    fn add_entries(&mut self, entries: &[&'h JournalEntry<'h>]) -> JournResult<()> {
        for entry in entries.into_iter().copied() {
            self.entries.insert(entry.into(), (None, entry));
        }

        // Perform this check after the entry has been inserted into the file. If the result is erroneous,
        // we will need to back up and remove it again.
        let r = self.check_balance_assertions();

        if let Err(e) = r {
            for entry in entries.into_iter().copied() {
                self.entries.remove(&entry.into());
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

    pub fn node(&self, index: &NodeId<'h>) -> &JournalNode<'h> {
        // We should not be able to panic as it should be impossible for the caller to obtain an invalid
        // index.
        for file in self.nodes_recursive() {
            if file.id() == index {
                return file;
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
