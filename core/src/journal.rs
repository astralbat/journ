/*
 * Copyright (c) 2019-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::alloc::HerdAllocator;
use crate::arguments::{Arguments, BalCommand, CsvCommand, RegCommand};
use crate::configuration::Configuration;
use crate::configuration::Filter;
use crate::directive::DirectiveKind;
use crate::error::{BlockContext, BlockContextError, JournErrors, JournResult};
use crate::ext::StrExt;
use crate::journal_entry::{EntryId, JournalEntry};
use crate::journal_node::{JournalNode, JournalNodeKind, NodeId};
use crate::parsing::parser::JournalFileParseNode;
use crate::parsing::text_block::TextBlock;
use crate::parsing::text_input::TextBlockInput;
use crate::posting::Posting;
use crate::postings_aggregation::{AggregatedPosting, PostingsAggregation};
use crate::python::mod_ledger::PythonLedgerModule;
use crate::reporting::balance::{AccountBalances, Balance};
use crate::unit::NumberFormat;
use crate::valuer::ValueResult::ValuationNeeded;
use crate::valuer::{SystemValuer, ValueResult};
use crate::{err, valuer};
use ansi_term::Colour::{Blue, Red};
use ansi_term::Style;
use chrono::DateTime;
use chrono_tz::Tz;
use nom_locate::LocatedSpan;
use normalize_path::NormalizePath;
use std::borrow::Cow;
use std::collections::{BTreeMap, HashSet};
use std::io::Write;
use std::ops::RangeBounds;
use std::path::Path;
use std::{io, thread};

pub struct Journal<'h> {
    root: &'h JournalNode<'h>,
    entries: BTreeMap<u64, (&'h TextBlock<'h>, &'h JournalEntry<'h>), &'h HerdAllocator<'h>>,
}

impl<'h> Journal<'h> {
    pub fn parse(
        args: &'static Arguments,
        filename: &'h Path,
        text_block: TextBlock<'h>,
        allocator: &'h HerdAllocator<'h>,
    ) -> JournResult<Journal<'h>> {
        //PythonEnvironment::startup();

        let short_args = allocator.alloc(args);
        let node_id = allocator.alloc(NodeId::new_root());
        let config = Configuration::from_args(short_args, allocator, node_id);
        let allocated_block = allocator.alloc(text_block);
        let node = allocator.alloc(JournalNode::new(
            None,
            node_id,
            Some(filename),
            JournalNodeKind::Entry,
            TextBlockInput::new(
                LocatedSpan::new(allocated_block.text()),
                allocated_block,
                allocator,
            ),
            vec![],
            allocator,
        ));

        let node_copy = &*node;
        let node = thread::scope(move |scope| {
            let parse_node = JournalFileParseNode::new_root(node_copy, config, scope);
            parse_node.parse()
        })?;

        /*
        let parse_errors = node.parse_errors();
        if !parse_errors.is_empty() {
            return Err(JournError::Multiple(parse_errors));
        }*/

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
            u64,
            (&'h TextBlock<'h>, &'h JournalEntry<'h>),
            &'h HerdAllocator<'h>,
        > = BTreeMap::new_in(allocator);
        for dir in root.all_directives_iter() {
            match dir.kind() {
                DirectiveKind::Entry(e) => {
                    entries.insert(e.date_id(), (dir.raw(), e));
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

        Journal { entries, root }
    }

    pub fn allocator(&self) -> &'h HerdAllocator<'h> {
        self.root.allocator()
    }

    // TODO: This is quite slow when inserting/replacing many entries. Could be sped up
    // perhaps by keeping track of them in a separate data structure.
    fn check_balance_assertions(&mut self) -> JournResult<()> {
        let mut bals = AccountBalances::new(vec![]);
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
                        errs.push(err!(err; "{}", raw_and_entry.0.location().unwrap()));
                    }
                }
            }
        }
        match errs.len() {
            0 => Ok(()),
            _ => Err(err!(JournErrors::new("Balance errors".to_string(), errs))),
        }
    }

    /*
    /// Gets the journal's configuration based on the root node's configuration
    /// after parsing.
    /// The configuration may not contain all accounts and units parsed if the
    /// configuration was branched at any point.
    pub fn config(&self) -> &Configuration<'h> {
        &self.config
    }*/

    /*
    /// Gets a mutable reference to the configuration that's based on the root node's
    /// configuration after parsing. This allows dynamic modification, such as when
    /// adding more accounts and units.
    pub fn config_mut(&mut self) -> &mut Configuration<'h> {
        &mut self.config
    }*/

    pub fn nodes_recursive(&self) -> Vec<&JournalNode<'h>> {
        self.root.children_recursive()
    }

    pub fn entry_range<'a, R>(
        &'a self,
        range: R,
    ) -> impl DoubleEndedIterator<Item = &'h JournalEntry<'h>> + 'a
    where
        R: RangeBounds<DateTime<Tz>>,
    {
        let range = EntryId::id_range(range);
        self.entries.range(range).map(|e| e.1 .1)
    }

    /*
    pub fn entry_range_mut<R>(
        &mut self,
        range: R,
    ) -> Box<impl DoubleEndedIterator<Item = MutexGuard<ParsedDirective<'h, JournalEntry<'h>>>>>
    where
        R: RangeBounds<DateTime<Tz>>,
    {
        let range = JournalEntry::id_range(range);
        Box::new(self.entries.range_mut(range).map(|e| e.1.lock().unwrap()))
    }

    pub fn entry_range_rc<R>(
        &self,
        range: R,
    ) -> Box<impl DoubleEndedIterator<Item = Arc<Mutex<ParsedDirective<'h, JournalEntry<'h>>>>> + '_>
    where
        R: RangeBounds<DateTime<Tz>>,
    {
        let range = JournalEntry::id_range(range);
        Box::new(self.entries.range(range).map(move |e| Arc::clone(e.1)))
    }*/

    /// Searches for an entry whose start date and description match those specified
    pub fn contains_entry(&self, start_date: DateTime<Tz>, description: &str) -> bool {
        for entry in self.entry_range(&start_date..=&start_date) {
            if entry.description() == description {
                return true;
            }
        }
        false
    }

    /// Finds all entries whose start date and description match those specified
    pub fn find_entries<'a, 'b>(
        &'a self,
        start_date: DateTime<Tz>,
        description: &'b str,
    ) -> impl Iterator<Item = &'h JournalEntry<'h>> + 'a
    where
        'b: 'a,
    {
        self.entry_range(start_date..=start_date).filter(move |e| e.description() == description)
    }

    pub fn entry(&self, entry_id: EntryId<'h>) -> &'h JournalEntry<'h> {
        self.node(entry_id.node_id()).entry(entry_id)
    }

    pub fn entry_by_date_id(&self, date_id: u64) -> Option<&'h JournalEntry<'h>> {
        self.entries.get(&date_id).map(|e| e.1)
    }

    pub fn append_entry(
        &mut self,
        mut entry: JournalEntry<'h>,
        index: &NodeId<'h>,
    ) -> JournResult<&'h JournalEntry<'h>> {
        entry.check()?;

        let pd = self.node(index).append_entry(entry);
        self.add_entry(pd, index)?;
        Ok(pd.1)
    }

    pub fn insert_entry(
        &mut self,
        mut entry: JournalEntry<'h>,
        index: &NodeId<'h>,
    ) -> JournResult<&'h JournalEntry<'h>> {
        entry.check()?;

        // Set the date Id before inserting as it's used for comparing.
        let pd = self.node(index).insert_entry(entry);
        self.add_entry(pd, index)?;
        Ok(pd.1)
    }

    /// Replaces an existing entry in the journal.
    ///
    /// # Panics
    /// If the entry does not exist
    pub fn replace_entry(
        &mut self,
        mut entry: JournalEntry<'h>,
        allocator: &'h HerdAllocator<'h>,
    ) -> JournResult<&'h JournalEntry<'h>> {
        entry.check().map_err(|e| {
            err!(e; BlockContextError::new(BlockContext::from(&TextBlock::from(entry.to_string().as_str())), "Entry check failed".to_string()))
        })?;

        let (old_entry, new_entry) =
            self.node(entry.id().node_id()).replace_entry(entry, allocator);
        // Make sure the old one is removed in case the date id has changed.
        self.entries.remove(&old_entry.date_id());

        self.add_entry(new_entry, new_entry.1.id().node_id())?;
        Ok(new_entry.1)
    }

    /// Adds the entry to the `entries` map.
    fn add_entry(
        &mut self,
        entry: (&'h TextBlock<'h>, &'h JournalEntry<'h>),
        index: &NodeId<'h>,
    ) -> JournResult<()> {
        self.entries.insert(entry.1.date_id(), (entry.0, entry.1));

        // Perform this check after the entry has been inserted into the file. If the result is erroneous,
        // we will need to back up and remove it again.
        let r = self.check_balance_assertions();

        if let Err(e) = r {
            let date_id = entry.1.date_id();
            self.entries.remove(&date_id);
            self.node(index).remove_entry(date_id);
            Err(e)
        } else {
            Ok(())
        }
    }

    /// Searches and removes the entry whose unique `date_id` is equal to the one
    /// specified.
    pub fn remove_entry(&mut self, date_id: u64) -> Option<&'h JournalEntry<'h>> {
        match self.entries.remove(&date_id) {
            Some(pje) => {
                for file in self.root.children_recursive() {
                    file.directives().retain(|dir| {
                        if let DirectiveKind::Entry(entry) = dir.kind() {
                            if entry.date_id() == date_id {
                                return false;
                            }
                        }
                        true
                    });
                }
                Some(pje.1)
            }
            None => None,
        }
    }

    pub fn root(&self) -> &'h JournalNode<'h> {
        self.root
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

    pub fn bal(&mut self, args: &'static Arguments) -> JournResult<AccountBalances<'h>> {
        let cmd: &BalCommand = args.cast_cmd().unwrap();
        let config = self.root.config();

        let mut value_units = vec![];
        for unit_code in cmd.value_units() {
            if let Some(unit) = config.get_unit(unit_code) {
                value_units.push(unit);
            }
        }
        let mut bals = AccountBalances::new(value_units.clone());

        let account_filter = cmd.account_filter();
        let unit_filter = cmd.unit_filter();
        let file_filter = cmd.file_filter();
        let mut modified_entries = vec![];
        for entry in self.entry_range(args.begin_end_range()) {
            let mut cow_entry = Cow::Borrowed(entry);
            if !file_filter.is_included(self.root.find_by_node_id(entry.id().node_id()).unwrap()) {
                continue;
            }

            // First pass: Ensure postings can be valued in the desired units.
            valuer::exec_optimistic(&mut cow_entry, |entry| {
                for pst in entry.postings() {
                    if !account_filter.is_included(pst.account()) {
                        continue;
                    }
                    if !unit_filter.is_included(pst.unit()) {
                        continue;
                    }
                    if args.real_postings() && pst.account().is_virtual() {
                        continue;
                    }

                    for val_unit in value_units.iter().copied() {
                        if pst.valued_amount().value_in(val_unit).is_none() {
                            return ValueResult::ValuationNeeded(pst.unit(), val_unit);
                        }
                    }
                }
                return ValueResult::Ok(());
            })?;

            // Second pass: Update balances
            for pst in cow_entry.postings() {
                if !account_filter.is_included(pst.account()) {
                    continue;
                }
                if !unit_filter.is_included(pst.unit()) {
                    continue;
                }
                if args.real_postings() && pst.account().is_virtual() {
                    continue;
                }

                bals.update_balance(pst.account(), pst.valued_amount(), false);
            }

            // Add to the list of entries to write back.
            if !cow_entry.is_borrowed() {
                modified_entries.push(cow_entry);
            }
        }

        if cmd.write_file() {
            let mut nodes = HashSet::new();
            for entry in modified_entries {
                nodes.insert(entry.id().node_id());
                self.replace_entry(entry.into_owned(), self.allocator())?;
            }
            for node in nodes {
                self.node(node).overwrite()?;
            }
        }

        /*
        if let Some(val_curr) = args.exchange_currency() {
            for curr in bals.currencies() {
                if curr != val_curr {
                    bals.add_valuation_price(valuer::get_value(
                        &mut self.root.config(),
                        curr,
                        val_curr,
                        args.base().end().unwrap_or_else(|| Tz::UTC.from_utc_datetime(&Utc::now().naive_utc())),
                    )?);
                }
            }
        }
         */
        Ok(bals)
    }

    pub fn reg(&mut self, args: &'static Arguments) -> JournResult<()> {
        let cmd: &RegCommand = args.cast_cmd().unwrap();
        let mut pa = PostingsAggregation::new(cmd.group_postings_by());
        let mut bals = vec![];
        // Subtract date width + number of spaces
        let date_width = if pa.group_by_date() {
            cmd.datetime_args.date_format.max_formatting_width()
        } else {
            cmd.datetime_args.date_format.max_formatting_width()
                + cmd.datetime_args.time_format.max_formatting_width()
                + 1
        };
        let cols = term_size::dimensions().map(|s| s.0 - (date_width + 4)).unwrap_or(100);
        let desc_cols = (0.3 * cols as f32) as usize;
        let acc_cols = (0.3 * cols as f32) as usize;
        let amount_cols = (0.2 * cols as f32) as usize;
        let bal_cols = (0.2 * cols as f32) as usize;
        let desc_filter = cmd.description_filter();
        let unit_filter = cmd.unit_filter();
        let file_filter = cmd.file_filter();
        for entry in self.entry_range(args.begin_end_range()) {
            if !desc_filter.is_included(entry.description()) {
                continue;
            }
            if !file_filter.is_included(self.root.find_by_node_id(entry.id().node_id()).unwrap()) {
                continue;
            }
            for pst in entry.postings() {
                if !cmd.account_filter().is_included(pst.account()) {
                    continue;
                }
                if !unit_filter.is_included(pst.unit()) {
                    continue;
                }
                bals += pst.amount();
                let ap =
                    AggregatedPosting::from_entry_and_posting(entry, pst, bals.balance(pst.unit()));
                pa += ap;
            }
        }
        for ap in pa.postings().iter() {
            let desc = ap.descriptions().join("; ").trim().ellipses(desc_cols);
            let account = ap.account().to_string().ellipses(acc_cols);
            let balance = ap.balance();
            let amount_str = ap.amount().format_precise().ellipses(amount_cols);
            let bal_str = balance.format_precise().ellipses(bal_cols);
            /*
            let date_str = if args.aux_date() {
                ap.datetime().convert_datetime_range(&cmd.datetime_args).to_string()
                    .with_timezone(cmd.datetime_args.timezone)
                    .aux_date_or_date_from()
                    .format(cmd.datetime_args.date_format.format_str())
            } else {
                ap.datetime()
                    .with_timezone(cmd.datetime_args.timezone)
                    .date_from()
                    .format(cmd.datetime_args.date_format.format_str())
            };
            let time_to_use = if args.aux_date() {
                ap.datetime().with_timezone(cmd.datetime_args.timezone).aux_time_or_time_from()
            } else {
                ap.datetime().with_timezone(cmd.datetime_args.timezone).time_from()
            };
            let time_str = if !pa.group_by_date() {
                format!(" {}", time_to_use.format(cmd.datetime_args.time_format.format_str()))
            } else {
                "".to_string()
            };*/
            let mut out = io::stdout();
            if atty::is(atty::Stream::Stdout) {
                writeln!(
                    out,
                    "{} {} {} {} {}",
                    ap.datetime().convert_datetime_range(&cmd.datetime_args).start(),
                    format_args!(
                        "{:width$}",
                        Style::new().bold().paint(desc).to_string(),
                        width = desc_cols + 8
                    ),
                    format_args!(
                        "{:width$}",
                        Blue.paint(account).to_string(),
                        width = acc_cols + 9
                    ),
                    format_args!(
                        "{:>width$}",
                        if ap.amount().is_negative() {
                            Red.paint(amount_str).to_string()
                        } else {
                            amount_str
                        },
                        width =
                            if ap.amount().is_negative() { amount_cols + 9 } else { amount_cols }
                    ),
                    format_args!(
                        "{:>width$}",
                        if balance.is_negative() {
                            Red.paint(bal_str).to_string()
                        } else {
                            bal_str
                        },
                        width = if balance.is_negative() { bal_cols + 9 } else { bal_cols }
                    ),
                )
                .map_err(|e| err!(e; "IO Error"))?;
            } else {
                writeln!(
                    out,
                    "{} {desc:desc_cols$} {account:acc_cols$} {amount_str:>amount_cols$} {bal_str:>bal_cols$}",
                    ap.datetime().convert_datetime_range(&cmd.datetime_args),
                )
                    .map_err(|e| err!(e; "IO Error"))?;
            }
        }
        Ok(())
    }

    pub fn csv(&mut self, args: &'static Arguments) -> JournResult<()> {
        let cmd: &CsvCommand = args.cast_cmd().unwrap();
        let mut bals = vec![];
        let mut pa = PostingsAggregation::new(cmd.group_postings_by());
        let mut out = io::stdout();

        writeln!(
            io::stdout(),
            "\"DateTime From\",\"DateTime To\",\"Description\",\"Account\",\"Currency\",\"Amount\",\"Balance\""
        )
            .map_err(|e| err!(e; "IO Error"))?;

        let desc_filter = cmd.description_filter();
        let unit_filter = cmd.unit_filter();
        let file_filter = cmd.file_filter();
        for entry in self.entry_range(args.begin_end_range()) {
            if !desc_filter.is_included(entry.description()) {
                continue;
            }
            if !file_filter.is_included(self.root.find_by_node_id(entry.id().node_id()).unwrap()) {
                continue;
            }
            let iter: Box<dyn Iterator<Item = &Posting<'h>>> = if Arguments::get().real_postings() {
                Box::new(entry.real_postings())
            } else {
                Box::new(entry.postings())
            };
            for pst in iter {
                if !cmd.account_filter().is_included(pst.account()) {
                    continue;
                }
                if !unit_filter.is_included(pst.unit()) {
                    continue;
                }
                bals += pst.amount();

                let ap =
                    AggregatedPosting::from_entry_and_posting(entry, pst, bals.balance(pst.unit()));
                pa += ap;
            }
        }
        for ap in pa.postings().iter() {
            //let dt_range = ap.datetime().utc_range();
            writeln!(
                out,
                "\"{}\",\"{}\",\"{}\",\"{}\",\"{}\",\"{}\"",
                ap.datetime().convert_datetime_range(&cmd.datetime_args),
                //dt_range.start.date().format(cmd.datetime_args.date_format.format_str()),
                //dt_range.start.time().format(cmd.datetime_args.time_format.format_str()),
                //dt_range.end.date().format(cmd.datetime_args.date_format.format_str()),
                //dt_range.end.time().format(cmd.datetime_args.time_format.format_str()),
                ap.descriptions().join("; "),
                ap.account(),
                ap.amount().unit(),
                NumberFormat::to_non_scientific(&mut ap.amount().quantity().to_string()),
                NumberFormat::to_non_scientific(&mut ap.balance().quantity().to_string())
            )
            .map_err(|e| err!(e; "IO Error"))?;
        }
        Ok(())
    }

    /*
    pub fn val(&mut self) -> JournResult<AccountBalances> {
        // The preferred approach is to value entries in order of date/time rather than in order of declaration.
        // This has the advantage that a price lookup function can optimise by returning many results after the time in question,
        // assuming that all further calls to that function will be for entries after that date/time.
        let mut bals = AccountBalances::default();
        let exchange_currency = self.args.exchange_currency().unwrap();
        let account_filter = self.args.account_filter().cloned();
        let mut valuer = Valuer::new();

        let range = JournalEntry::id_range(self.args.begin_end_range());
        for (_, entry) in self.entries.range(range) {
            let mut entry = entry.lock().unwrap();
            let entry_utc_average = entry.utc_average();
            let mut value_error = None;
            let mut value_sources = HashSet::new();
            for trans in entry.transactions_mut().unwrap() {
                // Filter
                if let Some(filter) = &account_filter {
                    if !trans.postings().any(|p| p.matches_account_filter(filter)) {
                        continue;
                    }
                }

                // Value
                if !trans.is_valued(&exchange_currency) {
                    match valuer.value_transaction(
                        &mut self.config,
                        trans,
                        entry_utc_average,
                        &exchange_currency,
                        &self.currencies,
                    ) {
                        Ok(sources) => value_sources.extend(sources),
                        Err(err) => value_error = Some(err),
                    }
                }
            }
            for source in value_sources {
                if !entry.has_metadata_tag_value("ValueSource", &source) {
                    entry.append_metadata("ValueSource".to_string(), source);
                }
            }
            if let Some(err) = value_error {
                let loc = entry.raw().location().map(|l| l.to_owned());
                return Err(errmsg!(err; "Unable to value entry {}",
                match loc {
                    Some(loc) => format!("at {}\n  {}", loc, entry.to_string()),
                    None => format!("\n  {}", entry.to_string()),
                }));
            }

            // Add Balances
            for pst in entry.postings().filter(|p| !p.account().is_virtual()) {
                if let Some(filter) = &account_filter {
                    if !pst.matches_account_filter(filter) {
                        continue;
                    }
                }
                if self.args.real_postings() && pst.account().is_virtual() {
                    continue;
                }
                let val = pst.amount_in(&exchange_currency);
                assert!(val.is_some());
                bals.update_balance(pst.account(), pst.amount(), false);
            }
        }

        Ok(bals)
    }*/
}

/*
#[cfg(feature = "testing")]
#[macro_export]
/// Parses a journal.
macro_rules! journ {
    ($text:expr) => {{
        $crate::journal::Journal::parse(
            $crate::configuration::Configuration::default(),
            $crate::journal_file::StreamType::Text($text.to_string()),
        )
    }};
}*/
