/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::configuration::{AccountFilter, DescriptionFilter, FileFilter, Filter, UnitFilter};
use crate::datetime::JDateTime;
use crate::error::JournResult;
use crate::journal_entry::{EntryDateId, JournalEntry};
use crate::posting::Posting;
use crate::report::expr::{PostingContext, ScalarExpr};
use std::collections::{BTreeMap, VecDeque};
use std::iter::Peekable;
use std::range::{Bound, RangeBounds};

#[derive(Default, Clone)]
pub struct EntryQuery {
    date_range: Option<(Bound<JDateTime>, Bound<JDateTime>)>,
    description_filter: Option<DescriptionFilter>,
    file_filter: Option<FileFilter>,
}

impl EntryQuery {
    pub fn between<R: RangeBounds<JDateTime>>(range: R) -> Self {
        Self {
            date_range: Some((range.start_bound().cloned(), range.end_bound().cloned())),
            ..Default::default()
        }
    }

    fn into_iter_no_dup_detect<'a, 'h>(
        self,
        entries: &'a BTreeMap<EntryDateId, &'h JournalEntry<'h>>,
    ) -> impl Iterator<Item = (&'a EntryDateId, &'h JournalEntry<'h>)> + Clone + 'a {
        entries
            .range(
                self.date_range
                    .map(|(start, end)| EntryDateId::date_range((start, end)))
                    .unwrap_or((Bound::Unbounded, Bound::Unbounded)),
            )
            .map(|(k, v)| (k, *v))
            .filter(move |(_, e)| {
                self.description_filter
                    .as_ref()
                    .map(|df| df.is_included(e.description()))
                    .unwrap_or(true)
            })
    }

    pub fn into_iter<'a, 'h>(
        self,
        entries: &'a BTreeMap<EntryDateId, &'h JournalEntry<'h>>,
    ) -> impl Iterator<Item = &'h JournalEntry<'h>> + 'a {
        let inner = self.into_iter_no_dup_detect(entries);
        DuplicateDetectIterator {
            inner: inner.peekable(),
            entries_overlapping_timestamp: VecDeque::new(),
            mode: Mode::Accumulating,
        }
        .map(|(_, v)| v)
    }
}

pub trait PostingFilter:
    for<'h> Fn((&'h JournalEntry<'h>, &'h Posting<'h>)) -> bool + 'static
{
}
type PostingItem<'h> = JournResult<(&'h JournalEntry<'h>, &'h Posting<'h>)>;
#[derive(Default)]
pub struct PostingQuery {
    entry_query: EntryQuery,
    account_filter: Option<AccountFilter>,
    unit_filter: Option<UnitFilter>,
    expr_filter: Vec<ScalarExpr>,
}

impl PostingQuery {
    pub fn between<R: RangeBounds<JDateTime>>(range: R) -> Self {
        Self { entry_query: EntryQuery::between(range), ..Default::default() }
    }

    pub fn with_account_filter(mut self, account_filter: AccountFilter) -> Self {
        self.account_filter = Some(account_filter);
        self
    }

    pub fn with_description_filter(mut self, description_filter: DescriptionFilter) -> Self {
        self.entry_query.description_filter = Some(description_filter);
        self
    }

    pub fn with_file_filter(mut self, file_filter: FileFilter) -> Self {
        self.entry_query.file_filter = Some(file_filter);
        self
    }

    pub fn with_unit_filter(mut self, unit_filter: UnitFilter) -> Self {
        self.unit_filter = Some(unit_filter);
        self
    }

    pub fn with_expr_filter(mut self, filter: Vec<ScalarExpr>) -> Self {
        self.expr_filter = filter;
        self
    }

    pub fn into_iter<'a, 'h>(
        self,
        entries: &'a BTreeMap<EntryDateId, &'h JournalEntry<'h>>,
    ) -> impl Iterator<Item = PostingItem<'h>> + 'a {
        let inner = self
            .entry_query
            .into_iter_no_dup_detect(entries)
            .flat_map(move |(k, v)| v.postings().map(move |p| (k, v, p)))
            .filter(move |(_, _, p)| {
                self.account_filter.as_ref().map(|af| af.is_included(p.account())).unwrap_or(true)
            })
            .filter(move |(_, _, p)| {
                self.unit_filter.as_ref().map(|uf| uf.is_included(p.unit())).unwrap_or(true)
            });

        // Installing the duplicate detector at this stage, after most of the easy filtering should
        // be more efficient in most realistic scenarios, even though we have to now operate it for each posting instead of each entry.
        DuplicateDetectIterator {
            inner: inner.peekable(),
            entries_overlapping_timestamp: VecDeque::new(),
            mode: Mode::Accumulating,
        }
        .filter_map(move |(_, e, p)| {
            let mut context = PostingContext::new(e, p);
            for filter_expr in &self.expr_filter {
                match filter_expr.eval(&mut context) {
                    Ok(val) => match val.as_lenient_bool() {
                        false => return None,
                        true => continue,
                    },
                    Err(e) => return Some(Err(e)),
                }
            }
            Some(Ok((e, p)))
        })
    }
}

#[derive(Clone)]
struct DuplicateDetectIterator<I: Iterator<Item = E> + Clone, E> {
    inner: Peekable<I>,
    entries_overlapping_timestamp: VecDeque<E>,
    mode: Mode,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Mode {
    Clearing,
    Accumulating,
    Dedup,
}

impl<'h, I, E> Iterator for DuplicateDetectIterator<I, E>
where
    I: Iterator<Item = E> + Clone,
    E: TimestampAndEntryItem<'h>,
{
    type Item = E;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.mode {
                Mode::Accumulating => {
                    if self.entries_overlapping_timestamp.is_empty() {
                        self.entries_overlapping_timestamp.push_back(self.inner.next()?);
                    }
                    if let Some(e) = self.inner.peek() {
                        let retain_any = self
                            .entries_overlapping_timestamp
                            .iter()
                            .any(|eot| e.start() < eot.end());
                        if retain_any {
                            self.entries_overlapping_timestamp
                                .push_back(self.inner.next().unwrap());
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
                            if self.entries_overlapping_timestamp[i].entry().is_super_duplicate_of(
                                self.entries_overlapping_timestamp[j].entry(),
                            ) {
                                self.entries_overlapping_timestamp.remove(j);
                                continue 'j;
                            } else if self.entries_overlapping_timestamp[j]
                                .entry()
                                .is_super_duplicate_of(
                                    self.entries_overlapping_timestamp[i].entry(),
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

trait TimestampAndEntryItem<'h> {
    fn start(&self) -> i64;

    fn end(&self) -> i64;

    fn entry(&self) -> &JournalEntry<'h>;
}

impl<'h> TimestampAndEntryItem<'h> for (&EntryDateId, &JournalEntry<'h>) {
    fn start(&self) -> i64 {
        self.0.timestamp_start()
    }

    fn end(&self) -> i64 {
        self.0.timestamp_end()
    }

    fn entry(&self) -> &JournalEntry<'h> {
        self.1
    }
}

impl<'h> TimestampAndEntryItem<'h> for (&EntryDateId, &JournalEntry<'h>, &Posting<'h>) {
    fn start(&self) -> i64 {
        self.0.timestamp_start()
    }
    fn end(&self) -> i64 {
        self.0.timestamp_end()
    }
    fn entry(&self) -> &JournalEntry<'h> {
        self.1
    }
}
