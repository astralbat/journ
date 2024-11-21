/*
 * Copyright (c) 2020-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::account::Account;
use crate::amount::Amount;
use crate::arguments::Arguments;
use crate::date_and_time::JDateTimeRange;
use crate::journal_entry::JournalEntry;
use crate::posting::Posting;
use std::ops::AddAssign;
use std::sync::Arc;

pub struct AggregatedPosting<'h> {
    datetime: JDateTimeRange<'h>,
    descriptions: Vec<String>,
    account: Arc<Account<'h>>,
    amount: Amount<'h>,
    balance: Amount<'h>,
}

impl<'h> AggregatedPosting<'h> {
    pub fn from_entry_and_posting(
        entry: &JournalEntry<'h>,
        posting: &Posting<'h>,
        balance: Amount<'h>,
    ) -> Self {
        let use_aux_date = Arguments::get().aux_date;
        AggregatedPosting {
            datetime: if use_aux_date {
                entry
                    .date_and_time()
                    .aux_date_time()
                    .map(|aux| JDateTimeRange::new(aux, None))
                    .unwrap_or(entry.date_and_time().datetime_range())
            } else {
                entry.date_and_time().datetime_range()
            },
            descriptions: vec![entry.description().to_string()],
            account: Arc::clone(posting.account()),
            amount: posting.amount(),
            balance,
        }
    }
    pub fn datetime(&self) -> JDateTimeRange {
        self.datetime
    }
    pub fn descriptions(&self) -> &Vec<String> {
        &self.descriptions
    }
    pub fn account(&self) -> &Account<'h> {
        &self.account
    }
    pub fn amount(&self) -> &Amount<'h> {
        &self.amount
    }
    pub fn balance(&self) -> &Amount<'h> {
        &self.balance
    }
}

/// An collection of postings that may be grouped by: date, description columns.
/// The postings collection is assumed to always be sorted by date which requires callers to add_assign
/// in date order to make searching faster.
pub struct PostingsAggregation<'h> {
    group_by_date: bool,
    group_by_description: bool,
    postings: Vec<AggregatedPosting<'h>>,
}

impl<'h> PostingsAggregation<'h> {
    pub fn new(group_by: Option<&String>) -> Self {
        PostingsAggregation {
            group_by_date: group_by.map(|gb| gb.split(',').any(|s| s == "date")).unwrap_or(false),
            group_by_description: group_by
                .map(|gb| gb.split(',').any(|s| s == "description"))
                .unwrap_or(false),
            postings: vec![],
        }
    }

    pub fn postings(&self) -> &Vec<AggregatedPosting<'h>> {
        &self.postings
    }

    pub fn group_by_date(&self) -> bool {
        self.group_by_date
    }
}

impl<'h> AddAssign<AggregatedPosting<'h>> for PostingsAggregation<'h> {
    fn add_assign(&mut self, rhs: AggregatedPosting<'h>) {
        if !self.group_by_date && !self.group_by_description {
            self.postings.push(rhs);
            return;
        }

        let mut merge_posting = None;
        for ps in self.postings.iter_mut().rev() {
            if rhs.amount().unit() != ps.amount().unit() {
                continue;
            }
            if self.group_by_date && ps.datetime().start().date() != rhs.datetime().start().date() {
                continue;
            }
            if self.group_by_description && ps.descriptions != rhs.descriptions {
                continue;
            }
            merge_posting = Some(ps);
            break;
        }
        match merge_posting {
            Some(ps) => *ps += rhs,
            None => {
                //if self.group_by_date {
                //    rhs.datetime
                //        .set_end(rhs.datetime.start().with_earliest_time() + Duration::days(1));
                //}
                self.postings.push(rhs)
            }
        }
    }
}

impl<'h> AddAssign<AggregatedPosting<'h>> for AggregatedPosting<'h> {
    fn add_assign(&mut self, rhs: AggregatedPosting<'h>) {
        self.datetime = self.datetime + rhs.datetime;
        for d in rhs.descriptions {
            if !self.descriptions.contains(&d) {
                self.descriptions.push(d);
            }
        }
        if self.account != rhs.account {
            self.account = Arc::new(Account::new("".to_string(), None, vec![]));
        }
        self.amount += rhs.amount;
        self.balance = rhs.balance;
    }
}
