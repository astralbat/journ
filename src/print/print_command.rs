/*
 * Copyright (c) 2024-2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::ExecCommand;
use chrono::DateTime;
use chrono_tz::Tz;
use journ_core::arguments::{Arguments, Command};
use journ_core::configuration::{AccountFilter, Filter};
use journ_core::directive::{Directive, DirectiveKind};
use journ_core::error::JournResult;
use journ_core::journal::Journal;
use journ_core::journal_entry::JournalEntry;
use journ_core::journal_node::JournalNode;
use std::ops::{Bound, RangeBounds};
use std::path::Path;
use std::process::exit;

#[derive(Default, Debug)]
pub struct PrintCommand {
    accounts: Vec<String>,
    print_file: Option<String>,
}

impl Command for PrintCommand {}

struct DatetimeEntryFilter<R: RangeBounds<DateTime<Tz>>> {
    range: R,
}
impl<R: RangeBounds<DateTime<Tz>>> Filter<JournalEntry<'_>> for DatetimeEntryFilter<R> {
    fn is_included(&self, entry: &JournalEntry<'_>) -> bool {
        self.range.contains(&entry.date_and_time().datetime_from())
    }
}

impl PrintCommand {
    pub fn set_accounts(&mut self, accounts: Vec<String>) {
        self.accounts = accounts;
    }

    fn create_dir_filter(&self, args: &Arguments) -> impl for<'h> Filter<Directive<'h>> {
        struct DirFilter {
            account_filter: AccountFilter,
            datetime_filter: DatetimeEntryFilter<(Bound<DateTime<Tz>>, Bound<DateTime<Tz>>)>,
        }
        impl<'h> Filter<Directive<'h>> for DirFilter {
            fn is_included(&self, item: &Directive<'h>) -> bool {
                if let DirectiveKind::Entry(je) = item.kind() {
                    self.datetime_filter.is_included(je)
                        && je.postings().any(|p| self.account_filter.is_included(p.account()))
                } else {
                    self.account_filter.is_empty()
                }
            }
        }

        DirFilter {
            account_filter: AccountFilter::new(self.accounts.iter()),
            datetime_filter: DatetimeEntryFilter { range: args.begin_end_range() },
        }
    }
}

impl ExecCommand for PrintCommand {
    fn execute<'j, 'h: 'j>(
        &self,
        journ: &'j mut Journal<'j>,
        args: &'h Arguments,
    ) -> JournResult<()> {
        let dir_filter = self.create_dir_filter(args);
        match self.print_file.as_ref().map(|s| s.to_string()) {
            Some(pf) => match journ.find_node_by_filename(Path::new(&pf)) {
                Some(jf) => jf.print(&dir_filter)?,
                None => {
                    eprintln!("No file matches: {}", pf);
                    exit(1)
                }
            },
            None => JournalNode::print_all(journ.root(), &dir_filter)?,
        }
        Ok(())
    }
}
