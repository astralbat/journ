/*
 * Copyright (c) 2024-2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::ExecCommand;
use journ_core::configuration::{AccountFilter, Filter};
use journ_core::datetime::JDateTime;
use journ_core::directive::{Directive, DirectiveKind};
use journ_core::error::JournResult;
use journ_core::journal::Journal;
use journ_core::journal_entry::JournalEntry;
use journ_core::journal_node::JournalNode;
use journ_core::report::command::arguments::{Command, DateTimeFormatCommand};
use journ_core::report::command::chained_result::ChainingResult;
use journ_core::report::command::cmd_line::BeginAndEndCommand;
use std::ops::RangeBounds;
use std::path::Path;
use std::process::exit;

#[derive(Default, Debug)]
pub struct PrintCommand {
    pub(super) datetime_fmt_cmd: DateTimeFormatCommand,
    pub(super) begin_and_end_cmd: BeginAndEndCommand,
    pub(super) accounts: Vec<String>,
    pub(super) print_file: Option<String>,
}

impl Command for PrintCommand {
    fn datetime_fmt_cmd(&self) -> &DateTimeFormatCommand {
        &self.datetime_fmt_cmd
    }

    fn begin_and_end_cmd(&self) -> &BeginAndEndCommand {
        &self.begin_and_end_cmd
    }
}

struct DatetimeEntryFilter<R> {
    range: R,
}
impl<'a, R> Filter<JournalEntry<'a>> for DatetimeEntryFilter<R>
where
    R: RangeBounds<JDateTime>,
{
    fn is_included(&self, entry: &JournalEntry<'a>) -> bool {
        self.range.contains(&entry.date_and_time().from())
    }
}

impl PrintCommand {
    fn create_dir_filter<'a, R>(&self, datetime_range: R) -> impl Filter<Directive<'a>>
    where
        R: RangeBounds<JDateTime>,
    {
        struct DirFilter<R> {
            account_filter: AccountFilter,
            datetime_filter: DatetimeEntryFilter<R>,
        }
        impl<'d, R> Filter<Directive<'d>> for DirFilter<R>
        where
            R: RangeBounds<JDateTime>,
        {
            fn is_included(&self, item: &Directive<'d>) -> bool {
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
            datetime_filter: DatetimeEntryFilter { range: datetime_range },
        }
    }
}

impl ExecCommand for PrintCommand {
    fn execute<'h>(
        &'h self,
        journ: &'h mut Journal<'h>,
        _chained: Option<ChainingResult>,
    ) -> JournResult<()> {
        let dir_filter = self.create_dir_filter(self.begin_and_end_cmd.begin_end_range());
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
