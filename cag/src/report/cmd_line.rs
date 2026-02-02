/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::cgt_configuration::EventPattern;
use crate::report::cag_command::CagCommand;
use clap::Parser;
use itertools::Itertools;
use journ_core::error::JournResult;
use journ_core::journal::Journal;
use journ_core::report::command::IntoExecCommand;
use journ_core::report::command::arguments::{Arguments, DateTimeFormatCommand};
use journ_core::report::command::cmd_line::BeginAndEndArguments;

#[derive(Parser, Debug)]
#[command(name = "cag", about = "Capital gains report")]
pub struct CagArguments {
    #[command(flatten)]
    begin_and_end: BeginAndEndArguments,
    #[arg(value_name = "UNIT", value_delimiter = ',', help = "Specify which units to report on")]
    unit_filter: Vec<String>,
    #[arg(short = 'a', long = "accounts", value_delimiter = ',')]
    account_filter: Vec<String>,
    #[arg(
        long = "events",
        number_of_values = 1,
        value_delimiter = ',',
        help = "Filter the kind of events to report. E.g. \"-e Pooled MyPool,Matched \""
    )]
    event_filter: Vec<EventPattern>,
    #[arg(short = 'p', long = "pool", value_delimiter = ',')]
    pool_filter: Vec<String>,
    #[arg(long, help = "Show only the first `head` results")]
    head: Option<usize>,
    #[arg(long, help = "Show only the last `tail` results")]
    tail: Option<usize>,
    #[arg(
        long = "group-deals-by-date",
        help = "Aggregate journal entry deals that occur on the same day before further processing. This is useful when you have multiple deals on the same day and you want to see the totals for the day."
    )]
    group_deals_by_date: bool,
    #[arg(long = "group-by", value_name = "GROUP_BY", help = "Group events together")]
    group_by: Option<String>,
    #[arg(
        long = "order-by",
        value_delimiter = ',',
        value_name = "ORDER_BY",
        help = "Order the output by the specified column(s). Valid values are: event-date, deal-date, unit, actual-cost, total-cost"
    )]
    order_by: Option<String>,
    #[arg(long = "descending")]
    order_descending: bool,
    #[arg(
        short = 'o',
        value_delimiter = ',',
        help = "A comma separated list of columns to print for each capital gains event.",
        default_value = "EventDate.Start.Date as Date, Description, Amount, Cost, pool.BalanceAfter.Amount, match.Gain as Gain"
    )]
    column_spec: Vec<String>,
    #[arg(long = "where", help = "Filter the results on conditions", value_delimiter = ',')]
    where_conditions: Vec<String>,
    #[arg(long, help = "Title to display for the table")]
    title: Option<String>,
    #[arg(short = 'H', long = "no-header", help = "do not print header row")]
    no_header: bool,
    #[arg(long = "total", help = "Show a total row")]
    show_total: bool,
    #[arg(
        long = "total-as",
        value_delimiter = ',',
        help = "Show a total row using the supplied column specification"
    )]
    #[arg(long, help = "Keep total to a single row in height in the output; do not show lists")]
    brief_total: bool,
    #[arg(long = "yaml", help = "Write output as yaml")]
    yaml: bool,
    #[arg(
        long,
        help = "Provide a column name to enable storing yaml rows in a map instead of a list by default - the keys of which are the values of this column"
    )]
    yaml_map_key: Option<String>,
    #[arg(long, allow_hyphen_values = true, num_args = 0..)]
    chain: Vec<String>,
}

impl IntoExecCommand for CagArguments {
    type Command = CagCommand;

    #[allow(clippy::field_reassign_with_default)]
    fn into_exec_cmd(self, journ: &Journal, args: &Arguments) -> JournResult<CagCommand> {
        let datetime_fmt_cmd = DateTimeFormatCommand::from_args_or_config(args, journ.config());
        let cmd: CagCommand = CagCommand {
            begin_and_end_cmd: self.begin_and_end.into_cmd(&datetime_fmt_cmd),
            datetime_fmt_cmd,
            account_filter: self.account_filter,
            unit_filter: self.unit_filter,
            event_filter: self.event_filter,
            pool_filter: self.pool_filter,
            head: self.head,
            tail: self.tail,
            group_deals_by_date: self.group_deals_by_date,
            group_by: self.group_by,
            order_by_spec: self.order_by,
            order_ascending: !self.order_descending,
            output_yaml: self.yaml,
            yaml_map_key: self.yaml_map_key,
            title: self.title,
            no_header: self.no_header,
            show_total: self.show_total,
            brief_total: self.brief_total,
            column_spec: self.column_spec.join(","),
            where_conditions: if self.where_conditions.is_empty() {
                None
            } else {
                Some(self.where_conditions.join(","))
            },
            chain: if self.chain.is_empty() {
                None
            } else {
                // Clap ignores first argument
                let mut chain_args = vec!["cag".to_string()];
                chain_args.append(&mut self.chain.clone());
                Some(Box::new(CagArguments::parse_from(chain_args).into_exec_cmd(journ, args)?))
            },
        };
        Ok(cmd)
    }
}
