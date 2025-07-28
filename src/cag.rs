/*
 * Copyright (c) 2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::{ExecCommand, IntoExecCommand, print_jerror, read_date_time_args};
use journ_cag::cgt_configuration::{
    CagCommand, CapitalGainsColumn, CapitalGainsGroupBy, CapitalGainsOrderBy, EventPattern,
};
use journ_cag::computer::CapitalGainsComputer;
use journ_core::arguments::Arguments;
use journ_core::date_and_time::JDateTime;
use journ_core::err;
use journ_core::error::JournResult;
use journ_core::journal::Journal;
use std::process::exit;

#[derive(clap::Args, Debug)]
#[command(name = "cag", about = "Capital gains tax reporting")]
pub struct CagArguments {
    #[arg(value_name = "UNIT", value_delimiter = ',', help = "Specify which units to report on")]
    unit_filter: Vec<String>,
    #[arg(
        short = 'e',
        long = "events",
        number_of_values = 1,
        value_delimiter = ',',
        help = "Filter the kind of events to report. E.g. \"-e Pooled MyPool,Matched \""
    )]
    event_filter: Vec<EventPattern>,
    #[arg(
        long = "disposed",
        help = "Disposed mode will group matched events on the same day. This implies --group-by deal-date and --order-by deal-date."
    )]
    disposed_mode: bool,
    #[arg(long = "write-valuations", help = "write the file(s) with valuations")]
    write_valuations: bool,
    #[arg(long = "write-metadata", help = "write deal metadata to the entries")]
    write_metadata: bool,
    #[arg(
        long = "show-time",
        help = "Show the time of day for events and deals in the output where possible. This is not possible when deals or events span a datetime range."
    )]
    show_time: bool,
    #[arg(
        long = "group-deals-by-date",
        help = "Aggregate journal entry deals that occur on the same day before further processing. This is useful when you have multiple deals on the same day and you want to see the totals for the day."
    )]
    group_deals_by_date: bool,
    #[arg(
        long = "group-by",
        value_name = "GROUP_BY",
        value_delimiter = ',',
        help = "Group events together. Valid values are: event-date, deal-date, pool, event, disposal"
    )]
    group_by: Vec<CapitalGainsGroupBy>,
    #[arg(
        long = "order-by",
        value_name = "COLUMNS",
        value_delimiter = ',',
        help = "Order the output by the specified column(s). Valid values are: event-date, deal-date, unit, actual-cost, total-cost"
    )]
    order_by: Vec<CapitalGainsOrderBy>,
    #[arg(
        long = "show-group",
        help = "When using --group-by, show the group breakdown in the output"
    )]
    show_group: bool,
    #[arg(
    long = "columns",
    value_name = "COLUMNS",
    value_delimiter = ',',
    default_values = vec!["deal-date", "unit", "event", "gain", "loss", "pool-bal-after"],
    help = "Specify which columns to display. Valid values are: event-date, deal-date, unit, event, pool, acquired, total-cost, disposed, net-proceeds, gain, loss, pool-bal-before, pool-bal-after, description.\nAdditional columns can be added based on associated entry metadata. E.g. +CAG-Note."
    )]
    columns: Vec<CapitalGainsColumn>,
    #[arg(long = "csv", help = "Write output as rows of comma separated values")]
    csv: bool,
    #[arg(long = "yaml", help = "Write output as yaml")]
    yaml: bool,
    #[arg(
        long = "from",
        help = "Specify the date from which to report. This is inclusive. E.g. \"2021-01-01\""
    )]
    from: Option<String>,
    #[arg(
        long = "to",
        help = "Specify the date to which to report. This is exclusive. E.g. \"2021-12-31\""
    )]
    to: Option<String>,
}

impl IntoExecCommand for CagArguments {
    type Command = CagCommand;

    #[allow(clippy::field_reassign_with_default)]
    fn into_exec_cmd(self, journ: &Journal) -> JournResult<CagCommand> {
        let mut cmd: CagCommand = CagCommand::default();
        cmd.datetime_args = read_date_time_args(journ)?;
        cmd.unit_filter = self.unit_filter;
        cmd.event_filter = self.event_filter;
        cmd.show_time = self.show_time;
        cmd.group_deals_by_date = self.group_deals_by_date;
        cmd.group_by = self.group_by;
        cmd.show_group = self.show_group;
        cmd.order_by = self.order_by;
        cmd.output_yaml = self.yaml;
        cmd.write_valuations = self.write_valuations;
        cmd.write_metadata = self.write_metadata;
        cmd.disposed_mode = self.disposed_mode;
        cmd.columns = self.columns;

        let df = &cmd.datetime_args.date_format;
        let tf = &cmd.datetime_args.time_format;
        let tz = cmd.datetime_args.timezone;
        cmd.from = self.from.map(|s| match JDateTime::parse(df, tf, tz)(s.as_str()) {
            Ok((_, date)) => date.datetime(),
            Err(e) => {
                print_jerror(err!(e.to_string()));
                exit(1)
            }
        });
        cmd.to = self.to.map(|s| match JDateTime::parse(df, tf, tz)(s.as_str()) {
            Ok((_, date)) => date.datetime(),
            Err(e) => {
                print_jerror(err!(e.to_string()));
                exit(1)
            }
        });
        Ok(cmd)
    }
}

impl ExecCommand for CagCommand {
    fn execute(&self, journ: Journal) -> JournResult<()> {
        let args = Arguments::get();

        let mut computer = CapitalGainsComputer::new(args);
        let result = computer.compute_gains(&journ);

        // Always write the price databases.
        journ.root().config().price_databases().into_iter().for_each(|db| db.write_file().unwrap());

        match result {
            Ok(cg) => {
                println!("{}", cg);
                if self.write_valuations || self.write_metadata {
                    journ.root().write_file_recursive()?;
                }
                Ok(())
            }
            Err(e) => Err(e),
        }
    }
}
