/*
 * Copyright (c) 2024-2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::cgt_configuration::{
    CapitalGainsColumn, CapitalGainsGroupBy, CapitalGainsOrderBy, EventFilter, EventPattern,
};
use crate::computer::CapitalGainsComputer;
use crate::report::cmd_line::CagArguments;
use clap::Parser;
use journ_core::account::Account;
use journ_core::configuration::{AccountFilter, Filter, UnitFilter};
use journ_core::error::JournResult;
use journ_core::journal::Journal;
use journ_core::module::ModuleCommand;
use journ_core::reporting::command::arguments::{Arguments, Command, DateTimeFormatCommand};
use journ_core::reporting::command::cmd_line::BeginAndEndCommand;
use journ_core::reporting::command::{ExecCommand, IntoExecCommand};
use journ_core::unit::Unit;

#[derive(Default, Debug)]
pub struct CagCommand {
    pub datetime_fmt_cmd: DateTimeFormatCommand,
    pub begin_and_end_cmd: BeginAndEndCommand,
    pub account_filter: Vec<String>,
    pub unit_filter: Vec<String>,
    pub event_filter: Vec<EventPattern>,
    pub group_by: Vec<CapitalGainsGroupBy>,
    pub group_deals_by_date: bool,
    pub show_group: bool,
    pub order_by: Vec<CapitalGainsOrderBy>,
    pub disposed_mode: bool,
    pub output_csv: bool,
    pub output_yaml: bool,
    pub show_time: bool,
    pub write_valuations: bool,
    pub write_metadata: bool,
    pub columns: Vec<CapitalGainsColumn>,
}

impl CagCommand {
    pub fn account_filter(&self) -> impl for<'h> Filter<Account<'h>> + '_ {
        AccountFilter::new(self.account_filter.iter())
    }

    pub fn set_account_filter(&mut self, account_filter: Vec<String>) {
        self.account_filter = account_filter;
    }

    pub fn unit_filter(&self) -> impl for<'h> Filter<Unit<'h>> + '_ {
        UnitFilter::new(self.unit_filter.iter())
    }

    pub fn set_unit_filter(&mut self, units: Vec<String>) {
        self.unit_filter = units;
    }

    pub fn event_filter(&self) -> EventFilter<'_> {
        EventFilter(&self.event_filter)
    }

    pub fn set_event_filter(&mut self, event_filter: Vec<EventPattern>) {
        self.event_filter = event_filter;
    }

    pub fn output_yaml(&self) -> bool {
        self.output_yaml
    }

    pub fn set_output_yaml(&mut self, output_yaml: bool) {
        self.output_yaml = output_yaml;
    }
}

impl Command for CagCommand {
    fn datetime_fmt_cmd(&self) -> &DateTimeFormatCommand {
        &self.datetime_fmt_cmd
    }

    fn begin_and_end_cmd(&self) -> &BeginAndEndCommand {
        &self.begin_and_end_cmd
    }
}

impl ModuleCommand for CagCommand {
    fn name(&self) -> &'static str {
        "cag"
    }

    fn create(
        &self,
        journal: &Journal,
        args: &Arguments,
        command_args: &[String],
    ) -> JournResult<Box<dyn ExecCommand>> {
        CagArguments::parse_from(command_args)
            .into_exec_cmd(journal, args)
            .map(Box::new)
            .map(|boxed| boxed as Box<dyn ExecCommand>)
    }
}

impl ExecCommand for CagCommand {
    fn execute<'j, 'h: 'j>(&self, journ: &'j mut Journal<'j>) -> JournResult<()> {
        let mut computer = CapitalGainsComputer::new();
        let result = computer.compute_gains(&journ);

        // Always write the price databases.
        journ.config().price_databases().into_iter().for_each(|db| db.write_file().unwrap());

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
