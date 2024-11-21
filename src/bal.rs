/*
 * Copyright (c) 2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::{ExecCommand, IntoExecCommand};
use journ_core::arguments::{Arguments, BalCommand};
use journ_core::error::JournResult;
use journ_core::journal::Journal;

#[derive(clap::Args, Debug)]
#[command(name = "bal", about = "Print balances of accounts")]
pub struct BalArguments {
    #[arg(value_name = "ACCOUNT_FILTER", help = "Filter accounts by regexp pattern")]
    account_filter: Vec<String>,
    #[arg(short = 'u', long = "unit", value_name = "UNIT", help = "Filter by unit expression")]
    unit_filter: Vec<String>,
    #[arg(short = 'v', long = "value", value_name = "VALUE", help = "Value in another unit")]
    value_units: Vec<String>,
    #[arg(short = 'w', long = "write", help = "write the file(s) with valuations")]
    write_file: bool,
}

impl IntoExecCommand for BalArguments {
    type Command = BalCommand;
    fn into_exec_cmd(self, _journ: &Journal) -> JournResult<Self::Command> {
        let mut bal_cmd = BalCommand::default();
        bal_cmd.set_account_filter(self.account_filter);
        bal_cmd.set_unit_filter(self.unit_filter);
        bal_cmd.set_value_units(self.value_units);
        bal_cmd.set_write_file(self.write_file);
        Ok(bal_cmd)
    }
}

impl ExecCommand for BalCommand {
    fn execute(&self, mut journ: Journal) -> JournResult<()> {
        let args = Arguments::get();
        let bals = journ.bal(args)?;
        print!("{}", bals);
        // We only need to write the price database.
        if self.write_file() {
            journ
                .root()
                .config()
                .price_databases()
                .into_iter()
                .for_each(|db| db.overwrite().unwrap());
        }
        Ok(())
    }
}
