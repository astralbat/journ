/*
 * Copyright (c) 2024-2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
mod bal_command;

use crate::bal::bal_command::BalCommand;
use crate::{ExecCommand, IntoExecCommand};
use clap::ValueEnum;
use journ_core::configuration::Filter;
use journ_core::error::JournResult;
use journ_core::journal::Journal;
use std::fmt::Display;
use std::str::FromStr;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BalColumn {
    Account,
    Amount,
    Value(String),
}

impl FromStr for BalColumn {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_str() {
            "account" => Ok(BalColumn::Account),
            "amount" => Ok(BalColumn::Amount),
            other if other.starts_with("value:") => {
                let unit = s[6..].to_string();
                Ok(BalColumn::Value(unit))
            }
            _ => Err(format!("Invalid column name: {s}")),
        }
    }
}

impl Display for BalColumn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BalColumn::Account => write!(f, "Account"),
            BalColumn::Amount => write!(f, "Amount"),
            BalColumn::Value(unit) => write!(f, "{unit}"),
        }
    }
}

#[derive(clap::Args, Debug)]
#[command(name = "bal", about = "Print balances of accounts")]
pub struct BalArguments {
    #[arg(value_name = "ACCOUNT_FILTER", help = "Filter accounts by regexp pattern")]
    account_filter: Vec<String>,
    #[arg(short = 'u', long = "unit", value_name = "UNIT", help = "Filter by unit expression")]
    unit_filter: Vec<String>,
    #[arg(long = "csv", help = "write the output in CSV format")]
    write_csv: bool,
    #[arg(
        short = 'o',
        help = "A comma separated list of columns to print. This can be 'account', 'amount' or 'value:<unit>'. Any valuation will be looked up on the entry or evaluated on the date/time of the entry",
        value_delimiter = ',',
        default_value = "account,amount"
    )]
    columns: Vec<BalColumn>,
    #[arg(
        short = 'w',
        long = "write",
        help = "write the file(s) with any new entry valuations if any were not explicitly set"
    )]
    write_file: bool,
}

impl IntoExecCommand for BalArguments {
    type Command = BalCommand;
    fn into_exec_cmd(self, _journ: &Journal) -> JournResult<Self::Command> {
        let mut bal_cmd = BalCommand::default();
        bal_cmd.set_account_filter(self.account_filter);
        bal_cmd.set_unit_filter(self.unit_filter);
        bal_cmd.set_write_csv(self.write_csv);
        bal_cmd.set_columns(self.columns);
        bal_cmd.set_write_file(self.write_file);
        Ok(bal_cmd)
    }
}
