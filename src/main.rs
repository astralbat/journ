/*
 * Copyright (c) 2017-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */

#![feature(cow_is_borrowed)]

mod bal;
mod cag;
mod column_expr;
mod csv;
mod grouping;
mod print;
mod reg;
pub mod report;

extern crate chrono;
#[macro_use]
extern crate log;
extern crate core;

use crate::bal::BalArguments;
use crate::cag::CagArguments;
use crate::csv::CsvArguments;
use crate::print::PrintArguments;
use crate::reg::RegArguments;
use bumpalo_herd::Herd;
use chrono_tz::Tz;
use clap::{Parser, Subcommand};

use env_logger::Builder;
use env_logger::fmt::style::{AnsiColor, Color, RgbColor, Style};
use journ_core::alloc::HerdAllocator;
use journ_core::arguments::{Arguments, Command, DateTimeArguments};
use journ_core::date_and_time::{
    DEFAULT_DATE_FORMAT, DEFAULT_TIME_FORMAT, DateFormat, JDateTime, TimeFormat,
};
use journ_core::error::{BlockContextError, JournError, JournResult};
use journ_core::journal::Journal;
use journ_core::module::MODULES;
use journ_core::parsing::text_block::TextBlock;
use journ_core::python::environment::PythonEnvironment;
use journ_core::{err, parsing};
use std::env;
use std::io::Write;
use std::path::PathBuf;
use std::process::exit;
use std::sync::LazyLock;
use std::time::SystemTime;
use sysinfo::Pid;

static START: LazyLock<SystemTime> = LazyLock::new(SystemTime::now);

#[derive(Parser, Debug)]
#[command(name = "journ", version = "0.2.0")]
struct MainArguments {
    #[command(flatten)]
    datetime_args: DateTimeFormatArguments,
    #[arg(short, long, help = "Output colors")]
    color: bool,
    #[arg(long = "no-color", help = "Disable output colors")]
    no_color: bool,
    #[arg(
        short = 'f',
        long = "file",
        required = true,
        value_name = "JOURNAL_FILE",
        help = "Sets the journal file to parse"
    )]
    file: String,
    #[arg(short = 'b', long = "begin", value_name = "BEGIN_DATE", help = "The date to begin")]
    begin: Option<String>,
    #[arg(short = 'e', long = "end", value_name = "END_DATE", help = "The date to end")]
    end: Option<String>,
    #[arg(
        long = "aux-date",
        help = "Use auxiliary date on postings instead as if it were the primary date"
    )]
    aux_date: bool,
    #[arg(long = "real", help = "Display/process only real, non-virtual postings")]
    real_postings: bool,
    #[command(subcommand)]
    command: CommandArguments,
}

#[derive(clap::Args, Debug)]
pub struct DateTimeFormatArguments {
    #[arg(
        long = "date-format",
        value_name = "DATE_FORMAT",
        help = "The reporting date format to use"
    )]
    date_format: Option<DateFormat<'static>>,
    #[arg(
        long = "time-format",
        value_name = "TIME_FORMAT",
        help = "The reporting time format to use"
    )]
    time_format: Option<TimeFormat<'static>>,
    #[arg(long = "timezone", value_name = "TIMEZONE", help = "The reporting timezone to use")]
    timezone: Option<Tz>,
}

#[derive(Subcommand, Debug)]
enum CommandArguments {
    Print(PrintArguments),
    Bal(BalArguments),
    Reg(RegArguments),
    Csv(CsvArguments),
    Cag(CagArguments),
}

impl CommandArguments {
    pub fn exec(self, journ: Journal) -> JournResult<()> {
        let args = Arguments::get();
        match self {
            CommandArguments::Print(print_args) => {
                args.set_cmd(print_args.into_exec_cmd(&journ)?).execute(journ)
            }
            CommandArguments::Bal(bal_args) => {
                args.set_cmd(bal_args.into_exec_cmd(&journ)?).execute(journ)
            }
            CommandArguments::Reg(reg_args) => {
                args.set_cmd(reg_args.into_exec_cmd(&journ)?).execute(journ)
            }
            CommandArguments::Csv(csv_args) => {
                args.set_cmd(csv_args.into_exec_cmd(&journ)?).execute(journ)
            }
            CommandArguments::Cag(cag_args) => {
                args.set_cmd(cag_args.into_exec_cmd(&journ)?).execute(journ)
            }
        }
    }
}

/// Used to create a command from command arguments.
pub trait IntoExecCommand {
    type Command: ExecCommand;

    fn into_exec_cmd(self, journ: &Journal) -> JournResult<Self::Command>;
}

pub trait ExecCommand: Command {
    fn execute<'h>(&self, journ: Journal) -> JournResult<()>;
}

fn main() {
    use num_format::{SystemLocale, ToFormattedString};

    // Safe because there are no other threads.
    unsafe {
        env::set_var("RUST_BACKTRACE", "full");
    }

    let error_style = Style::default().fg_color(Some(Color::Ansi(AnsiColor::Red))).bold();
    let warn_style = Style::default().fg_color(Some(Color::Ansi(AnsiColor::Yellow))).bold();
    let info_style = Style::default().bold();
    let debug_style = Style::default();
    // Emulates 'faint' in the terminal. Half intensity, not knowing whether the user has a white or black background.
    let trace_style =
        Style::default().fg_color(Some(Color::Rgb(RgbColor::from((128u8, 128u8, 128u8)))));
    Builder::from_default_env()
        .format(move |buf, record| {
            let level_style = match record.level() {
                log::Level::Error => error_style,
                log::Level::Warn => warn_style,
                log::Level::Info => info_style,
                log::Level::Debug => debug_style,
                log::Level::Trace => trace_style,
            };
            writeln!(
                buf,
                "{:08} [{}] - {level_style}{}{level_style:#}",
                START.elapsed().unwrap().as_micros(),
                record.level(),
                //record.module_path().unwrap_or_default(),
                record.args()
            )
        })
        .init();

    // Maybe in the future there will be a better way to do this dynamically.
    MODULES.lock().unwrap().push(journ_cag::module_init::initialize());

    let main_args = MainArguments::parse();
    let mut args = Arguments::default();

    // Initialise the configuration. This is done after parsing the journal so that the price
    // database can search/update currencies.
    args.set_aux_date(main_args.aux_date);
    args.set_real_postings(main_args.real_postings);
    args.set_color(main_args.color);
    args.set_no_color(main_args.no_color);
    args.datetime_args.date_format =
        main_args.datetime_args.date_format.unwrap_or(DEFAULT_DATE_FORMAT.clone());
    args.datetime_args.time_format =
        main_args.datetime_args.time_format.unwrap_or(DEFAULT_TIME_FORMAT.clone());
    args.datetime_args.timezone = main_args.datetime_args.timezone.unwrap_or(Tz::UTC);

    let now = SystemTime::now();
    let herd = Herd::new();
    let herd_allocator = herd.get().alloc(HerdAllocator::new(&herd));

    // The filename must be in utf-8.
    let file_path = PathBuf::from(main_args.file);
    // Set the process working directory to the file's directory.
    if let Some(parent) = file_path.parent() {
        // We haven't validated yet so we need this check. Also, the parent may be "".
        if parent.is_dir() {
            env::set_current_dir(parent).expect("Unable to set working directory");
        }
    }

    let df = &args.datetime_args.date_format;
    let tf = &args.datetime_args.time_format;
    let tz = args.datetime_args.timezone;
    let begin = if let Some(begin) = main_args.begin {
        match JDateTime::parse(df, tf, tz)(begin.as_str()) {
            Ok((_, date)) => Some(date.datetime()),
            Err(e) => {
                print_jerror(err!(e.to_string()));
                exit(1)
            }
        }
    } else {
        None
    };
    let end = if let Some(end) = main_args.end {
        match JDateTime::parse(df, tf, tz)(end.as_str()) {
            Ok((_, date)) => Some(date.datetime()),
            Err(e) => {
                print_jerror(err!(e.to_string()));
                exit(1)
            }
        }
    } else {
        None
    };
    args.set_begin(begin);
    args.set_end(end);

    // Parse the ledger
    let args = Arguments::set(args);
    let file_name = herd_allocator.alloc(PathBuf::from(file_path.file_name().unwrap()));
    PythonEnvironment::startup();
    let journ = match TextBlock::from_file(file_name.as_path(), herd_allocator, None)
        .and_then(|block| Journal::parse(args, file_name.as_path(), block, herd_allocator))
    {
        Ok(journ) => {
            info!(
                "Parsing took {}ns",
                now.elapsed()
                    .unwrap()
                    .as_nanos()
                    .to_formatted_string(&SystemLocale::default().unwrap())
            );
            // Print memory usage of the journal.
            let mut sys = sysinfo::System::new_all();
            sys.refresh_all();
            if let Some(process) = sys.process(Pid::from(std::process::id() as usize)) {
                debug!("Memory usage: {} MB", process.memory() / 1024 / 1024);
                debug!("Virtual memory: {} MB", process.virtual_memory() / 1024 / 1024);
            }

            journ
        }
        Err(err) => {
            print_jerror(err);
            exit(1)
        }
    };

    // Execute the command
    if let Err(e) = main_args.command.exec(journ) {
        print_jerror(e);
        exit(1)
    }
}

fn print_jerror(mut e: JournError) {
    e.prune_except_last::<BlockContextError>();

    eprint!("{}:", env::args().next().unwrap(),);
    eprintln!(" {}", e);
}

/// Used to flatten an argument that may be specified as zero or more comma separated value
/// strings.
/// This expands them to a single list for consistent processing.
pub fn flatten_csv(field: &Vec<String>) -> Vec<String> {
    let mut flattened = vec![];
    for f in field {
        flattened.extend(parsing::util::separated_fields(',', f).map(|s| s.to_string()));
    }
    flattened
}

/// Converts date and time arguments to a `DateTimeArguments` struct.
/// Uses the root configuration for fallback if the arguments are not specified.
pub fn read_date_time_args(journ: &Journal) -> JournResult<DateTimeArguments> {
    let config = journ.config();
    let mut dt_args = DateTimeArguments::default();
    let main_args = MainArguments::parse();
    dt_args.date_format = match main_args.datetime_args.date_format {
        Some(df) => df,
        None => config.date_format().clone().into_owned(),
    };
    dt_args.time_format = match main_args.datetime_args.time_format {
        Some(tf) => tf,
        None => config.time_format().clone().into_owned(),
    };
    dt_args.timezone = match main_args.datetime_args.timezone {
        Some(tz) => tz,
        None => config.timezone(),
    };
    Ok(dt_args)
}
