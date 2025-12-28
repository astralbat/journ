/*
 * Copyright (c) 2017-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */

#![feature(cow_is_borrowed)]
#![feature(bool_to_result)]

mod bal;
mod print;
mod reg;

extern crate chrono;
#[macro_use]
extern crate log;
extern crate core;

use bumpalo_herd::Herd;
use chrono_tz::Tz;
use clap::{Parser, Subcommand};

use crate::bal::cmd_line::BalArguments;
use crate::print::cmd_line::PrintArguments;
use crate::reg::cmd_line::RegArguments;
use env_logger::fmt::style::{AnsiColor, Color, RgbColor, Style};
use env_logger::{Builder, Env};
use journ_core::alloc::HerdAllocator;
use journ_core::date_and_time::{DateFormat, TimeFormat};
use journ_core::error::JournResult;
use journ_core::journal::Journal;
use journ_core::module::{MODULES, Module};
use journ_core::parsing::text_block::TextBlock;
use journ_core::python::environment::PythonEnvironment;
use journ_core::reporting::command::arguments::{Arguments, Cmd, DateTimeFormatCommand};
use journ_core::reporting::command::{ExecCommand, IntoExecCommand, print_jerror};
use journ_core::{err, parsing};
use num_format::{SystemLocale, ToFormattedString};
use std::env;
use std::io::Write;
use std::path::{Path, PathBuf};
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
    date_format: Option<&'static DateFormat<'static>>,
    #[arg(
        long = "time-format",
        value_name = "TIME_FORMAT",
        help = "The reporting time format to use"
    )]
    time_format: Option<&'static TimeFormat<'static>>,
    #[arg(long = "timezone", value_name = "TIMEZONE", help = "The reporting timezone to use")]
    timezone: Option<Tz>,
}

#[derive(Subcommand, Debug)]
enum CommandArguments {
    Print(PrintArguments),
    Bal(BalArguments),
    Reg(RegArguments),
    #[command(external_subcommand)]
    Module(Vec<String>),
}

impl CommandArguments {
    pub fn exec<'j, 'h: 'j>(self, journ: &'j mut Journal<'j>, args: &Arguments) -> JournResult<()> {
        match self {
            CommandArguments::Print(print_args) => {
                Cmd::set(Box::new(print_args.into_exec_cmd(journ, args)?)).execute(journ)
            }
            CommandArguments::Bal(bal_args) => {
                Cmd::set(Box::new(bal_args.into_exec_cmd(journ, args)?)).execute(journ)
            }
            CommandArguments::Reg(reg_args) => {
                Cmd::set(Box::new(reg_args.into_exec_cmd(journ, args)?)).execute(journ)
            }
            CommandArguments::Module(module_args) => {
                match MODULES
                    .lock()
                    .unwrap()
                    .iter()
                    .filter_map(Module::command)
                    .find(|c| c.name() == module_args[0])
                {
                    Some(command) => {
                        let cmd: &dyn ExecCommand =
                            Cmd::set(command.create(journ, args, &module_args[1..])?);
                        cmd.execute(journ)
                    }
                    None => Err(err!("Unknown command: '{}'", module_args[0])),
                }
            }
        }
    }
}

fn main() {
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
    Builder::from_env(Env::default().default_filter_or("off"))
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
    let args = Arguments {
        datetime_cmd: DateTimeFormatCommand::new(
            main_args.datetime_args.date_format,
            main_args.datetime_args.time_format,
            main_args.datetime_args.timezone,
        ),
        aux_date: main_args.aux_date,
        real_postings: main_args.real_postings,
        color: main_args.color,
        no_color: main_args.no_color,
    };
    let args = Cmd::set_args(args);

    // The filename must be in utf-8.
    let file_path = PathBuf::from(main_args.file);
    // Set the process working directory to the file's directory.
    if let Some(parent) = file_path.parent() {
        // We haven't validated yet so we need this check. Also, the parent may be "".
        if parent.is_dir() {
            env::set_current_dir(parent).expect("Unable to set working directory");
        }
    }

    let herd = Herd::new();
    let herd_allocator = herd.get().alloc(HerdAllocator::new(&herd));
    let mut journ = parse(&file_path, args, herd_allocator);
    // Execute the command
    if let Err(e) = main_args.command.exec(&mut journ, &args) {
        print_jerror(e);
        exit(1)
    }
    //herd.reset();
}

/// Parse the ledger
fn parse<'h>(
    file_path: &Path,
    args: &Arguments,
    herd_allocator: &'h HerdAllocator<'h>,
) -> Journal<'h> {
    let now = SystemTime::now();
    let file_name = herd_allocator.alloc(PathBuf::from(file_path.file_name().unwrap()));
    PythonEnvironment::startup();
    match TextBlock::from_file(file_name.as_path(), herd_allocator, None)
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
    }
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
