/*
 * Copyright (c) 2023-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::account::Account;
use crate::date_and_time::{DateAndTime, DateFormat, JDateTime, TimeFormat};
use crate::directive::{Directive, DirectiveKind};
use crate::error::parsing::{IParseError, promote};
use crate::error::{JournErrors, JournResult};
use crate::journal_node::JournalNodeKind;
use crate::module::MODULES;
use crate::parsing::amount::unit;
use crate::parsing::text_block::TextBlock;
use crate::parsing::text_input::{
    BlockInput, ConfigInput, LocatedInput, NodeInput, TextBlockInput, TextInput,
};
use crate::parsing::util::{
    double_quoted, line_value, multiline_value_string, param_value, repeat0, rest_line1,
    separated_field, word,
};
use crate::parsing::{IParseResult, entry};
use crate::parsing::{JParseResult, util};
use crate::price::Price;
use crate::price_db::PriceDatabase;
use crate::python::lambda::Lambda;
use crate::python::mod_ledger::PythonLedgerModule;
use crate::unit::{RoundingStrategy, Unit, Units};
use crate::{err, match_block, match_blocks, parsing};
use chrono_tz::Tz;
use nom::branch::alt;
use nom::character::complete::{space0, space1};
use nom::combinator::{consumed, map, map_parser, map_res, opt, rest};
use nom::sequence::{pair, preceded};
use nom::{Err as NomErr, Finish, Parser};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;

pub const E_LEADING_SPACE: &str = "Leading Space Expected";

pub const E_UNKNOWN_PARAMETER: &str = "Unknown parameter";
pub const E_VALUE: &str = "Value expected";

pub const E_ACCOUNT_NAME: &str = "Account name expected";
pub const E_METADATA: &str = "Metadata expected";

fn account_directive<'h, I>(input: I) -> JParseResult<I, Directive<'h>>
where
    I: TextInput<'h> + ConfigInput<'h> + BlockInput<'h> + LocatedInput<'h>,
{
    let (input, full_name) =
        promote(E_ACCOUNT_NAME, preceded(space0, alt((double_quoted, word))))(input)?;
    let mut metadata = vec![];

    let rem = match_blocks!(input.clone(),
        entry::metadata => |m| Ok(metadata.push(m)),
        util::comment => |_| Ok(()),
        rest => |i: I| Err(NomErr::Error(err!(i.into_err(E_METADATA))))
    )?
    .0;
    let mut config_mut = input.config_mut();
    let parent =
        Account::parent_str(full_name.text()).map(|s| config_mut.get_or_create_account(&s));
    let acc = Arc::new(Account::new(full_name.text().to_string(), parent, metadata));
    let merged = config_mut.merge_account(acc);
    Ok((rem, Directive::new(input.block(), DirectiveKind::Account(merged))))
}

pub const E_INVALID_DATE_FORMAT: &str = "Invalid date format";

fn dateformat_directive<'h, I>(input: I) -> JParseResult<I, Directive<'h>>
where
    I: TextInput<'h> + ConfigInput<'h> + BlockInput<'h>,
{
    let (rem, df) = DateFormat::parse_format(input.clone())?;
    let mut config = input.config_mut();
    config.set_date_format(df);
    Ok((
        rem,
        Directive::new(
            input.block(),
            DirectiveKind::DateFormat(config.as_herd_ref().date_format()),
        ),
    ))
}

fn timeformat_directive<'h, I>(input: I) -> JParseResult<I, Directive<'h>>
where
    I: TextInput<'h> + ConfigInput<'h> + BlockInput<'h>,
{
    let (rem, tf) = TimeFormat::parse_format(input.clone())?;
    let mut config = input.config_mut();
    config.set_time_format(tf);
    Ok((
        rem,
        Directive::new(
            input.block(),
            DirectiveKind::TimeFormat(config.as_herd_ref().time_format()),
        ),
    ))
}

pub const E_BAD_TIMEZONE: &str = "Bad timezone";

fn timezone_directive<'h, I>(input: I) -> JParseResult<I, Directive<'h>>
where
    I: TextInput<'h> + ConfigInput<'h> + BlockInput<'h>,
{
    let (rem, input) = rest::<_, ()>(input).unwrap();
    let tz = Tz::from_str(input.text())
        .map_err(|_| NomErr::Failure(input.clone().into_err(E_BAD_TIMEZONE)))?;
    input.config_mut().set_time_zone(tz);
    Ok((rem, Directive::new(input.block(), DirectiveKind::TimeZone(tz))))
}

fn read_unit<'h, 's, 'e, 'p, 'a, I>(
    unit_code: &'h str,
    all_codes: &'a [&'h str],
    primary: Option<&'h Unit<'h>>,
) -> impl FnMut(I) -> JParseResult<I, &'h Unit<'h>> + 'a
where
    I: TextInput<'h>
        + ConfigInput<'h>
        + NodeInput<'h, 's, 'e, 'p>
        + BlockInput<'h>
        + LocatedInput<'h>,
    'h: 'e,
    'e: 's,
    's: 'p,
{
    move |input: I| {
        let node = input.parse_node();
        let mut unit = Unit::new(unit_code);
        unit.set_aliases(all_codes.iter().map(|c| c.to_string()).collect());
        let mut metadata = vec![];
        let rem = match_blocks!(input.clone(),
                param_value("name") => |input: I| {
                    let name = promote("Invalid unit name", line_value)(input)?.1;
                    unit.set_name(Some(name.text().to_string()));
                    Ok(())
                },
                param_value("format") => |input| {
                    let format = promote("Invalid unit format", map_parser(line_value, parsing::amount::unit_format(true)))(input)?.1;
                    unit.set_format(format);
                    Ok(())
                },
                param_value("rounding") => |input: I| {
                    let rounding = promote("Rounding mode must be one of: 'up', 'down', 'halfup', 'halfdown' or 'halfeven'", map_res(line_value, |v: I| RoundingStrategy::from_str(v.text())))(input)?.1;
                    unit.set_rounding_strategy(rounding);
                    Ok(())
                },
                param_value("ranking") => |input: I| {
                    let ranking = promote("Ranking must be an integer >= 0", line_value)(input)?.1;
                    unit.set_conversion_ranking(Some(
                        usize::from_str(ranking.text())
                            .map_err(|e| NomErr::Error(ranking.into_err(E_INVALID_UNIT_RANKING).with_source(e)))?,
                    ));
                    Ok(())
                },
                param_value("value") => |input: I| {
                    let lambda_expr = promote(E_INVALID_VALUE_EXPRESSION, map_res(multiline_value_string, |i| Lambda::from_str(&i)))(input)?.1;
                    unit.set_conversion_expression(Some(lambda_expr));
                    Ok(())
                },
                param_value("prices") => |input| {
                    let (_, (block, path)) = stream(input)?;

                        // Don't parse the price database multiple times.
                        match primary {
                            Some(primary) => {
                                if let Some(pd) = primary.prices() {
                                    unit.set_prices(Some(Arc::clone(pd)));
                                }
                            }
                            None => {
                                let pd = Arc::new(PriceDatabase::new(node.branch_kind(block, path, JournalNodeKind::Prices)));
                                unit.set_prices(Some(pd));
                            }
                        }
                    Ok(())
                },
                util::comment => |_| Ok(()),
                entry::metadata => |m| Ok(metadata.push(m)),
                rest => |input: I| Err(NomErr::Error(input.into_err(E_UNKNOWN_UNIT_KEY)))
            )?.0;
        unit.set_metadata(metadata);
        Ok((rem, input.config_mut().merge_unit(&unit, input.parse_node().allocator())))
    }
}

pub const E_INVALID_ROUNDING_STRATEGY: &str = "Unit rounding strategy expected";
pub const E_INVALID_UNIT_RANKING: &str = "Unit ranking must be a positive integer";
pub const E_INVALID_VALUE_EXPRESSION: &str = "Unit value expression must be a valid lambda";
pub const E_UNKNOWN_UNIT_KEY: &str = "Unknown unit configuration parameter";
pub const E_NAME_EXPECTED: &str = "Unit name expected";

fn unit_directive<'h, 's, 'e, 'p, I>(input: I) -> JParseResult<I, Directive<'h>>
where
    I: TextInput<'h>
        + ConfigInput<'h>
        + NodeInput<'h, 's, 'e, 'p>
        + BlockInput<'h>
        + LocatedInput<'h>,
    'h: 'e,
    'e: 's,
    's: 'p,
{
    // Read the unit codes
    let mut all_codes = vec![];
    let unit_code = || map(separated_field(','), |s: I| s.text());
    // Read the first unit code
    let (input, primary_code) = promote("Unit code expected", unit_code())(input)?;
    all_codes.push(primary_code);
    // Read aliases
    let input = repeat0(unit_code().and_then(|code: &'h str| {
        all_codes.push(code);
        Ok(("", ()))
    }))(input.clone())
    .unwrap()
    .0;
    // Parse the primary unit
    let (rem, primary) = read_unit(primary_code, &all_codes, None)(input.clone())?;
    //let mut units = Vec::with_capacity(1);
    //units.push(primary);

    /*
    // Parse any additional alias units
    for code in all_codes.iter().filter(|c| **c != primary_code) {
        let r = read_unit(code, &all_codes, Some(primary))(input.clone())?;
        rem = r.0;
        units.push(r.1);
    }*/

    Ok((rem, Directive::new(input.block(), DirectiveKind::Unit(primary))))
}

fn units_directive<'h, 's, 'e, 'p, I>(input: I) -> JParseResult<I, Directive<'h>>
where
    I: TextInput<'h>
        + NodeInput<'h, 's, 'e, 'p>
        + BlockInput<'h>
        + ConfigInput<'h>
        + LocatedInput<'h>,
    'h: 'e,
    'e: 's,
    's: 'p,
{
    let node = input.parse_node();
    let mut units = Units::default();
    let mut def_unit = Unit::none();
    let mut metadata = vec![];

    let rem = match_blocks!(input.clone(),
        param_value("rounding") => |input| {
            let rounding = promote("Rounding mode must be one of: 'up', 'down', 'halfup', 'halfdown' or 'halfeven'", map_res(line_value, |v: I| RoundingStrategy::from_str(v.text())))(input)?.1;
            def_unit.set_rounding_strategy(rounding);
            Ok(())
        },
        param_value("value") => |input| {
            let lambda_expr = promote(E_INVALID_VALUE_EXPRESSION, map_res(multiline_value_string, |i| Lambda::from_str(&i)))(input)?.1;
            def_unit.set_conversion_expression(Some(lambda_expr));
            Ok(())
        },
        param_value("prices") => |input: I| {
            let (_, (block, path)) = stream(input)?;
            let pd = Arc::new(PriceDatabase::new(node.branch_kind(block, path, JournalNodeKind::Prices)));
            PythonLedgerModule::set_default_price_database(&pd, node.node().id().journal_incarnation());
            def_unit.set_prices(Some(pd));
            Ok(())
        },
        util::comment => |_| Ok(()),
        entry::metadata => |m| Ok(metadata.push(m))
    )?.0;
    def_unit.set_metadata(metadata);
    let def_unit = input.config_mut().merge_default_unit(&def_unit, input.parse_node().allocator());
    units.set_default_unit(Some(def_unit));

    Ok((rem, Directive::new(input.block(), DirectiveKind::Units(units))))
}

fn python_directive<'h, 's, 'e, 'p, I>(input: I) -> JParseResult<I, Directive<'h>>
where
    I: TextInput<'h> + BlockInput<'h> + NodeInput<'h, 's, 'e, 'p> + LocatedInput<'h>,
    'h: 'e,
    'e: 's,
    's: 'p,
{
    let orig_block = input.block();
    let (rem, (block, path)) = stream(input.clone())?;
    // Do not branch Python execution by default. Other python directives further on may choose to depend on
    // certain functions earlier defined being available.
    Ok((
        rem,
        Directive::new(
            orig_block,
            DirectiveKind::Python(
                input
                    .parse_node()
                    .include_kind(block, path, JournalNodeKind::Python)
                    .map_err(NomErr::Failure)?,
            ),
        ),
    ))
}

pub const E_BASE_UNIT: &str = "Unable to read base unit";
pub const E_PARSE_PRICE: &str = "Unable to read quote price";
pub const E_PARSE_DATETIME: &str = "Unable to read date/time";

fn price<'h, 's, 'e, 'p, I>(input: I) -> JParseResult<I, Directive<'h>>
where
    I: TextInput<'h> + NodeInput<'h, 's, 'e, 'p> + ConfigInput<'h> + BlockInput<'h>,
    'h: 'e,
    'e: 's,
    's: 'p,
{
    let tz = input.parse_node().config().borrow().timezone();
    let (dir_rem, rem_value) = promote("Price date expected", line_value)(input.clone())?;

    // Parse date
    let (rem_value, datetime) =
        map_res(pair(entry::date, preceded(space1, entry::time)), |(date, time)| {
            JDateTime::from_date_time(date, Some(time), tz)
        })(rem_value.clone())
        .map_err(|_| {
            NomErr::Failure(rem_value.into_err(E_PARSE_DATETIME).with_source(format!(
                "Expected datetime format: {} {}",
                input.config().date_format().format_str(),
                input.config().time_format().format_str()
            )))
        })?;

    // Parse base unit
    let (rem_value, base_unit) = {
        let (rem, unit_code) = unit(rem_value.clone())
            .map_err(|_| NomErr::Failure(err!(rem_value.into_err(E_BASE_UNIT))))?;
        let mut config = input.config_mut();
        match config.get_unit(unit_code) {
            Some(unit) => (rem, unit),
            None => {
                let unit = Unit::new(unit_code);
                (rem, config.merge_unit(&unit, input.parse_node().allocator()))
            }
        }
    };

    // Parse price
    let (rem_value, price) = promote(
        "Unable to read price component",
        preceded(space1, parsing::amount::amount),
    )(rem_value.clone())?;
    //.map_err(|e| NomErr::Failure(err!(rem_value.into_err("Unable to read price component"))))?;

    // Parse sources
    let sources = map(opt(preceded(space1, rest_line1)), |opt| opt.map(|i: I| i.text()))(rem_value)
        .unwrap()
        .1;

    let price = Price::new(datetime, base_unit, price, sources);
    Ok((dir_rem, Directive::new(input.block(), DirectiveKind::Price(Arc::new(price)))))
}

fn entry<'h, 's, 'e, 'p, I>(
    input: I,
    date_and_time: DateAndTime<'h>,
) -> JParseResult<I, Directive<'h>>
where
    I: TextInput<'h>
        + BlockInput<'h>
        + ConfigInput<'h>
        + NodeInput<'h, 's, 'e, 'p>
        + LocatedInput<'h>,
    'h: 'e,
    'e: 's,
    's: 'p,
{
    let (rem, entry) = entry::entry(input.clone(), date_and_time)?;
    Ok((
        rem,
        Directive::new(
            input.block(),
            DirectiveKind::Entry(input.parse_node().allocator().alloc(entry)),
        ),
    ))
}

fn filename<'h, 's, 'e, 'p, I>(input: I) -> JParseResult<I, &'h Path>
where
    I: TextInput<'h> + NodeInput<'h, 's, 'e, 'p> + BlockInput<'h> + LocatedInput<'h>,
    'h: 'e,
    'e: 's,
    's: 'p,
{
    let (rem, filename) =
        promote("Filename expected", map(rest_line1, |i: I| i.text()))(input.clone())?;
    let parent_stream = input.parse_node().node().nearest_filename();
    let file_path = {
        let child_buf = PathBuf::from_str(filename.trim()).expect("infallible");
        if child_buf.is_absolute() {
            child_buf
        } else if let Some(parent_path) = parent_stream {
            parent_path.with_file_name(child_buf)
        } else {
            child_buf
        }
    };
    Ok((rem, input.parse_node().allocator().alloc(file_path).as_path()))
}

/// Reads the input as a new stream. This will attempt to interpret the input as a valid file path,
/// and follow this as a new file stream. If unsuccessful, it will be assumed to be a continuation
/// of the input.
fn stream<'h, 's, 'e, 'p, I>(input: I) -> JParseResult<I, (I, Option<&'h Path>)>
where
    I: TextInput<'h> + LocatedInput<'h> + BlockInput<'h> + NodeInput<'h, 's, 'e, 'p>,
    'h: 'e,
    'e: 's,
    's: 'p,
{
    // Test if the input is just a single line. Do this without counting over all of them.
    let input_is_single_line = input.text().trim().lines().nth(1).is_none();

    if input_is_single_line {
        let (rem, filename) = filename(input.clone())?;

        // Try to open the file, failing silently if it doesn't exist.
        if let Ok(tb) =
            TextBlock::from_file(filename, input.parse_node().allocator(), Some(input.block()))
                .map_err(|e| {
                    NomErr::Error(input.clone().into_err("Cannot stream file").with_source(e))
                })
        {
            let new_input = input.with_child(tb);
            return Ok((rem, (new_input, Some(filename))));
        }
    }

    // Interpret the input as a continuation of the current stream.
    let (rem, input) = rest::<_, ()>(input).unwrap();
    Ok((rem, (input, None)))
}

fn branch<'h, 's, 'e, 'p, I>(kind: JournalNodeKind) -> impl Fn(I) -> JParseResult<I, Directive<'h>>
where
    'h: 'e,
    'e: 's,
    's: 'p,
    I: TextInput<'h> + NodeInput<'h, 's, 'e, 'p> + LocatedInput<'h> + BlockInput<'h>,
{
    move |input| {
        let orig_block = input.block();
        let (rem, (input, filename)) = stream(input)?;
        Ok((
            rem,
            Directive::new(
                orig_block,
                DirectiveKind::Branch(input.parse_node().branch_kind(input, filename, kind)),
            ),
        ))
    }
}

fn include<'h, 's, 'e, 'p, I>(kind: JournalNodeKind) -> impl Fn(I) -> JParseResult<I, Directive<'h>>
where
    I: TextInput<'h> + NodeInput<'h, 's, 'e, 'p> + LocatedInput<'h> + BlockInput<'h>,
    'h: 'e,
    'e: 's,
    's: 'p,
{
    move |input| {
        let orig_block = input.block();
        let (rem, (input, filename)) = stream(input)?;

        Ok((
            rem.clone(),
            (Directive::new(
                orig_block,
                DirectiveKind::Include(
                    input
                        .parse_node()
                        .include_kind(input, filename, kind)
                        .map_err(NomErr::Failure)?,
                ),
            )),
        ))
    }
}

pub fn entry_file_directives<'h, 's, 'e, 'p, I>(
    input: I,
) -> JParseResult<I, Vec<Vec<Directive<'h>>>>
where
    I: TextInput<'h>
        + BlockInput<'h>
        + LocatedInput<'h>
        + NodeInput<'h, 's, 'e, 'p>
        + ConfigInput<'h>,
    'h: 'e,
    'e: 's,
    's: 'p,
{
    let plugin_dir = |input: I| -> IParseResult<'h, I, (&'static str, I)> {
        for module in MODULES.lock().unwrap().iter() {
            for dir in module.directives() {
                if let Ok((_, input)) = param_value(dir.name())(input.clone()) {
                    return Ok((input.clone(), (dir.name(), input)));
                }
            }
        }
        Err(NomErr::Error(IParseError::new("Unknown plugin directive", input)))
    };

    let plugin_directive = |dir: &'static str, input: I| {
        let (rem, dir_input) = rest::<_, ()>(input.clone()).unwrap();
        let tbi = TextBlockInput::new(
            dir_input.into_located_span(rem.parse_node()),
            rem.block(),
            rem.parse_node().allocator(),
        );
        let modules_lock = MODULES.lock().unwrap();
        match modules_lock.iter().find_map(|m| m.directives().find(|d| d.name() == dir)) {
            Some(module_dir) => {
                let (_, out) = module_dir.parse(tbi).finish().map_err(|e| {
                    NomErr::Error(err!(
                        JournErrors::new(format!("Failed to parse module '{}'", dir), vec![e])
                            .flatten()
                    ))
                })?;
                Ok((rem, out))
            }
            None => Err(NomErr::Error(
                input.into_err(&format!("Unable to locate plugin with dir: {}", dir)),
            )),
        }
    };

    let mut segments = vec![vec![]];
    let mut push_directive =
        |directive: Directive<'h>, start_new_segment: bool| -> Result<(), NomErr<_>> {
            segments.last_mut().unwrap().push(directive);
            if start_new_segment {
                segments.push(vec![]);
            }
            Ok(())
        };

    let rem = match_blocks!(input,
        entry::entry_date_and_remainder => |(date_and_time, input)| {
            push_directive(entry(input, date_and_time)?.1, false)
        },
        util::comment => |c: I| push_directive(Directive::new(c.block(), DirectiveKind::Comment(c.text())), false),
        param_value("account") => |input| push_directive(account_directive(input)?.1, false),
        param_value("unit") => |input| push_directive(unit_directive(input)?.1, false),
        param_value("branch") => |input| {
            push_directive(branch(JournalNodeKind::Entry)(input)?.1, true)
        },
        param_value("include") => |input| {
            push_directive(include(JournalNodeKind::Entry)(input)?.1, true)
        },
        param_value("dateformat") => |input| push_directive(dateformat_directive(input)?.1, false),
        param_value("timeformat") => |input| push_directive(timeformat_directive(input)?.1, false),
        param_value("timezone") => |input| push_directive(timezone_directive(input)?.1, false),
        param_value("python") => |input| push_directive(python_directive(input)?.1, false),
        param_value("units") => |input| push_directive(units_directive(input)?.1, false),
        plugin_dir => |(dir, input)| push_directive(plugin_directive(dir, input)?.1, false),
        rest => |input: I| Err(NomErr::Error(input.into_err("Unknown directive")))
    )?.0;
    Ok((rem, segments))
}

pub fn price_file_directives<'h, 's, 'e, 'p, I>(
    input: I,
) -> JParseResult<I, Vec<Vec<Directive<'h>>>>
where
    I: TextInput<'h>
        + ConfigInput<'h>
        + LocatedInput<'h>
        + BlockInput<'h>
        + NodeInput<'h, 's, 'e, 'p>,
    'h: 'e,
    'e: 's,
    's: 'p,
{
    let mut dirs = vec![];
    let mut amongst_prices = false;

    let bad_dir_check = |input: I, amongst_prices| {
        if amongst_prices {
            Err(NomErr::Error(err!(
                input.into_err("Directive not allowed in price file once price listing begins")
            )))
        } else {
            Ok(())
        }
    };

    let rem = match_blocks!(input,
        param_value("p") => |input| {
            let pd = price(input)?.1;
            amongst_prices = true;
            Ok(dirs.push(pd))
        },
        util::comment => |c: I| Ok(dirs.push(Directive::new(c.block(), DirectiveKind::Comment(c.text())))),
        param_value("dateformat") => |input| {
            let (cons, df) = consumed(dateformat_directive)(input)?.1;
            bad_dir_check(cons, amongst_prices)?;
            Ok(dirs.push(df))
        },
        param_value("timeformat") => |input| {
            let (cons, tf) = consumed(timeformat_directive)(input)?.1;
            bad_dir_check(cons, amongst_prices)?;
            Ok(dirs.push(tf))
        },
        param_value("timezone") => |input| {
            let (cons, tz) = consumed(timezone_directive)(input)?.1;
            bad_dir_check(cons, amongst_prices)?;
            Ok(dirs.push(tz))
        },
        rest => |input: I| Err(NomErr::Error(input.into_err("Unknown directive")))
    )?
        .0;
    Ok((rem, vec![dirs]))
}

pub fn directives<'h, 's, 'e, 'p, I>(input: I) -> JournResult<Vec<Vec<Directive<'h>>>>
where
    I: TextInput<'h>
        + BlockInput<'h>
        + ConfigInput<'h>
        + LocatedInput<'h>
        + NodeInput<'h, 's, 'e, 'p>,
    'h: 'e,
    'e: 's,
    's: 'p,
{
    let r = if input.parse_node().node().file_kind() == JournalNodeKind::Prices {
        price_file_directives(input)
    } else {
        entry_file_directives(input)
    }
    .finish()?;
    Ok(r.1)
}

#[cfg(test)]
mod tests {
    use crate::date_and_time::{JDate, JTime};
    use crate::ext::StrExt;
    use crate::metadata::Metadata;
    use crate::parsing::directive::*;
    use crate::parsing::util::str_res;
    use crate::*;
    use chrono::{NaiveDate, NaiveTime};
    use indoc::indoc;

    #[test]
    fn test_entry() {
        let ent = |s: &'static str| dir!(s).map(str_res);

        // Description is optional
        assert_eq!(ent("2000-01-01"), Ok(("", "2000-01-01")));
        assert_eq!(ent("2000-01-01\n AccA  £0"), Ok(("", "2000-01-01\n AccA  £0")));

        assert_eq!(ent("2000-01-01  desc"), Ok(("", "2000-01-01  desc")));
        assert_eq!(ent("2000-01-01    desc"), Ok(("", "2000-01-01    desc")));
        assert_eq!(
            ent("2000-01-01    desc\n  AccB  £0"),
            Ok(("", "2000-01-01    desc\n  AccB  £0"))
        );

        // Entry config is updated
        assert!(entry!("2000-01-01\n  AccC  $0").config().get_unit("$").is_some());
    }

    #[test]
    fn test_entry_directive() {
        let ent = |s: &'static str| {
            let dir = dir!(s);
            dir.map(|d| match d.1.kind() {
                DirectiveKind::Entry(e) => (d.0, *e),
                _ => panic!("Expected entry directive"),
            })
        };

        let (rem, entry) = ent("2000-01-01").unwrap();
        assert_eq!(rem, "");
        assert_eq!(entry.date().naive_local(), NaiveDate::from_ymd(2000, 1, 1));
    }

    #[test]
    fn test_account_directive() {
        let journ = journ!(indoc! {"
            account ABC:DEF
              +key
              ; comment
        "});
        let config = journ.root().config();
        assert!(config.get_account("ABC:DEF").is_some());
        assert!(config.get_account("ABC").is_some());
        assert_eq!(
            config.get_account("ABC:DEF").and_then(|a| a.metadata().first()),
            Some(&Metadata::new("+", "key".into(), "", None))
        );
    }

    #[test]
    fn test_unit_directive() {
        let journ = journ!(indoc! {r#"
              unit euros, EE, E
                name Euro
                format -€1.000,00
                ranking 1
                rounding halfeven
                value d, b, q ->
                  1
                prices
                  P 2000-01-01 10:11:12Z "€" "$1.00"
           "#});

        let config = journ.root().config();
        assert!(config.get_unit("euros").is_some());
        let unit = config.get_unit("euros").unwrap();

        assert!(config.get_unit("E").is_some());
        assert!(config.get_unit("EE").is_some());
        assert_eq!(unit.name(), Some(&"Euro".to_string()));
        assert_eq!(unit.conversion_ranking(), Some(1));
        assert_eq!(
            unit.format(),
            &parse!("-€1.000,00", parsing::amount::unit_format(true)).unwrap().1
        );
        assert_eq!(
            unit.conversion_expression(),
            Some(Lambda::from_str("d, b, q -> 1").unwrap()).as_ref()
        );
        assert!(unit.prices().is_some());
        assert_eq!(unit.prices().unwrap().prices().len(), 1, "Price DB contains one price");
        assert_eq!(
            unit.prices().unwrap().prices().iter().next().unwrap().to_string(),
            "P 2000-01-01 10:11:12Z \"€\" $1.00"
        )
    }

    #[test]
    fn test_default_unit_directive() {
        let journ = journ!("default unit rounding halfeven");
        assert_eq!(
            journ.root().config().default_unit().rounding_strategy(),
            RoundingStrategy::HalfEven
        );

        let journ = journ!(indoc! { r#"
        default unit value d, b, q ->
                      1
          prices
           P 2000-01-01 10:11:12Z "€" "$1.00""# });

        let config = journ.root().config();
        let unit = config.default_unit();
        assert_eq!(
            unit.conversion_expression(),
            Some(Lambda::from_str("d, b, q -> 1").unwrap()).as_ref()
        );
        assert!(unit.prices().is_some());
        assert_eq!(unit.prices().unwrap().prices().len(), 1, "Price DB contains one price");
        assert_eq!(
            unit.prices().unwrap().prices().iter().next().unwrap().to_string(),
            "P 2000-01-01 10:11:12Z \"€\" $1.00"
        )
    }

    #[test]
    fn test_branch() {
        let journ = journ!(indoc! {r#"
            branch
              dateformat dd/mm/yyyy
              branch
                dateformat mm/dd/yyyy
        "#});
        let files = journ.nodes_recursive();
        assert_eq!(files.len(), 3);
        let root_config = files[0].config();
        let branch_config1 = files[1].config();
        let branch_config2 = files[2].config();

        assert_eq!(root_config.date_format().format_str(), "yyyy-mm-dd");
        assert_eq!(branch_config1.date_format().format_str(), "dd/mm/yyyy");
        assert_eq!(branch_config2.date_format().format_str(), "mm/dd/yyyy");
    }

    #[test]
    fn test_include() {
        let journ = journ!(
            r#"include
                 dateformat dd/mm/yyyy"#
        );
        let files = journ.nodes_recursive();
        assert_eq!(files.len(), 2);
        let root_config = files[0].config();
        let include_config = files[1].config();

        assert_eq!(root_config.date_format().format_str(), "dd/mm/yyyy");
        assert_eq!(include_config.date_format().format_str(), "dd/mm/yyyy");
    }

    #[test]
    fn test_price_directive() {
        let parse_price = |s: &'static str| dir_kind!(s, Price, config!(), JournalNodeKind::Prices);

        let price = parse_price("P 2000-01-01 12:00:00 \"$\" \"€1.1\" s1, s2").unwrap().1;

        assert_eq!(price.datetime().date().naive_utc(), NaiveDate::from_ymd(2000, 1, 1));
        assert_eq!(price.datetime().time(), NaiveTime::from_hms(12, 0, 0));
        assert_eq!(price.base_unit(), &Unit::new("$"));
        assert_eq!(price.price(), amount!("€1.1"));
        assert_eq!(price.sources().as_slice(), &["s1".to_string(), "s2".to_string()]);
    }

    #[test]
    fn test_dateformat_directive() {
        let df_dir = |s: &'static str| match dir_kind!(s, DateFormat) {
            Ok((rem, df)) => {
                let date = JDate::new(NaiveDate::from_ymd(1999, 1, 2), df).to_string().intern();
                Ok((rem, date))
            }
            Err(e) => Err(e),
        };

        assert_eq!(df_dir("dateformat dd/mm/yyyy\nabc"), Ok(("\nabc", "02/01/1999")));
        assert_eq!(df_dir("dateformat d/m/yy"), Ok(("", "2/1/99")));
        assert_eq!(df_dir("dateformat d/mmm/yy"), Ok(("", "2/Jan/99")));
        assert_eq!(df_dir("dateformat d/mmmm/yy"), Ok(("", "2/January/99")));
        assert_eq!(df_dir("dateformat %"), Ok(("", "%")));
    }

    #[test]
    fn test_timeformat_directive() {
        let tf_dir = |s: &'static str, hour: u32, min: u32, sec: u32| match dir_kind!(s, TimeFormat)
        {
            Ok((rem, tf)) => {
                let time =
                    JTime::new(NaiveTime::from_hms(hour, min, sec), tf, false).to_string().intern();
                Ok((rem, time))
            }
            Err(e) => Err(e),
        };

        assert_eq!(tf_dir("timeformat hh:mm:ss\nabc", 1, 2, 3), Ok(("\nabc", "01:02:03")));
        assert_eq!(tf_dir("timeformat ii:mm:ss p", 13, 1, 2), Ok(("", "01:01:02 pm")));
    }

    #[test]
    fn test_timezone_directive() {
        let journ = journ!("timezone Europe/London");
        assert_eq!(journ.root().config().timezone(), chrono_tz::Europe::London);
    }

    #[test]
    fn test_python_directive() {
        let python_str = |s: &'static str| match dir_kind!(s, Python) {
            Ok((rem, jf)) => Ok((rem, jf.block().text_outdented().intern())),
            Err(e) => Err(e),
        };
        assert_eq!(
            python_str("python\n a = 123\n b = 456\nabc"),
            Ok(("\nabc", "a = 123\nb = 456"))
        );
    }

    #[test]
    fn test_cgt_directive() {
        let journ = journ!(indoc! {r#"
            capitalGains
              rounddeals true
              pools
                pool 1D
                  unitofaccount £
                  maxAge 1
                  method fifo
                pool 30D
                  maxAge 30
                  method fifo
                  matchWhen b,s ->
                    s.date() < b.date()
                pool
                      
            capitalgains pool 1D maxAge 2
        "#});

        let config = journ.root().config();
        let cgt_config = config.cgt_config();
        assert!(cgt_config.round_deals());
        assert_eq!(cgt_config.pools().first().unwrap().max_age(), Some(2));
    }

    /// Test the formatting output visually.
    #[test]
    #[should_panic]
    fn test_unknown_directive() {
        journ!(indoc! {r#"
            unknown
              a b c
            default currency $
        "#});
    }
}
