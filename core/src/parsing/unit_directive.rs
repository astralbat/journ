/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::directive::{Directive, DirectiveKind};
use crate::error::parsing::promote;
use crate::journal_node::JournalNodeKind;
use crate::parsing::directive::stream;
use crate::parsing::input::{BlockInput, ConfigInput, LocatedInput, NodeInput, TextInput};
use crate::parsing::util::{
    line_value, multiline_value_string, param_value, repeat0, separated_field,
};
use crate::parsing::{JParseResult, entry, util};
use crate::price_db::PriceDatabase;
use crate::python::lambda::Lambda;
use crate::python::mod_ledger::PythonLedgerModule;
use crate::unit::{RoundingStrategy, Unit, Units};
use crate::{match_blocks, parsing};
use nom::Parser;
use nom::combinator::{map, map_parser, map_res, rest};
use std::str::FromStr;
use std::sync::Arc;

pub const E_INVALID_UNIT_RANKING: &str = "Unit ranking must be a positive integer";
pub const E_INVALID_VALUE_EXPRESSION: &str = "Unit value expression must be a valid lambda";
pub const E_UNKNOWN_UNIT_KEY: &str = "Unknown unit configuration parameter";

pub fn unit_definition_body<'h, 's, 'e, 'p, I>(input: I) -> JParseResult<I, &'h Unit<'h>>
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
    Ok((rem, primary))
}

pub fn unit_directive_body<'h, 's, 'e, 'p, I>(input: I) -> JParseResult<I, Directive<'h>>
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
    let (rem, mut unit) = unit_definition_body(input.clone())?;
    unit = input.config_mut().merge_unit(unit, input.parse_node().allocator());
    Ok((rem, Directive::new(Some(input.block()), DirectiveKind::Unit(unit))))
}

pub fn units_directive<'h, 's, 'e, 'p, I>(input: I) -> JParseResult<I, Directive<'h>>
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

    Ok((rem, Directive::new(Some(input.block()), DirectiveKind::Units(units))))
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
        unit.set_aliases(all_codes.iter().copied().map(|c| c.into()).collect());
        let mut metadata = vec![];
        let rem = match_blocks!(input.clone(),
                param_value("name") => |input: I| {
                    let name = promote("Invalid unit name", line_value)(input)?.1;
                    unit.set_name(Some(name.text().into()));
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
        Ok((rem, node.allocator().alloc(unit)))
    }
}
