/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::configuration::Expression;
use crate::directive::{Directive, DirectiveKind};
use crate::error::parsing::promote;
use crate::journal_node::JournalNodeKind;
use crate::parsing::amount::{decimal_expr, unit};
use crate::parsing::directive::{StreamResult, stream_file};
use crate::parsing::input::{BlockInput, ConfigInput, LocatedInput, NodeInput, TextInput};
use crate::parsing::util::{
    double_quoted, line_value, multiline_value_string, param_value, repeat0, separated_field, word,
};
use crate::parsing::{JParseResult, amount, entry, util};
use crate::price_db::PriceDatabase;
use crate::python::lambda::Lambda;
use crate::unit::{RoundingStrategy, Unit};
use crate::{match_blocks, parsing};
use nom::Parser;
use nom::branch::alt;
use nom::character::complete::{char, space0};
use nom::combinator::{map, map_parser, map_res, opt, rest};
use nom::sequence::{preceded, terminated, tuple};
use std::str::FromStr;
use std::sync::Arc;

pub const E_INVALID_UNIT_RANKING: &str = "Unit ranking must be a positive integer";
pub const E_INVALID_VALUE_EXPRESSION: &str = "Unit value expression must be a valid lambda";
pub const E_UNKNOWN_UNIT_KEY: &str = "Unknown unit configuration parameter";
pub const E_INVALID_PRICES_WITHIN_SECS: &str = "Unit priceWithinSecs must be a positive integer";

pub fn unit_definition_body<'h, 's, 'p, I>(
    input: I,
) -> JParseResult<I, (Unit<'h>, Vec<&'h str>, Option<&'h str>)>
where
    I: TextInput<'h> + ConfigInput<'h> + NodeInput<'h, 's, 'p> + BlockInput<'h> + LocatedInput<'h>,
    'h: 's,
    's: 'p,
{
    // Read the unit codes
    //let unit_code = || map(separated_field(','), |s: I| s.text());
    // Read the first unit code
    let (input, primary_code) = promote("Unit code expected", preceded(space0, unit))(input)?;
    // Read aliases
    let mut aliases = vec![];
    let alias_code = || preceded(tuple((space0, char(','), space0)), unit);
    let (input, _) = repeat0(map(alias_code(), |code: &'h str| {
        aliases.push(code);
    }))(input.clone())
    .unwrap();
    // Parse the primary unit
    let (rem, (primary, new_code)) = read_unit(primary_code, None)(input.clone())?;
    Ok((rem, (primary, aliases, new_code)))
}

pub fn unit_directive_body<'h, 's, 'e, 'p, I>(input: I) -> JParseResult<I, Directive<'h>>
where
    I: TextInput<'h> + ConfigInput<'h> + NodeInput<'h, 's, 'p> + BlockInput<'h> + LocatedInput<'h>,
    'h: 'e,
    'e: 's,
    's: 'p,
{
    let (rem, (unit, aliases, new_code)) = unit_definition_body(input.clone())?;
    let unit_ref = input.config_mut().merge_unit(unit, &aliases, new_code);
    Ok((rem, Directive::new(Some(input.block()), DirectiveKind::Unit(unit_ref))))
}

pub fn units_directive<'h, 's, 'p, I>(input: I) -> JParseResult<I, Directive<'h>>
where
    I: TextInput<'h> + NodeInput<'h, 's, 'p> + BlockInput<'h> + ConfigInput<'h> + LocatedInput<'h>,
    'h: 's,
    's: 'p,
{
    let (input, unit_pattern) =
        promote("Unit pattern expected", alt((double_quoted, word)))(input)?;
    let node = input.parse_node();
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
            param_value("format") => |input| {
                let format = promote("Invalid unit format", map_parser(line_value, parsing::amount::unit_format(true)))(input)?.1;
                def_unit.set_format(format);
                Ok(())
            },
            param_value("prices") => |input: I| {
                let node = match stream_file(input, JournalNodeKind::Prices) {
                    StreamResult::AlreadyStreamed(_, node) => node,
                    StreamResult::NewStream(_, branch_input) => node.branch(branch_input),
                    StreamResult::Err(e) => return Err(e)
                };

                let pd = Arc::new(PriceDatabase::new(node));
                def_unit.set_prices(Some(pd));
                Ok(())
            },
            param_value("pricedbLookupWithinSecs") => |input: I| {
                let d = promote(E_INVALID_PRICES_WITHIN_SECS, map_res(decimal_expr, |d| {
                    if !d.is_integer() || !d.is_sign_positive() {
                        Err(err!("Prices within seconds must evaluate to a positive integer: {}", d))
                    } else {
                        Ok(d)
                    }
                }))(input)?.1;
                def_unit.set_pricedb_lookup_within_secs(usize::try_from(d).unwrap());
                Ok(())
            },
            util::comment => |_| Ok(()),
            entry::metadata => |m| Ok(metadata.push(m)),
            rest => |input: I| Err(NomErr::Error(input.into_err(E_UNKNOWN_UNIT_KEY)))
        )?.0;
    def_unit.set_metadata(metadata);
    let allocated = input
        .config_mut()
        .merge_unit_pattern(Expression::from_glob_pattern(unit_pattern.text(), None), def_unit);

    Ok((rem, Directive::new(Some(input.block()), DirectiveKind::Units(allocated))))
}

fn read_unit<'h, 's, 'e, 'p, 'a, I>(
    unit_code: &'h str,
    primary: Option<&'h Unit<'h>>,
) -> impl FnMut(I) -> JParseResult<I, (Unit<'h>, Option<&'h str>)> + 'a
where
    I: TextInput<'h> + ConfigInput<'h> + NodeInput<'h, 's, 'p> + BlockInput<'h> + LocatedInput<'h>,
    'h: 'e,
    'e: 's,
    's: 'p,
    's: 'a,
{
    move |input: I| {
        let node = input.parse_node();
        let mut unit = Unit::new(unit_code);
        //unit.set_aliases(all_codes.iter().copied().map(|c| c.into()).collect());
        let mut metadata = vec![];
        let mut new_code = None;
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
                    // Don't parse the price database multiple times.
                    match primary {
                        Some(primary) => {
                            if let Some(pd) = primary.prices() {
                                unit.set_prices(Some(Arc::clone(pd)));
                            }
                        }
                        None => {
                            let node = match stream_file(input, JournalNodeKind::Prices) {
                                StreamResult::AlreadyStreamed(_, node) => node,
                                StreamResult::NewStream(_, branch_input) => node.branch(branch_input),
                                StreamResult::Err(e) => return Err(e)
                            };
                            let pd = Arc::new(PriceDatabase::new(node));
                            unit.set_prices(Some(pd));
                        }
                    }
                    Ok(())
                },
                param_value("pricedbLookupWithinSecs") => |input: I| {
                    let d = promote(E_INVALID_PRICES_WITHIN_SECS, map_res(decimal_expr, |d| {
                        if !d.is_integer() || !d.is_sign_positive() {
                            Err(err!("Prices within seconds must evaluate to a positive integer: {}", d))
                        } else {
                            Ok(d)
                        }
                    }))(input)?.1;
                    unit.set_pricedb_lookup_within_secs(usize::try_from(d).unwrap());
                    Ok(())
                },
                // Rename is a more explicit way of writing: "unit <new_code>, <old_code>".
                // We cannot release the <old_code> from the configuration, as it would create ambiguity if redefined.
                param_value("rename") => |input: I| {
                    let code = promote("New unit name expected", amount::unit)(input)?.1.text();
                    new_code = Some(code);
                    Ok(())
                },
                util::comment => |_| Ok(()),
                entry::metadata => |m| Ok(metadata.push(m)),
                rest => |input: I| Err(NomErr::Error(input.into_err(E_UNKNOWN_UNIT_KEY)))
            )?.0;
        unit.set_metadata(metadata);
        Ok((rem, (unit, new_code)))
    }
}

#[cfg(test)]
mod tests {
    use crate::parsing::text_block::{TextBlockBuf, TextBlockWriter};
    use crate::python::lambda::Lambda;
    use crate::unit::{RoundingStrategy, Unit};
    use crate::{journ, parse, parsing};
    use indoc::indoc;
    use std::str::FromStr;

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

        let config = journ.config();
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

        let price = unit.prices().unwrap().prices().iter().next().cloned().unwrap();
        let mut buf = Vec::new();
        let mut block_writer = TextBlockWriter::new(&mut buf);
        block_writer
            .write(&TextBlockBuf::with_obj(price.as_ref(), Some(config)).as_text_block(), None)
            .unwrap();
        assert_eq!(String::from_utf8(buf).unwrap().as_str(), "P 2000-01-01 10:11:12Z \"€\" $1.00")
    }

    #[test]
    fn test_default_unit_directive() {
        let journ = journ!("units * rounding halfeven");
        let merged = journ.config().merged_unit(Unit::new("ABC"), &[], None);
        assert_eq!(merged.rounding_strategy(), RoundingStrategy::HalfEven);

        let journ = journ!(indoc! { r#"
        units * value d, b, q ->
                      1
          prices
           P 2000-01-01 10:11:12Z "€" "$1.00""# });

        let config = journ.config();
        let merged = journ.config().merged_unit(Unit::new("ABC"), &[], None);
        assert_eq!(
            merged.conversion_expression(),
            Some(Lambda::from_str("d, b, q -> 1").unwrap()).as_ref()
        );
        assert!(merged.prices().is_some());
        assert_eq!(merged.prices().unwrap().prices().len(), 1, "Price DB contains one price");

        let price = merged.prices().unwrap().prices().iter().next().cloned().unwrap();
        let mut buf = Vec::new();
        let mut block_writer = TextBlockWriter::new(&mut buf);
        block_writer.write(price.as_ref(), Some(config)).unwrap();
        assert_eq!(String::from_utf8(buf).unwrap().as_str(), "P 2000-01-01 10:11:12Z \"€\" $1.00")
    }
}
