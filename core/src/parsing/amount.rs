/*
 * Copyright (c) 2022-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::{Amount, AmountExpr};
use crate::error::parsing::{IErrorMsg, IParseError, tag_err};
use crate::ext::StrExt;
use crate::parsing::text_input::{ConfigInput, TextInput};
use crate::parsing::util::recognize_rtrim;
use crate::parsing::{IParseResult, util};
use crate::unit::{
    CodeFormat, CodePosition, DEFAULT_UNIT_FORMAT, NegativePosition, NegativeStyle, NumberFormat,
    Unit, UnitFormat,
};
use nom::Parser;
use nom::branch::{alt, permutation};
use nom::bytes::complete::{tag, take, take_till1, take_while1};
use nom::character::complete::{char, space0};
use nom::combinator::{consumed, flat_map, map, map_res, opt, recognize, rest, verify};
use nom::error::context;
use nom::multi::fold_many0;
use nom::sequence::{delimited, pair, preceded};
use rust_decimal::Decimal;
use smartstring::alias::String as SS;
use std::iter::Sum;
use std::mem;

type UnitQuantity<'t> = (Option<&'t Unit<'t>>, Decimal);

/// Gets whether the specified character is illegal in unquoted `Unit` codes.
pub fn illegal_unit_code_char(c: char) -> bool {
    #[rustfmt::skip]
    // '(', ')', '*', '+', ',', '-', '.', '/', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?', '@'
    // '#' appears in number formats.
    let illegal = ((c as u32) >= 0x28 && (c as u32) <= 0x40)
        || c == '#'
        || c == '"'
        // Mainly '\r' and '\n'. Unit has to finish on the same line.  
        || (c as u32) < 0x20;
    illegal
}

/// Parses a Unit string. Unit may be preceded by spaces.
pub fn unit<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, &'h str> {
    let unquoted = recognize_rtrim(take_till1(illegal_unit_code_char));

    let (rem, unit_code) = context(
        "Unable to read unit",
        map(preceded(space0, alt((unquoted, util::double_quoted))), |out: I| out.text()),
    )(input)?;
    Ok((rem, unit_code))
}

pub fn pos_decimal<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, &'h str> {
    let digit_taker = |input: I| {
        let mut last_digit = 0usize;
        for (i, c) in input.text().char_indices() {
            match c {
                d if d.is_ascii_digit() => last_digit = i + 1,
                ',' | '.' => {}
                _ => {
                    break;
                }
            }
        }
        take(last_digit)(input)
    };
    map(preceded(space0, digit_taker), |output: I| output.text())(input)
}

/// Parses a negative (only) decimal to a string
pub fn neg_decimal<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, &'h str> {
    map(
        preceded(
            space0,
            recognize(alt((
                preceded(char('-'), pos_decimal),
                delimited(char('('), pos_decimal, char(')')),
            ))),
        ),
        |out: I| out.text(),
    )(input)
}

/// Parses a positive or negative decimal to a string
pub fn decimal<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, &'h str> {
    alt((neg_decimal, pos_decimal))(input)
}

/// As [decimal()], but forces the parsing according to the `Configuration's` [Configuration::number_format()].
pub fn parsed_decimal<'h, I: TextInput<'h> + ConfigInput<'h>>(
    input: I,
) -> IParseResult<'h, I, Decimal> {
    let nf = input.config().number_format();
    tag_err(IErrorMsg::NUMBER, map_res(decimal, move |output| output.to_decimal(&nf)))(input)
}

pub fn pos_decimal_format<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, &'h str> {
    map(
        tag_err(
            IErrorMsg::POSITIVE_NUMBER_FORMAT,
            preceded(
                space0,
                take_while1(|c: char| {
                    c.is_ascii_digit() || c == ',' || c == '.' || c == '#' || c == '?'
                }),
            ),
        ),
        |output: I| output.text(),
    )(input)
}

pub fn neg_decimal_format<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, &'h str> {
    tag_err(
        IErrorMsg::NEGATIVE_NUMBER_FORMAT,
        alt((
            preceded(char('-'), pos_decimal_format),
            delimited(char('('), pos_decimal_format, char(')')),
        )),
    )(input)
}

pub fn decimal_format<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, &'h str> {
    tag_err(IErrorMsg::NUMBER_FORMAT, alt((neg_decimal_format, pos_decimal_format)))(input)
}

/// Reads a positive amount. This does not necessarily require the quantity to be positive, only that there is
/// no negative sign preceding the amount string.
///
/// E.g. "$10" or "$-10" but not "-$10" or "($10)".
pub fn pos_amount<'h, I, F>(dec_parser: F) -> impl FnMut(I) -> IParseResult<'h, I, Amount<'h>>
where
    F: Fn(I) -> IParseResult<'h, I, &'h str>,
    I: TextInput<'h> + ConfigInput<'h>,
{
    move |input| {
        let (input, (consumed, (unit_code, decimal_s))) =
            consumed(permutation((unit, &dec_parser)))(input)?;

        // On finding the unit, this checks whether there's a format being used. A missing format can arise where a unit
        // declaration has been made without specifying a format. Now that we've seen the 'first use' of a particular unit,
        // we can make sure a format is applied. Future declarations of units we parse with a format is the only way to override it.
        let found_unit = input.config().get_unit(unit_code);
        let unit = match found_unit {
            Some(unit) if unit.has_format() => unit,
            unit_no_format => {
                let mut unit = match unit_no_format {
                    Some(unit) => unit.clone(),
                    None => Unit::new(unit_code),
                };
                unit.set_format(unit_format(false)(consumed)?.1);
                let mut mut_config = input.config_mut();
                let allocator = mut_config.allocator();
                mut_config.merge_unit(&unit, allocator);
                mut_config.get_unit(unit_code).unwrap()
            }
        };

        let decimal = decimal_s.to_decimal(unit.number_format()).unwrap();
        Ok((input, Amount::new(unit, decimal)))
    }
}

/// Reads a negative [Amount]
///
/// E.g. "-$10" or "($10)"
pub fn neg_amount<'h, I>(input: I) -> IParseResult<'h, I, Amount<'h>>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    map(
        alt((
            preceded(char('-'), pos_amount(&pos_decimal)),
            delimited(char('('), pos_amount(&pos_decimal), char(')')),
        )),
        |out| out.negate(),
    )(input)
}

pub fn amount<'h, I>(input: I) -> IParseResult<'h, I, Amount<'h>>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    context("Unable to read amount", alt((neg_amount, pos_amount(decimal))))(input)
}

pub fn unit_format<'h, I: TextInput<'h>>(
    definitive: bool,
) -> impl FnMut(I) -> IParseResult<'h, I, UnitFormat> {
    move |mut input| {
        // Skip past any initial space. This will be the case when parsing within a unit directive.
        input = space0::<I, ()>(input).unwrap().0;

        // Strip off any leading/trailing negative and note it.
        let (input, neg_preceded, neg_style) = match consumed::<_, _, _, ()>(alt((
            preceded(tag("-"), rest),
            delimited(tag("("), take(input.text().chars().count() - 2), tag(")")),
        )))(input.clone())
        {
            Ok((_, (consumed, output))) => (
                output,
                true,
                Some(if consumed.text().starts_with('-') {
                    NegativeStyle::NegativeSign
                } else {
                    NegativeStyle::Brackets
                }),
            ),
            Err(_) => (input, false, None),
        };

        let (input, code_position, unit_s, decimal_s) = {
            let decimal_format_fn = if neg_preceded { pos_decimal_format } else { decimal_format };
            match pair(unit, space0)(input.clone()) {
                Ok((input, (unit_s, padding))) => {
                    let (input, decimal_s) = decimal_format_fn(input)?;
                    (
                        input,
                        CodePosition::PaddedLeft(padding.text().into()),
                        Some(unit_s.text().into()),
                        decimal_s,
                    )
                }
                Err(_) => {
                    let (input, decimal_s) = decimal_format_fn(input)?;
                    let (input, padding) = space0::<_, IParseError<_>>(input).unwrap();
                    let (input, unit_s) = opt(unit)(input).unwrap();
                    (
                        input,
                        CodePosition::PaddedRight(padding.text().into()),
                        unit_s.map(|s| s.text().into()),
                        decimal_s,
                    )
                }
            }
        };

        let negative_position = {
            if neg_preceded
                && mem::discriminant(&CodePosition::PaddedLeft(SS::new()))
                    == mem::discriminant(&code_position)
            {
                NegativePosition::CodeAdjacent
            } else if decimal_s.starts_with('-') || decimal_s.starts_with('(') {
                NegativePosition::QuantityAdjacent
            } else {
                DEFAULT_UNIT_FORMAT.negative_position()
            }
        };

        let mut number_format = NumberFormat::parse(decimal_s, definitive);
        if let Some(neg_style) = neg_style {
            number_format.set_negative_style(neg_style)
        }

        let code_format = if definitive {
            match unit_s {
                Some(s) if s == "$UNIT$" => CodeFormat::Unit,
                Some(s) => CodeFormat::Literal(s),
                None => CodeFormat::Never,
            }
        } else {
            match unit_s {
                Some(_) => CodeFormat::Unit,
                _ => CodeFormat::Never,
            }
        };

        let unit_format =
            UnitFormat::new(number_format, code_format, code_position, negative_position);
        Ok((input, unit_format))
    }
}

/// We parse any expr surrounded by parens, ignoring all whitespaces around those
fn parens<'h, I>(input: I) -> IParseResult<'h, I, (UnitQuantity<'h>, u32)>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    delimited(
        space0,
        delimited(
            char('('),
            tag_err(IErrorMsg::MULTIPLE_AMOUNTS, verify(amount_expr_inner, |output| output.1 > 1)),
            char(')'),
        ),
        space0,
    )(input)
}

// Reads either a decimal or an amount
fn factor<'h, I>(input: I) -> IParseResult<'h, I, (UnitQuantity<'h>, u32)>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    let amount = |input: I| map(amount, |a| ((Some(a.unit()), a.quantity()), 1))(input);
    // Try parenthesis expression first. It will fail if there is only one factor inside, then allowing the amount
    // to be parsed as a negative instead.
    alt((parens, amount, parsed_decimal.map(|d| ((None, d), 1))))(input)
}

// We read an initial factor and for each time we find
// a * or / operator followed by another factor, we do
// the math by folding everything
fn amount_term<'h, I>(input: I) -> IParseResult<'h, I, (UnitQuantity<'h>, u32)>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    flat_map(factor, |(init, factor_count)| {
        fold_many0(
            pair(
                preceded(space0, alt((char('*'), char('/')))),
                tag_err(
                    IErrorMsg::MATCHING_UNITS,
                    verify(factor, move |(uq, _)| match (init.0, uq.0) {
                        (Some(unit_a), Some(unit_b)) => unit_a == unit_b,
                        _ => true,
                    }),
                ),
            ),
            move || (init, factor_count),
            |(acc, acc_factor_count), (op, (val, factor_count)): (char, (UnitQuantity, u32))| {
                if op == '*' {
                    ((acc.0.or(val.0), acc.1 * val.1), acc_factor_count + factor_count)
                } else {
                    ((acc.0.or(val.0), acc.1 / val.1), acc_factor_count + factor_count)
                }
            },
        )
    })(input)
}

fn amount_expr_inner<'h, I>(input: I) -> IParseResult<'h, I, (UnitQuantity<'h>, u32)>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    flat_map(preceded(space0, amount_term), |(init, factor_count)| {
        fold_many0(
            pair(
                preceded(space0, alt((char('+'), char('-')))),
                tag_err(
                    IErrorMsg::MATCHING_UNITS,
                    verify(amount_term, move |(uq, _)| match (init.0, uq.0) {
                        (Some(unit_a), Some(unit_b)) => unit_a == unit_b,
                        _ => true,
                    }),
                ),
            ),
            move || (init, factor_count),
            |(acc, acc_factor_count), (op, (val, factor_count)): (char, (UnitQuantity, u32))| {
                if op == '+' {
                    ((acc.0.or(val.0), acc.1 + val.1), acc_factor_count + factor_count)
                } else {
                    ((acc.0.or(val.0), acc.1 - val.1), acc_factor_count + factor_count)
                }
            },
        )
    })(input)
}

pub fn amount_expr<'h, I>(input: I) -> IParseResult<'h, I, AmountExpr<'h>>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    map(
        consumed(tag_err(IErrorMsg::UNIT, verify(amount_expr_inner::<I>, |out| out.0.0.is_some()))),
        |(cons, ((unit, quantity), _))| {
            let (consumed, preceding_space) = space0::<_, ()>(cons).unwrap();
            AmountExpr::new(
                Amount::new(unit.unwrap(), quantity),
                preceding_space.text(),
                Some(consumed.text()),
            )
        },
    )(input)
}

impl<'h> Sum<Amount<'h>> for Option<Amount<'h>> {
    fn sum<I: Iterator<Item = Amount<'h>>>(mut iter: I) -> Self {
        let mut sum = iter.next();
        for a in iter {
            sum = sum.map(|s| s + a);
        }
        sum
    }
}

#[cfg(test)]
mod tests {
    use crate::parsing::amount::*;
    use crate::parsing::util::str_res;
    use crate::*;

    #[test]
    fn test_unit() {
        let unit = |s: &'static str| parse!(s, unit).map(str_res);

        assert_eq!(unit("USD"), Ok(("", "USD")));
        assert_eq!(unit(" USD"), Ok(("", "USD")));
        assert_eq!(unit("USD "), Ok((" ", "USD")));
        assert_eq!(unit("Kg Gold"), Ok(("", "Kg Gold")));
        assert_eq!(unit("£"), Ok(("", "£")));
        assert_eq!(unit("$"), Ok(("", "$")));
        assert_eq!(unit("€"), Ok(("", "€")));
        assert_eq!(unit("\"123\""), Ok(("", "123")));
        assert_eq!(unit("\"123\"\"abc\""), Ok(("", "123\"\"abc")));
        assert_eq!(unit("1"), Err(util::E_SURR_QUOTE));
        assert_eq!(unit("\"123"), Err(util::E_SURR_QUOTE));
    }

    #[test]
    fn test_amount() {
        let amount = |s: &'static str| parse!(s, parsing::amount::amount);

        assert_eq!(amount("$10"), Ok(("", Amount::new(unit!("$"), dec!(10)))));
        assert_eq!(amount("-$10"), Ok(("", Amount::new(unit!("$"), dec!(-10)))));
        assert_eq!(amount("£-10"), Ok(("", Amount::new(unit!("£"), dec!(-10)))));
        assert_eq!(amount("10 USD"), Ok(("", Amount::new(unit!("USD"), dec!(10)))));
        assert_eq!(amount("10USD"), Ok(("", Amount::new(unit!("USD"), dec!(10)))));
        assert_eq!(amount("1.1 Kg Gold"), Ok(("", Amount::new(unit!("Kg Gold"), dec!(1.1)))));
        assert_eq!(amount("10 GBP 1"), Ok((" 1", Amount::new(unit!("GBP"), dec!(10)))));
        assert_eq!(amount("GBP 10 "), Ok((" ", Amount::new(unit!("GBP"), dec!(10)))));
    }

    #[test]
    fn test_formatting() {
        let format = |s: &'static str, amount: Amount| match parse!(s, unit_format(true)) {
            Ok((_, f)) => Ok(f.format(amount)),
            Err(e) => Err(e),
        };

        // Padding
        assert_eq!(format("€ 1", amount!("€10")), Ok("€ 10".to_string()));
        assert_eq!(format("1 \"USD\"", amount!("10 USD")), Ok("10 \"USD\"".to_string()));
        assert_eq!(
            format("1 \"\"\"USD\"\"\"", amount!("10 USD")),
            Ok("10 \"\"\"USD\"\"\"".to_string())
        );

        // Thousands/Decimal Sep
        assert_eq!(format("€1,000,000.0", amount!("€1000.5")), Ok("€1,000.5".to_string()));
        assert_eq!(format("€1.000.000,0", amount!("€1000.5")), Ok("€1.000,5".to_string()));

        // Min/Max Scale
        assert_eq!(format("$1.#", amount!("$1.5")), Ok("$1.5".to_string()));

        // Bad format
        assert_eq!(format("1 1A", amount!("1A")), Err(util::E_SURR_QUOTE));
    }

    #[test]
    fn test_amount_expression() {
        let expr = |s: &'static str| parse!(s, (map(amount_expr, |e| e.amount())));

        assert_eq!(expr("$10 --10"), Ok(("", amount!("$20"))));
        assert_eq!(expr("$10 ---10"), Ok((" ---10", amount!("$10"))));
        assert_eq!(expr("(10 + 10) * $2"), Ok(("", amount!("$40"))));
        assert_eq!(expr("( 10 + (10 - 5) ) * $2 "), Ok((" ", amount!("$30"))));
    }
}
