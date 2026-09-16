/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::Quantity;
use crate::error::parsing::{IErrorMsg, IParseError, tag_err};
use crate::ext::StrExt;
use crate::parsing::IParseResult;
use crate::parsing::input::{ConfigInput, TextInput};
use nom::branch::alt;
use nom::bytes::complete::take;
use nom::character::complete::{char, space0};
use nom::combinator::{map, map_res, recognize};
use nom::sequence::{delimited, preceded};

/// Parses a positive decimal.
///
/// ```
/// # use journ_core::parse;
/// # use journ_core::parse_obj;
/// # use rust_decimal_macros::dec;
/// # use journ_core::parsing::amount::pos_decimal_str;
///
/// assert_eq!(parse_obj!("123 ABC", pos_decimal_str), "123");
/// assert_eq!(parse_obj!("123- ABC", pos_decimal_str), "123");
/// assert!(parse!("-123", pos_decimal_str).is_err());
///
/// // Can be in scientific format
/// assert_eq!(parse_obj!("123e6 ABC", pos_decimal_str), "123e6");
/// assert_eq!(parse_obj!("123E+6 ABC", pos_decimal_str), "123E+6");
/// assert_eq!(parse_obj!("123E-6 ABC", pos_decimal_str), "123E-6");
/// assert_eq!(parse_obj!("123E- ABC", pos_decimal_str), "123");
/// ```
pub fn pos_decimal_str<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, &'h str> {
    let digit_taker = |input: I| {
        let mut last_digit = 0usize;
        let mut last_char = None;
        let mut peek_char = input.text().char_indices().peekable();
        while let Some((i, c)) = peek_char.next() {
            match c {
                d if d.is_ascii_digit() => last_digit = i + 1,
                ',' | '.' | 'e' | 'E' => {}
                '-' | '+' if last_char == Some('e') || last_char == Some('E') => {}
                _ => {
                    break;
                }
            }
            last_char = Some(c);
        }
        if last_digit == 0 {
            return Err(nom::Err::Error(IParseError::new(IErrorMsg::NUMBER, input)));
        }
        take(last_digit)(input)
    };
    map(preceded(space0, digit_taker), |output: I| output.text())(input)
}

/// Parses a negative (only) decimal to a string
pub fn neg_decimal_str<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, &'h str> {
    map(
        preceded(
            space0,
            recognize(alt((
                preceded(char('-'), pos_decimal_str),
                delimited(char('('), pos_decimal_str, char(')')),
            ))),
        ),
        |out: I| out.text(),
    )(input)
}

/// Parses a positive or negative decimal to a string
pub fn decimal_str<'s, I: TextInput<'s>>(input: I) -> IParseResult<'s, I, &'s str> {
    alt((neg_decimal_str, pos_decimal_str))(input)
}

/// As [decimal_str()], but forces the parsing according to the `Configuration's` [Configuration::number_format()].
pub fn decimal<'h, I: TextInput<'h> + ConfigInput<'h>>(input: I) -> IParseResult<'h, I, Quantity> {
    let nf = input.config().number_format();
    tag_err(IErrorMsg::NUMBER, map_res(decimal_str, move |output| output.to_decimal(&nf)))(input)
}
