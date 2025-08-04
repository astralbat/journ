/*
 * Copyright (c) 2023-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::error::JournError;
use crate::error::parsing::{IErrorMsg, IParseError, tag_err};
use crate::ext::StrExt;
use crate::parsing::text_input::TextInput;
use crate::parsing::{IParseResult, JParseResult};
use nom::Finish;
use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case, take, take_till1, take_while, take_while1};
use nom::character::complete::{char, line_ending, multispace0, space0};
use nom::combinator::{eof, iterator, map, opt, peek, recognize, rest, verify};
use nom::error::ParseError;
use nom::sequence::{delimited, preceded, terminated};
use nom::{Err as NomErr, Slice};
use nom::{IResult, Parser};
use std::ops::RangeFrom;
use std::{fmt, iter};

/// Maps a parser to one that returns the result as string, storing them in the static allocator for
/// added convenience. For testing only.
pub fn map_str<'h, F, I: TextInput<'h>, O>(
    mut f: F,
) -> impl FnMut(I) -> IParseResult<'h, I, &'h str>
where
    F: FnMut(I) -> IParseResult<'h, I, O>,
    O: fmt::Display,
{
    move |input: I| {
        let res_to_str = |res: O| crate::alloc::HERD.get().alloc(res.to_string()).as_str();
        f(input).map(|(rem, res)| (rem, res_to_str(res)))
    }
}

pub fn map_str_rem_and_err<'h, F, I: TextInput<'h>, O>(
    mut f: F,
) -> impl FnMut(I) -> Result<(&'h str, O), JournError>
where
    F: FnMut(I) -> JParseResult<I, O>,
{
    move |input: I| f(input).map(|(rem, res)| (rem.text(), res)).finish()
}

/// Convenience function to map a string output/remainder into `&str`.
pub fn str_res<O>(res: (&str, O)) -> (&str, &str)
where
    O: fmt::Display,
{
    let res_to_str = |res: O| crate::alloc::HERD.get().alloc(res.to_string()).as_str();
    (res.0, res_to_str(res.1))
}

/// Convenience function to map a string remainder into `&str`.
pub fn map_str_rem<'h, I: TextInput<'h>, F, O>(
    mut f: F,
) -> impl FnMut(I) -> Result<(&'h str, O), NomErr<JournError>>
where
    F: FnMut(I) -> JParseResult<I, O>,
{
    move |input: I| f(input).map(|(rem, res)| (rem.text(), res))
}
/*
/// Convenience function to map a string remainder into `&str`.
pub fn str_err<'h, I: TextInput<'h>>(err: NomErr<JournError<I>>) -> &'h str {
    match err {
        nom::Err::Error(e) => e.higher_context().unwrap_or(e.lower_context().unwrap_or("")),
        nom::Err::Failure(f) => f.higher_context().unwrap_or(f.lower_context().unwrap_or("")),
        nom::Err::Incomplete(_) => "",
    }
}*/

/// Gets whether the character represents blank space i.e. ' ' or '\t'.
pub fn interim_space(c: char) -> bool {
    c == ' ' || c == '\t'
}

/// Defines space characters between words, including newlines.
pub fn interim_space_or_newline(c: char) -> bool {
    interim_space(c) || c == '\n' || c == '\r'
}

pub const E_SURR_QUOTE: &str = "Expected surrounding quotes";
pub const E_UNTERMINATED_QUOTES: &str = "Unterminated \"";

/// Parses a double quoted string that starts and ends with ", stripping them from the output.
/// Double quotes inside the string may be escaped by doubling ("") them.
/// The string must stay on the same line.
pub fn double_quoted<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, I> {
    let escape_quotes = |input: I| {
        let mut iter = input.text().chars();
        let mut n: usize = 0;
        while let Some(c) = iter.next() {
            if c == '"' {
                if iter.next() == Some('"') {
                    n += 1;
                } else {
                    break;
                }
            }
            if c == '\r' || c == '\n' {
                return Err(NomErr::Failure(IParseError::new(IErrorMsg::QUOTES, input)));
            }
            n += 1;
        }
        take(n)(input)
    };
    delimited(char('"'), escape_quotes, char('"'))(input)
}

/// Runs the inner parser, but any trailing space is pushed back on to the end of the input and stripped off the end of the output.
/// # Examples
/// ```
/// # use journ_core::parsing::util::{recognize_rtrim, rest_line0, map_str};
/// # use journ_core::parse;
///
/// assert_eq!(parse!("abc\n", map_str(recognize_rtrim(rest_line0))), Ok(("\n", "abc")));
/// ```
pub fn recognize_rtrim<'h, I: TextInput<'h>, O, E: ParseError<I>, F>(
    mut parser: F,
) -> impl FnMut(I) -> IResult<I, I, E>
where
    F: Parser<I, O, E>,
{
    move |input: I| {
        let (rem, _) = parser.parse(input.clone())?;

        let mut i = input.offset(&rem);
        for c in input.text()[..i].chars().rev() {
            if !interim_space_or_newline(c) {
                break;
            }
            i -= c.len_utf8();
        }
        Ok(input.take_split(i))
    }
}

/// Runs the inner parser, but any trailing space is pushed back on to the end of the input and stripped off the beginning of the output.
/// This is like `recognize_rtrim`, only it returns the output result as well.
/// # Examples
/// ```
/// # use std::str::FromStr;
/// use nom::bytes::complete::tag;
/// use nom::character::complete::{alpha0, digit0};
/// use journ_core::parsing::util::{consumed_rtrim, rest_line0, map_str, spaced_word};
/// # use journ_core::parse;
///
/// assert_eq!(consumed_rtrim(spaced_word)("123"), Ok(("", ("123", "123"))));
/// ```
pub fn consumed_rtrim<'h, I: TextInput<'h>, O, E: ParseError<I>, F>(
    mut parser: F,
) -> impl FnMut(I) -> IResult<I, (I, O), E>
where
    F: Parser<I, O, E>,
{
    move |input: I| {
        let (rem, o) = parser.parse(input.clone())?;

        let mut i = input.offset(&rem);
        for c in input.text()[..i].chars().rev() {
            if !interim_space_or_newline(c) {
                break;
            }
            i -= c.len_utf8();
        }
        let (rem, consumed) = input.take_split(i);
        Ok((rem, (consumed, o)))
    }
}

fn repeat<'h, I: TextInput<'h>, F, O, E: ParseError<I>>(
    parser: &mut F,
    input: I,
) -> IParseResult<'h, I, (I, usize)>
where
    F: Parser<I, O, E>,
{
    let mut counter = 0;
    let reader = |mut input: I| {
        while let Ok((rem, _out)) = parser.parse(input.clone()) {
            if rem.input_len() == input.input_len() {
                break;
            }
            counter += 1;
            input = rem;
            if input.input_len() == 0 {
                break;
            }
        }
        Ok((input, ()))
    };
    let res = recognize(reader)(input);
    res.map(|(i, o)| (i, (o, counter)))
}

/// Runs the inner parser repeatedly until it fails, returning the input that was successfully
/// parsed. This is like `fold_many0`, only it does not return an error when the inner parser
/// fails to accumulate.
pub fn repeat0<'h, I: TextInput<'h>, O, E: ParseError<I>, F>(
    mut parser: F,
) -> impl FnMut(I) -> IParseResult<'h, I, I>
where
    F: Parser<I, O, E>,
{
    move |input| {
        let (rem, out) = repeat(&mut parser, input)?;
        Ok((rem, out.0))
    }
}

pub fn repeat1<'h, I: TextInput<'h>, O, E: ParseError<I>, F>(
    mut parser: F,
) -> impl FnMut(I) -> IParseResult<'h, I, I>
where
    F: Parser<I, O, E>,
{
    move |input| {
        let (rem, (out, count)) = repeat(&mut parser, input.clone())?;
        if count == 0 {
            return Err(NomErr::Error(IParseError::new(IErrorMsg::REPEAT1, input)));
        }
        Ok((rem, out))
    }
}

/// Like `rest`, except it will fail if there is no input left.
pub fn rest1<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, I> {
    if input.input_len() == 0 {
        Err(NomErr::Error(IParseError::new(IErrorMsg::VALUE, input)))
    } else {
        Ok((input.slice(..input.input_len()), input))
    }
}

/// Reads a parameter name at the beginning of a block. This will confirm but not read that the parameter
/// is terminated with a space, eol or eof.
pub fn param<'h, I: TextInput<'h>>(name: &'h str) -> impl FnMut(I) -> IParseResult<'h, I, I> {
    tag_err(
        IErrorMsg::PARAM,
        recognize_rtrim(preceded(
            space0,
            terminated(tag_no_case(name), alt((tag(" "), tag("\t"), line_ending, eof))),
        )),
    )
}

/// Gets the parameter value with whitespace trimmed from the start.
pub fn param_value<'h, I: TextInput<'h>>(name: &'h str) -> impl FnMut(I) -> IParseResult<'h, I, I> {
    preceded(param(name), preceded(multispace0, rest))
}

pub fn param_value_untrimmed<'h, I: TextInput<'h>>(
    name: &'h str,
) -> impl FnMut(I) -> IParseResult<'h, I, I> {
    preceded(param(name), rest)
}

pub static E_VAL: &str = "Value expected";

/// Gets the value as the remainder of the current line, skipping over any blank lines, and without any trailing spaces.
/// Consumes the line ending.
/// Any comment on the current line is ignored.
/// Returns an error if the value is empty.
pub fn line_value<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, I> {
    preceded(
        blank_lines0,
        tag_err(
            IErrorMsg::VALUE,
            terminated(
                take_while1(|c| c != '\r' && c != '\n' && c != ';'),
                map(rest_line0, |l| trimmed_end(l)),
            ),
        ),
    )(input)
}

pub fn multiline_value_mapper<'h, I: TextInput<'h>, F: FnMut(I) -> R, R>(
    input: I,
    map_fn: F,
) -> IParseResult<'h, I, ()> {
    let mut it = iterator(input.clone(), line_value);
    let mapped_it = it.map(map_fn);
    let mut found = false;
    for _val in mapped_it {
        found = true;
    }
    if !found {
        return Err(NomErr::Error(IParseError::new(IErrorMsg::VALUE, input)));
    }
    it.finish()
}

/// Reads a value spanning multiple lines, stripping any comments.
/// All indenting space is preserved.
pub fn multiline_value_string<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, String> {
    let mut s = String::new();
    let rem = multiline_value_mapper(input, |i| {
        s.push_str(i.text());
        s.push('\n');
    })?
    .0;
    s.truncate(s.len() - 1);
    Ok((rem, s))
}

pub static E_UNEXPECTED_EOL: &str = "Unexpected end of line";

/// Reads the rest of the line, consuming any line ending.
/// If this is the last line, the remaining input will be consumed instead.
///
/// This cannot fail.
/// # Examples
/// ```
/// # use journ_core::parsing::util::{map_str, rest_line0};
/// # use journ_core::parse;
/// assert_eq!(parse!("", map_str(rest_line0)), Ok(("", "")));
/// assert_eq!(parse!("\r\n", map_str(rest_line0)), Ok(("", "")));
/// assert_eq!(parse!("abc", map_str(rest_line0)), Ok(("", "abc")));
/// assert_eq!(parse!("abc\ndef", map_str(rest_line0)), Ok(("def", "abc")));
/// assert_eq!(parse!("abc\r\ndef", map_str(rest_line0)), Ok(("def", "abc")));
/// ```
pub fn rest_line0<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, I> {
    terminated(take_while(|c| c != '\r' && c != '\n'), opt(line_ending))(input)
}

/// Reads the rest of the line, but not the line ending.
/// # Examples
/// ```
/// # use journ_core::parsing::util::{map_str, until_line_ending0};
/// # use journ_core::parse;
/// assert_eq!(parse!("", map_str(until_line_ending0)), Ok(("", "")));
/// assert_eq!(parse!("abc", map_str(until_line_ending0)), Ok(("", "abc")));
/// assert_eq!(parse!("abc\r\n", map_str(until_line_ending0)), Ok(("\r\n", "abc")));
/// ```
pub fn until_line_ending0<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, I> {
    take_while(|c| c != '\r' && c != '\n')(input)
}

/// Reads the rest of the line, consuming any line ending.
/// If this is the last line, the remaining input will be consumed instead.
///
/// Will fail if this would output nothing.
/// # Examples
/// ```
/// # use journ_core::parsing::util::{map_str, rest_line1, E_UNEXPECTED_EOL};
/// # use journ_core::parse;
/// assert_eq!(parse!("abc", map_str(rest_line1)), Ok(("", "abc")));
/// assert_eq!(parse!("abc\ndef", map_str(rest_line1)), Ok(("def", "abc")));
/// assert_eq!(parse!("abc\r\ndef", map_str(rest_line1)), Ok(("def", "abc")));
/// assert_eq!(parse!("", map_str(rest_line1)), Err(E_UNEXPECTED_EOL));
/// assert_eq!(parse!("\r\n", map_str(rest_line1)), Err(E_UNEXPECTED_EOL));
/// ```
pub fn rest_line1<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, I> {
    tag_err(IErrorMsg::VALUE, terminated(take_while1(|c| c != '\r' && c != '\n'), opt(line_ending)))(
        input,
    )
}

/// Reads the rest of the line, but not the line ending.
pub fn until_line_ending1<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, I> {
    take_while1(|c| c != '\r' && c != '\n')(input)
}

/// Consumes `interim_space` characters followed by newline or eof.
/// # Example
/// ```
/// # use journ_core::parse;
/// # use journ_core::parsing::util::{blank_line0, E_BLANK_LINE_EXPECTED, map_str};
/// assert_eq!(parse!("", map_str(blank_line0)), Ok(("", "")));
/// assert_eq!(parse!(" \t ", map_str(blank_line0)), Ok(("", " \t ")));
/// assert_eq!(parse!("\n\n", map_str(blank_line0)), Ok(("\n", "")));
/// assert_eq!(parse!("abc", map_str(blank_line0)), Err(E_BLANK_LINE_EXPECTED));
/// ```
pub static E_BLANK_LINE_EXPECTED: &str = "blank line expected";

pub fn blank_line0<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, I> {
    // This implementation is performance optimized due to its frequent use.
    // Original slower implementation: context(E_BLANK_LINE_EXPECTED, terminated(take_while(interim_space), alt((line_ending, eof))))(input)

    let mut to_take = 0usize;
    for c in input.text().chars() {
        match c {
            ' ' | '\t' => {}
            '\r' | '\n' => {
                return Ok((
                    <I as Slice<RangeFrom<usize>>>::slice(&input, to_take + 1..),
                    input.slice(..to_take),
                ));
            }
            _ => return Err(NomErr::Error(IParseError::new(IErrorMsg::BLANK_LINE, input))),
        }
        to_take += 1;
    }
    if to_take > 0 {
        Ok((<I as Slice<RangeFrom<usize>>>::slice(&input, to_take..), input.slice(..to_take)))
    } else {
        Ok((input.clone(), input))
    }
}

/// Consumes `interim_space` characters followed by newline or eof.
/// # Example
/// ```
/// # use journ_core::parse;
/// # use journ_core::parsing::util::{blank_line1, map_str, str_res};
/// assert_eq!(parse!(" \n\n", map_str(blank_line1)), Ok(("\n", " ")));
/// ```
pub fn blank_line1<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, I> {
    tag_err(IErrorMsg::BLANK_LINE, terminated(take_while1(interim_space), alt((line_ending, eof))))(
        input,
    )
}

/// Consumes 0 or more blank lines.
///
/// This can never fail.
///
/// # Example
/// ```
/// # use journ_core::parse;
/// # use journ_core::parsing::util::{blank_lines0, map_str};
/// assert_eq!(parse!("abc", map_str(blank_lines0)), Ok(("abc", "")));
/// assert_eq!(parse!("\n\n\n", map_str(blank_lines0)), Ok(("", "\n\n\n")));
/// ```
pub fn blank_lines0<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, I> {
    repeat0(blank_line0)(input)
}

/// Consumes 1 or more blank lines.
/// # Example
/// ```
/// # use journ_core::parse;
/// # use journ_core::parsing::util::{blank_lines1, E_BLANK_LINE_EXPECTED, map_str};
/// assert_eq!(parse!("\n\nabc", map_str(blank_lines1)), Ok(("abc", "\n\n")));
/// assert_eq!(parse!("\n\n\n", map_str(blank_lines1)), Ok(("", "\n\n\n")));
/// assert_eq!(parse!("abc", map_str(blank_lines1)), Err(E_BLANK_LINE_EXPECTED))
/// ```
pub fn blank_lines1<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, I> {
    tag_err(IErrorMsg::BLANK_LINE, repeat1(blank_line0))(input)
}

pub static E_WORD_EXPECTED: &str = "Word expected";

/// Reads a single word that must contain at least one character.
/// # Examples
/// ```
/// # use journ_core::parsing::util::{E_WORD_EXPECTED, map_str, word};
/// assert_eq!(word("abc"), Ok(("", "abc")));
/// assert_eq!(word("\n"), Err(E_WORD_EXPECTED));
/// ```
pub fn word<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, I> {
    tag_err(IErrorMsg::VALUE, take_till1(interim_space_or_newline))(input)
}

pub fn tagged_ignore_case<'h, I: TextInput<'h>>(input: I, tag: &str) -> bool {
    tag_no_case::<_, _, ()>(tag)(input).is_ok()
}

/// Reads until a double space, tab, end of line terminators are found.
/// # Examples
/// ```
/// # use journ_core::parsing::util::{map_str, spaced_word};
/// # use journ_core::parse;
/// assert_eq!(parse!("ABC DEF", map_str(spaced_word)), Ok(("", "ABC DEF")));
/// assert_eq!(parse!("ABC DEF ", map_str(spaced_word)), Ok((" ", "ABC DEF")));
/// assert_eq!(parse!("ABC DEF  ", map_str(spaced_word)), Ok(("  ", "ABC DEF")));
/// assert_eq!(parse!("ABC DEF\r\n", map_str(spaced_word)), Ok(("\r\n", "ABC DEF")));
/// assert_eq!(parse!("ABC DEF\n", map_str(spaced_word)), Ok(("\n", "ABC DEF")));
/// assert_eq!(parse!("ABC£\tDEF\n", map_str(spaced_word)), Ok(("\tDEF\n", "ABC£")));
/// ```
pub fn spaced_word<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, I> {
    let mut end = 0;
    let mut iter = input.text().chars().peekable();
    while let Some(c) = iter.next() {
        match c {
            ' ' if iter.peek() == Some(&' ') => {
                break;
            }
            '\r' | '\n' | '\t' => {
                break;
            }
            _ => end += c.len_utf8(),
        }
    }
    if end == 0 {
        Err(NomErr::Error(IParseError::new(IErrorMsg::VALUE, input)))
    } else {
        // Remove any trailing space
        if input.slice(..end).text().ends_with(' ') {
            end -= 1;
        }
        Ok((input.slice(end..), input.slice(..end)))
    }
}

/// Reads a 'double space' that separates account names from posting amounts in postings, or dates
/// from descriptions in entries.
pub fn double_space<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, I> {
    alt((tag("  "), tag("\t")))(input)
}

/// Reads a comment, up to any line ending or eof.
/// # Examples
/// ```
/// # use journ_core::parsing::util::{comment, map_str};
/// # use journ_core::parse;
/// assert_eq!(parse!(";abc", map_str(comment)), Ok(("", ";abc")));
/// assert_eq!(parse!(";abc\ndef", map_str(comment)), Ok(("\ndef", ";abc")));
/// assert_eq!(parse!("abc", map_str(comment)), Err(""));
/// ```
pub fn comment<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, I> {
    // Performance optimized version of the following:
    //    recognize(tuple((space0, char(';'), until_line_ending0)))(input)

    let mut to_take = 0usize;
    for c in input.text().chars() {
        match c {
            ' ' | '\t' => to_take += 1,
            ';' => {
                to_take += 1;
                for c in input.text()[to_take..].chars() {
                    if c == '\r' || c == '\n' {
                        break;
                    }
                    to_take += c.len_utf8();
                }
                return Ok(input.take_split(to_take));
            }
            _ => break,
        }
    }
    Err(NomErr::Error(IParseError::new(IErrorMsg::COMMENT, input)))
}

pub fn indented<'h, I: TextInput<'h>>(
    min_indent: u16,
) -> impl Fn(I) -> IParseResult<'h, I, &'h str> {
    move |input| {
        tag_err(
            IErrorMsg::INDENT_TOO_SMALL,
            map(verify(space0, |s: &I| s.text().indented_amount() >= min_indent), |s: I| s.text()),
        )(input)
    }
}

fn trimmed<'h, I: TextInput<'h>>(input: I) -> I {
    trimmed_start(trimmed_end(input))
}

fn trimmed_end<'h, I: TextInput<'h>>(input: I) -> I {
    let mut end_index = input.input_len();
    for i in input.as_bytes().iter().rev() {
        let c = (*i) as char;
        if c != ' ' && c != '\t' && c != '\r' && c != '\n' {
            break;
        }
        end_index -= 1;
    }
    input.slice(..end_index)
}

fn trimmed_start<'h, I: TextInput<'h>>(input: I) -> I {
    preceded(multispace0::<_, ()>, rest)(input).unwrap().1
}

/// Reads a separated field such as a comma separated field followed by its separator.
/// Double quotes will be allowed as a way to protect the separator from being interpreted.
///
///
/// Reads a field until the separation char or eol/eof is reached. Any eol is not consumed.
/// The field may be optionally surrounded by quotes. In which case, they will be stripped off.
/// # Examples
/// ```
/// # use journ_core::parsing::util::{map_str, separated_field};
/// # use journ_core::parse;
/// assert_eq!(parse!("abc , def", map_str(separated_field(','))), Ok((" def", "abc")));
/// assert_eq!(parse!("\"abc,123\"", map_str(separated_field(','))), Ok(("", "abc,123")));
/// assert_eq!(parse!("\"abc,123\" def, 456", map_str(separated_field(','))), Ok((" 456", "\"abc,123\" def")));
/// ```
pub fn separated_field<'h, I: TextInput<'h>>(
    sep_char: char,
) -> impl Fn(I) -> IParseResult<'h, I, I> {
    move |input| {
        // Str interface is required in alt tag.
        let mut tmp = [0u8; 4];
        let sep_str = &*sep_char.encode_utf8(&mut tmp);

        let terminator = |input: I| alt((tag(sep_str), peek(line_ending), eof))(input);
        let double_quoted = || delimited(space0, double_quoted, space0);
        let unquoted = || take_while1(|c| c != sep_char && c != '\n' && c != '\r');

        let single =
            terminated(map(alt((double_quoted(), unquoted())), |s: I| trimmed(s)), terminator);
        let multi = terminated(
            map(repeat0(alt((double_quoted(), unquoted()))), |s: I| trimmed(s)),
            terminator,
        );

        let (rem, out) = alt((single, multi))(input.clone())?;

        // It is an error not to consume any input
        if rem.input_len() == input.input_len() {
            return Err(NomErr::Error(IParseError::new(IErrorMsg::FIELD, input)));
        }
        Ok((rem, out))
    }
}

/// Reads all separated fields in a given `input`.
pub fn separated_fields(separator: char, mut input: &str) -> impl Iterator<Item = &str> {
    let sep_fn = separated_field(separator);
    iter::from_fn(move || match sep_fn(input) {
        Ok(r) => {
            input = r.0;
            Some(r.1)
        }
        Err(_) => None,
    })
}
