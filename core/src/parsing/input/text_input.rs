/*
 * Copyright (c) 2023-2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::configuration::Configuration;
use crate::error::JournError;
use crate::parsing::DerefMutAndDebug;
use crate::parsing::parser::JournalParseNode;
use nom::{
    AsBytes, Compare, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset, Slice,
};
use nom_locate::LocatedSpan;
use std::cell::RefCell;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::{Range, RangeFrom, RangeTo};
use std::str::{CharIndices, Chars};

pub trait TextInput<'h>:
    Clone
    + Hash
    + AsBytes
    + InputLength
    + InputIter<Item = char, IterElem = Chars<'h>, Iter = CharIndices<'h>>
    + InputTake
    + Offset
    + Slice<RangeTo<usize>>
    + Slice<RangeFrom<usize>>
    + Slice<Range<usize>>
    + InputTakeAtPosition<Item = char>
    + for<'a> Compare<&'a str>
    + Debug
{
    fn text(&self) -> &'h str;

    fn into_err(self, error: &str) -> JournError;
}

impl<'h> TextInput<'h> for &'h str {
    fn text(&self) -> &'h str {
        self
    }

    fn into_err(self, error: &str) -> JournError {
        JournError::new(format!("{}: {}", error, self.text()))
    }
}

impl<'h> TextInput<'h> for LocatedSpan<&'h str> {
    fn text(&self) -> &'h str {
        self.fragment()
    }

    fn into_err(self, error: &str) -> JournError {
        JournError::new(format!("{}: {}", error, self.text()))
    }
}

impl<'h, 'p, 's, 'e> TextInput<'h> for LocatedSpan<&'h str, &'s JournalParseNode<'h, 's>>
where
    'h: 'e,
    'e: 'e,
    's: 'p,
{
    fn text(&self) -> &'h str {
        self.fragment()
    }

    fn into_err(self, error: &str) -> JournError {
        JournError::new(format!("{}: {}", error, self.text()))
    }
}

impl<'h, 'p, 's> TextInput<'h>
    for LocatedSpan<&'h str, &'p RefCell<dyn DerefMutAndDebug<'h, 's, Configuration<'h>>>>
{
    fn text(&self) -> &'h str {
        self.fragment()
    }

    fn into_err(self, error: &str) -> JournError {
        JournError::new(format!("{}: {}", error, self.text()))
    }
}

impl<'h> TextInput<'h> for LocatedSpan<&'h str, RefCell<Configuration<'h>>> {
    fn text(&self) -> &'h str {
        self.fragment()
    }

    fn into_err(self, error: &str) -> JournError {
        JournError::new(format!("{}: {}", error, self.text()))
    }
}

/// Creates a `TextBlockInput` from a string literal and reads it as though it is the first
/// child. Useful for only for testing.
#[macro_export]
macro_rules! bin {
    ($text:expr) => {{
        use $crate::ext::StrExt;

        let s: &str = $text;
        let root = Box::leak(Box::new($crate::parsing::text_input:TextBlockInputt::new(
            $crate::fin!(s),
            None,
            Some(s.indented_amount() as u16),
        )));
        $crate::parsing::text_input:TextBlockInputt::new($crate::fin!(s), Some(root), Some(s.indented_amount() as u16))
    }};
}

#[macro_export]
macro_rules! block_err {
    ($text_block:expr, $err_section:expr, $err_string:expr) => {{
        let mut err = JParseError::new($err_section, Some($err_string));
        err.set_parent_block($text_block);
        $crate::parsing::text_input::BlockErr::Error(err)
    }};
    ($text_block:expr, $err_section:expr, $err_string1:expr, $err_string2:expr) => {{
        let mut err = JParseError::new($err_section, None);
        err.set_parent_block($text_block);
        err.set_lower_context($err_string2);
        err.set_higher_context($err_string1);
        $crate::parsing::text_input::BlockErr::Error(err)
    }};
}

/// Reads a `TextBlock` and checks it (leading blank lines trimmed) against a number of `$parser` expressions, applying their
/// `$handler` with the `$parser` output as an argument if matched.
///
/// The `$parser` expression is expected to return a `Result<_, NomErr<IParseError>>`, while the `$handler` expression is expected
/// to produce a `Result<_, JournError<'static>>` result.
///
/// This will only produce an error if a `$parser` returns a `NomErr::Failure`, the associated `$handler` fails,
/// no `$parser` matches or a block cannot be read (because there is no more input).
#[macro_export]
macro_rules! match_block {
    ($input:expr, $($parser:expr => $handler:expr),+) => {{
        use $crate::parsing::text_block::block;
        use $crate::parsing::util::blank_lines0;
        use $crate::match_parser;
        use $crate::err;
        use nom::Err as NomErr;

        match block($input) {
            Ok((rem, block)) => {
                let block_trimmed_blanks = blank_lines0(block).unwrap().0;
                match match_parser!(block_trimmed_blanks, $($parser => $handler),+) {
                    Ok(parser_res) => Ok((rem, parser_res.1)),
                    Err(e) => Err(e)
                }
            }
            Err(NomErr::Error(e)) => Err(NomErr::Error(err!("Unable to read text block").with_source(e.into_err()))),
            Err(NomErr::Failure(e)) => Err(NomErr::Failure(err!("Unable to read text block").with_source(e.into_err()))),
            Err(NomErr::Incomplete(needed)) => Err(NomErr::Incomplete(needed)),
        }
    }}
}

/// Repeatedly reads a `TextBlock`, checks it against a number of `$parser` expressions, applying their
/// `$handler` with the `$parser` output as an argument if matched.
///
/// The `$parser` expression is expected to return a `Result<_, NomErr<IParseError>>`, while the `$handler` expression is expected
/// to produce a `Result<_, JournError<'static>>` result.
///
/// This will only produce an error if a `$parser` returns a `NomErr::Failure`, the associated `$handler` fails or
/// no `$parser` matches.
#[macro_export]
macro_rules! match_blocks {
    ($input:expr, $($parser:expr => $handler:expr),+) => {{
        use $crate::parsing::text_block::block;
        use $crate::match_block;
        use $crate::err;
        use nom::Err as NomErr;

        let mut errs = vec![];
        let mut inp = $input;
        loop {
            if inp.text().trim_start().is_empty() {
                break;
            }

            match match_block!(inp.clone(), $($parser => $handler),+) {
                Ok((rem, _handler_res)) => {
                    inp = rem;
                },
                Err(NomErr::Incomplete(_)) => break,
                Err(NomErr::Error(e) | NomErr::Failure(e)) => {
                    // Skip over the block, accumulating errors
                    errs.push(e);
                    let (rem, _block_text) = block(inp).expect("Input is non-empty; block should be readable");
                    inp = rem;
                }
            }
        }
        if errs.is_empty() {
            Ok((inp, ()))
        } else {
            Err(NomErr::Failure(err!($crate::error::JournErrors::new("Multiple parse errors".to_string(), errs))))
        }
    }}
}
