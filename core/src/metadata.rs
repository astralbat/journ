/*
 * Copyright (c) 2019-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::err;
use crate::error::{BlockContext, BlockContextError, JournError};
use crate::ext::StrExt;
use crate::parsing::text_block::TextBlock;
use crate::parsing::text_input::TextInput;
use crate::parsing::util::{blank_lines0, double_space, param_value, rest1, spaced_word};
use crate::parsing::IParseResult;
use nom::bytes::complete::tag;
use nom::character::complete::{space0, space1};
use nom::combinator::{opt, recognize};
use nom::sequence::{preceded, tuple};
use std::fmt;
use std::fmt::Formatter;
use std::ops::Deref;

/// &str wrapper for key, to override PartialEq behavior.
/// Metadata key comparisons are case-insensitive.
#[derive(Debug, Copy, Clone)]
pub struct MetadataKey<'h>(&'h str);

impl PartialEq for MetadataKey<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq_ignore_ascii_case(other.0)
    }
}

impl Eq for MetadataKey<'_> {}

impl PartialEq<&str> for MetadataKey<'_> {
    fn eq(&self, other: &&str) -> bool {
        self.0.eq_ignore_ascii_case(other)
    }
}

impl Deref for MetadataKey<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct Metadata<'h> {
    block: &'h TextBlock<'h>,
    //raw_block: Option<&'h TextBlock<'h>>,
    //pretext: &'h str,
    //key: MetadataKey<'h>,
    ///// An optional value. The value is as parsed until the end of the metadata block, and may have preceding whitespace.
    //value: Option<&'h str>,
}

impl<'h> Metadata<'h> {
    /*
    pub fn new(pretext: &'h str, key: &'h str, value: Option<&'h str>) -> Self {
        Self { pretext, key: MetadataKey(key), value, raw_block: None }
    }*/

    /// The raw block from which this metadata was parsed.
    pub fn block(&self) -> &'h TextBlock<'h> {
        self.block
    }

    /*
    /// Sets the text text block from which this metadata was parsed.
    pub fn set_block(&mut self, text_block: TextBlock<'h>) {
        self.block = text_block;
    }*/

    pub fn leading_whitespace(&self) -> &str {
        self.parse_components().0.leading_whitespace()
    }

    pub fn pretext(&self) -> &str {
        self.parse_components().0
    }

    pub fn key(&self) -> MetadataKey {
        MetadataKey(self.parse_components().1)
    }

    pub fn value(&self) -> Option<&'h str> {
        self.parse_components().2
    }

    /// The same as `value()`, but with the value unindented if it was read from a raw block.
    pub fn value_unindented(&self) -> Option<String> {
        //match self.raw_block {
        //    Some(block) => {
        let block_outdented = self.block.text_outdented(2);
        let block_outdented_skip_plus = &block_outdented[1..];
        let x =
            Some(param_value(self.key().deref())(block_outdented_skip_plus).unwrap().1.to_string());
        x
        //}
        //    None => self.value.map(|v| v.to_string()),
        //}
    }

    /// Creates an error that includes the text block of the metadata.
    pub fn err(&self, msg: String, highlight_text: Option<&str>) -> JournError {
        let mut context = BlockContext::from(&*self.block);
        if let Some(highlight_text) = highlight_text {
            context.highlight(highlight_text);
        }
        err!(BlockContextError::new(context, msg))
    }

    fn parse_components(&self) -> (&'h str, &'h str, Option<&'h str>) {
        let (_rem, (pretext, key, value)) =
            tuple((Self::parse_pretext, spaced_word, opt(preceded(double_space, rest1))))(
                &*self.block.text(),
            )
            .unwrap();
        (pretext, key, value)
    }

    pub fn parse_pretext<I>(input: I) -> IParseResult<'h, I, I>
    where
        I: TextInput<'h>,
    {
        recognize(tuple((blank_lines0, space1, tag("+"), space0)))(input)
    }
}

impl PartialEq for Metadata<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.key() == other.key() && self.value() == other.value()
    }
}

impl Eq for Metadata<'_> {}

impl fmt::Display for Metadata<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.block)?;
        /*
        write!(f, "{}{}", self.pretext, self.key.0)?;
        if let Some(value) = &self.value {
            write!(f, "  {value}")?;
        }*/
        Ok(())
    }
}

impl<'h> From<&'h TextBlock<'h>> for Metadata<'h> {
    fn from(block: &'h TextBlock<'h>) -> Self {
        Self { block }
    }
}
