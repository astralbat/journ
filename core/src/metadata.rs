/*
 * Copyright (c) 2019-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::alloc::HerdAllocator;
use crate::configuration::{Configuration, Expression};
use crate::error::{BlockContext, BlockContextError, JournError, JournResult};
use crate::journal_obj::JournalObj;
use crate::parsing::input::{BlockInput, ConfigInput, LocatedInput, TextBlockInput, TextInput};
use crate::parsing::text_block::{TextBlock, block, block_remainder1};
use crate::parsing::util::{blank_lines0, double_space, spaced_word};
use crate::parsing::{IParseResult, block_parse, entry};
use crate::{err, impl_journal_obj_common};
use nom::bytes::complete::tag;
use nom::character::complete::{space0, space1};
use nom::combinator::{map, opt, recognize};
use nom::sequence::{preceded, tuple};
use smallvec::SmallVec;
use std::borrow::Cow;
use std::cell::RefCell;
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::sync::OnceLock;

/// &str wrapper for key, to override PartialEq behavior.
/// Metadata key comparisons are case-insensitive.
#[derive(Debug, Clone, Hash)]
pub struct MetadataKey<'h>(pub Cow<'h, str>);

impl MetadataKey<'_> {
    pub fn is_match(&self, pattern: &str) -> bool {
        Expression::from(pattern).is_match(&self.0)
    }
}

impl PartialEq for MetadataKey<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq_ignore_ascii_case(&other.0)
    }
}
impl Eq for MetadataKey<'_> {}

impl PartialEq<&str> for MetadataKey<'_> {
    fn eq(&self, other: &&str) -> bool {
        self.0.eq_ignore_ascii_case(other)
    }
}

impl PartialEq<&str> for &MetadataKey<'_> {
    fn eq(&self, other: &&str) -> bool {
        self.0.eq_ignore_ascii_case(other)
    }
}
impl AsRef<str> for MetadataKey<'_> {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/*
#[derive(Debug, Clone)]
pub struct MetadataValue<'h>(&'h str);

impl<'h> MetadataValue<'h> {
    pub fn as_str(&self) -> &str {
        self.0.block().text().trim()
    }

    /// Reads each sub-block of the value, attempting to interpret them as metadata
    pub fn as_metadata_lines(&self) -> SmallVec<[Metadata<'h>; 4]> {
        let mut lines = SmallVec::new();

        let mut input = self.0.clone();
        while let Ok((rem, maybe_metadata)) = block(input) {
            input = rem;
            if let Ok(lmd) = entry::metadata(maybe_metadata).map(|r| r.1) {
                lines.push(lmd)
            }
        }
        lines
    }

    pub fn unindented(&self) -> String {
        self.0.block().text_outdented(3)
    }
}

impl PartialEq for MetadataValue<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.unindented().eq(&other.unindented())
    }
}

impl PartialEq<&str> for &MetadataValue<'_> {
    fn eq(&self, other: &&str) -> bool {
        self.unindented().eq(other)
    }
}

impl fmt::Display for MetadataValue<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.text())
    }
}*/

/// Metadata provides additional key/value information on `JournalEntries`.
///
/// # Thread Safety
/// Metadata must be both `Send` and `Sync` as per requirement of `Unit` which can hold `Metadata` items.
#[derive(Debug, Clone)]
pub struct Metadata<'h> {
    inner: OnceLock<MetadataInner<'h>>,
    block: OnceLock<&'h TextBlock<'h>>,
    config: Configuration<'h>,
}

impl_journal_obj_common!(Metadata<'h>);

impl<'h> Metadata<'h> {
    pub fn new<K: Into<Cow<'h, str>>, V: Into<Cow<'h, str>>>(
        config: Configuration<'h>,
        key: K,
        value: Option<V>,
    ) -> Self {
        Self {
            block: OnceLock::new(),
            inner: OnceLock::from(MetadataInner {
                key: MetadataKey(key.into()),
                value: value.map(Into::into),
            }),

            config,
        }
    }

    /// Creates a new lazy metadata
    pub fn lazy(config: Configuration<'h>, block: &'h TextBlock<'h>) -> Self {
        Self { config, block: OnceLock::from(block), inner: OnceLock::new() }
    }

    fn inner(&self) -> &MetadataInner<'h> {
        self.inner.get_or_init(|| {
            MetadataInner::init(self.block.get().expect("Illegal metadata state"), &self.config)
        })
    }

    pub fn key(&self) -> &MetadataKey<'h> {
        self.inner().key()
    }

    /// Gets the metadata value.
    ///
    /// The returned string is not trimmed.
    pub fn value(&self) -> Option<&str> {
        self.inner().value()
    }

    /// Gets the pretext - the padded '+' before the key.
    pub fn pretext(&self) -> Option<&'h str> {
        self.block.get().map(|block| {
            let (_rem, out) =
                block_parse(block, self.config.clone(), Self::pretext_parser).unwrap();
            out.1
        })
    }

    pub fn parse_value<F, O>(&self, parser: F, block_context_error: &'static str) -> JournResult<O>
    where
        F: FnMut(
            TextBlockInput<'h, RefCell<Configuration<'h>>>,
        ) -> IParseResult<'h, TextBlockInput<'h, RefCell<Configuration>>, O>,
    {
        let (_rem, out) = block_parse(
            self.block(),
            self.config.clone(),
            preceded(tuple((Self::pretext_parser, Self::key_parser)), parser),
        )
        .map_err(|e| self.err(block_context_error, None).with_source(e))?;
        Ok(out.1)
    }

    /// Reads each sub-block of the value, attempting to interpret them as metadata
    pub fn value_as_metadata_lines(&self) -> SmallVec<[Metadata<'h>; 4]> {
        self.parse_value(Self::value_as_nested_metadata_parser, "No nested metadata").unwrap()
    }

    /// Creates an error that includes the text block of the metadata.
    pub fn err<E: Into<JournError>>(&self, msg: E, highlight_text: Option<&str>) -> JournError {
        let mut context = BlockContext::from(self.block());
        if let Some(highlight_text) = highlight_text {
            context.highlight(highlight_text);
        }
        err!(BlockContextError::new(context, msg))
    }

    fn pretext_parser<I: TextInput<'h>>(input: I) -> IParseResult<'h, I, &'h str> {
        map(recognize(tuple((blank_lines0, space1, tag("+"), space0))), |s: I| s.text())(input)
    }
    fn key_parser<I: TextInput<'h>>(input: I) -> IParseResult<'h, I, MetadataKey<'h>> {
        map(spaced_word, |s: I| MetadataKey(Cow::Borrowed(s.text())))(input)
    }
    fn value_parser<I: TextInput<'h> + BlockInput<'h>>(
        input: I,
    ) -> IParseResult<'h, I, Option<Cow<'h, str>>> {
        opt(preceded(double_space, map(block_remainder1, |r: I| Cow::Borrowed(r.text()))))(input)
    }
    fn value_as_nested_metadata_parser<
        I: TextInput<'h> + LocatedInput<'h> + ConfigInput<'h> + BlockInput<'h>,
    >(
        mut input: I,
    ) -> IParseResult<'h, I, SmallVec<[Metadata<'h>; 4]>> {
        let mut lines = SmallVec::new();
        while let Ok((rem, maybe_metadata)) = block(input.clone()) {
            input = rem;
            if let Ok(md) = entry::metadata(maybe_metadata).map(|r| r.1) {
                lines.push(md)
            }
        }
        Ok((input, lines))
    }
}

impl fmt::Display for Metadata<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match (self.block.get(), self.inner.get()) {
            (Some(block), _) => write!(f, "{}", block),
            (None, Some(inner)) => write!(f, " +{}", inner),
            (None, None) => unreachable!("Illegal metadata state"),
        }
    }
}

impl PartialEq for Metadata<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.key().eq(other.key()) && self.value().eq(&other.value())
    }
}

impl Eq for Metadata<'_> {}

impl Hash for Metadata<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.key().hash(state);
        self.value().hash(state);
    }
}

/*
#[derive(Debug)]
pub struct LazyMetadata<'h> {
    block: &'h TextBlock<'h>,
    allocator: &'h HerdAllocator<'h>,
    metadata: OnceLock<Metadata<'h>>,
}
impl<'h> LazyMetadata<'h> {
    pub fn new(block: &'h TextBlock<'h>, allocator: &'h HerdAllocator<'h>) -> Self {
        LazyMetadata { block, allocator, metadata: OnceLock::new() }
    }

    pub fn get_or_init(&self) -> &Metadata<'h> {
        self.metadata.get_or_init(|| {
            let pretext = recognize(tuple((blank_lines0, space1, tag("+"), space0)));

            let (_rem, (pretext, key, value)) =
                // There has been enough prevalidation during main parsing to assert that this won't fail
                tuple((pretext, spaced_word, opt(block_remainder1)))(
                    self.block.as_input(self.allocator),
                ).unwrap();
            Metadata {
                pretext: pretext.text(),
                key: MetadataKey(key.text()),
                value: value.map(|v| MetadataValue(v.block(), self.allocator)),
            }
        })
    }

    pub fn into_inner(self) -> Metadata<'h> {
        self.get_or_init();
        self.metadata.into_inner().unwrap()
    }

    /// Creates an error that includes the text block of the metadata.
    pub fn err(&self, msg: String, highlight_text: Option<&str>) -> JournError {
        let mut context = BlockContext::from(self.block);
        if let Some(highlight_text) = highlight_text {
            context.highlight(highlight_text);
        }
        err!(BlockContextError::new(context, msg))
    }
}

impl<'h> Deref for LazyMetadata<'h> {
    type Target = Metadata<'h>;
    fn deref(&self) -> &Self::Target {
        self.get_or_init()
    }
}

impl Clone for LazyMetadata<'_> {
    fn clone(&self) -> Self {
        Self { block: self.block, allocator: self.allocator, metadata: OnceLock::new() }
    }
}

impl PartialEq for LazyMetadata<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.block == other.block
    }
}

impl Eq for LazyMetadata<'_> {}

impl fmt::Display for LazyMetadata<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.block)
    }
}*/

#[derive(Debug, Clone)]
struct MetadataInner<'h> {
    //pretext: &'h str,
    key: MetadataKey<'h>,
    value: Option<Cow<'h, str>>,
}

impl<'h> MetadataInner<'h> {
    pub fn init(block: &'h TextBlock<'h>, config: &Configuration<'h>) -> MetadataInner<'h> {
        let (_rem, (_config, (key, value))) =
                // There has been enough prevalidation during main parsing to assert that this won't fail
                block_parse(block, config.clone(), tuple((preceded(Metadata::pretext_parser, Metadata::key_parser), Metadata::value_parser))).unwrap();
        Self { key, value }
    }

    /*
    pub fn leading_whitespace(&self) -> &str {
        self.pretext.leading_whitespace()
    }

    pub fn pretext(&self) -> &str {
        self.pretext
    }*/

    pub fn key(&self) -> &MetadataKey<'h> {
        &self.key
    }

    pub fn value(&self) -> Option<&str> {
        self.value.as_deref()
    }
}

impl PartialEq for MetadataInner<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.key() == other.key() && self.value() == other.value()
    }
}

impl Eq for MetadataInner<'_> {}

impl fmt::Display for MetadataInner<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.key.0)?;
        if let Some(value) = &self.value {
            write!(f, "  {value}")?;
        }
        Ok(())
    }
}
