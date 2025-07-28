/*
 * Copyright (c) 2023-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::alloc::HerdAllocator;
use crate::configuration::Configuration;
use crate::err;
use crate::error::{BlockContext, BlockContextError, BlockContextLine, JournError};
use crate::journal_node::JournalNode;
use crate::parsing::DerefMutAndDebug;
use crate::parsing::parser::JournalFileParseNode;
use crate::parsing::text_block::TextBlock;
use nom::Err as NomErr;
use nom::error::{ErrorKind, ParseError};
use nom::{
    AsBytes, Compare, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Needed,
    Offset, Slice,
};
use nom_locate::LocatedSpan;
use std::cell::{Ref, RefCell, RefMut};
use std::fmt;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::{Deref, DerefMut, Range, RangeFrom, RangeTo};
use std::str::{CharIndices, Chars};

pub trait JournalNodeAccess<'h> {
    fn node(&self) -> &JournalNode<'h>;
}

impl<'h, 'p, 's, 'e> JournalNodeAccess<'h> for &'p JournalFileParseNode<'h, 's, 'e>
where
    'h: 'e,
    'e: 's,
{
    fn node(&self) -> &JournalNode<'h> {
        JournalFileParseNode::node(self)
    }
}

impl<'h, 'p, 's, 'e> JournalNodeAccess<'h>
    for LocatedSpan<&'h str, &'p JournalFileParseNode<'h, 's, 'e>>
where
    'h: 'e,
    'e: 's,
{
    fn node(&self) -> &JournalNode<'h> {
        self.extra.node()
    }
}

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

pub trait LocatedInput<'h> {
    fn file(&self) -> Option<&'h str>;

    /// Gets the current line number where the first line is 1.
    fn line(&self) -> u32;

    /// Gets the current column index of the input where the first column is 1.
    fn column(&self) -> u32;

    fn location_offset(&self) -> usize;

    fn into_located_span<X>(self, extra: X) -> LocatedSpan<&'h str, X>;
}

pub trait ConfigInput<'h> {
    fn config(&self) -> impl Deref<Target = Configuration<'h>>;
    fn config_mut(&self) -> impl DerefMut<Target = Configuration<'h>>;
}

impl<'s, 'h> ConfigInput<'h>
    for LocatedSpan<&'h str, &RefCell<dyn DerefMutAndDebug<'h, 's, Configuration<'h>>>>
{
    fn config(&self) -> impl Deref<Target = Configuration<'h>> {
        Ref::map(self.extra.borrow(), |c| &**c)
    }

    fn config_mut(&self) -> impl DerefMut<Target = Configuration<'h>> {
        RefMut::map(self.extra.borrow_mut(), |c| &mut **c)
    }
}

pub trait NodeInput<'h, 's, 'e, 'p> {
    fn parse_node(&self) -> &'p JournalFileParseNode<'h, 's, 'e>;
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

impl<'h, 's> LocatedInput<'h>
    for LocatedSpan<&'h str, &RefCell<dyn DerefMutAndDebug<'h, 's, Configuration<'h>>>>
{
    fn file(&self) -> Option<&'h str> {
        None
    }

    fn line(&self) -> u32 {
        self.location_line()
    }

    fn column(&self) -> u32 {
        self.naive_get_utf8_column() as u32
    }

    fn location_offset(&self) -> usize {
        self.location_offset()
    }

    fn into_located_span<X>(self, extra: X) -> LocatedSpan<&'h str, X> {
        self.map_extra(|_| extra)
    }
}

impl<'h, 's, 'e, 'p> LocatedInput<'h> for LocatedSpan<&'h str, &'p JournalFileParseNode<'h, 's, 'e>>
where
    'h: 'e,
    'e: 's,
    's: 'p,
{
    fn file(&self) -> Option<&'h str> {
        self.extra.node().nearest_filename().map(|f| f.to_str().unwrap())
    }

    fn line(&self) -> u32 {
        self.location_line()
    }

    fn location_offset(&self) -> usize {
        self.location_offset()
    }

    fn column(&self) -> u32 {
        self.naive_get_utf8_column() as u32
    }

    fn into_located_span<X2>(self, extra: X2) -> LocatedSpan<&'h str, X2> {
        self.map_extra(|_| extra)
    }
}

pub trait BlockInput<'h> {
    fn block(&self) -> &'h TextBlock<'h>;

    /// Changes the current block of the input.
    fn with_child(self, block: TextBlock<'h>) -> Self;
}

impl<'h, 'p, 's, 'e> TextInput<'h> for LocatedSpan<&'h str, &'p JournalFileParseNode<'h, 's, 'e>>
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

pub enum ConfigAccessRef<'p, 'h, 'c> {
    Owned(Ref<'p, Configuration<'h>>),
    RefMut(Ref<'p, &'c mut Configuration<'h>>),
}

impl<'p, 'h, 'c> Deref for ConfigAccessRef<'p, 'h, 'c> {
    type Target = Configuration<'h>;

    fn deref(&self) -> &Self::Target {
        match self {
            ConfigAccessRef::Owned(config) => config,
            ConfigAccessRef::RefMut(config) => config,
        }
    }
}

pub enum ConfigAccessMut<'p, 'h, 'c> {
    Owned(RefMut<'p, Configuration<'h>>),
    RefMut(RefMut<'p, &'c mut Configuration<'h>>),
}

impl<'p, 'h, 'c> Deref for ConfigAccessMut<'p, 'h, 'c> {
    type Target = Configuration<'h>;

    fn deref(&self) -> &Self::Target {
        match self {
            ConfigAccessMut::Owned(config) => config,
            ConfigAccessMut::RefMut(config) => config,
        }
    }
}

impl<'p, 'h, 'c> DerefMut for ConfigAccessMut<'p, 'h, 'c> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            ConfigAccessMut::Owned(config) => config,
            ConfigAccessMut::RefMut(config) => config,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TextBlockInput<'h, X> {
    block: &'h TextBlock<'h>,
    inner: LocatedSpan<&'h str, X>,
    allocator: &'h HerdAllocator<'h>,
}

impl<'h, X> TextBlockInput<'h, X> {
    pub fn new(
        inner: LocatedSpan<&'h str, X>,
        block: &'h TextBlock<'h>,
        allocator: &'h HerdAllocator<'h>,
    ) -> Self {
        Self { block, allocator, inner }
    }

    pub fn block(&self) -> &'h TextBlock<'h> {
        self.block
    }

    pub fn map_extra<X2, F: FnOnce(X) -> X2>(self, f: F) -> TextBlockInput<'h, X2> {
        TextBlockInput {
            inner: self.inner.map_extra(f),
            block: self.block,
            allocator: self.allocator,
        }
    }

    fn with_inner(self, inner: LocatedSpan<&'h str, X>) -> Self {
        Self { inner, ..self }
    }
}

impl<'h, X> Deref for TextBlockInput<'h, X> {
    type Target = LocatedSpan<&'h str, X>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'h, X> InputLength for TextBlockInput<'h, X> {
    fn input_len(&self) -> usize {
        self.inner.input_len()
    }
}

impl<'h, X: Clone> InputTake for TextBlockInput<'h, X> {
    fn take(&self, count: usize) -> Self {
        let new_inner = LocatedSpan::take(&self.inner, count);
        self.clone().with_inner(new_inner)
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (left, right) = LocatedSpan::take_split(&self.inner, count);
        (self.clone().with_inner(left), self.clone().with_inner(right))
    }
}

impl<'h, X> Compare<&str> for TextBlockInput<'h, X> {
    fn compare(&self, t: &str) -> nom::CompareResult {
        self.inner.compare(t)
    }

    fn compare_no_case(&self, t: &str) -> nom::CompareResult {
        self.inner.compare_no_case(t)
    }
}

impl<'h, X> Hash for TextBlockInput<'h, X> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state)
    }
}

impl<'h, X> AsBytes for TextBlockInput<'h, X> {
    fn as_bytes(&self) -> &[u8] {
        self.inner.as_bytes()
    }
}

impl<'h, X> InputIter for TextBlockInput<'h, X> {
    type Item = char;
    type Iter = CharIndices<'h>;
    type IterElem = Chars<'h>;

    fn iter_indices(&self) -> Self::Iter {
        self.inner.iter_indices()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.inner.iter_elements()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.inner.position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        self.inner.slice_index(count)
    }
}

impl<'h, X> Offset for TextBlockInput<'h, X> {
    fn offset(&self, second: &Self) -> usize {
        self.inner.offset(&second.inner)
    }
}

impl<'h, X: Clone> Slice<RangeTo<usize>> for TextBlockInput<'h, X> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.clone().with_inner(self.inner.slice(range))
    }
}

impl<'h, X: Clone> Slice<RangeFrom<usize>> for TextBlockInput<'h, X> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.clone().with_inner(self.inner.slice(range))
    }
}

impl<'h, X: Clone> Slice<Range<usize>> for TextBlockInput<'h, X> {
    fn slice(&self, range: Range<usize>) -> Self {
        self.clone().with_inner(self.inner.slice(range))
    }
}

impl<'h, X: Clone> InputTakeAtPosition for TextBlockInput<'h, X> {
    type Item = char;

    fn split_at_position<P, E>(&self, predicate: P) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
        E: ParseError<Self>,
    {
        match self.fragment().position(predicate) {
            Some(n) => Ok(self.take_split(n)),
            None => Err(NomErr::Incomplete(nom::Needed::new(1))),
        }
    }

    fn split_at_position1<P, E>(&self, predicate: P, e: ErrorKind) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
        E: ParseError<Self>,
    {
        match self.fragment().position(predicate) {
            Some(0) => Err(NomErr::Error(E::from_error_kind(self.clone(), e))),
            Some(n) => Ok(self.take_split(n)),
            None => Err(NomErr::Incomplete(nom::Needed::new(1))),
        }
    }

    fn split_at_position_complete<P, E>(&self, predicate: P) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
        E: ParseError<Self>,
    {
        match self.split_at_position(predicate) {
            Err(NomErr::Incomplete(_)) => Ok(self.take_split(self.input_len())),
            res => res,
        }
    }

    fn split_at_position1_complete<P, E>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
        E: ParseError<Self>,
    {
        match self.fragment().position(predicate) {
            Some(0) => Err(NomErr::Error(E::from_error_kind(self.clone(), e))),
            Some(n) => Ok(self.take_split(n)),
            None => {
                if self.fragment().input_len() == 0 {
                    Err(NomErr::Error(E::from_error_kind(self.clone(), e)))
                } else {
                    Ok(self.take_split(self.input_len()))
                }
            }
        }
    }
}

impl<'h, X: Clone + fmt::Debug> TextInput<'h> for TextBlockInput<'h, X>
where
    TextBlockInput<'h, X>: LocatedInput<'h>,
    LocatedSpan<&'h str, X>: LocatedInput<'h>,
{
    fn text(&self) -> &'h str {
        self.inner.text()
    }

    fn into_err(self, ctx: &str) -> JournError {
        let mut block = self.block().trimmed_start_lines();

        // Go up the block tree while the parent block is on the same line as the current block.
        // We need full lines so that indexing works correctly.
        while let Some(parent) = block.parent() {
            if block.column() == 1 {
                break;
            }
            //if parent.column() == 1 || parent.text().lines().count() > 20 {
            //    break;
            //}
            block = parent.trimmed_start_lines();
        }
        let mut context_lines = vec![];
        let mut line_num = block.location().unwrap().line() as usize;
        let input_end = self.inner.slice(self.inner.input_len()..);
        for line in block.skip_leading_blank_lines().0.lines() {
            let highlight_range = {
                if line_num < self.inner.line() as usize || line_num > input_end.line() as usize {
                    0..0
                } else {
                    let start = if line_num == self.inner.line() as usize {
                        self.inner.get_line_beginning().len()
                            - self.inner.lines().next().unwrap().len()
                    } else {
                        0
                    };
                    let end = if line_num == input_end.line() as usize {
                        input_end.get_line_beginning().len()
                    } else {
                        line.len()
                    };
                    start..end
                }
            };

            context_lines.push(BlockContextLine::new(
                Some(line_num),
                line.to_string(),
                highlight_range,
            ));
            line_num += 1;
        }
        let context = BlockContext::new(
            self.block.location().and_then(|l| l.file().map(|f| f.to_string())),
            Some(self.inner.line() as usize),
            Some(self.inner.column() as usize),
            context_lines,
        );
        let bce = BlockContextError::new(context, ctx.to_string());
        err!(bce)
        //err!(err!(context); format!("{} {}", context.location_string().unwrap(), ctx.to_string()))
    }
}

impl<'h> LocatedInput<'h> for TextBlockInput<'h, ()> {
    fn file(&self) -> Option<&'h str> {
        None
    }

    fn line(&self) -> u32 {
        self.inner.location_line()
    }

    fn column(&self) -> u32 {
        self.inner.naive_get_utf8_column() as u32
    }

    fn location_offset(&self) -> usize {
        self.inner.location_offset()
    }

    fn into_located_span<X>(self, extra: X) -> LocatedSpan<&'h str, X> {
        self.inner.map_extra(|_| extra)
    }
}

impl<'h, 's> LocatedInput<'h>
    for TextBlockInput<'h, &RefCell<dyn DerefMutAndDebug<'h, 's, Configuration<'h>>>>
{
    fn file(&self) -> Option<&'h str> {
        None
    }

    fn line(&self) -> u32 {
        self.inner.location_line()
    }

    fn column(&self) -> u32 {
        self.inner.naive_get_utf8_column() as u32
    }

    fn location_offset(&self) -> usize {
        self.inner.location_offset()
    }

    fn into_located_span<X>(self, extra: X) -> LocatedSpan<&'h str, X> {
        self.inner.map_extra(|_| extra)
    }
}

impl<'h, 's, 'e, 'p> LocatedInput<'h> for TextBlockInput<'h, &'p JournalFileParseNode<'h, 's, 'e>>
where
    'h: 'e,
    'e: 's,
    's: 'p,
{
    fn file(&self) -> Option<&'h str> {
        self.inner.extra.node().nearest_filename().map(|f| f.to_str().unwrap())
    }

    fn line(&self) -> u32 {
        self.inner.location_line()
    }

    fn column(&self) -> u32 {
        self.inner.naive_get_utf8_column() as u32
    }

    fn location_offset(&self) -> usize {
        self.inner.location_offset()
    }

    fn into_located_span<X>(self, extra: X) -> LocatedSpan<&'h str, X> {
        self.inner.map_extra(|_| extra)
    }
}

impl<'h, X> BlockInput<'h> for TextBlockInput<'h, X> {
    fn block(&self) -> &'h TextBlock<'h> {
        self.block
    }

    fn with_child(self, block: TextBlock<'h>) -> Self {
        let allocated = self.allocator.alloc(block);

        // Create a new LC, only if we are moving to a new file. Otherwise, the location
        // data is reset.
        let inner = if allocated.is_file_root() {
            LocatedSpan::new_extra(allocated.text(), self.inner.extra)
        } else {
            self.inner
        };

        Self { block: allocated, inner, allocator: self.allocator }
    }
}

impl<'h, 's, 'e, 'p> ConfigInput<'h> for TextBlockInput<'h, &'p JournalFileParseNode<'h, 's, 'e>>
where
    'h: 'e,
    'e: 's,
    's: 'p,
{
    fn config(&self) -> impl Deref<Target = Configuration<'h>> {
        self.extra.config().borrow()
    }

    fn config_mut(&self) -> impl DerefMut<Target = Configuration<'h>> {
        self.extra.config().borrow_mut()
    }
}

impl<'h, 's> ConfigInput<'h>
    for TextBlockInput<'h, &RefCell<dyn DerefMutAndDebug<'h, 's, Configuration<'h>>>>
{
    fn config(&self) -> impl Deref<Target = Configuration<'h>> {
        Ref::map(self.extra.borrow(), |c| &**c)
    }

    fn config_mut(&self) -> impl DerefMut<Target = Configuration<'h>> {
        RefMut::map(self.extra.borrow_mut(), |c| &mut **c)
    }
}

impl<'h, 's, 'e, 'p> NodeInput<'h, 's, 'e, 'p>
    for TextBlockInput<'h, &'p JournalFileParseNode<'h, 's, 'e>>
{
    fn parse_node(&self) -> &'p JournalFileParseNode<'h, 's, 'e> {
        self.inner.extra
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
