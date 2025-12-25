/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::alloc::HerdAllocator;
use crate::err;
use crate::error::{BlockContext, BlockContextError, BlockContextLine, JournError};
use crate::parsing::input::{LocatedInput, TextInput};
use crate::parsing::text_block::TextBlock;
use nom::Err as NomErr;
use nom::error::{ErrorKind, ParseError};
use nom::{
    AsBytes, Compare, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Needed,
    Offset, Slice,
};
use nom_locate::LocatedSpan;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::{Deref, Range, RangeFrom, RangeTo};
use std::str::{CharIndices, Chars};

pub trait BlockInput<'h> {
    /// Gets the current block of the input.
    fn block(&self) -> &'h TextBlock<'h>;

    /// Changes the current block of the input.
    fn with_child(self, block: TextBlock<'h>) -> Self;

    fn allocator(&self) -> &'h HerdAllocator<'h>;
}

/// An input type that's able to parse a `TextBlock`.
///
/// Users of this type may find it more convenient to use the `parse_block!()` macro.
#[derive(Debug, Clone, Copy)]
pub struct TextBlockInput<'h, X> {
    block: &'h TextBlock<'h>,
    pub(in crate::parsing) inner: LocatedSpan<&'h str, X>,
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

    pub fn into_extra(self) -> X {
        self.inner.into_fragment_and_extra().1
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

impl<'h, X: Clone + Debug> TextInput<'h> for TextBlockInput<'h, X>
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

    fn allocator(&self) -> &'h HerdAllocator<'h> {
        self.allocator
    }
}
