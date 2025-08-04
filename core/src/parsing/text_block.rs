/*
 * Copyright (c) 2023-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::alloc::HerdAllocator;
use crate::err;
use crate::error::JournResult;
use crate::error::parsing::{IErrorMsg, IParseError};
use crate::ext::StrExt;
use crate::parsing::IParseResult;
use crate::parsing::text_input::{BlockInput, LocatedInput, TextInput};
use crate::parsing::util::{
    blank_lines0, comment, indented, recognize_rtrim, repeat0, rest_line0, rest_line1,
};
use nom::character::complete::{multispace0, space0};
use nom::combinator::rest;
use nom::sequence::{pair, preceded};
use nom::{Err as NomErr, Parser};
use nom_locate::LocatedSpan;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt;
use std::path::Path;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TextBlockLocation<'h> {
    file: Option<&'h str>,
    /// The row number where the first row is 1. This will be `None` when row information
    /// is not being tracked.
    line: u32,
    /// The byte offset in the stream.
    offset: usize,
}

impl<'h> TextBlockLocation<'h> {
    pub fn new(file: Option<&'h str>, line: u32, offset: usize) -> Self {
        debug_assert!(line >= 1);

        Self { file, line, offset }
    }

    pub fn file(&self) -> Option<&'h str> {
        self.file
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn offset(&self) -> usize {
        self.offset
    }
}

impl<'h> fmt::Display for TextBlockLocation<'h> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.file {
            Some(file) => write!(f, "{}:{}", file, self.line)?,
            None => write!(f, "{}", self.line)?,
        }
        Ok(())
    }
}

/// A text block may be preceded by one or more blank lines and/or comments.
/// This must implement `Send` and `Sync` as it can move over thread boundaries.
///
/// # Rules about blocks
/// 1. A block can have zero or more children blocks, each indented more than itself.
/// 2. A block may be preceded by zero or more spaces, blank lines and/or comments.
/// 3. A block may start anywhere in a line, but will always encompass line remainder at least.
/// 4. A block always terminates before spaces or newlines (these are part of the next block).
#[derive(Debug, Clone)]
pub struct TextBlock<'h> {
    text: Cow<'h, str>,
    parent: Option<&'h TextBlock<'h>>,
    /// All blocks will have a location when `parent` is `Some`.
    location: Option<TextBlockLocation<'h>>,
}

impl<'h> TextBlock<'h> {
    pub fn from_file(
        file: &'h Path,
        allocator: &HerdAllocator<'h>,
        parent: Option<&'h TextBlock<'h>>,
    ) -> JournResult<Self> {
        if !file.exists() {
            return Err(err!("File '{}' does not exist", file.display()));
        }
        let text = std::fs::read_to_string(file).map_err(
            |e| err!(err!("IO Error: {}", e); "Cannot open file for reading; check that it exists"),
        )?;
        match parent {
            Some(parent) => Ok(Self::new_child(
                allocator.alloc(text),
                TextBlockLocation::new(Some(file.to_str().unwrap()), 1, 0),
                parent,
            )),
            None => Ok(Self::new_root(
                allocator.alloc(text),
                Some(TextBlockLocation::new(Some(file.to_str().unwrap()), 1, 0)),
            )),
        }
    }

    fn new_root(text: &'h str, location: Option<TextBlockLocation<'h>>) -> Self {
        TextBlock { text: Cow::Borrowed(text), location, parent: None }
    }

    pub(super) fn new_child(
        text: &'h str,
        location: TextBlockLocation<'h>,
        parent: &'h TextBlock<'h>,
    ) -> Self {
        Self { text: Cow::Borrowed(text), location: Some(location), parent: Some(parent) }
    }

    pub fn parent(&self) -> Option<&'h TextBlock<'h>> {
        self.parent
    }

    /// Gets whether this block is the file root. There are two primary circumstances to consider:
    /// whether the block is part of a node or not.
    pub fn is_file_root(&self) -> bool {
        // If the block has no parent, then being the root is always true.
        if self.parent.is_none() {
            return true;
        }
        // Otherwise, look at the parent's node and see whether it changes.
        match self.location.map(|l| l.file) {
            Some(file) => file != self.parent.unwrap().location.map(|l| l.file).unwrap(),
            None => false,
        }
    }

    pub fn location(&self) -> Option<TextBlockLocation<'h>> {
        self.location
        //TextBlockLocation::new(self.filename(), self.text.location_line(), self.text.naive_get_utf8_column())
    }

    pub fn file(&self) -> Option<&'h str> {
        self.location.and_then(|l| l.file)
    }

    /// Gets the location line where this block starts.
    pub fn line(&self) -> u32 {
        self.location.map(|l| l.line).unwrap_or(1)
    }

    pub fn location_offset(&self) -> usize {
        self.location.map(|l| l.offset).unwrap_or(0)
    }

    /// Gets the location column of where this block starts.
    /// Since blocks must never end with whitespace, the column of all blocks, but
    /// the first block is really one more than the last column of the last line of the
    /// preceding block.
    ///
    /// The column is a 1-based index.
    pub fn column(&self) -> u32 {
        match self.location {
            Some(loc) => unsafe {
                LocatedSpan::new_from_raw_offset(loc.offset, loc.line, &*self.text, ())
                    .naive_get_utf8_column() as u32
            },
            // No location means there's no parent, so we're the root block.
            None => 1,
        }
    }

    pub fn trimmed_start_lines(&self) -> TextBlock {
        let (rem, blanks) = blank_lines0(&*self.text).unwrap();
        let blank_lines_count = blanks.lines().count();

        let location = match &self.location {
            Some(loc) => {
                if blank_lines_count > 0 {
                    TextBlockLocation::new(
                        loc.file,
                        loc.line + blank_lines_count as u32,
                        loc.offset + blanks.len(),
                    )
                } else {
                    loc.clone()
                }
            }
            None => TextBlockLocation::new(None, 1 + blank_lines_count as u32, blanks.len()),
        };
        TextBlock { text: Cow::Borrowed(rem), parent: self.parent, location: Some(location) }
    }

    /// Gets the line number after any leading blank lines.
    pub fn first_content_line(&self) -> u32 {
        let lines_skipped = self.skip_leading_blank_lines().1.lines().count();
        // No location means there's no parent, so we're the root block.
        self.location.map(|loc| loc.line + lines_skipped as u32).unwrap_or(lines_skipped as u32 + 1)
    }

    pub fn last_line(&self) -> u32 {
        self.line() + self.text().lines().count() as u32 - 1
    }

    /// Gets the text of the block, including any leading whitespace.
    pub fn text(&self) -> &str {
        &*self.text
    }

    /// Gets the amount of indentation for this block. This is usually the amount of spaces
    /// and tabs that precede its content on the first line.
    pub fn indented_amount(&self) -> u16 {
        // When the parent and this block are on the same line, the starting indent is the column of
        // where this block starts; otherwise, the block always starts with the newline of the preceding
        // block (or the start of the file) so the indent is 0.
        let mut indent = match self.parent() {
            Some(parent) if parent.location().unwrap().line == self.line() => self.column() - 1,
            _ => 0,
        };
        for c in self.text().chars() {
            match c {
                ' ' => indent += 1,
                '\t' => indent += 8,
                '\r' | '\n' => indent = 0,
                _ => break,
            }
        }
        indent as u16
    }

    /*
    /// Skips the parameter name of the block and attempts to determine the indentation level of the block
    /// by examining the first two lines. This level of indent is then stripped from the remaining lines.
    pub fn text_outdented(&self, param_name: &'h str) -> String {
        let s = param_value_untrimmed(param_name)(self.text).map(|r| r.1).unwrap_or(self.text());

        // We use the indent of the second line if it exists, otherwise the first line.
        // E.g.
        // ```
        // python import datetime
        //   import sys
        // ```
        let indent = s.lines().nth(1).map(|l| l.indented_amount()).unwrap_or(s.indented_amount());

        let outdented_contents =
            s.lines().map(|l| l.outdent(indent).unwrap_or(l).to_string()).join("\n");
        outdented_contents
    }*/

    /// Gets the text of the block with the block's indent stripped from the first line, and subsequently, for each level:
    /// * 1 - nothing more,
    /// * 2 - the block's indent + 1 stripped from every subsequent line.
    /// * 3 - the full indent of the second line if it exists, otherwise the first line.
    ///
    /// This is useful because it allows the user to control over the leading space for each line.
    /// # Example
    /// ```
    /// # use journ_core::block;
    /// # use journ_core::parsing::text_block::TextBlock;
    /// assert_eq!(block!("+MyMetadata\n This\n valuehas\n nospaces").text_outdented(2), "+MyMetadata\nThis\nvaluehas\nnospaces");
    /// assert_eq!(block!("+MyMetadata\n  This\n  valuehas\n  onespace").text_outdented(2), "+MyMetadata\n This\n valuehas\n onespace");
    /// ```
    pub fn text_outdented(&self, level: usize) -> String {
        assert!(level > 0, "Level must be greater than 0");
        assert!(level <= 3, "Only levels 1, 2, and 3 are supported");

        let trimmed_start_lines = self.trimmed_start_lines();

        let block_indent = match level {
            1 => trimmed_start_lines.indented_amount(),
            2 => trimmed_start_lines.indented_amount() + 1,
            3 => trimmed_start_lines
                .text()
                .lines()
                .nth(1)
                .map(|l| l.indented_amount())
                .unwrap_or(trimmed_start_lines.indented_amount()),
            _ => unreachable!(),
        };

        // Outdent the first line which may not be indented as much as block_indent if its parent is on the same line.
        let mut outdented_contents = trimmed_start_lines
            .text()
            .lines()
            .nth(0)
            .map(|l| l.trim_start_matches(|c| c == ' ' || c == '\t'))
            .unwrap()
            .to_string();
        for l in trimmed_start_lines.text().lines().skip(1) {
            outdented_contents.push('\n');
            outdented_contents.push_str(l.outdent(block_indent).unwrap());
        }
        outdented_contents
    }

    /// Gets the whitespace that leads this block.
    pub fn leading_blank_lines(&self) -> &str {
        blank_lines0(&*self.text).unwrap().1
    }

    /// Pre-read any leading blank lines.
    /// Do not read leading blank lines for the root block (i.e. the entire file).
    pub fn skip_leading_blank_lines(&self) -> (&str, &str) {
        blank_lines0(&*self.text).unwrap()
    }

    pub fn with_leading_whitespace(
        &self,
        whitespace: &str,
        allocator: &'h HerdAllocator<'h>,
    ) -> TextBlock<'h> {
        let mut t = String::with_capacity(whitespace.len() + self.text.len());
        t.push_str(whitespace);
        t.push_str(self.text.trim_start());

        let mut block = self.clone();
        block.text = Cow::Borrowed(allocator.alloc(t));
        block
    }

    pub fn is_comment(&self) -> bool {
        comment(self.skip_leading_blank_lines().0).is_ok()
    }
}

impl<'h> fmt::Display for TextBlock<'h> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.text())
    }
}

impl<'h> Ord for TextBlock<'h> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.text().cmp(other.text())
    }
}

impl<'h> PartialOrd for TextBlock<'h> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'h> From<&'h str> for TextBlock<'h> {
    fn from(text: &'h str) -> Self {
        TextBlock::new_root(text, None)
    }
}

impl PartialEq for TextBlock<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.text() == other.text()
    }
}

impl Eq for TextBlock<'_> {}

/// Parser for reading a block within a parent block, the `BlockInput`.
/// A valid block will be returned if the stream has at least one non-multispace character.
pub fn block<'h, I>(input: I) -> IParseResult<'h, I, I>
where
    I: TextInput<'h> + LocatedInput<'h> + BlockInput<'h>,
{
    let orig_input = input.clone();
    let rem = blank_lines0(input).expect("Infallible").0;

    // Read the preceding whitespace of the block's first content line. This will tell us its
    // indent.
    let rem = space0::<_, ()>(rem).unwrap().0;
    let min_indent = rem.column() as u16;

    // A block is at least till the end of the line.
    let rem = recognize_rtrim(rest_line0)(rem).expect("Infallible").0;

    // Read all subsequent lines that are indented at least one more than the first line.
    // This should mean that the block ends with a single newline character for all but the last block which may not have.
    let rem = recognize_rtrim(repeat0(preceded(
        pair(blank_lines0, indented(min_indent)),
        rest_line1,
    )))(rem)
    .expect("Infallible")
    .0;

    let block_text = orig_input.slice(..orig_input.input_len() - rem.input_len());
    if multispace0(block_text.clone())?.0.input_len() == 0 {
        return Err(NomErr::Error(IParseError::new(IErrorMsg::VALUE, block_text)));
    }
    let block = TextBlock::new_child(
        block_text.text(),
        TextBlockLocation::new(orig_input.file(), orig_input.line(), orig_input.location_offset()),
        orig_input.block(),
    );
    let new_input = block_text.with_child(block);
    Ok((rem, new_input))
}

/// Reads the remainder of the current block, expecting at least one non-space char.
pub fn block_remainder1<'h, I>(input: I) -> IParseResult<'h, I, I>
where
    I: TextInput<'h> + BlockInput<'h>,
{
    let (rem, rest) = rest(input)?;
    if rest.text().trim().is_empty() {
        return Err(NomErr::Error(IParseError::new(IErrorMsg::VALUE, rest)));
    }
    Ok((rem, rest))
}

/// Returns `Ok` if the inner parser consumes the remainder of the block.
pub fn all_block_consuming<'h, I, O, F>(mut f: F) -> impl FnMut(I) -> IParseResult<'h, I, O>
where
    I: TextInput<'h> + BlockInput<'h>,
    F: Parser<I, O, IParseError<I>>,
{
    move |input: I| {
        let (rem, res) = f.parse(input)?;
        let end = space0::<_, IParseError<_>>(rem.clone()).unwrap().0;
        if end.input_len() == 0 {
            Ok((end, res))
        } else {
            Err(NomErr::Error(IParseError::new("Unexpected value", rem)))
        }
    }
}

#[macro_export]
macro_rules! block {
    ($text:expr) => {
        TextBlock::from($text)
    };
}
