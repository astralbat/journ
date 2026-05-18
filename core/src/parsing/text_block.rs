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
use crate::error::JournResult;
use crate::error::parsing::{IErrorMsg, IParseError};
use crate::ext::StrExt;
use crate::journal_node::JournalNode;
use crate::parsing::IParseResult;
use crate::parsing::input::{BlockInput, LocatedInput, TextBlockInput, TextInput};
use crate::parsing::util::{
    blank_lines0, comment, indented, recognize_rtrim, repeat0, rest_line0, rest_line1,
};
use nom::character::complete::{multispace0, space0};
use nom::combinator::{recognize, rest};
use nom::sequence::{pair, preceded, tuple};
use nom::{Err as NomErr, Parser};
use nom_locate::LocatedSpan;
use smartstring::alias::String as SS;
use std::cmp::Ordering;
use std::fmt::Write;
use std::io::ErrorKind;
use std::ops::Add;
use std::path::Path;
use std::{fmt, io};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TextBlockLocation<'h> {
    node: Option<&'h JournalNode<'h>>,
    /// The row number where the first row is 1. This will be `None` when row information
    /// is not being tracked.
    line: u32,
    /// The byte offset in the stream.
    offset: usize,
}

impl<'h> TextBlockLocation<'h> {
    pub fn new(node: Option<&'h JournalNode<'h>>, line: u32, offset: usize) -> Self {
        debug_assert!(line >= 1);

        Self { node, line, offset }
    }

    pub fn file(&self) -> Option<&'h Path> {
        self.node?.nearest_filename()
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
        match self.file() {
            Some(file) => write!(f, "{}:{}", file.display(), self.line)?,
            None => write!(f, "{}", self.line)?,
        }
        Ok(())
    }
}

impl<'h> Default for TextBlockLocation<'h> {
    fn default() -> Self {
        TextBlockLocation { node: None, line: 1, offset: 0 }
    }
}

impl<'h> Add<&str> for TextBlockLocation<'h> {
    type Output = Self;
    fn add(self, other: &str) -> Self {
        TextBlockLocation {
            node: self.node,
            line: self.line + other.lines().count() as u32,
            offset: self.offset + other.len(),
        }
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
    text: &'h str,
    parent: Option<&'h TextBlock<'h>>,
    location: TextBlockLocation<'h>,
}

impl<'h> TextBlock<'h> {
    pub fn from_file(
        file: &'h Path,
        allocator: &HerdAllocator<'h>,
        parent: Option<&'h TextBlock<'h>>,
    ) -> JournResult<Self> {
        if !file.exists() {
            return Err(err!(io::Error::new(
                ErrorKind::InvalidFilename,
                file.display().to_string()
            )));
        }
        let text = std::fs::read_to_string(file).map_err(
            |e| err!(err!("IO Error: {}", e); "Cannot open file for reading; check that it exists"),
        )?;

        match parent {
            Some(parent) => {
                Ok(Self::new_child(allocator.alloc(text), TextBlockLocation::default(), parent))
            }
            None => Ok(Self::new_root(allocator.alloc(text))),
        }
    }

    fn new_root(text: &'h str) -> Self {
        TextBlock { text, location: TextBlockLocation::default(), parent: None }
    }

    pub(super) fn new_child(
        text: &'h str,
        location: TextBlockLocation<'h>,
        parent: &'h TextBlock<'h>,
    ) -> Self {
        Self { text, location, parent: Some(parent) }
    }

    pub fn set_node(&mut self, node: &'h JournalNode<'h>) {
        self.location.node = Some(node)
    }

    pub fn parent(&self) -> Option<&'h TextBlock<'h>> {
        self.parent
    }

    /// Gets whether this block is the file root. There are two primary circumstances to consider:
    /// whether the block is part of a node or not.
    pub fn is_file_root(&self) -> bool {
        let parent = match self.parent() {
            Some(parent) => parent,
            // If the block has no parent, then being the root is always true.
            None => return true,
        };

        // Otherwise, look at the parent's node and see whether it changes.
        match self.location.node {
            Some(node) => node != parent.location.node.unwrap(),
            None => false,
        }
    }

    pub fn location(&self) -> TextBlockLocation<'h> {
        self.location
        //TextBlockLocation::new(self.filename(), self.text.location_line(), self.text.naive_get_utf8_column())
    }

    pub fn node(&self) -> Option<&'h JournalNode<'h>> {
        self.location.node
    }

    /// Gets the location line where this block starts.
    pub fn line(&self) -> u32 {
        self.location.line
    }

    pub fn location_offset(&self) -> usize {
        self.location.offset
    }

    /// Gets the location column of where this block starts.
    /// Since blocks must never end with whitespace, the column of all blocks, but
    /// the first block is really one more than the last column of the last line of the
    /// preceding block.
    ///
    /// The column is a 1-based index.
    pub fn column(&self) -> u32 {
        unsafe {
            LocatedSpan::new_from_raw_offset(
                self.location.offset,
                self.location.line,
                &*self.text,
                (),
            )
            .naive_get_utf8_column() as u32
        }
    }

    pub fn trimmed_start_lines(&self) -> TextBlock<'h> {
        let (rem, blanks) = blank_lines0(&*self.text).unwrap();
        let blank_lines_count = blanks.lines().count();

        let location = if blank_lines_count > 0 {
            TextBlockLocation::new(
                self.location.node,
                self.location.line + blank_lines_count as u32,
                self.location.offset + blanks.len(),
            )
        } else {
            self.location.clone()
        };
        TextBlock { text: rem, parent: self.parent, location }
    }

    /// Gets the line number after any leading blank lines.
    pub fn first_content_line(&self) -> u32 {
        let lines_skipped = self.skip_leading_blank_lines().1.lines().count();
        self.location.line + lines_skipped as u32
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
            Some(parent) if parent.location().line == self.line() => self.column() - 1,
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
        block.text = allocator.alloc(t);
        block
    }

    pub fn is_comment(&self) -> bool {
        comment(self.skip_leading_blank_lines().0).is_ok()
    }

    /// Gets the block as a kind of input, ready for parsing.
    pub fn as_input(&'h self, allocator: &'h HerdAllocator<'h>) -> TextBlockInput<'h, ()> {
        // SAFETY: Safe since the block offset will always be a valid offset in the
        // node's original text that this block's &str slice points to.
        unsafe {
            TextBlockInput::new(
                LocatedSpan::new_from_raw_offset(
                    self.location().offset(),
                    self.location().line(),
                    self.text(),
                    (),
                ),
                self,
                allocator,
            )
        }
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
        TextBlock::new_root(text)
    }
}

impl PartialEq for TextBlock<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.text() == other.text()
    }
}

impl Eq for TextBlock<'_> {}

/// Parser that reads the optional whitespace at the beginning of a block.
/// This may consist of blank lines and leading space.
pub fn block_leading_whitespace<'h, I>(input: I) -> IParseResult<'h, I, I>
where
    I: TextInput<'h>,
{
    recognize(tuple((blank_lines0, space0)))(input)
}

/// Parser for reading a block within a parent block, the `BlockInput`.
/// A valid block will be returned if the stream has at least one non-multispace character.
pub fn block<'h, I>(input: I) -> IParseResult<'h, I, I>
where
    I: TextInput<'h> + LocatedInput<'h> + BlockInput<'h>,
{
    let orig_input = input.clone();

    let rem = block_leading_whitespace(input).expect("Infallible").0;
    // This tells us the block's indent.
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
        TextBlockLocation::new(orig_input.node(), orig_input.line(), orig_input.location_offset()),
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

/// `PaddingPolicies` are used to add newlines between block objects written
/// to a `TextBlockBuf` or `TextBlockWriter`.
#[derive(Copy, Clone, Default)]
pub enum PaddingPolicy {
    /// Always write the same padding before every block written, except the first block.
    Fixed(usize),
    /// Retains the padding from the block written, falling back to the fixed
    /// amount if it does not write any.
    Retain(usize),
    /// Use the padding from the last block written.
    Last,
    /// Retains the padding from the block written, falling back to the padding from the last block
    /// written, or 0 if this is the first block.
    #[default]
    RetainOrLast,
}

/// A writeable `TextBlock`.
pub struct TextBlockBuf {
    text: String,
    //parent: Option<Box<RefCell<&'h TextBlockBuf<'h>>>>,
    //location: TextBlockLocation<'h>,
    include_elided: bool,
    /// The number of spaces to indent child blocks with
    child_indent_size: usize,
    /// The style of newlines. Either "\n" or "\r\n".
    newline_style: &'static str,
    /// Padding between blocks.
    padding_policy: PaddingPolicy,
    last_block_padding: Option<usize>,
}

impl TextBlockBuf {
    pub fn new() -> Self {
        TextBlockBuf {
            text: String::new(),
            //location: TextBlockLocation::new(node, 1, 0),
            include_elided: false,
            child_indent_size: 2,
            newline_style: "\n",
            padding_policy: PaddingPolicy::default(),
            last_block_padding: None,
        }
    }

    pub fn with_obj<Obj: BlockObject>(obj: &Obj, config: Option<&Configuration>) -> Self {
        let mut buf = TextBlockBuf::new();
        buf.write(obj, config);
        buf
    }

    pub fn include_elided(&self) -> bool {
        self.include_elided
    }

    pub fn set_include_elided(&mut self, include: bool) {
        self.include_elided = include;
    }

    pub fn set_padding_policy(&mut self, padding_policy: PaddingPolicy) {
        self.padding_policy = padding_policy;
    }

    fn new_child(&self) -> Self {
        TextBlockBuf {
            text: String::new(),
            //location: self.location + self.text.as_str(),
            include_elided: self.include_elided,
            child_indent_size: self.child_indent_size,
            newline_style: self.newline_style,
            padding_policy: self.padding_policy,
            last_block_padding: None,
        }
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    pub fn clear(&mut self) {
        self.text.clear();
    }

    pub fn as_text_block(&self) -> TextBlock<'_> {
        TextBlock::from(self.text.as_str())
        //TextBlock { text: self.text.as_str(), parent: None, location: TextBlock }
    }

    /// Appends the block to this one without any indentation.
    pub fn write<Obj: BlockObject + ?Sized>(&mut self, obj: &Obj, config: Option<&Configuration>) {
        let pad_start = self.text.len();
        obj.write(self, config);
        let (padding, _pad_len) = Self::get_padding(&self.text[pad_start..]);

        let get_last = || match self.last_block_padding {
            Some(last_block_padding) => last_block_padding.max(1),
            None => 0,
        };
        match self.padding_policy {
            PaddingPolicy::Fixed(size) => {
                if !self.text.is_empty() {
                    self.write_padding(size, pad_start);
                }
            }
            PaddingPolicy::Retain(size) => {
                if padding == 0 && self.last_block_padding.is_some() {
                    self.write_padding(size, pad_start);
                }
            }
            PaddingPolicy::Last => self.write_padding(get_last(), pad_start),
            PaddingPolicy::RetainOrLast => {
                if padding == 0 {
                    self.write_padding(get_last(), pad_start);
                }
            }
        }
        self.last_block_padding = Some(padding);
    }

    /// Appends a child block, indented with the configured indent.
    ///
    /// If `newline` is set, this ensures the child is always written on an indented newline.
    pub fn write_child<Obj: BlockObject>(
        &mut self,
        child: &Obj,
        config: Option<&Configuration>,
        newline: bool,
    ) {
        /*
        match child.block() {
            Some(block) => {
                let indent_to_add = self.child_indent_size as isize - block.indented_amount() as isize;
                if newline
                    && (!block.text().trim_matches(char::is_space).starts_with('\n')
                        || !block.text().trim_matches(char::is_whitespace).starts_with("\r\n"))
                {
                    self.text.push_str(self.newline_style);
                    self.write_indent();
                }
                for line in block.text().lines() {
                    if indent_to_add >= 0 {
                        for _ in 0..indent_to_add {
                            self.text.push(' ');
                        }
                        self.text.push_str(self.newline_style);
                        self.text.push_str(line);
                    } else {
                        let mut line_text = line.to_string();
                        line_text.outdent_exact(-indent_to_add as u16);
                    }
                }
            }
            None => {*/
        let mut child_buf = self.new_child();
        if newline {
            //child_buf.text.push_str(self.newline_style);
            child_buf.write_indent();
        }
        child.write(&mut child_buf, config);

        // The base indent of the child if it were appended to this buffer.
        let lws = &child_buf.text[0..child_buf.text.len() - child_buf.text.trim_start().len()];
        let lws_has_newline = lws.contains("\n");
        let child_base_indent = if newline || lws_has_newline {
            0
        } else {
            self.text.lines().last().map(|ll| ll.chars().count()).unwrap_or(0)
                + lws.indented_amount() as usize
        };
        child_buf.set_indent(child_base_indent + self.child_indent_size, child_base_indent == 0);

        if newline && !lws_has_newline {
            child_buf.text.insert_str(0, self.newline_style);
        }
        write!(self, "{}", child_buf.text).unwrap()
        //}
        //}
    }

    fn set_indent(&mut self, indent: usize, include_first_line: bool) {
        let mut indent_str = SS::new();
        for _ in 0..indent {
            indent_str.push(' ');
        }

        let mut text_len = self.text.len();
        let mut i = 0;
        while i < text_len {
            if (i == 0 && include_first_line) || self.text.as_bytes()[i] == b'\n' {
                let ws_start = if i == 0 { 0 } else { i + 1 };
                let ws = self.text[ws_start..].leading_whitespace();
                let ws_len = ws.len();
                self.text.replace_range(ws_start..ws_start + ws.len(), &indent_str);
                text_len = text_len - ws_len + indent_str.len();
            }
            i += 1;
        }
    }

    /// Gets the number of newlines at the start of the block.
    ///
    /// Blocks should have a padding of at least 1 if they aren't the first block.
    /// Returns a tuple of the (padding, num_padding_chars).
    fn get_padding(text: &str) -> (usize, usize) {
        let mut pos = 0;
        let mut padding = 0;
        while text.as_bytes()[pos] == b'\n'
            || text.as_bytes()[pos] == b'\r'
            || text.as_bytes()[pos] == b' '
            || text.as_bytes()[pos] == b'\t'
        {
            if text.as_bytes()[pos] == b'\n' {
                padding += 1
            }
            pos += 1;
        }
        (padding, pos)
    }

    fn write_padding(&mut self, padding: usize, pos: usize) {
        let (_, padding_chars) = Self::get_padding(&self.text[pos..]);

        self.text.replace_range(pos..pos + padding_chars, "");
        for _ in 0..padding {
            self.text.insert_str(pos, self.newline_style);
        }
    }

    fn write_indent(&mut self) {
        for _ in 0..self.child_indent_size {
            self.text.push(' ');
        }
    }
}

impl fmt::Display for TextBlockBuf {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.text)
    }
}

impl Write for TextBlockBuf {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.text.push_str(s);
        Ok(())

        /*
        if self.text.is_empty() || self.text.ends_with('\n') {
            self.write_indent();
        }
        if s.trim().is_empty() {
            self.text.push_str(s);
            return Ok(());
        }

        // Get whitespace prefix and suffix.
        let prefix = &s[..s.len() - s.trim_start().len()];
        let suffix = if s.len() > prefix.len() { &s[s.trim_end().len()..] } else { "" };

        // Write indent between newlines
        self.text.push_str(prefix);
        for line_or_nl in s.trim().split("\n").intersperse("\n") {
            self.text.push_str(line_or_nl);
            if line_or_nl == "\n" {
                self.write_indent();
            }
        }
        self.text.push_str(suffix);
        Ok(())*/
    }
}

/*
impl<Obj: BlockObject> From<&Obj> for TextBlockBuf {
    fn from(value: &Obj) -> Self {
        let mut buf = TextBlockBuf::new();
        buf.write(value);
        buf
    }
}*/

pub struct TextBlockWriter<W: io::Write> {
    writer: W,
    padding_policy: PaddingPolicy,
    buf: TextBlockBuf,
    written: usize,
}

impl<W: io::Write> TextBlockWriter<W> {
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            padding_policy: PaddingPolicy::default(),
            buf: TextBlockBuf::new(),
            written: 0,
        }
    }

    pub fn set_padding_policy(&mut self, new_padding: PaddingPolicy) {
        self.padding_policy = new_padding;
    }

    /// Writes the block object to the stream.
    pub fn write<Obj: BlockObject + ?Sized>(
        &mut self,
        obj: &Obj,
        config: Option<&Configuration>,
    ) -> io::Result<()> {
        self.buf.clear();

        if let PaddingPolicy::Fixed(size) = self.padding_policy {
            self.buf.set_padding_policy(PaddingPolicy::Fixed(0));
            if self.written > 0 {
                for _ in 0..size {
                    write!(self.writer, "{}", self.buf.newline_style)?;
                    self.written += self.buf.newline_style.len();
                }
            }
        } else {
            self.buf.set_padding_policy(self.padding_policy);
        }
        self.buf.write(obj, config);
        let res = write!(self.writer, "{}", self.buf.text());
        self.written += self.buf.text().len();
        res
    }

    /*
    pub fn write_block(&mut self, block: &TextBlock) -> io::Result<()> {
        if self.written > 0 {
            for _ in 0..self.padding {
                writeln!(self.writer)?;
                self.written += 1;
            }
        }
        writeln!(self.writer, "{}", block.text)?;
        self.written += block.text.len() + 1;
        Ok(())
    }*/
}

/// Block objects are those that relate to a `TextBlock`.
pub trait BlockObject {
    /// Write the object to the `buf`.
    ///
    /// The `config` may be passed if known and should reflect the configuration
    /// state of the object when it was parsed. It is not always possible to be `Some`.
    /// For instance, `Posting` objects do not store their Configuration.
    fn write(&self, buf: &mut TextBlockBuf, config: Option<&Configuration>);
}

impl BlockObject for TextBlock<'_> {
    fn write(&self, buf: &mut TextBlockBuf, _config: Option<&Configuration>) {
        write!(buf, "{}", self.text).unwrap();
    }
}

impl BlockObject for str {
    fn write(&self, buf: &mut TextBlockBuf, _config: Option<&Configuration>) {
        write!(buf, "{}", self).unwrap();
    }
}

#[cfg(test)]
mod test {
    use crate::parsing::text_block::{PaddingPolicy, TextBlockBuf, TextBlockWriter};
    use std::fmt::Write;

    #[test]
    fn test_fixed_padding() {
        let mut buf = Vec::new();
        let mut writer = TextBlockWriter::new(&mut buf);
        writer.set_padding_policy(PaddingPolicy::Fixed(2));
        writer.write("hello block", None).unwrap();
        writer.write("goodbye block", None).unwrap();
        // Two newlines between the two is fixed
        assert_eq!(str::from_utf8(&buf).unwrap(), "hello block\n\ngoodbye block");
    }

    #[test]
    fn test_set_indent() {
        // Indent can be added to unindented block
        let mut buf = TextBlockBuf::new();
        write!(&mut buf, "hello block").unwrap();
        buf.set_indent(2, true);
        assert_eq!(buf.text, "  hello block");

        // Indent can be removed from indented block
        let mut buf = TextBlockBuf::new();
        write!(&mut buf, "  hello block").unwrap();
        buf.set_indent(0, true);
        assert_eq!(buf.text, "hello block");
    }
}
