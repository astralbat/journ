/*
 * Copyright (c) 2023. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */

use crate::metadata::Metadata;

/*
pub type TextReaderResult<'h, I, O> = Result<O, NomErr<JParseError<'h, I>>>;

#[derive(Copy, Clone)]
pub struct TextReader<'h, I: TextInput<'h>> {
    pub(super) cursor: I,
    phantom: std::marker::PhantomData<&'h ()>,
}
impl<'h, I: TextInput<'h>> TextReader<'h, I> {
    pub fn new(text: I) -> Self {
        Self { cursor: text, phantom: std::marker::PhantomData }
    }

    pub(super) fn cursor(&self) -> &I {
        &self.cursor
    }

    /*
    pub fn extra(&self) -> &X {
        &self.cursor.extra
    }*/

    pub fn read<O, F: Parser<I, O, JParseError<'h, I>>>(&mut self, mut parser: F) -> TextReaderResult<'h, I, O> {
        let (rem, res) = parser.parse(self.cursor.clone())?;
        self.cursor = rem;
        Ok(res)
    }

    pub fn read_rtrim<O, F: Parser<I, O, JParseError<'h, I>>>(&mut self, parser: F) -> TextReaderResult<'h, I, O> {
        let (rem, res) = consumed_rtrim(parser).parse(self.cursor.clone())?;
        self.cursor = rem;
        Ok(res.1)
    }

    /// Reads with the parser but the reading must not include a newline.
    ///
    /// # Panics
    /// If the reading would cause the cursor to read the end of the line.
    ///
    /// # Examples
    /// ```
    /// # use nom::bytes::complete::tag;
    /// # use journ_core::parse_block1;
    /// # use journ_core::parsing::util::{map_str, str_err};
    /// assert_eq!(parse_block1!("abc\ndef", |b| b.read_on_line(map_str(tag("a")))), Ok("a"));
    /// assert_eq!(parse_block1!("abc\ndef", |b| b.read_on_line(map_str(tag("abc")))), Ok("abc"));
    /// ```
    /// This will panic:
    /// ```should_panic
    /// # use nom::bytes::complete::tag;
    /// # use journ_core::parsing::util::{map_str, str_err};
    /// # use journ_core::parse_block1;
    /// parse_block1!("abc\ndef", |mut b| b.read_on_line(map_str(tag("abc\n"))));
    /// ```
    pub fn read_on_line<O, F>(&mut self, parser: F) -> TextReaderResult<'h, I, O>
    where
        F: Parser<I, O, JParseError<'h, I>>,
    {
        let (rem, (cons, res)) = consumed(parser)(self.cursor.clone())?;
        if rem.line() != cons.line() {
            panic!(
                "Must not read a newline char: '{}{}{}'",
                cons.line().map(|l| format!("{}:", l)).unwrap_or_else(|| format!("")),
                cons.column().map(|c| format!("{}:", c)).unwrap_or_else(|| format!("")),
                cons.text()
            );
        }
        self.cursor = rem;
        Ok(res)
    }

    /// Runs the supplied `parser` repeatedly, accumulating the result with the accumulator (created by the
    /// supplied `init` function) in the supplied `accumulator` function.
    ///
    /// The parser is run repeatedly until if fails, ensuring that it is ran at least once.
    pub fn repeat1<O, F: Parser<I, O, JParseError<'h, I>>, H, G, R>(
        &mut self,
        mut parser: F,
        mut init: H,
        mut accumulator: G,
    ) -> TextReaderResult<'h, I, R>
    where
        H: FnMut() -> R,
        G: FnMut(R, O) -> R,
    {
        let mut counter = 0;
        let mut accumulation = init();
        let err = loop {
            match parser.parse(self.cursor.clone()) {
                Ok((rem, res)) => {
                    self.cursor = rem;
                    accumulation = accumulator(accumulation, res);
                    counter += 1;
                }
                Err(e) => break e,
            }
        };
        match counter {
            0 => Err(err),
            _ => Ok(accumulation),
        }
    }

    pub fn remainder(&self) -> I {
        self.cursor.clone()
    }
}

impl<'h, I: TextInput<'h>> Deref for TextReader<'h, I> {
    type Target = I;

    fn deref(&self) -> &Self::Target {
        &self.cursor
    }
}

impl<'h, I: TextInput<'h>> DerefMut for TextReader<'h, I> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.cursor
    }
}

 */

pub enum BlockObject<'h> {
    /// Comment position, followed by comment block
    Comments(&'h str),
    Metadata(Metadata<'h>),
    Ordinary,
}

/*
pub type BlockErr<'h, 's, 'e, 'p> = nom::Err<JParseError<'h, TextBlockInput<'h, &'p JournalFileParseNode<'h, 's, 'e>>>>;
//pub type RawBlock = TextBlock<'static>;
type BlockParseResult<'h, 's, 'e, 'p, T> = ParseResult<'h, TextFileInput<'h, 's, 'e, 'p>, T>;

#[derive(Copy, Clone)]
pub struct TextBlockReader<'h, X = ()>
where
    LocatedSpan<&'h str, X>: TextInput<'h>,
{
    pub(super) block: &'h TextBlock<'h>,
    reader: TextReader<'h, LocatedSpan<&'h str, X>>,
    //cursor: LocatedSpan<&'h str, X>,
}

/*
impl<'h, X: Copy> Copy for TextBlockReader<'h, X>
where
    LocatedSpan<&'h str, X>: Copy,
    X: Copy,
{
}*/

impl<'h> TextBlockReader<'h, ()> {
    pub fn new(block: &'h TextBlock<'h>) -> Self {
        Self { reader: TextReader::new(block.skip_leading_blank_lines()), block }
    }
}

impl<'h, X> TextBlockReader<'h, X>
where
    X: Copy,
    LocatedSpan<&'h str, X>: TextInput<'h>,
{
    pub fn new_extra(block: &'h TextBlock<'h>, extra: X) -> Self {
        Self { reader: TextReader::new(block.skip_leading_blank_lines().map_extra(|_| extra)), block }
    }

    /// Gets the block we're reading.
    pub fn block(&self) -> &'h TextBlock<'h> {
        self.block
    }

    pub fn block_input(&self) -> LocatedSpan<&'h str, X> {
        self.block.located_text().map_extra(|_| self.reader.cursor.extra)
    }

    /// Gets whether the block is ordinary.
    /// See [`TextBlock::is_ordinary()`].
    pub fn is_ordinary_block(&self) -> bool {
        self.block.is_ordinary()
    }

    pub fn extra(&self) -> X {
        self.reader.cursor().extra
    }

    pub fn with_cursor(&self, cursor: LocatedSpan<&'h str, X>) -> Self {
        Self { reader: TextReader::new(cursor), block: self.block }
    }

    pub fn location(&self) -> TextBlockLocation<'h> {
        TextBlockLocation::new(
            self.block.filename(),
            self.reader.cursor.location_line(),
            self.reader.cursor.naive_get_utf8_column(),
        )
    }

    /// Gets the line the cursor is currently positioned on.
    /// If the beginning of the line has already been read, this returned result will be as though
    /// it wasn't.
    pub fn get_line_beginning(&self) -> LocatedSpan<&'h str, X> {
        // Here we transmute the lifetime. This is safe since the lifetime will be 'a, but this
        // can't be inferred by the compiler since lifetime information is missing in the impl.
        let first_line_bytes: &[u8] = unsafe { mem::transmute(self.reader.cursor.get_line_beginning()) };
        // Safe since LocatedSpan is just returning a byte representation of the string which is being converted back.
        LocatedSpan::new_extra(std::str::from_utf8(first_line_bytes).unwrap(), self.reader.cursor.extra)
    }

    pub fn read<O, F: Parser<LocatedSpan<&'h str, X>, O, JParseError<'h, LocatedSpan<&'h str, X>>>>(
        &mut self,
        parser: F,
    ) -> TextBlockResult<'h, O, X> {
        self.reader.read(parser)
    }

    pub fn read_rtrim<O, F: Parser<LocatedSpan<&'h str, X>, O, JParseError<'h, LocatedSpan<&'h str, X>>>>(
        &mut self,
        parser: F,
    ) -> TextBlockResult<'h, O, X> {
        self.reader.read_rtrim(parser)
    }

    pub fn read_or<O, F: Parser<LocatedSpan<&'h str, X>, O, JParseError<'h, LocatedSpan<&'h str, X>>>>(
        &mut self,
        parser: F,
        err: &'static str,
    ) -> TextBlockResult<'h, O, X> {
        let res = self.reader.read(parser).map_err(|_| {
            let rem: LocatedSpan<&'h str, X> = self.read_remainder();
            self.raise_fatal(Some(rem), err, None)
        })?;
        Ok(res)
    }

    pub fn remainder(&self) -> LocatedSpan<&'h str, X> {
        self.reader.cursor
        //self.reader.cursor.slice(..self.reader.cursor.text().len() - self.block.surplus_text().len()).rtrim()
    }

    /// Reads the remaining text until the end of the block is reached.
    /// This repeats the logic in TextBlock#surplus_text, but starts from the cursor position
    /// instead of the start of the block.
    pub(crate) fn read_remainder(&mut self) -> LocatedSpan<&'h str, X> {
        let (rem, rest) = rest::<_, ()>(self.reader.cursor).unwrap();
        self.reader.cursor = rem;
        rest
        /*
        let curr_pos = self.reader.cursor;
        match self.block.end() {
            Some(end) => self.reader.cursor = self.reader.cursor.slice(self.reader.cursor.len() - end.len()..),
            None => {
                let min_indent = self.block.min_child_indent() as usize;

                // We haven't yet read the first line of the block. A block is at least till the end of the line.
                if self.reader.cursor.location_line() == self.block.first_content_line() {
                    self.read(recognize_rtrim(rest_line0)).unwrap();
                }

                // Read all subsequent lines that are indented at least one more than the first line.
                self.read(recognize_rtrim(repeat0(preceded(pair(blank_lines0, indented(min_indent)), rest_line1))))
                    .unwrap();

                self.block.set_end(self.reader.cursor.text());
            }
        }

        curr_pos.slice(..curr_pos.text().len() - self.reader.cursor.len())*/
    }

    pub fn read_remainder_or(&mut self, err: &'static str) -> TextBlockResult<'h, LocatedSpan<&'h str, X>, X> {
        match self.read_remainder() {
            v if !v.is_empty() => Ok(v),
            v => Err(self.raise_fatal(Some(v), err, None)),
        }
    }

    pub fn read_remainder_or_err(&mut self) -> TextBlockResult<'h, LocatedSpan<&'h str, X>, X> {
        self.read_remainder_or(E_VALUE)
    }

    pub fn read_word(&mut self) -> TextBlockResult<'h, LocatedSpan<&'h str, X>, X> {
        self.read_on_line(preceded(space0, recognize_rtrim(alt((util::double_quoted, word))))).map_err(|_| {
            let line_rem = self.clone().read(rest_line0).unwrap();
            self.raise_fatal(Some(line_rem), E_VALUE, None)
        })
    }

    /// Reads with the parser but the reading must not include a newline.
    ///
    /// # Panics
    /// If the reading would cause the cursor to read the end of the line.
    pub fn read_on_line<O, F>(&mut self, parser: F) -> TextBlockResult<'h, O, X>
    where
        F: Parser<LocatedSpan<&'h str, X>, O, TextBlockErr<'h, X>>,
    {
        let (cons, res) = self.reader.read_on_line(consumed(parser))?;

        // Do not allow this as it could be possible that the reading has gone beyond the end of the
        // block. And this would set the cursor to be inconsistent.
        if cons.text().ends_with(' ') || cons.text().ends_with('\t') {
            panic!(
                "Reading must not terminate with space or tab: '{}:{}:{}:{}'",
                self.block.filename().unwrap_or(""),
                cons.location_line(),
                cons.naive_get_utf8_column(),
                cons.text()
            );
        }
        Ok(res)
    }

    pub fn iterate_line<O, F, H>(&mut self, parser: F, mut handler: H) -> TextBlockResult<'h, (), X>
    where
        F: FnMut(LocatedSpan<&'h str, X>) -> IParseResult<'h, LocatedSpan<&'h str, X>, O>,
        H: FnMut(O) -> TextBlockResult<'h, (), X>,
        X: JournalNodeAccess<'h>,
    {
        let mut iterator = iterator(self.reader.cursor, parser);
        for next in &mut iterator {
            handler(next)?;
        }
        let (rem, _) = iterator.finish()?;
        self.reader.cursor = rem;
        Ok(())
    }

    pub fn read_metadata(&mut self) -> TextBlockResult<'h, Metadata<'h>, X> {
        self.read(entry::metadata)
    }

    /// Reads a new block from the current position. This will always succeed if there
    /// is any text left in the block.
    pub fn read_block(&mut self) -> TextBlockResult<'h, TextBlock<'h>, X> {
        // End already set to our position.
        if self.cursor.text().is_empty() {
            return Err(self.raise_fatal(None, "Value expected", None));
        }

        let child_text = TextBlock::read_text_from(self.cursor.map_extra(|_| ()), self.block.min_child_indent());
        self.reader.cursor = self.reader.cursor.slice(child_text.len()..);
        let block = TextBlock::new_child(self.block.node(), child_text, self.block);
        Ok(block)
    }

    /*
    /// Starts reading child blocks from the current cursor position. The first block will behave
    /// much like [Self::read_block].
    /// Subsequent blocks will be the continuation of the natural children of this block.
    pub fn read_children(&mut self) -> impl Iterator<Item = TextBlock<'h>> + '_ {
        // Set the indent for further blocks
        let child_min_indent = self.block.min_child_indent();

        iter::from_fn(move || {
            if self.cursor.text().is_empty() {
                return None;
            }
            let child_text = TextBlock::read_text_from(self.cursor.map_extra(|_| ()), child_min_indent);

            let block = TextBlock::new_child(self.block.node(), child_text, self.block);
            if block.indented_amount() >= child_min_indent {
                self.reader.cursor = self.reader.cursor.slice(child_text.len()..);
                Some(block)
            } else {
                None
            }
        })
    }

    /// The same as [Self::read_children] but each child is wrapped with a reader, retaining
    /// the extra data.
    pub fn read_children_with_reader(&mut self, allocator: &'h HerdAllocator<'h>) -> impl Iterator<Item = Self> + '_ {
        let extra = self.reader.cursor.extra;
        self.read_children().map(move |b| Self::new_extra(allocator.alloc(b), extra))
    }*/

    pub fn raise(
        &self,
        span: Option<LocatedSpan<&'h str, X>>,
        error: &'h str,
        extra: Option<&'h str>,
    ) -> TextBlockErr<'h, X> {
        let mut err = JParseError::new(
            span.unwrap_or(self.block.located_text().map_extra(|_| self.reader.cursor.extra).trimmed()),
            Some(error),
        );
        err.set_text_block(self.block);
        if let Some(extra) = extra {
            err.set_higher_context(extra)
        }
        err
    }

    /// Raises an error with the block to indicate a problem that can be recovered from by trying
    /// another path.
    pub fn raise_error(
        &self,
        span: Option<LocatedSpan<&'h str, X>>,
        error: &'h str,
        extra: Option<&'h str>,
    ) -> NomErr<TextBlockErr<'h, X>> {
        NomErr::Error(self.raise(span, error, extra))
    }

    /// Raises an error with the block to indicate an unrecoverable problem. The error is considered
    /// fatal only to the processing of the current block, not the program.
    pub fn raise_fatal(
        &self,
        span: Option<LocatedSpan<&'h str, X>>,
        error: &'h str,
        extra: Option<&'h str>,
    ) -> NomErr<TextBlockErr<'h, X>> {
        NomErr::Failure(self.raise(span, error, extra))
    }

    pub fn set_as_parent(&self, err: NomErr<TextBlockErr<'h, X>>) -> NomErr<TextBlockErr<'h, X>> {
        match err {
            NomErr::Error(mut e) => {
                e.set_text_block(self.block);
                NomErr::Error(e)
            }
            NomErr::Failure(mut f) => {
                f.set_text_block(self.block);
                NomErr::Failure(f)
            }
            other => other,
        }
    }
}

impl<'h, 's, 'e, 'p> TextBlockReader<'h, &'p JournalFileParseNode<'h, 's, 'e>>
where
    'h: 'e,
    'e: 's,
{
    pub fn parse_node(&self) -> &'p JournalFileParseNode<'h, 's, 'e> {
        self.extra()
    }

    pub fn config(&self) -> Ref<Configuration<'h>> {
        self.extra().config().borrow()
    }

    pub fn config_mut(&self) -> RefMut<Configuration<'h>> {
        self.extra().config().borrow_mut()
    }

    /// Reads the remainder of the line or lines if this is a multiline value and creates a text block for subsequent parsing.
    /// The resulting `TextBlock` will have its parent set to this block.
    pub fn read_stream(
        &mut self,
    ) -> ParseResult<'h, LocatedSpan<&'h str, &'p JournalFileParseNode<'h, 's, 'e>>, (TextBlock<'h>, Option<&'h Path>)>
    {
        let block_rem = self.read_remainder_or_err()?;
        if block_rem.text().lines().count() == 1 {
            let file_path = {
                let parent_stream = self.block.file();
                let child_buf = PathBuf::from_str(block_rem.text().trim()).unwrap();
                let file_path = if child_buf.is_absolute() {
                    child_buf
                } else if let Some(parent_path) = parent_stream {
                    parent_path.with_file_name(child_buf)
                } else {
                    child_buf
                };
                self.extra.allocator().alloc(file_path).as_path()
            };
            let mut tb = TextBlock::from_file(file_path, self.extra.allocator())
                .map_err(|e| self.raise_fatal_from_jerror(Some(block_rem), e))?;
            tb.set_parent(Some(self.block));
            Ok((tb, Some(file_path)))
        } else {
            // Node will be replaced later.
            let mut tb = TextBlock::new_child(self.block.node(), block_rem.map_extra(|_| ()), self.block);
            Ok((tb, None))
        }
    }

    /// Raises a fatal error in as far as the current block goes, converting from a `JournError`.
    pub fn raise_fatal_from_jerror(
        &self,
        span: Option<LocatedSpan<&'h str, &'p JournalFileParseNode<'h, 's, 'e>>>,
        e: JournError<'h>,
    ) -> NomErr<TextBlockErr<'h, &'p JournalFileParseNode<'h, 's, 'e>>> {
        self.raise_fatal(
            span,
            match e {
                JournError::Message(s) => s,
                JournError::Custom(s) => self.extra.allocator().alloc(s),
                JournError::Multiple(errs) => {
                    let mut s = String::new();
                    for err in errs {
                        s.push_str(&format!("{}\n", err));
                    }
                    s.pop();
                    self.extra.allocator().alloc(s)
                }
                JournError::Code(n) => self.extra.allocator().alloc(format!("Error code {}", n)),
                JournError::Chain(cause, error) => {
                    let mut s = String::new();
                    s.push_str(&format!("{}: ", error));
                    s.push_str(&format!("{}", cause));
                    self.extra.allocator().alloc(s)
                }
            },
            None,
        )
    }
}

impl<'h, X> Deref for TextBlockReader<'h, X>
where
    LocatedSpan<&'h str, X>: TextInput<'h>,
{
    type Target = TextReader<'h, LocatedSpan<&'h str, X>>;

    fn deref(&self) -> &Self::Target {
        &self.reader
    }
}

impl<'h, X> DerefMut for TextBlockReader<'h, X>
where
    LocatedSpan<&'h str, X>: TextInput<'h>,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.reader
    }
}

/*
#[derive(Copy, Clone)]
pub struct NodeReader<'h, 's, 'e, 'p>
where
    'h: 'e,
{
    block_reader: TextBlockReader<'h, &'p JournalFileParseNode<'h, 's, 'e>>,
}
impl<'h, 's, 'e, 'p> NodeReader<'h, 's, 'e, 'p>
where
    'h: 'e,
{
    pub fn new(block: &'h TextBlock<'h>, node: &'p JournalFileParseNode<'h, 's, 'e>) -> Self {
        Self { block_reader: TextBlockReader::new_extra(block, node) }
    }

    fn read_child<F, T>(
        &mut self,
        child: &mut NodeReader<'h, 's, 'e, 'p>,
        handler: F,
    ) -> Option<BlockParseResult<'h, 's, 'e, 'p, T>>
    where
        F: FnOnce(&mut Self) -> BlockParseResult<'h, 's, 'e, 'p, T>,
    {
        let (res, end) = {
            // Run the handler and set the parent_block on any error that occurs
            let res = handler(child).map_err(|err| self.set_as_parent(err));
            child.read_remainder();
            (res, child.block_reader.reader.cursor)
        };
        self.block_reader.reader.cursor = end;
        Some(res)
    }

    /// Reads the next block object and calls the `handler` to process it.
    /// # Examples
    /// ```
    /// # use journ_core::{parse_block0, match_map};
    /// # use journ_core::parsing::util::{str_err};
    /// # use journ_core::parsing::reader::BlockObject::Comments;
    ///
    /// assert_eq!(parse_block0!(" abc\n", |mut b| b.read_child_object(|handler| Ok(()))), Ok(()));
    /// assert_eq!(parse_block0!("; abc\n; def\n", |mut b| b.read_child_object(|handler| {
    ///     Ok(match_map!(handler, Comments(_, s) => s).unwrap())
    /// })), Ok("; abc\n; def\n"));
    /// ```
    pub fn read_child_object<'a, F, T>(&'a mut self, handler: F) -> Option<BlockParseResult<'h, 's, 'e, 'p, T>>
    where
        F: for<'o> FnOnce(BlockObject<'h, 's, 'e, 'o, 'p>) -> BlockParseResult<'h, 's, 'e, 'p, T>,
        'h: 'e,
    {
        if self.block_reader.reader.cursor.text().is_empty() {
            return None;
        }

        let child_min_indent = self.block_reader.block.min_child_indent();
        let next_child = TextBlock::new_child(
            self.block.node(),
            self.block_reader.reader.cursor.map_extra(|_| ()),
            self.block_reader.block,
        );

        // Next block isn't a child of this one.
        if next_child.indented_amount() < child_min_indent || self.block_reader.reader.cursor.trim_start().is_empty() {
            return None;
        }

        let next_child = self.parse_node().allocator().alloc(next_child);
        let mut next_child_reader = NodeReader::new(next_child, self.parse_node());

        // Read comment lines, greedily consuming any preceding/trailing newlines.
        if let Ok(comments) = self.block_reader.read_rtrim(preceded(
            blank_lines0,
            verify(comment, |c: &TextFileInput| c.indented_amount() as u16 >= child_min_indent),
        )) {
            return Some(handler(BlockObject::Comments(&next_child_reader, comments.text())));
        }

        if let Ok(mut metadata) = self.block_reader.read_rtrim(verify(entry::metadata, |m: &Metadata| {
            m.pretext().lines().last().unwrap().indented_amount() as u16 >= child_min_indent
        })) {
            metadata.set_raw_block(next_child);
            return Some(handler(BlockObject::Metadata(&next_child_reader, metadata)));
        }

        self.read_child(&mut next_child_reader, |tb| handler(BlockObject::Block(tb)))
    }

    pub fn read_all_children<'a, F>(&'a mut self, mut handler: F) -> BlockParseResult<'h, 's, 'e, 'p, ()>
    where
        F: for<'o> FnMut(BlockObject<'h, 's, 'e, 'o, 'p>) -> BlockParseResult<'h, 's, 'e, 'p, ()>,
    {
        while let Some(res) = self.read_child_object(&mut handler) {
            res?
        }
        Ok(())
    }
}

impl<'h, 's, 'e, 'p> Deref for NodeReader<'h, 's, 'e, 'p> {
    type Target = TextBlockReader<'h, &'p JournalFileParseNode<'h, 's, 'e>>;

    fn deref(&self) -> &Self::Target {
        &self.block_reader
    }
}
impl<'h, 's, 'e, 'p> DerefMut for NodeReader<'h, 's, 'e, 'p> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.block_reader
    }
}*/

/// Parses block parameters
#[macro_export]
macro_rules! match_param2 {
    ($text_block_reader:ident, $($name:expr => $handler:expr),+) => {{
        use nom::bytes::complete::{tag_no_case};
        use nom::character::complete::{space0};
        use nom::sequence::{preceded};
        use nom::branch::alt;
        use $crate::parsing::util::{word, double_quoted};
        use $crate::parsing::directive::E_UNKNOWN_PARAMETER;

        'handler_match: {
            $(
                if $text_block_reader.read_on_line(preceded(space0, tag_no_case($name))).is_ok() {
                    break 'handler_match ($handler);
                }
            )+
            if $text_block_reader.remainder().is_empty() {
                return Err($text_block_reader.raise_fatal(None, "", Some("Missing parameter")));
            } else {
                // Don't consume the reader
                let unknown = preceded(space0, alt((word, double_quoted)))($text_block_reader.remainder()).unwrap().1;
                return Err($text_block_reader.raise_fatal(Some(unknown), unknown.text(), Some(E_UNKNOWN_PARAMETER)));
            }
        }
    }}
}

 */

/// Parses a string block interpreted as the root.
#[macro_export]
macro_rules! parse_block0 {
    ($text:expr, $func:expr) => {{
        std::thread::scope(|scope| {
            use $crate::config;
            use $crate::parsing::text_block::{NodeReader, TextBlock};
            use $crate::parsing::text_input::TextInput;
            use $crate::sin;

            // Function to map lifetimes of input func to TextBlockInput's.
            fn higher_func<'h, 's, 'e, 'p, F, O>(
                input: NodeReader<'h, 's, 'e, 'p>,
                mut func: F,
            ) -> O
            where
                F: FnMut(NodeReader<'h, 's, 'e, 'p>) -> O,
            {
                func(input)
            }

            let s: &str = $text;
            let config = config!();
            let parse_node = sin!(s, scope, config.clone(), $crate::journal_file::FileKind::Entry);
            let text_block_root: &TextBlock<'_> =
                parse_node.allocator().alloc(parse_node.file().stream().into());

            let tbr = text_block_root.node_reader(&parse_node);

            let res = higher_func(tbr, $func);
            res.expect("At least 1 child expected").map_err($crate::parsing::util::str_err)
        })
    }};
}

/// Parses a block 1 level down from the supplied string, interpreted as the root.
/// The caller should supply a function that takes a `&mut TextBlockReader` as its input.
/// Returns a `Result<T, &str>`.
#[macro_export]
macro_rules! parse_block1 {
    ($text:expr, $func:expr) => {{
        std::thread::scope(|scope| {
            use $crate::config;
            use $crate::parse_node;
            use $crate::parsing::text_block::{NodeReader, TextBlock};

            // Function to map lifetimes of input func to TextBlockInput's.
            fn higher_func<'h, 's, 'e, 'p, F, O>(
                input: &mut NodeReader<'h, 's, 'e, 'p>,
                mut func: F,
            ) -> O
            where
                F: FnMut(&mut NodeReader<'h, 's, 'e, 'p>) -> O,
            {
                func(input)
            }

            let s: &str = $text;
            let config = config!();
            let parse_node =
                parse_node!(s, scope, config.clone(), $crate::journal_node::JournalNodeKind::Entry);
            // Create a root and a child so that we are one level down when starting.
            let text_block_root: &TextBlock<'_> = parse_node.node().block();
            let res =
                text_block_root.node_reader(&parse_node).read_child(|tbr| higher_func(tbr, $func));
            res.expect("At least 1 child expected").map_err($crate::parsing::util::str_err)
        })
    }};
}
