/*
 * Copyright (c) 2019-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::arguments::Arguments;
use crate::parsing::text_block::TextBlock;
use crate::parsing::util::interim_space;
use crate::reporting::table::row::Row;
use crate::reporting::table::Table;
use crate::reporting::table::{Alignment, Cell};
use crate::reporting::term_style::{Colour, Style};
use std::any::Any;
use std::error::Error;
use std::ops::{DerefMut, Range};
use std::{fmt, mem};

pub type JournResult<T> = Result<T, JournError>;

#[derive(Debug)]
pub struct JournError {
    msg: Box<dyn Error + Send + Sync>,
    source: Option<Box<dyn Error + Send + Sync>>,
}

impl JournError {
    pub fn new<E: Into<Box<dyn Error + Send + Sync + 'static>>>(msg: E) -> Self {
        let msg = msg.into();
        if msg.is::<JournErrors>() {
            let mut je = msg.downcast::<JournErrors>().unwrap();
            if je.errors.len() == 1 {
                je.errors.pop().unwrap()
            } else {
                Self { msg: je, source: None }
            }
        } else {
            Self { msg, source: None }
        }
    }

    pub fn msg_mut<T: Error + Send + Sync + 'static>(&mut self) -> Option<&mut T> {
        //Some(self.msg.downcast_mut::<Box<T>>()?.deref_mut())
        self.msg.downcast_mut::<T>()
    }

    pub fn with_source<S: Into<Box<dyn Error + Send + Sync + 'static>>>(self, source: S) -> Self {
        Self { msg: self.msg, source: Some(source.into()) }
    }

    /// Gets the error message as a &'static str if it is one.
    pub fn as_message(&self) -> Option<&'static str> {
        match (&self.msg as &dyn Any).downcast_ref::<&'static str>() {
            Some(s) => Some(s),
            None => None,
        }
    }

    /// If the message is a list of errors, flatten it.
    pub fn flatten_message(&mut self) {
        let msg = mem::replace(&mut self.msg, "".into());
        self.msg = match msg.downcast::<JournErrors>() {
            Ok(errors) => Box::new(errors.flatten()),
            Err(msg) => msg,
        };
    }

    /// Gets whether the error has a block context down the chain.
    pub fn has_block_context(&self) -> bool {
        match &self.source {
            Some(source) => {
                if let Some(_bc) = source.downcast_ref::<BlockContextError>() {
                    return true;
                } else if let Some(je) = source.downcast_ref::<JournErrors>() {
                    for e in je.errors.iter() {
                        if e.has_block_context() {
                            return true;
                        }
                    }
                } else if let Some(err) = source.downcast_ref::<JournError>() {
                    return err.has_block_context();
                }
                false
            }
            None => false,
        }
    }

    /// Finds whether the error type appears in the error chain and returns the
    /// first instance of it.
    pub fn find<E: Error + 'static>(&self) -> Option<&E> {
        if let Some(err) = self.msg.downcast_ref::<E>() {
            return Some(err);
        }
        if let Some(source) = &self.source {
            if let Some(err) = source.downcast_ref::<E>() {
                return Some(err);
            }
        }

        if let Some(je) = self.msg.downcast_ref::<JournError>() {
            if let Some(found) = je.find::<E>() {
                return Some(found);
            }
        }
        if let Some(je) = self.source.as_ref().and_then(|s| s.downcast_ref::<JournError>()) {
            return je.find::<E>();
        }

        None
    }

    /// Returns `true` if the whole error (`self`) should be pruned from the parent.
    /// Otherwise, prunes by setting the msg to be the source, or setting the source to `None`.
    pub fn prune_all<E: Error + 'static>(&mut self) -> bool {
        if self.msg.is::<E>() {
            if self.source.as_ref().map(|s| s.is::<E>()).unwrap_or(false) {
                return true;
            } else if let Some(source) = self.source.take() {
                self.msg = source;
            } else {
                return true;
            }
        } else if self.source.as_ref().map(|s| s.is::<E>()).unwrap_or(false) {
            self.source = None;
        }

        if let Some(je) = self.msg.downcast_mut::<JournError>() {
            if je.prune_all::<E>() {
                if let Some(je) = self.source.as_mut().and_then(|s| s.downcast_mut::<JournError>())
                {
                    if je.prune_all::<E>() {
                        return true;
                    }
                }
                if let Some(source) = self.source.take() {
                    self.msg = source;
                } else {
                    return true;
                }
            } else if let Some(je) =
                self.source.as_mut().and_then(|s| s.downcast_mut::<JournError>())
            {
                if je.prune_all::<E>() {
                    self.source = None;
                }
            }
        }

        false
    }

    pub fn prune_except_last<E: Error + 'static>(&mut self) {
        let mut msg_pruned = false;
        let mut take_source = false;
        if let Some(src_je) = self.source.as_mut().and_then(|s| s.downcast_mut::<JournError>()) {
            if src_je.find::<E>().is_some() {
                if let Some(msg_je) = self.msg.downcast_mut::<JournError>() {
                    if msg_je.find::<E>().is_some() && msg_je.prune_all::<E>() {
                        take_source = true;
                    }
                } else if self.msg.is::<E>() {
                    take_source = true;
                }
                src_je.prune_except_last::<E>();
                msg_pruned = true;
            }
        }
        if !msg_pruned {
            if let Some(msg_je) = self.msg.downcast_mut::<JournError>() {
                msg_je.prune_except_last::<E>();
            }
        } else if take_source {
            self.msg = self.source.take().unwrap();
        }
    }
}

impl Error for JournError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.source.as_ref().map(|e| e.as_ref() as &dyn Error)
    }
}

impl fmt::Display for JournError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Write indents after newlines, if there isn't already an indent.
        let write_indented = |err: String, f: &mut fmt::Formatter<'_>| -> fmt::Result {
            let mut peekable = err.chars().peekable();
            while let Some(c) = peekable.next() {
                write!(f, "{}", c)?;
                if c == '\n' {
                    if peekable.peek() == Some(&' ') || peekable.peek() == Some(&'\u{21b3}') {
                        continue;
                    }
                    write!(f, "  ")?;
                }
            }
            Ok(())
        };

        write_indented(self.msg.to_string(), f)?;

        if let Some(source) = &self.source {
            writeln!(f)?;
            write!(f, "  \u{21b3} ")?;

            if let Some(_source) = source.downcast_ref::<JournErrors>() {
                write!(f, "\n\u{2192} {}", source)?;
            } else {
                write_indented(source.to_string(), f)?;
            }
        }
        Ok(())
    }
}

impl From<JournErrors> for JournError {
    fn from(value: JournErrors) -> Self {
        Self::new(value)
    }
}

impl From<(&TextBlock<'_>, &'static str)> for BlockContextError {
    fn from((block, ctx): (&TextBlock<'_>, &'static str)) -> Self {
        let context = BlockContext::from(block);
        BlockContextError::new(context, ctx.to_string())
    }
}

#[derive(Debug)]
pub struct JournErrors {
    message: String,
    errors: Vec<JournError>,
}

impl JournErrors {
    pub fn new(message: String, errors: Vec<JournError>) -> Self {
        Self { message, errors }
    }

    pub fn len(&self) -> usize {
        self.errors.len()
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn with_message(self, message: String) -> Self {
        JournErrors::new(message, self.errors)
    }

    /// Flattens the error chain.
    pub fn flatten(self) -> Self {
        let mut errors = vec![];
        for err in self.errors {
            match err.msg.downcast::<JournErrors>() {
                Ok(msg) => errors.extend(msg.flatten().errors),
                Err(msg) => {
                    errors.push(match err.source {
                        Some(source) => JournError::new(msg).with_source(source),
                        None => JournError::new(msg),
                    });
                }
            }
        }
        Self::new(self.message, errors)
    }
}

impl Error for JournErrors {}

impl fmt::Display for JournErrors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:", self.message)?;
        for err in self.errors.iter() {
            let err_str = err.to_string();
            writeln!(f, "\n\u{2192} {}", err_str.replace('\n', "\n  "))?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct BlockContextLine {
    line_num: Option<usize>,
    /// The range of byte indices of the line that should be highlighted.
    highlight_range: Range<usize>,
    text: String,
}

impl BlockContextLine {
    pub fn new(line_num: Option<usize>, text: String, highlight_range: Range<usize>) -> Self {
        assert!(highlight_range.end <= text.len());

        Self { line_num, text, highlight_range }
    }
}

impl From<&BlockContextLine> for Row<'static> {
    fn from(value: &BlockContextLine) -> Self {
        let highlight_style = if Arguments::get().print_std_err_in_color() {
            Style::default().with_fg(Colour::Red)
        } else {
            Style::default()
        };

        let mut cells = vec![];
        if let Some(line_num) = value.line_num {
            let mut line_num_cell: Cell = format!("{}", line_num).into();
            line_num_cell.set_alignment(Alignment::Right);
            line_num_cell.set_foreground(Some(Colour::RGB(120, 120, 120)));
            cells.push(line_num_cell);
        }
        let mut buf = String::new();
        buf.push_str(&value.text[..value.highlight_range.start]);
        buf.push_str(
            &highlight_style.paint(&value.text[value.highlight_range.clone()]).to_string(),
        );
        buf.push_str(&value.text[value.highlight_range.end..]);
        cells.push(buf.into());
        Row::new(cells)
    }
}

/// A set of lines from a journal file or entry to show and highlight the cause of an error.
#[derive(Debug)]
pub struct BlockContext {
    file: Option<String>,
    line: Option<usize>,
    col: Option<usize>,
    lines: Vec<BlockContextLine>,
}

impl BlockContext {
    pub fn new(
        file: Option<String>,
        line: Option<usize>,
        col: Option<usize>,
        lines: Vec<BlockContextLine>,
    ) -> Self {
        Self { file, line, col, lines }
    }

    pub fn file(&self) -> Option<&str> {
        self.file.as_deref()
    }

    pub fn set_file(&mut self, file: Option<String>) {
        self.file = file;
    }

    pub fn set_line(&mut self, line: Option<usize>) {
        self.line = line;
    }

    pub fn location_string(&self) -> Option<String> {
        let mut s = String::new();
        if let Some(file) = self.file() {
            s.push_str(file);
            s.push(':');
        }
        if let Some(line) = self.line {
            s.push_str(&line.to_string());
        }
        if let Some(col) = self.col {
            s.push(':');
            s.push_str(&col.to_string());
        }
        if s.is_empty() {
            None
        } else {
            Some(s)
        }
    }

    pub fn clear_highlights(&mut self) {
        for line in self.lines.iter_mut() {
            line.highlight_range = 0..0;
        }
    }

    /// Searches amongst the lines for the first occurrence of `text` and highlights it.
    pub fn highlight(&mut self, text: &str) -> bool {
        for line in self.lines.iter_mut() {
            if line.text.contains(text) {
                line.highlight_range =
                    line.text.find(text).unwrap()..line.text.find(text).unwrap() + text.len();
                return true;
            }
        }
        false
    }

    pub fn highlight_line(&mut self, line_num: usize) {
        assert!(line_num > 0);

        self.lines.binary_search_by(|l| l.line_num.unwrap_or(0).cmp(&line_num)).ok().map(|i| {
            self.lines[i].highlight_range = 0..self.lines[i].text.len();
        });
    }

    /// Shrinks the lines to the number of context lines, surrounding the lines
    /// highlighted with a maximum number of `context_lines`.
    pub fn shrink(&mut self, context_lines: usize) {
        let first_highlighted_line =
            self.lines.iter().position(|l| !l.highlight_range.is_empty()).unwrap_or(0);
        if first_highlighted_line > context_lines {
            self.lines.drain(0..first_highlighted_line - context_lines);
        }

        let last_highlighted_line = self
            .lines
            .iter()
            .rposition(|l| !l.highlight_range.is_empty())
            .unwrap_or(self.lines.len());
        if self.lines.len() - last_highlighted_line > context_lines {
            self.lines.drain(last_highlighted_line + context_lines..);
        }
    }
}

impl fmt::Display for BlockContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut table = Table::default();
        table.set_indent(2);
        table.set_column_separator("  ");
        for next_line in self.lines.iter() {
            table.add_row(next_line);
        }
        table.print(f)?;
        Ok(())
    }
}

//impl Error for BlockContext {}

impl From<&TextBlock<'_>> for BlockContext {
    fn from(block: &TextBlock<'_>) -> Self {
        // This needs to pass for the indexing below to work.
        assert!(
            block.location().is_some() || block.parent().is_none(),
            "Block has a parent, but no location"
        );

        // Go up the block tree while the parent block is on the same line as the current block.
        // We need full lines so that indexing works correctly.
        let mut parent = block;
        while let Some(p) = parent.parent() {
            if parent.trimmed_start_lines().column() == 1 {
                break;
            }
            //if p.column() == 1 || p.text().lines().count() > 20 {
            //break;
            //}
            parent = p;
        }
        let t_block = block.trimmed_start_lines();
        let mut context_lines = vec![];
        let base_line_num = parent.line() as usize;
        let mut col_adj = 0;
        for (line_num, line) in
            parent.text().lines().enumerate().map(|(i, l)| (i + base_line_num, l))
        {
            if context_lines.is_empty() && line.chars().all(interim_space) {
                continue;
            }
            let highlight_range = {
                if line_num < t_block.line() as usize || line_num > t_block.last_line() as usize {
                    0..0
                } else if line.is_empty() {
                    0..0
                } else {
                    let start = if line_num == t_block.line() as usize {
                        line.char_indices()
                            .nth(t_block.column() as usize - 1)
                            .map(|(i, _)| i)
                            .unwrap_or(line.len())
                    } else {
                        0
                    };
                    if line_num == t_block.line() as usize {
                        while line.as_bytes()[start + col_adj] == b' '
                            || line.as_bytes()[start + col_adj] == b'\t'
                        {
                            col_adj += 1;
                        }
                    }

                    let end = if line_num == t_block.last_line() as usize {
                        t_block.text().lines().last().unwrap().len()
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
        }
        BlockContext::new(
            t_block.file().map(|f| f.to_string()),
            Some(t_block.line() as usize),
            Some(t_block.column() as usize + col_adj),
            context_lines,
        )
    }
}

#[derive(Debug)]
pub struct BlockContextError {
    context: BlockContext,
    message: String,
}
impl BlockContextError {
    pub fn new(context: BlockContext, message: String) -> Self {
        Self { context, message }
    }

    pub fn context_mut(&mut self) -> &mut BlockContext {
        &mut self.context
    }
}
impl fmt::Display for BlockContextError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}\n{}", self.context.location_string().unwrap(), self.message, self.context)
    }
}
impl Error for BlockContextError {}

#[macro_export]
macro_rules! err {
    ($cause:expr; $msg:expr) => {{
        $crate::error::JournError::new($msg).with_source($cause)
    }};
    ($cause:expr; $msg:expr, $($args:tt)*) => {{
        $crate::error::JournError::new(format!($msg, $($args)*)).with_source($cause)
    }};
    ($msg:expr) => {{
        $crate::error::JournError::new($msg)
    }};
    ($msg:expr, $($args:tt)*) => {{
        $crate::error::JournError::new(format!($msg, $($args)*))
    }};
}

pub mod parsing {
    use crate::error::JournError;
    use crate::parsing::text_input::TextInput;
    use crate::parsing::IParseResult;
    use nom::error::{ContextError, ErrorKind, FromExternalError};
    use nom::Err as NomErr;
    use nom::{IResult, Parser};
    use std::error::Error;
    use std::fmt;

    #[derive(Debug, Clone, Copy)]
    pub enum IErrorKind {
        Tag,
        MapRes,
        MapOpt,
        Alt,
        IsNot,
        IsA,
        SeparatedList,
        SeparatedNonEmptyList,
        Many0,
        Many1,
        ManyTill,
        Count,
        TakeUntil,
        LengthValue,
        TagClosure,
        Alpha,
        Digit,
        HexDigit,
        OctDigit,
        AlphaNumeric,
        Space,
        MultiSpace,
        LengthValueFn,
        Eof,
        Switch,
        TagBits,
        OneOf,
        NoneOf,
        Char,
        CrLf,
        RegexpMatch,
        RegexpMatches,
        RegexpFind,
        RegexpCapture,
        RegexpCaptures,
        TakeWhile1,
        Complete,
        Fix,
        Escaped,
        EscapedTransform,
        NonEmpty,
        ManyMN,
        Not,
        Permutation,
        Verify,
        TakeTill1,
        TakeWhileMN,
        TooLarge,
        Many0Count,
        Many1Count,
        Float,
        Satisfy,
        Fail,
    }

    impl IErrorKind {
        pub fn map_from(kind: nom::error::ErrorKind) -> Self {
            match kind {
                ErrorKind::Tag => Self::Tag,
                ErrorKind::MapRes => Self::MapRes,
                ErrorKind::MapOpt => Self::MapOpt,
                ErrorKind::Alt => Self::Alt,
                ErrorKind::IsNot => Self::IsNot,
                ErrorKind::IsA => Self::IsA,
                ErrorKind::SeparatedList => Self::SeparatedList,
                ErrorKind::SeparatedNonEmptyList => Self::SeparatedNonEmptyList,
                ErrorKind::Many0 => Self::Many0,
                ErrorKind::Many1 => Self::Many1,
                ErrorKind::ManyTill => Self::ManyTill,
                ErrorKind::Count => Self::Count,
                ErrorKind::TakeUntil => Self::TakeUntil,
                ErrorKind::LengthValue => Self::LengthValue,
                ErrorKind::TagClosure => Self::TagClosure,
                ErrorKind::Alpha => Self::Alpha,
                ErrorKind::Digit => Self::Digit,
                ErrorKind::HexDigit => Self::HexDigit,
                ErrorKind::OctDigit => Self::OctDigit,
                ErrorKind::AlphaNumeric => Self::AlphaNumeric,
                ErrorKind::Space => Self::Space,
                ErrorKind::MultiSpace => Self::MultiSpace,
                ErrorKind::LengthValueFn => Self::LengthValueFn,
                ErrorKind::Eof => Self::Eof,
                ErrorKind::Switch => Self::Switch,
                ErrorKind::TagBits => Self::TagBits,
                ErrorKind::OneOf => Self::OneOf,
                ErrorKind::NoneOf => Self::NoneOf,
                ErrorKind::Char => Self::Char,
                ErrorKind::CrLf => Self::CrLf,
                ErrorKind::RegexpMatch => Self::RegexpMatch,
                ErrorKind::RegexpMatches => Self::RegexpMatches,
                ErrorKind::RegexpFind => Self::RegexpFind,
                ErrorKind::RegexpCapture => Self::RegexpCapture,
                ErrorKind::RegexpCaptures => Self::RegexpCaptures,
                ErrorKind::TakeWhile1 => Self::TakeWhile1,
                ErrorKind::Complete => Self::Complete,
                ErrorKind::Fix => Self::Fix,
                ErrorKind::Escaped => Self::Escaped,
                ErrorKind::EscapedTransform => Self::EscapedTransform,
                ErrorKind::NonEmpty => Self::NonEmpty,
                ErrorKind::ManyMN => Self::ManyMN,
                ErrorKind::Not => Self::Not,
                ErrorKind::Permutation => Self::Permutation,
                ErrorKind::Verify => Self::Verify,
                ErrorKind::TakeTill1 => Self::TakeTill1,
                ErrorKind::TakeWhileMN => Self::TakeWhileMN,
                ErrorKind::TooLarge => Self::TooLarge,
                ErrorKind::Many0Count => Self::Many0Count,
                ErrorKind::Many1Count => Self::Many1Count,
                ErrorKind::Float => Self::Float,
                ErrorKind::Satisfy => Self::Satisfy,
                ErrorKind::Fail => Self::Fail,
            }
        }

        pub fn message(&self) -> &'static str {
            match self {
                Self::Tag => "Tag",
                Self::MapRes => "Map on Result",
                Self::MapOpt => "Map on Option",
                Self::Alt => "Alternative",
                Self::IsNot => "IsNot",
                Self::IsA => "IsA",
                Self::SeparatedList => "Separated list",
                Self::SeparatedNonEmptyList => "Separated non empty list",
                Self::Many0 => "Many0",
                Self::Many1 => "Many1",
                Self::ManyTill => "ManyTill",
                Self::Count => "Count",
                Self::TakeUntil => "Take until",
                Self::LengthValue => "Length followed by value",
                Self::TagClosure => "Tag closure",
                Self::Alpha => "Alphabetic",
                Self::Digit => "Digit",
                Self::HexDigit => "Hexadecimal Digit",
                Self::OctDigit => "Octal digit",
                Self::AlphaNumeric => "AlphaNumeric",
                Self::Space => "Space",
                Self::MultiSpace => "Multiple spaces",
                Self::LengthValueFn => "LengthValueFn",
                Self::Eof => "End of file",
                Self::Switch => "Switch",
                Self::TagBits => "Tag on bitstream",
                Self::OneOf => "OneOf",
                Self::NoneOf => "NoneOf",
                Self::Char => "Char",
                Self::CrLf => "CrLf",
                Self::RegexpMatch => "RegexpMatch",
                Self::RegexpMatches => "RegexpMatches",
                Self::RegexpFind => "RegexpFind",
                Self::RegexpCapture => "RegexpCapture",
                Self::RegexpCaptures => "Reg",
                Self::TakeWhile1 => "TakeWhile1",
                Self::Complete => "Complete",
                Self::Fix => "Fix",
                Self::Escaped => "Escaped",
                Self::EscapedTransform => "EscapedTransform",
                Self::NonEmpty => "NonEmpty",
                Self::ManyMN => "Many(m, n)",
                Self::Not => "Negation",
                Self::Permutation => "Permutation",
                Self::Verify => "predicate verification",
                Self::TakeTill1 => "TakeTill1",
                Self::TakeWhileMN => "TakeWhileMN",
                Self::TooLarge => "Needed data size is too large",
                Self::Many0Count => "Count occurrence of >=0 patterns",
                Self::Many1Count => "Count occurrence of >=1 patterns",
                Self::Float => "Float",
                Self::Satisfy => "Satisfy",
                Self::Fail => "Fail",
            }
        }
    }

    impl fmt::Display for IErrorKind {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.message())
        }
    }

    pub struct IErrorMsg {}

    impl IErrorMsg {
        pub const REPEAT1: &'static str = "At least one occurrence expected";
        pub const PARAM: &'static str = "Parameter expected";
        pub const VALUE: &'static str = "Value expected";
        pub const QUOTES: &'static str = "Quotes expected";
        pub const BLANK_LINE: &'static str = "Blank line expected";
        pub const COMMENT: &'static str = "Comment expected";
        pub const INDENT_TOO_SMALL: &'static str = "Indent too small";
        pub const FIELD: &'static str = "Field expected";
        pub const NUMBER: &'static str = "Number expected";
        pub const NUMBER_FORMAT: &'static str = "Number format expected";
        pub const POSITIVE_NUMBER_FORMAT: &'static str = "Positive number format expected";
        pub const NEGATIVE_NUMBER_FORMAT: &'static str = "Negative number format expected";
        pub const AMOUNT: &'static str = "Amount expected";
        pub const MULTIPLE_AMOUNTS: &'static str = "Multiple amounts expected";
        pub const MATCHING_UNITS: &'static str = "Matching units expected";
        pub const UNIT: &'static str = "Unit expected";
        pub const DATE: &'static str = "Date expected";
        pub const DATE_OR_TIME: &'static str = "Date or time expected";
        pub const ACCOUNT: &'static str = "Account expected";
        pub const VALID_VALUATION: &'static str = "Valid valuation expected";
    }

    #[derive(Debug)]
    pub struct IParseError<I> {
        msg: &'static str,
        input: I,
    }

    impl<I> IParseError<I> {
        pub fn new(msg: &'static str, input: I) -> Self {
            Self { msg, input }
        }
    }

    impl<'h, I: TextInput<'h>> IParseError<I> {
        pub fn into_err(self) -> JournError {
            self.input.into_err(self.msg)
        }
    }

    /*
    impl<'h, I: TextInput<'h>> From<IParseError<I>> for Box<dyn ErrorMsg> {
        fn from(value: IParseError<I>) -> Self {
            value.into_err().msg
        }
    }*/

    impl<'h, I: TextInput<'h>> From<IParseError<I>> for JournError {
        fn from(value: IParseError<I>) -> Self {
            value.into_err()
        }
    }

    impl<I> fmt::Display for IParseError<I> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "Parse error: {}", self.msg)
        }
    }

    impl<'h, I: TextInput<'h>> nom::error::ParseError<I> for IParseError<I> {
        fn from_error_kind(input: I, kind: ErrorKind) -> Self {
            IParseError::new(IErrorKind::map_from(kind).message(), input)
        }

        fn append(input: I, kind: ErrorKind, _other: Self) -> Self {
            IParseError::new(IErrorKind::map_from(kind).message(), input)
        }
    }

    /// The usage of this implementation is less usual due to it allocating.
    impl<'h, I: TextInput<'h>> nom::error::ParseError<I> for JournError {
        fn from_error_kind(input: I, kind: ErrorKind) -> Self {
            IParseError::new(IErrorKind::map_from(kind).message(), input).into_err()
        }

        fn append(input: I, kind: ErrorKind, other: Self) -> Self {
            IParseError::new(IErrorKind::map_from(kind).message(), input)
                .into_err()
                .with_source(other)
        }
    }

    pub trait PromoteError<I: Clone> {
        fn promote_err(input: I, ctx: &'static str, error: IParseError<I>) -> Self;
    }

    /// Promotes a `nom::error::Error` to another Error (`JournError` typically), adding the supplied `context` as an error message.
    /// The idea is that promotion happens when a parser error becomes an actual problem that needs to be reported to the user.
    pub fn promote<I: Clone, E: PromoteError<I>, F, O>(
        context: &'static str,
        mut f: F,
    ) -> impl FnMut(I) -> IResult<I, O, E>
    where
        F: Parser<I, O, IParseError<I>>,
    {
        move |i: I| match f.parse(i.clone()) {
            Ok(o) => Ok(o),
            Err(NomErr::Incomplete(i)) => Err(NomErr::Incomplete(i)),
            Err(NomErr::Error(e)) => Err(NomErr::Error(E::promote_err(i, context, e))),
            Err(NomErr::Failure(e)) => Err(NomErr::Failure(E::promote_err(i, context, e))),
        }
    }

    impl<'h, I: TextInput<'h>> PromoteError<I> for JournError {
        fn promote_err(_input: I, ctx: &'static str, error: IParseError<I>) -> Self {
            error.input.into_err(ctx).with_source(error.msg)
        }
    }

    impl<'h, I: TextInput<'h>> PromoteError<I> for Vec<JournError> {
        fn promote_err(_input: I, ctx: &'static str, error: IParseError<I>) -> Self {
            vec![error.input.into_err(ctx)]
        }
    }

    /// Adds a particular ParseErrorKind in case of an error whilst running the parser.
    pub fn tag_err<'h, I: Clone, O, F>(
        tag: &'static str,
        mut parser: F,
    ) -> impl FnMut(I) -> IParseResult<'h, I, O>
    where
        F: Parser<I, O, IParseError<I>>,
    {
        move |i: I| match parser.parse(i.clone()) {
            Ok(f) => Ok(f),
            Err(NomErr::Error(_)) => Err(NomErr::Error(IParseError::new(tag, i))),
            Err(NomErr::Failure(_)) => Err(NomErr::Failure(IParseError::new(tag, i))),
            Err(NomErr::Incomplete(e)) => Err(NomErr::Incomplete(e)),
        }
    }

    impl<'h, I: TextInput<'h>> ContextError<I> for IParseError<I> {
        fn add_context(input: I, ctx: &'static str, _other: Self) -> Self {
            IParseError::new(ctx, input)
        }
    }

    impl<'h, I: TextInput<'h>> ContextError<I> for JournError {
        fn add_context(input: I, ctx: &'static str, other: Self) -> Self {
            input.into_err(ctx).with_source(other)
        }
    }

    impl<'h, I, E> FromExternalError<I, E> for JournError
    where
        I: TextInput<'h>,
        E: Error + Send + Sync + 'static,
    {
        fn from_external_error(input: I, _kind: ErrorKind, e: E) -> Self {
            input.into_err("").with_source(e)
        }
    }

    impl<I, E> FromExternalError<I, E> for IParseError<I> {
        fn from_external_error(input: I, kind: ErrorKind, _e: E) -> Self {
            Self::new(IErrorKind::map_from(kind).message(), input)
        }
    }
}
