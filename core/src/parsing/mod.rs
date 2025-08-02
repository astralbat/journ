/*
 * Copyright (c) 2023-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
pub mod amount;
pub mod directive;
pub mod entry;
pub mod parser;
pub mod reader;
pub mod text_block;
pub mod text_input;
pub mod util;

use crate::configuration::Configuration;
use crate::error::JournError;
use crate::error::parsing::IParseError;
use crate::parsing::parser::JournalParseNode;
use nom::Err as NomErr;
use nom_locate::LocatedSpan;
use std::cell::RefCell;
use std::fmt;
use std::ops::DerefMut;

/// A note on lifetimes:
/// * `'h` is the lifetime of the heap allocator.
/// * `'p` is the lifetime of the parse node.
/// * `'s` is the lifetime of the thread scope.

pub type StringInput<'h, 'p, 's> =
    LocatedSpan<&'h str, &'p RefCell<dyn DerefMutAndDebug<'h, 's, Configuration<'h>> + 's>>;
pub type TextFileInput<'h, 's, 'e, 'p> = LocatedSpan<&'h str, &'p JournalParseNode<'h, 's, 'e>>;

//pub type IParseResult<'h, I, T> =
//    Result<(I, T), NomErr<JournError<'h, InputErr<'h, <I as TextInput<'h>>::Error>>>>;
pub type IParseResult<'h, I, O> = Result<(I, O), NomErr<IParseError<I>>>;
pub type JParseResult<I, O> = Result<(I, O), NomErr<JournError>>;

/// A trait whose implementors provide both `DerefMut` and `Debug` implementations.
pub trait DerefMutAndDebug<'h, 's, T>: DerefMut<Target = T> + fmt::Debug + 's {}

/// Parses a string with the supplied function and a configuration.
/// A configuration is required unless testing.
#[macro_export]
macro_rules! parse {
    ($str:expr, $func:expr, $config:expr) => {{
        use nom::Finish;
        use $crate::error::JournError;
        use $crate::parsing::StringInput;
        use $crate::parsing::text_input::TextInput;

        let wrapped_config = std::cell::RefCell::new($config);
        let input = StringInput::new_extra($str, &wrapped_config);
        let res: Result<(&str, _), JournError> =
            $func(input).finish().map(|(rem, out)| (rem.text(), out)).map_err(Into::into);
        // Return the config as well so it can be used for further parsing.
        (res, wrapped_config.into_inner())
    }};
    ($str:expr, $func:expr) => {{
        use $crate::config;
        let mut config = config!();
        parse!($str, $func, &mut config).0
    }};
}

#[macro_export]
macro_rules! parse_block {
    ($block:expr, $func:expr, $config:expr) => {{
        use nom::Finish;
        use $crate::error::JournError;
        use $crate::parsing::StringInput;
        use $crate::parsing::text_input::TextInput;

        let wrapped_config = std::cell::RefCell::new($config);
        let block = $block;
        // Should always be safe.
        let input = unsafe {
            StringInput::new_from_raw_offset(
                block.location_offset(),
                block.line(),
                block.text(),
                &wrapped_config,
            )
        };
        //let block = $crate::parsing::text_block::TextBlock::from($str);
        let block_input = $crate::parsing::text_input::TextBlockInput::new(
            input,
            &block,
            wrapped_config.borrow().allocator(),
        );

        let res: Result<(&str, _), JournError> =
            $func(block_input).finish().map(|(rem, out)| (rem.text(), out)).map_err(Into::into);
        // Return the config as well so it can be used for further parsing.
        (res, wrapped_config.into_inner())
    }};
    ($str:expr, $func:expr) => {{
        use $crate::config;
        let mut config = config!();
        parse_block!($str, $func, &mut config).0
    }};
}

/// Matches a parser and applied the handler.
/// The parser is expected to return a `Result<_, NomErr<IParseError>>`, while the handler is expected
/// to produce a `Result<_, JournError<'static>>` result.
///
/// This will only produce an error if a parser returns a `NomErr::Failure`, the `$handler` fails or
/// no parser matches.
#[macro_export]
macro_rules! match_parser {
    ($input:expr, $($parser:expr => $handler:expr),+) => {{
        use nom::Err as NomErr;
        use $crate::error::parsing::IParseError;

        let orig_input = $input;
        let handler_match_res = 'handler_match: {
            $(
                #[allow(clippy::unit_arg)]
                match $parser(orig_input.clone()) {
                    Ok((rem, out)) => {
                        #[allow(unused_mut)]
                        let mut handler = $handler;
                        match handler(out) {
                            Ok(res) => break 'handler_match Ok((rem, res)),
                            Err(e) => break 'handler_match Err(e),
                        }
                    }
                    Err(NomErr::Error(_)) => {}
                    // This type hint on the error forces all parsers to use IParseError.
                    Err(NomErr::<IParseError<_>>::Failure(e)) => break 'handler_match Err(NomErr::Failure(orig_input.into_err("A failure occurred with parser expression").with_source(e.into_err()))),
                    Err(NomErr::Incomplete(_)) => break 'handler_match Err(NomErr::Failure(orig_input.into_err("Incomplete input"))),
                }
            )+
            Err(NomErr::Error(orig_input.into_err("No parser matched expression")))
        };
        match handler_match_res {
            Err(e) => Err(e),
            Ok((rem, out)) => Ok((rem, out)),
        }
    }}
}

/// Repeatedly matches a parser and applies an associated handler.
/// This will only produce an error when a parser returns a `Failure` or if an handler fails.
#[macro_export]
macro_rules! match_parsers {
    ($input:expr, $($parser:expr => $handler:expr),+) => {{
        use $crate::match_parser;

        let mut inp = $input;
        loop {
            if inp.text().is_empty() {
                break Ok((inp.clone(), inp));
            }

            match match_parser!(inp.clone(), $($parser => $handler),+) {
                Ok((rem, _handler_res)) => {
                    inp = rem;
                }
                Err(e) => break Err(e),
            }
        }
    }}
}

/// Creates a new `JournalFileParseNode` from the supplied expression.
#[macro_export]
macro_rules! parse_node {
    ($str:expr, $scope:expr, $config:expr, $node_kind:expr) => {{
        use std::sync::Arc;

        let allocator = $config.allocator();
        let node = Arc::new($crate::journal_node::JournalNode::new(
            None,
            $crate::journal_node::NodeId::allocate(),
            $node_kind,
            $config,
            allocator.alloc($crate::parsing::text_block::TextBlock::from($str)),
            vec![],
        ));
        $crate::parsing::parser::JournalParseNode::new_root(&node, $scope)
    }};
    ($str:expr, $scope:expr, $config:expr) => {{ parse_node!($str, $scope, $config, $crate::journal_node::JournalNodeKind::Entry) }};
    // This uses a static allocator, and so should only be used for testing.
    ($str:expr, $scope:expr) => {{
        parse_node!(
            $str,
            $scope,
            $crate::configuration::Configuration::new($crate::alloc::HerdAllocator::new(
                &$crate::alloc::HERD
            )),
            $crate::journal_node::JournalNodeKind::Entry
        )
    }};
}

/*
impl<'h, I: TextInput<'h>> ContextError<I> for JournError<'static, &'static str> {
    /// Creates a new error from an input position, a static string and an existing error.
    /// This is used mainly in the [context] combinator, to add user friendly information
    /// to errors when backtracking through a parse tree
    fn add_context(_input: I, ctx: &'static str, existing: Self) -> Self {
        err!(existing; ctx)
    }
}*/

/*

/// An error type for the parsing module.
///
/// We don't use the [JournError] type here as we expect lots of backtracking and we don't wish to allocate
/// Strings every time.
#[derive(PartialEq)]
pub struct JParseError<'h, I> {
    /// The part of the input stream relevant to the error.
    input: I,
    // Lower context: the context closest to the fundamental error
    l_context: Option<&'h str>,
    // Higher context: the context furthest from the fundamental error
    h_context: Option<&'h str>,
    // When parsing blocks of input, this may refer to the immediate parent for additional context.
    parent_block: Option<&'h TextBlock<'h>>,
}
impl<'h, I: TextInput<'h>> JParseError<'h, I> {
    pub fn new(input: I, l_context: Option<&'h str>) -> Self {
        Self { input, l_context, h_context: None, parent_block: None }
    }

    /// The fixed error message closest to the problem
    pub fn lower_context(&self) -> Option<&'h str> {
        self.l_context
    }

    pub fn set_lower_context(&mut self, l_context: &'h str) {
        self.l_context = Some(l_context)
    }

    /// The fixed error message furthest from the problem
    pub fn higher_context(&self) -> Option<&'h str> {
        self.h_context
    }

    pub fn set_higher_context(&mut self, h_context: &'h str) {
        self.h_context = Some(h_context)
    }

    /// Sets the text block that the error is contained in.
    pub fn set_text_block(&mut self, text_block: &'h TextBlock<'h>) {
        // Examine the parents of the text block to find the first useful parent
        // to display.
        let mut parent = Some(text_block);
        let curr_file = text_block.file();
        while let Some(tb) = parent {
            // Never consider the root for error display as it is outside of the scope of
            // a directive.
            if tb.is_root() {
                break;
            }
            // Don't go beyond the current file for display.
            if tb.file() != curr_file {
                break;
            }

            self.parent_block = Some(tb);
            // Ideally at least 10 lines of context if a parent can provide it.
            if tb.text().trim_start().lines().count() > 10 {
                break;
            }

            parent = tb.parent();
        }
    }

    /// Maps the input information
    pub fn map_input<I2, F: FnOnce(I) -> I2>(self, func: F) -> JParseError<'h, I2> {
        JParseError {
            input: func(self.input),
            l_context: self.l_context,
            h_context: self.h_context,
            parent_block: self.parent_block,
        }
    }
}

impl<'h, I: TextInput<'h>> fmt::Debug for JParseError<'h, I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (self as &dyn fmt::Display).fmt(f)
    }
}

impl<'h, I: TextInput<'h>> fmt::Display for JParseError<'h, I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let stream = match self.input.node() {
            Some(file) => match file.block().filename() {
                Some(filename) => {
                    let mut s = filename.to_string();
                    s.push(':');
                    s
                }
                None => String::new(),
            },
            None => String::new(),
        };

        let line_desc = map(rest_line0, |s: I| s.text())(self.input.clone()).unwrap().1;

        if let (Some(line), Some(col)) = (self.input.line(), self.input.column()) {
            write!(f, "At {}{}:{}", stream, line, col)?;
        }
        if let Some(msg) = &self.h_context {
            write!(f, ": {msg}")?;
        }
        if let Some(msg) = &self.l_context {
            write!(f, ": {msg}")?;
        }

        let highlight_style =
            if Arguments::get().print_std_err_in_color() { Style::default().fg(Color::Red) } else { Style::default() };
        let line_num_style = if Arguments::get().print_std_err_in_color() {
            Style::default().fg(Color::RGB(120, 120, 120))
        } else {
            Style::default()
        };

        if let Some(parent_block) = self.parent_block.as_ref().map(|p| p.trimmed_start_lines()) {
            writeln!(f)?;
            let mut line_num = parent_block.first_content_line();
            let line_num_width = parent_block.last_line().to_string().len();
            let input_lines = self.input.text().lines().count() as u32;
            for line in parent_block.text().lines() {
                write!(f, "  {:>line_num_width$} ", line_num_style.paint(line_num.to_string()))?;

                match (self.input.line(), self.input.column()) {
                    (Some(input_line), Some(input_column)) => {
                        // Line is before input starts, so there is no overlap
                        if line_num < input_line {
                            writeln!(f, "{}", line)?;
                        // Input starts on line. Return the part from the column
                        } else if input_line == line_num {
                            let err_end = (input_column - 1 + self.input.input_len()).min(line.len());
                            write!(f, "{}", &line[..input_column - 1])?;
                            write!(f, "{}", highlight_style.paint(&line[input_column - 1..err_end]))?;
                            writeln!(f, "{}", &line[err_end..])?;
                        // Line wholly within input
                        } else if line_num < input_line + input_lines - 1 {
                            writeln!(f, "{}", highlight_style.paint(line))?;
                        // Input ends on this line.
                        } else if line_num == input_line + input_lines - 1 {
                            write!(
                                f,
                                "{}",
                                highlight_style.paint(&line[..self.input.text().lines().last().unwrap_or("").len()])
                            )?;
                            writeln!(f, "{}", &line[self.input.text().lines().last().unwrap_or("").len()..],)?;
                        } else {
                            writeln!(f, "{}", line)?;
                        }
                    }
                    _ => writeln!(f, "{}", line)?,
                }
                line_num += 1;
            }
        } else if !line_desc.is_empty() {
            write!(f, ": '{}'", highlight_style.paint(line_desc))?;
        }
        Ok(())
    }
}

impl<'h, I: TextInput<'h>> nom::error::ParseError<I> for JParseError<'h, I> {
    fn from_error_kind(input: I, _kind: nom::error::ErrorKind) -> Self {
        Self::new(input, None)
    }

    fn append(_input: I, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }

    fn from_char(input: I, _c: char) -> Self {
        Self::new(input, None)
    }
}

impl<'h, I: TextInput<'h>> ContextError<I> for JParseError<'h, I> {
    /// Creates a new error from an input position, a static string and an existing error.
    /// This is used mainly in the [context] combinator, to add user friendly information
    /// to errors when backtracking through a parse tree
    fn add_context(_input: I, ctx: &'static str, existing: Self) -> Self {
        // Use the existing error as input for the new error as the passed input will
        // not have line/col information relating to where the context message was raised.
        match existing.l_context {
            None => JParseError { l_context: Some(ctx), h_context: None, ..existing },
            Some(context) => JParseError { l_context: Some(context), h_context: Some(ctx), ..existing },
        }
    }
}

impl<'h, I: TextInput<'h>> FromExternalError<I, JournError<'h>> for JParseError<'h, I> {
    fn from_external_error(input: I, _kind: ErrorKind, e: JournError<'h>) -> Self {
        match e {
            JournError::Message(msg) => JParseError::new(input, Some(msg)),
            JournError::Chain(_e1, e2) => FromExternalError::from_external_error(input, _kind, *e2),
            _ => JParseError::new(input, None),
        }
    }
}

impl<'h, I: TextInput<'h>> From<nom::Err<JParseError<'h, I>>> for JournError<'h> {
    fn from(pe: nom::Err<JParseError<'h, I>>) -> Self {
        match pe {
            nom::Err::Failure(e) => errmsg!("{}", e),
            nom::Err::Error(e) => errmsg!("{}", e),
            nom::Err::Incomplete(e) => errmsg!("{:?}", e),
        }
    }
}*/
