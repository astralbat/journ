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
pub mod input;
pub mod parser;
pub mod reader;
pub mod text_block;
mod unit_directive;
pub mod util;

use crate::configuration::Configuration;
use crate::error::parsing::IParseError;
use crate::error::{JournError, JournResult};
use crate::parsing::input::TextBlockInput;
use crate::parsing::parser::JournalParseNode;
use crate::parsing::text_block::TextBlock;
use nom::{Err as NomErr, Finish, IResult};
use nom_locate::LocatedSpan;
use std::cell::RefCell;
use std::fmt;
use std::ops::DerefMut;
use std::rc::Rc;

/// A note on lifetimes:
/// * `'h` is the lifetime of the heap allocator.
/// * `'p` is the lifetime of the parse node.
/// * `'s` is the lifetime of the thread scope.

pub type StringInput<'h, 'p, 's> = LocatedSpan<&'h str, Rc<RefCell<Configuration<'h>>>>;
pub type TextFileInput<'h, 's, 'e, 'p> = LocatedSpan<&'h str, &'p JournalParseNode<'h, 's, 'e>>;

//pub type IParseResult<'h, I, T> =
//    Result<(I, T), NomErr<JournError<'h, InputErr<'h, <I as TextInput<'h>>::Error>>>>;
pub type IParseResult<'h, I, O> = Result<(I, O), NomErr<IParseError<I>>>;
pub type JParseResult<I, O> = Result<(I, O), NomErr<JournError>>;

/// A trait whose implementors provide both `DerefMut` and `Debug` implementations.
pub trait DerefMutAndDebug<'h, 's, T>: DerefMut<Target = T> + fmt::Debug + 's {}

fn str_parse<'h, I, O, E, F>(
    str: I,
    config: Configuration<'h>,
    mut func: F,
) -> JournResult<(I, (O, Configuration<'h>))>
where
    E: Into<JournError>,
    F: FnMut(
        LocatedSpan<I, RefCell<Configuration<'h>>>,
    ) -> IResult<LocatedSpan<I, RefCell<Configuration<'h>>>, O, E>,
{
    let wrapped_config = RefCell::new(config);
    let input = LocatedSpan::new_extra(str, wrapped_config);
    let (rem, out) = func(input).finish().map_err(Into::into)?;
    let (fragment, extra) = rem.into_fragment_and_extra();
    Ok((fragment, (out, extra.into_inner())))
}

fn block_parse<'h, O, E, F>(
    block: &'h TextBlock<'h>,
    config: Configuration<'h>,
    mut func: F,
) -> JournResult<(&'h str, (O, Configuration<'h>))>
where
    E: Into<JournError>,
    F: FnMut(
        TextBlockInput<'h, RefCell<Configuration<'h>>>,
    ) -> IResult<TextBlockInput<'h, RefCell<Configuration<'h>>>, O, E>,
{
    let allocator = config.allocator();
    let wrapped_config = RefCell::new(config);
    // Should always be safe.
    let input = unsafe {
        LocatedSpan::new_from_raw_offset(
            block.location_offset(),
            block.line(),
            block.text(),
            wrapped_config,
        )
    };
    let block_input = TextBlockInput::new(input, block, allocator);

    let (rem, out) = func(block_input).finish().map_err(Into::into)?;
    let (fragment, extra) = rem.inner.into_fragment_and_extra();
    Ok((fragment, (out, extra.into_inner())))
}

/*
fn str_parse<'h, O, E, F>(
    str: &'h str,
    config: Arc<Configuration<'h>>,
    mut func: F,
) -> JournResult<(&'h str, (O, Arc<Configuration<'h>>))>
where
    E: Into<JournError>,
    F: FnMut(
        LocatedSpan<&'h str, RefCell<Arc<Configuration<'h>>>>,
    ) -> IResult<LocatedSpan<&'h str, RefCell<Arc<Configuration<'h>>>>, O, E>,
{
    let wrapped_config = RefCell::new(config);
    let input = LocatedSpan::new_extra(str, wrapped_config);
    let (rem, out) = func(input).finish().map_err(Into::into)?;
    let (fragment, extra) = rem.into_fragment_and_extra();
    Ok((fragment, (out, extra.into_inner())))
}*/

/// Parses a string with the supplied function and a configuration.
/// A configuration is required unless testing.
#[macro_export]
macro_rules! parse {
    ($str:expr, $func:expr, $config:expr) => {{
        use $crate::parsing::OwnedOrMutConfigParseHelper;
        $config.handle_str($str, $func)
    }};
    ($str:expr, $func:expr) => {{
        use $crate::config;
        let config = config!();
        parse!($str, $func, config)
    }};
}

/// `$config` should be a `Arc<Configuration>` type that may get modified as a result of the parser's actions.
#[macro_export]
macro_rules! parse_block {
    ($block:expr, $func:expr, $config:expr) => {{
        use $crate::parsing::OwnedOrMutConfigParseHelper;
        $config.handle_block($block, $func)
    }};
    ($str:expr, $func:expr) => {{
        use $crate::config;
        let mut config = config!();
        parse_block!($str, $func, config)
    }};
}

pub trait OwnedOrMutConfigParseHelper<'h> {
    fn handle_str<F, O, E>(self, str: &'h str, func: F) -> JournResult<(&'h str, O)>
    where
        F: FnMut(
            LocatedSpan<&'h str, RefCell<Configuration<'h>>>,
        ) -> IResult<LocatedSpan<&'h str, RefCell<Configuration<'h>>>, O, E>,
        E: Into<JournError>;
    fn handle_block<F, O, E>(self, block: &'h TextBlock<'h>, func: F) -> JournResult<(&'h str, O)>
    where
        F: FnMut(
            TextBlockInput<'h, RefCell<Configuration<'h>>>,
        ) -> IResult<TextBlockInput<'h, RefCell<Configuration<'h>>>, O, E>,
        E: Into<JournError>;
}
impl<'h> OwnedOrMutConfigParseHelper<'h> for Configuration<'h> {
    fn handle_str<F, O, E>(self, str: &'h str, func: F) -> JournResult<(&'h str, O)>
    where
        F: FnMut(
            LocatedSpan<&'h str, RefCell<Configuration<'h>>>,
        ) -> IResult<LocatedSpan<&'h str, RefCell<Configuration<'h>>>, O, E>,
        E: Into<JournError>,
    {
        let (rem, (out, _config)) = str_parse(str, self, func)?;
        Ok((rem, out))
    }

    fn handle_block<F, O, E>(self, block: &'h TextBlock<'h>, func: F) -> JournResult<(&'h str, O)>
    where
        F: FnMut(
            TextBlockInput<'h, RefCell<Configuration<'h>>>,
        ) -> IResult<TextBlockInput<'h, RefCell<Configuration<'h>>>, O, E>,
        E: Into<JournError>,
    {
        let (rem, (out, _config)) = block_parse(block, self, func)?;
        Ok((rem, out))
    }
}

impl<'h> OwnedOrMutConfigParseHelper<'h> for &mut Configuration<'h> {
    fn handle_str<F, O, E>(self, str: &'h str, func: F) -> JournResult<(&'h str, O)>
    where
        F: FnMut(
            LocatedSpan<&'h str, RefCell<Configuration<'h>>>,
        ) -> IResult<LocatedSpan<&'h str, RefCell<Configuration<'h>>>, O, E>,
        E: Into<JournError>,
    {
        let config = self.clone();
        let (rem, (out, config)) = str_parse(str, config, func)?;
        *self = config;
        Ok((rem, out))
    }

    fn handle_block<F, O, E>(self, block: &'h TextBlock<'h>, func: F) -> JournResult<(&'h str, O)>
    where
        F: FnMut(
            TextBlockInput<'h, RefCell<Configuration<'h>>>,
        ) -> IResult<TextBlockInput<'h, RefCell<Configuration<'h>>>, O, E>,
        E: Into<JournError>,
    {
        let config = self.clone();
        let (rem, (out, config)) = block_parse(block, config, func)?;
        *self = config;
        Ok((rem, out))
    }
}

/// Matches a parser and applied the handler.
/// The parser is expected to return a `Result<_, NomErr<IParseError>>`, while the handler is expected
/// to produce a `Result<_, JournError<'static>>` result.
///
/// This will only produce an error if a parser returns a `NomErr::Failure`, the `$handler` fails or
/// no parser matches.
#[macro_export]
macro_rules! match_parser {
    ($input:expr, $($parser:expr $(, if $cond:expr)? => $handler:expr),+) => {{
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
            // Special "None" message meaning no parsers matched. We check for this below
            // when looping.
            use $crate::err;
            Err(NomErr::Error(err!("None")))
        };
        match handler_match_res {
            Err(e) => Err(e),
            Ok((rem, out)) => Ok((rem, out)),
        }
    }}
}

/// Repeatedly matches a parser and applies an associated handler.
/// This will only produce an error when a parser returns a `Failure` or if an handler fails.
///
/// If no parsers match the expressions, then `Ok(I, I)` is returned with the remaining input.
#[macro_export]
macro_rules! match_parsers {
    ($input:expr, $($parser:expr $(, if $cond:expr)? => $handler:expr),+) => {{
        use $crate::match_parser;

        let mut inp = $input;
        loop {
            if inp.text().is_empty() {
                break Ok((inp.clone(), inp));
            }

            match match_parser!(inp.clone(), $($parser $(, if $cond)? => $handler),+) {
                Ok((rem, _handler_res)) => {
                    inp = rem;
                    continue;
                }
                // Special "None" message meaning no parsers matched. We return from the loop.
                Err(NomErr::Error(e)) if e == "None" => break Ok((inp.clone(), inp)),
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
