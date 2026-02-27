/*
 * Copyright (c) 2023. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::alloc::HerdAllocator;
use crate::configuration::Configuration;
use crate::journal::Journal;
use crate::parsing::text_block::TextBlock;
use crate::report::command::arguments::Arguments;
use bumpalo_herd::Herd;

#[macro_export]
/// Parses a journal.
macro_rules! journ {
    ($text:expr) => {{ $crate::test_util::journ($text) }};
}

pub fn journ<'h>(text: &'h str) -> Journal<'h> {
    let herd = Box::leak(Box::new(Herd::new()));
    let allocator = herd.get().alloc(HerdAllocator::new(herd));
    match Journal::parse(&Arguments::default(), None, TextBlock::from(text), allocator) {
        Ok(journal) => journal,
        Err(err) => {
            eprintln!("{}", err);
            panic!("Parsing errors encountered. See above")
        }
    }
}

#[macro_export]
macro_rules! config {
    () => {{ $crate::test_util::config("") }};
    ($text:expr) => {{ $crate::test_util::config($text) }};
}

pub fn config<'h>(text: &'h str) -> Configuration<'h> {
    let journ = journ!(text);
    Configuration::clone(journ.config())
}

#[macro_export]
/// Creates an `Amount` with the specified unit code.
/// Only to be used for testing.
macro_rules! amount {
    ($str:expr) => {{
        let (_, amount) = parse!($str, $crate::parsing::amount::amount).unwrap();
        amount
    }};
    ($code:expr, $value:expr) => {{ $crate::amount::Amount::new($crate::unit_ref!($code), $value) }};
}

#[macro_export]
macro_rules! entry {
    ($text:expr) => {{
        use crate::journal_entry::JournalEntry;

        let journ = journ!($text);
        let entry: JournalEntry = journ.entry_range(..).next().expect("No entry found").clone();
        entry
    }}; /*
        ($text:expr, $config:expr) => {{
            journ!()

            use crate::parsing::directive::entry_file_directives;
            use $crate::parse_node;
            use $crate::parsing::text_block::TextBlock;

            std::thread::scope(|scope| {
                //let parse_node =
                //    parse_node!($text, scope, $config, $crate::journal_node::JournalNodeKind::Entry);
                //let text_block: &TextBlock<'_> = parse_node.node().block();
                //let input = text_block.as_input($config.allocator());

                let (_rem, dirs) =
                    $crate::parsing::parse_block_with_config_mut($text, entry_file_directives, $config);
                //let (_rem, dirs) = entry_file_directives(input).unwrap();
                for dir in dirs {
                    if let $crate::directive::DirectiveKind::Entry(entry) = dir.kind() {
                        return entry;
                    }
                }
                panic!("No entry found")
            })
        }};*/
}

#[macro_export]
macro_rules! parse_node {
    ($text:expr, $func:expr) => {{
        use $crate::parsing::testing::node_input;
        use $crate::ext::StrExt;

        let text = $text;
        //if text.starts_with("\n") {
        //    text = &text[1..];
        //}
        let outdented = text.outdent_lines().unwrap().intern();
        std::thread::scope(|s| {
            let jpn = node_input(&outdented, None, s);
            $func(jpn.input())
                .finish()
            .map_err($crate::error::JournError::from)
                .map(move |(rem, out)| ((rem.config().clone(), rem.fragment().to_string()), out)) //.map(|(rem, out)| out)
        })
    }}
}

/// Parses a directive reading result, reading only the first one and returning it together with the remainder of the stream.
/// Panics if no directive is found.
#[macro_export]
macro_rules! dir {
    ($text:expr) => {{
        let journ: $crate::journal::Journal = journ!($text);
        let mut found = None;
        for (seg, _dir) in journ.root().all_directives_iter() {
            found = Some(seg.remove_directive(0).unwrap());
            break;
        }
        found.expect("No directive found")
    }}; /*
        ($text:expr, $config:expr) => {{ dir!($text, $config, $crate::journal_node::JournalNodeKind::Entry) }};
        ($text:expr, $config:expr, $node_kind:expr) => {{
            use $crate::parse_node;

            std::thread::scope(|scope| {
                let parse_node = parse_node!($text, scope, $config, $node_kind);
                let text_block = parse_node.node().block();
                let input = text_block.as_input($config.allocator());

                let (_rem, dirs) = entry_file_directives(input).unwrap();
                dirs.first().expect("Failed to read directive")
            })
        }};*/
}

#[macro_export]
macro_rules! dir_kind {
    ($text:expr, $kind:tt) => {{
        let dir = dir!($text);
        if let $crate::directive::DirectiveKind::$kind(obj) = dir.kind() {
            *obj
        } else {
            panic!("Wrong directive kind");
        }
    }};
}

#[macro_export]
macro_rules! entry_dir {
    ($text:expr) => {{
        let journ = journ!($text);
        let mut found = None;
        for (_seg, dir) in journ.root().all_directives_iter() {
            if let $crate::directive::DirectiveKind::Entry(entry) = dir.kind() {
                found = Some($crate::directive::Directive::new(
                    dir.parsed(),
                    $crate::directive::DirectiveKind::Entry(entry),
                ));
                break;
            }
        }
        found.expect("No directive found")
    }}; /*
        ($config:expr, $text:expr) => {{
            use crate::parsing::directive::entry_file_directives;
            use $crate::parse_node;
            use $crate::parsing::text_block::TextBlock;

            std::thread::scope(|scope| {
                let parse_node = parse_node!($text, scope, $config);
                let text_block: &TextBlock<'_> = parse_node.node().block();
                let input = text_block.as_input($config.allocator());

                let (_rem, dirs) = entry_file_directives(input).unwrap();
                for dir in dirs {
                    if let $crate::directive::DirectiveKind::Entry(entry) = dir.kind() {
                        return dir;
                    }
                }
                panic!("No entry found")
            })
        }};*/
}

/// A more friendly way to test a parser. Maps the input from a string and any output error to a string.
#[macro_export]
macro_rules! mapped_parser {
    ($fn_name:expr) => {{
        move |s: &'static str| {
            $fn_name(parsing::TextFileInput::new_extra(
                s,
                <&'static crate::parsing::JournalFileNode>::from(s),
            ))
            .map(|(pi, o)| (*pi, o))
            .map_err(|e| match e {
                nom::Err::Error(je) => je.lower_context().unwrap_or(""),
                nom::Err::Failure(je) => je.lower_context().unwrap_or(""),
                _ => "",
            })
        }
    }};
}

/// A more friendly way to test a parser. Maps the input from a string and any output error to a string.
#[macro_export]
macro_rules! mapped_config_parser {
    ($fn_name:expr, $config:expr) => {{
        // Move a clone of config in to the following function.
        let conf = $config.clone();
        move |s: &'static str| {
            $fn_name(parsing::StringInput::new_extra(
                s,
                std::rc::Rc::new(std::cell::RefCell::new(conf)),
            ))
            .map(|(pi, o)| (*pi, o))
            .map_err(|e| match e {
                nom::Err::Error(je) => je.lower_context().unwrap_or(""),
                nom::Err::Failure(je) => je.lower_context().unwrap_or(""),
                _ => "",
            })
        }
    }};
}

/// A more friendly way to test parsers that accept `NodeSpan`.
#[macro_export]
macro_rules! mapped_node_parser {
    ($fn_name:expr, $config:expr) => {{
        // Move a clone of config in to the following function.
        let config = $config.clone();
        move |s: &'static str| {
            let node = std::rc::Rc::new(parsing::parser::JournalFileNode::new(
                config,
                journal_file::Stream::Text(""),
            ));
            let res = $fn_name(parsing::NodeSpan::new_extra(s, std::rc::Rc::clone(&node)))
                .map(|(input, o)| (input.extra.config().clone(), *input, o))
                .map_err(|e| match e {
                    nom::Err::Error(je) => je.lower_context().unwrap_or(""),
                    nom::Err::Failure(je) => je.lower_context().unwrap_or(""),
                    _ => "",
                });
            for e in node.parse_errors().iter() {
                eprintln!("{}", e);
            }
            assert!(node.parse_errors().is_empty());
            res
        }
    }};
}
