/*
 * Copyright (c) 2022-2023. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
/// Handy utility for quickly matching and mapping enums. See https://github.com/rust-lang/rfcs/issues/2960
#[macro_export]
macro_rules! match_map {
    ($expression:expr, $( $pattern:pat )|+ $( if $guard: expr )? => $ret:expr) => {
        match $expression {
            $( $pattern )|+ $( if $guard )? => Some($ret),
            _ => None
        }
    }
}

/// A more friendly way to test a parser. Maps the input from a string and any output error to a string.
#[macro_export]
macro_rules! mapped_parser {
    ($fn_name:expr) => {{
        move |s: &'static str| {
            $fn_name(parsing::Span::new(s)).map(|(pi, o)| (*pi, o)).map_err(|e| match e {
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
            $fn_name(parsing::Span::new_extra(s, std::rc::Rc::new(std::cell::RefCell::new(conf))))
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

#[macro_export]
macro_rules! unit {
    ($code:expr) => {{
        let unit = Unit::new($code);
        Box::leak(Box::new(unit))
    }};
}

/// Creates an `Amount` with the specified unit code.
#[macro_export]
macro_rules! amount {
    ($str:expr) => {{
        let mut config =
            std::rc::Rc::new(std::cell::RefCell::new(configuration::Configuration::default()));
        let (_, amount) =
            parsing::amount::amount(parsing::ConfigSpan::new_extra($str, config)).unwrap();
        amount
    }};
    ($code:expr, $value:expr) => {{
        amount::Amount::new($crate::unit_ref!($code), $value)
    }};
}

#[macro_export]
/// Parses a journal.
macro_rules! journ {
    ($text:expr) => {{
        match journal::Journal::parse(
            arguments::Arguments::default(),
            journal_file::Stream::Text($text),
        ) {
            Ok(journ) => journ,
            Err(err) => {
                eprintln!("{}", err);
                panic!("Parsing errors encountered. See above")
            }
        }
    }};
}

#[macro_export]
macro_rules! je {
    ($text:expr) => {{
        je!(Configuration::default(), $text)
    }};
    ($config:expr, $text:expr) => {{
        let dr = DirectiveReader::new(($config, $text));
        while let Some(res) = dr.next() {
            let dir = res.expect("Failed to read directive")
            if let Directive::Entry(pje) = dir {
                return pje.try_unwrap().unwrap().into_inner();
            }
        }
        panic!("No entry");
    }};
}

#[macro_export]
macro_rules! je_dir {
    ($text:expr) => {{
        let node = std::rc::Rc::new(parsing::parser::JournalFileNode::new(
            configuration::Configuration::default(),
            journal_file::Stream::Text(""),
        ));
        let (_, mut entry) =
            parsing::entry::entry(parsing::NodeSpan::new_extra($text, node)).unwrap();
        directive::Directive::Entry(std::sync::Arc::new(std::sync::Mutex::new(
            directive::ParsedDirective::new(directive::RawDirective::new($text), entry),
        )))
    }};
    ($config:expr, $text:expr) => {{
        let node = std::rc::Rc::new(parsing::parser::JournalFileNode::new(
            $config,
            journal_file::Stream::Text(""),
        ));
        let (_, mut entry) =
            parsing::entry::entry(parsing::NodeSpan::new_extra($text, node)).unwrap();
        directive::Directive::Entry(std::sync::Arc::new(std::sync::Mutex::new(
            directive::ParsedDirective::new(directive::RawDirective::new($text), entry),
        )))
    }};
}
