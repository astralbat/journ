/*
 * Copyright (c) 2023. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
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
