/*
 * Copyright (c) 2023-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
#[macro_export]
macro_rules! unit {
    ($code:expr) => {{
        let unit = $crate::unit::Unit::new($code);
        Box::leak(Box::new(unit))
    }};
}

#[macro_export]
/// Creates an `Amount` with the specified unit code.
/// Only to be used for testing.
#[cfg(test)]
macro_rules! amount {
    ($str:expr) => {{
        let (_, amount) = parse!($str, $crate::parsing::amount::amount).unwrap();
        amount
    }};
    ($code:expr, $value:expr) => {{ $crate::amount::Amount::new($crate::unit_ref!($code), $value) }};
}

#[macro_export]
macro_rules! val {
    ($str:expr) => {{
        let mut config = $crate::config!();
        let config_cell = std::cell::RefCell::new(&mut config);
        let (_, va) = $crate::parsing::entry::valued_amount(
            $crate::parsing::StringInput::new_extra($str, &config_cell),
        )
        .unwrap();
        va
    }};
}

#[macro_export]
#[cfg(test)]
/// Parses a journal.
macro_rules! journ {
    ($text:expr) => {{
        match $crate::journal::Journal::parse(
            &mut $crate::arguments::Arguments::default(),
            $crate::parsing::text_block::TextBlock::from($text),
            $crate::alloc::ThreadAllocator::new(&$crate::alloc::HERD),
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
macro_rules! config {
    () => {{
        let allocator = $crate::alloc::HERD
            .get()
            .alloc($crate::alloc::HerdAllocator::new(&$crate::alloc::HERD));
        let node_id = allocator.alloc($crate::journal_node::NodeId::new_root());
        let args = allocator.alloc($crate::arguments::Arguments::default());
        $crate::configuration::Configuration::from_args(args, allocator, node_id)
    }};
    ($text:expr) => {{
        let journ = journ!($text);
        let config = $crate::configuration::Configuration::clone(&journ.root().config());
        config
    }};
}

/// Handy utility for quickly matching and mapping enums. See https://github.com/rust-lang/rfcs/issues/2960
#[macro_export]
macro_rules! match_map {
    ($expression:expr, $( $pattern:pat_param )|+ $( if $guard: expr )? => $ret:expr) => {
        match $expression {
            $( $pattern )|+ $( if $guard )? => Some($ret),
            _ => None
        }
    }
}

/// Handy utility for matching a pattern and doing something with it.
/// # Panics
/// If the match fails.
#[macro_export]
macro_rules! match_then {
    ($expression:expr, $( $pattern:pat_param )|+ $( if $guard: expr )? => $then:expr) => {
        match $expression {
            $( $pattern )|+ $( if $guard )? => { $then },
            _ => panic!("Match failed")
        }
    }
}

#[macro_export]
macro_rules! entry {
    ($text:expr) => {{
        use $crate::config;
        entry!($text, config!())
    }};
    ($text:expr, $config:expr) => {{
        use $crate::parse_node;
        use $crate::parsing::text_block::TextBlock;

        std::thread::scope(|scope| {
            let parse_node =
                parse_node!($text, scope, $config, $crate::journal_node::JournalNodeKind::Entry);
            let text_block: &TextBlock<'_> = parse_node.node().block();
            let tbr = text_block.node_reader(&parse_node);

            let mut dr = $crate::parsing::directive::DirectiveReader::new(tbr);
            loop {
                if let Some(res) = dr.next() {
                    let dir = res.expect("Failed to read directive");
                    let kind = dir.into_inner().1;
                    if let $crate::directive::DirectiveKind::Entry(entry) = kind {
                        break entry;
                    }
                } else {
                    panic!("No entry found")
                }
            }
        })
    }};
}

/// Parses a directive reading result, reading only the first one and returning it together with the remainder of the stream.
/// Panics if no directive is found.
#[macro_export]
macro_rules! dir {
    ($text:expr) => {{
        use $crate::config;
        dir!($text, config!())
    }};
    ($text:expr, $config:expr) => {{ dir!($text, $config, $crate::journal_node::JournalNodeKind::Entry) }};
    ($text:expr, $config:expr, $node_kind:expr) => {{
        use $crate::parse_node;

        std::thread::scope(|scope| {
            let parse_node = parse_node!($text, scope, $config, $node_kind);
            let text_block = parse_node.node().block();
            let tbr = text_block.node_reader(&parse_node);

            let mut dr = $crate::parsing::directive::DirectiveReader::new(tbr);
            let next = dr.next().expect("Failed to read directive");
            next.map(|dir| (dr.stream_remainder(), dir)).map_err($crate::parsing::util::str_err)
        })
    }};
}

#[macro_export]
macro_rules! dir_kind {
    ($text:expr, $kind:tt) => {{
        use $crate::config;
        dir_kind!($text, $kind, config!(), $crate::journal_node::JournalNodeKind::Entry)
    }};
    ($text:expr, $kind:tt, $config:expr) => {{ dir_kind!($text, $kind, $config, $crate::journal_node::JournalNodeKind::Entry) }};
    ($text:expr, $kind:tt, $config:expr, $file_kind:expr) => {{
        let dir_result = dir!($text, $config, $file_kind);
        match dir_result {
            Ok((rem, dir)) => {
                let kind = dir.into_inner().1;
                if let $crate::directive::DirectiveKind::$kind(obj) = kind {
                    Ok((rem, obj))
                } else {
                    Err("Wrong directive kind")
                }
            }
            Err(err) => Err(err),
        }
    }};
}

#[macro_export]
macro_rules! entry_dir {
    ($text:expr) => {{
        use $crate::config;
        entry_dir!(config!(), $text)
    }};
    ($config:expr, $text:expr) => {{
        use $crate::parse_node;
        use $crate::parsing::text_block::TextBlock;

        std::thread::scope(|scope| {
            let parse_node = parse_node!($text, scope, $config);
            let text_block: &TextBlock<'_> = parse_node.node().block();
            let tbr = text_block.node_reader(&parse_node);

            let mut dr = $crate::parsing::directive::DirectiveReader::new(tbr);
            loop {
                if let Some(res) = dr.next() {
                    let dir = res.expect("Failed to read directive");
                    if let $crate::directive::DirectiveKind::Entry(ref _entry) = dir.kind() {
                        break dir;
                    }
                } else {
                    panic!("No entry found")
                }
            }
        })
    }};
}
