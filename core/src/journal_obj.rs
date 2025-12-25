/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::configuration::Configuration;
use crate::parsing::text_block::TextBlock;
use std::fmt;

pub trait JournalObj<'h>: fmt::Display {
    fn block(&self) -> &'h TextBlock<'h>;

    fn config(&self) -> &Configuration<'h>;
}

/// Common behaviour to all journal objects.
#[macro_export]
macro_rules! impl_journal_obj_common {
    ($obj_type:ty) => {
        impl<'h> JournalObj<'h> for $obj_type {
            fn block(&self) -> &'h TextBlock<'h> {
                &self.block.get_or_init(|| {
                    Self::block_from_text(self.to_string(), self.config().allocator())
                })
            }

            fn config(&self) -> &Configuration<'h> {
                &self.config
            }
        }

        impl<'h> $obj_type {
            #[allow(dead_code)]
            fn block_input(&self) -> TextBlockInput<'h, RefCell<Configuration<'h>>> {
                self.block()
                    .as_input(self.config.allocator())
                    .map_extra(|_| RefCell::new(self.config.clone()))
            }

            /// Allocates a new block from the `text` provided.
            fn block_from_text(text: String, allocator: &HerdAllocator<'h>) -> &'h TextBlock<'h> {
                allocator.alloc(TextBlock::from(allocator.alloc(text).as_str()))
            }
        }
    };
}

/*

#[derive(Debug)]
pub struct JournalObj<'h, T, B>
where
    T: Into<TextBlock<'h>>,
    B: AsRef<TextBlock<'h>>,
{
    /// Will be `Some` if parsed from a file.
    block: Option<Cell<B>>,
    /// A copy of the configuration at the time of the parse.
    config: Option<Configuration<'h>>,
    obj: T,
}

impl<'h, T, B> JournalObj<'h, T, B>
where
    B: AsRef<TextBlock<'h>>,
    T: Into<TextBlock<'h>>,
{
    pub fn new_parsed(block: B, config: Configuration<'h>, parsed: T) -> Self {
        JournalObj { block: Some(Cell::new(block)), config: Some(config), obj: parsed }
    }

    pub fn new(obj: T) -> Self {
        JournalObj { block: None, config: None, obj }
    }

    pub fn obj(&self) -> &T {
        &self.obj
    }

    pub fn block(&self) -> Option<&TextBlock<'h>> {
        self.block.as_ref().map(|b| b.get_mut())
    }

    pub fn config(&self) -> Option<&Configuration<'h>> {
        self.config.as_ref()
    }

    pub fn as_block_input(&mut self) -> TextBlockInput<'h, ()> {
        let block = self.block.get_or_init(|| self.obj.into());
        match self.block {
            Some(block) => block.as_input(self.config.unwrap().allocator()),
            None => {
                self.block.set(self.obj.into());
                self.as_block_input()
            }
        }
    }
}

impl<'h, T, F> JournalObj<'h, LazyCell<T, F>>
where
    F: FnOnce() -> T,
    T: Into<TextBlock<'h>>,
{
    pub fn new_lazy(block: &'h TextBlock<'h>, config: Configuration<'h>, init: F) -> Self {
        JournalObj { block: Some(block), config: Some(config), obj: LazyCell::new(init) }
    }
}

impl<'h, T, F> From<LazyCell<T, F>> for TextBlock<'h>
where
    T: Into<TextBlock<'h>>,
{
    fn from(lazy: LazyCell<T, F>) -> Self {
        lazy.into()
    }
}*/
