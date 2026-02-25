/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::configuration::Configuration;
use crate::parsing::DerefMutAndDebug;
use crate::parsing::input::TextBlockInput;
use crate::parsing::parser::JournalParseNode;
use nom_locate::LocatedSpan;
use std::cell::RefCell;

pub trait LocatedInput<'h> {
    fn file(&self) -> Option<&'h str>;

    /// Gets the current line number where the first line is 1.
    fn line(&self) -> u32;

    /// Gets the current column index of the input where the first column is 1.
    fn column(&self) -> u32;

    fn location_offset(&self) -> usize;

    fn into_located_span<X>(self, extra: X) -> LocatedSpan<&'h str, X>;
}

macro_rules! impl_located_input1 {
    ($ty:ty) => {
        impl<'h, 's> LocatedInput<'h> for $ty {
            fn file(&self) -> Option<&'h str> {
                None
            }

            fn line(&self) -> u32 {
                self.location_line()
            }

            fn column(&self) -> u32 {
                self.naive_get_utf8_column() as u32
            }

            fn location_offset(&self) -> usize {
                self.location_offset()
            }

            fn into_located_span<X>(self, extra: X) -> LocatedSpan<&'h str, X> {
                self.map_extra(|_| extra)
            }
        }
    };
}
impl_located_input1!(LocatedSpan<&'h str, ()>);
impl_located_input1!(
    LocatedSpan<&'h str, &RefCell<dyn DerefMutAndDebug<'h, 's, Configuration<'h>>>>
);
impl_located_input1!(LocatedSpan<&'h str, RefCell<Configuration<'h>>>);
impl_located_input1!(LocatedSpan<&'h str, RefCell<&mut Configuration<'h>>>);

macro_rules! impl_located_input2 {
    ($ty:ty) => {
        impl<'h, 's> LocatedInput<'h> for $ty {
            fn file(&self) -> Option<&'h str> {
                None
            }

            fn line(&self) -> u32 {
                self.inner.location_line()
            }

            fn column(&self) -> u32 {
                self.inner.naive_get_utf8_column() as u32
            }

            fn location_offset(&self) -> usize {
                self.inner.location_offset()
            }

            fn into_located_span<X>(self, extra: X) -> LocatedSpan<&'h str, X> {
                self.inner.map_extra(|_| extra)
            }
        }
    };
}
impl_located_input2!(TextBlockInput<'h, ()>);
impl_located_input2!(TextBlockInput<'h, &RefCell<dyn DerefMutAndDebug<'h, 's, Configuration<'h>>>>);
impl_located_input2!(TextBlockInput<'h, RefCell<Configuration<'h>>>);
impl_located_input2!(TextBlockInput<'h, RefCell<&mut Configuration<'h>>>);

impl<'h, 's, 'p> LocatedInput<'h> for LocatedSpan<&'h str, &'p JournalParseNode<'h, 's>>
where
    'h: 's,
{
    fn file(&self) -> Option<&'h str> {
        self.extra.node().nearest_filename().map(|f| f.to_str().unwrap())
    }

    fn line(&self) -> u32 {
        self.location_line()
    }

    fn column(&self) -> u32 {
        self.naive_get_utf8_column() as u32
    }

    fn location_offset(&self) -> usize {
        self.location_offset()
    }

    fn into_located_span<X2>(self, extra: X2) -> LocatedSpan<&'h str, X2> {
        self.map_extra(|_| extra)
    }
}

impl<'h, 's, 'p> LocatedInput<'h> for TextBlockInput<'h, &'p JournalParseNode<'h, 's>>
where
    'h: 's,
{
    fn file(&self) -> Option<&'h str> {
        self.inner.extra.node().nearest_filename().map(|f| f.to_str().unwrap())
    }

    fn line(&self) -> u32 {
        self.inner.location_line()
    }

    fn column(&self) -> u32 {
        self.inner.naive_get_utf8_column() as u32
    }

    fn location_offset(&self) -> usize {
        self.inner.location_offset()
    }

    fn into_located_span<X>(self, extra: X) -> LocatedSpan<&'h str, X> {
        self.inner.map_extra(|_| extra)
    }
}
