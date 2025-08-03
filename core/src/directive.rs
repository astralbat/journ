/*
 * Copyright (c) 2019-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::account::Account;
use crate::alloc::HerdAllocator;
use crate::date_and_time::{DateFormat, TimeFormat};
use crate::journal_entry::JournalEntry;
use crate::journal_node::JournalNode;
use crate::module::ModuleDirectiveObj;
use crate::parsing::text_block::TextBlock;
use crate::price::Price;
use crate::unit::{Unit, Units};
use chrono_tz::Tz;
use std::cmp::Ordering;
use std::fmt;
use std::sync::Arc;

//pub type ParsedJournalEntry<'h> = Arc<Mutex<ParsedDirective<'h, JournalEntry<'h>>>>;

#[derive(Debug)]
#[repr(u8)]
pub enum DirectiveKind<'h> {
    Entry(&'h JournalEntry<'h>) = 0,
    Price(Arc<Price<'h>>) = 1,
    Account(Arc<Account<'h>>) = 3,
    Unit(&'h Unit<'h>) = 4,
    Units(Units<'h>) = 2,
    Include(&'h JournalNode<'h>) = 5,
    Branch(&'h JournalNode<'h>) = 6,
    Python(&'h JournalNode<'h>) = 7,
    DateFormat(&'h DateFormat<'h>) = 8,
    TimeFormat(&'h TimeFormat<'h>) = 9,
    TimeZone(Tz) = 10,
    //Cgt(CgtConfiguration<'h>) = 11,
    Module(&'h dyn ModuleDirectiveObj) = 12,
    Comment(&'h str) = 13,
}

impl<'h> PartialEq<DirectiveKind<'h>> for DirectiveKind<'h> {
    fn eq(&self, other: &DirectiveKind<'h>) -> bool {
        match (self, other) {
            (DirectiveKind::Entry(entry), DirectiveKind::Entry(other_entry)) => {
                entry == other_entry
            }
            (DirectiveKind::Price(price), DirectiveKind::Price(other_price)) => {
                price == other_price
            }
            (DirectiveKind::Account(account), DirectiveKind::Account(other_account)) => {
                account == other_account
            }
            (DirectiveKind::Unit(unit), DirectiveKind::Unit(other_unit)) => unit == other_unit,
            (DirectiveKind::Units(unit), DirectiveKind::Units(other_unit)) => unit == other_unit,
            (DirectiveKind::Include(include), DirectiveKind::Include(other_include)) => {
                include == other_include
            }
            (DirectiveKind::Branch(branch), DirectiveKind::Branch(other_branch)) => {
                branch == other_branch
            }
            (DirectiveKind::Python(python), DirectiveKind::Python(other_python)) => {
                python == other_python
            }
            (
                DirectiveKind::DateFormat(date_format),
                DirectiveKind::DateFormat(other_date_format),
            ) => date_format == other_date_format,
            (
                DirectiveKind::TimeFormat(time_format),
                DirectiveKind::TimeFormat(other_time_format),
            ) => time_format == other_time_format,
            (DirectiveKind::TimeZone(time_zone), DirectiveKind::TimeZone(other_time_zone)) => {
                time_zone == other_time_zone
            }
            //(DirectiveKind::Cgt(cgt), DirectiveKind::Cgt(other_cgt)) => cgt == other_cgt,
            (DirectiveKind::Module(module_dir), DirectiveKind::Module(_other_module_dir)) => {
                &**module_dir == other
            }
            (DirectiveKind::Comment(comment), DirectiveKind::Comment(other_comment)) => {
                comment == other_comment
            }
            _ => false,
        }
    }
}

/// Some kinds of directive can be compared, for others, it doesn't make sense. This comparison
/// is used for sorting directives in a journal file such as prices, or inserting an entry in
/// date order.
impl PartialOrd for DirectiveKind<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (DirectiveKind::Entry(entry), DirectiveKind::Entry(other_entry)) => {
                Some(entry.cmp(other_entry))
            }
            (DirectiveKind::Price(price), DirectiveKind::Price(other_price)) => {
                Some(price.cmp(other_price))
            }
            (DirectiveKind::Account(account), DirectiveKind::Account(other_account)) => {
                Some(account.cmp(other_account))
            }
            (DirectiveKind::Unit(unit), DirectiveKind::Unit(other_unit)) => {
                Some(unit.cmp(other_unit))
            }
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct Directive<'h> {
    raw: &'h TextBlock<'h>,
    kind: DirectiveKind<'h>,
}

impl<'h> Directive<'h> {
    pub fn new(raw: &'h TextBlock<'h>, kind: DirectiveKind<'h>) -> Self {
        Self { raw, kind }
    }

    pub fn raw(&self) -> &'h TextBlock<'h> {
        self.raw
    }

    pub fn kind(&self) -> &DirectiveKind<'h> {
        &self.kind
    }

    pub fn kind_mut(&mut self) -> &mut DirectiveKind<'h> {
        &mut self.kind
    }

    pub fn into_inner(self) -> (&'h TextBlock<'h>, DirectiveKind<'h>) {
        (self.raw, self.kind)
    }

    /*
    fn raw_op<F, R>(&self, op: F) -> R
    where
        F: FnOnce(&TextBlock<'h>) -> R,
    {
        match self {
            Directive::Entry(dir) => op(&dir.lock().unwrap().raw),
            Directive::Price(dir) => op(&dir.raw),
            Directive::Account(dir) => op(&dir.raw),
            Directive::Unit(dir) => op(&dir.raw),
            Directive::DefaultUnit(dir) => op(&dir.raw),
            Directive::Branch(dir) => op(&dir.raw),
            Directive::Include(dir) => op(&dir.raw),
            Directive::Python(dir) => op(&dir.raw),
            Directive::DateFormat(dir) => op(&dir.raw),
            Directive::TimeFormat(dir) => op(&dir.raw),
            Directive::TimeZone(dir) => op(&dir.raw),
            Directive::Cgt(dir) => op(&dir.raw),
            Directive::Comment(dir) => op(&dir.raw),
            Directive::Raw(raw) => op(raw),
        }
    }

    fn raw_op_mut<F, R>(&mut self, op: F) -> R
    where
        F: FnOnce(&mut TextBlock<'h>) -> R,
    {
        match self {
            Directive::Entry(dir) => op(&mut dir.lock().unwrap().raw),
            Directive::Price(dir) => op(&mut dir.raw),
            Directive::Account(dir) => op(&mut dir.raw),
            Directive::Unit(dir) => op(&mut dir.raw),
            Directive::DefaultUnit(dir) => op(&mut dir.raw),
            Directive::Branch(dir) => op(&mut dir.raw),
            Directive::Include(dir) => op(&mut dir.raw),
            Directive::Python(dir) => op(&mut dir.raw),
            Directive::DateFormat(dir) => op(&mut dir.raw),
            Directive::TimeFormat(dir) => op(&mut dir.raw),
            Directive::TimeZone(dir) => op(&mut dir.raw),
            Directive::Cgt(dir) => op(&mut dir.raw),
            Directive::Comment(dir) => op(&mut dir.raw),
            Directive::Raw(dir) => op(dir),
        }
    }*/

    /*
    pub fn leading_whitespace<F, R>(&self, op: F) -> R
    where
        F: FnOnce(&str) -> R,
    {
        self.raw_op(|raw| {
            let contents = raw.text();
            op(&contents[0..contents.trim_start().len() - contents.len()])
        })
    }*/

    pub fn leading_whitespace(&self) -> &'h str {
        let contents = self.raw.text();
        &contents[0..contents.len() - contents.trim_start().len()]
    }

    pub fn set_leading_whitespace(
        &mut self,
        space: impl AsRef<str> + Sync + Send + 'static,
        allocator: &'h HerdAllocator<'h>,
    ) {
        self.raw = allocator.alloc(self.raw.with_leading_whitespace(space.as_ref(), allocator));
    }

    /// Gets the whitespace at the end of this directive
    pub fn trailing_white_space(&self) -> &'h str {
        let contents = self.raw.text();
        &contents[contents.trim_end().len()..]
    }

    pub fn line_count(&self) -> usize {
        self.raw.text().lines().count()
    }
}

impl<'h> fmt::Display for Directive<'h> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.raw.text())
    }
}

impl<'h> PartialEq<Directive<'h>> for Directive<'h> {
    fn eq(&self, other: &Directive<'h>) -> bool {
        self.kind == other.kind
    }
}

impl<'h> Eq for Directive<'h> {}

impl<'h> PartialOrd<Directive<'h>> for Directive<'h> {
    fn partial_cmp(&self, other: &Directive<'h>) -> Option<Ordering> {
        self.kind.partial_cmp(&other.kind)
    }
}

/*
pub struct DirectiveIndex(u64);

impl DirectiveIndex {
    pub fn new(file_number: u64, directive_line_num: u32) -> DirectiveIndex {
        DirectiveIndex((file_number << 32) + directive_line_num as u64)
    }
}*/

/*
#[derive(Debug, Clone)]
pub struct DirectiveLocation<'t> {
    node: Arc<JournalFile<'t>>,
    /// The line offset in the journal file from where this directive begins.
    line_offset: usize,
}
impl<'t> DirectiveLocation<'t> {
    pub fn new(node: Arc<JournalFile>, line_offset: usize) -> DirectiveLocation {
        DirectiveLocation { node, line_offset }
    }

    /// Gets the line number of the directive with the first line being 1.
    pub fn line_number(&self) -> usize {
        self.line_offset + 1
    }

    fn filename_or_text(&self) -> &str {
        self.node.stream().filename().unwrap_or(self.node.stream().text())
    }
}
impl<'t> fmt::Display for DirectiveLocation<'t> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.filename_or_text(), self.line_offset)
    }
}
impl<'t> PartialEq for DirectiveLocation<'t> {
    fn eq(&self, other: &Self) -> bool {
        self.line_offset == other.line_offset && self.filename_or_text() == other.filename_or_text()
    }
}
impl<'t> Eq for DirectiveLocation<'t> {}

impl<'t> PartialOrd for DirectiveLocation<'t> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'t> Ord for DirectiveLocation<'t> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.filename_or_text().cmp(other.filename_or_text()).then(self.line_offset.cmp(&other.line_offset))
    }
}*/
/*
pub struct RawDirective<'t> {
    location: Option<DirectiveLocation<'t>>,
    leading_space: Box<dyn AsRef<str> + Send + Sync>,
    contents: Box<dyn AsRef<str> + Send + Sync>,
}

impl<'t> RawDirective<'t> {
    pub fn new(
        leading_space: impl AsRef<str> + Sync + Send + 'static,
        contents: impl AsRef<str> + Sync + Send + 'static,
        location: Option<DirectiveLocation<'t>>,
    ) -> Self {
        Self { location, leading_space: Box::new(leading_space), contents: Box::new(contents) }
    }

    pub fn location(&self) -> Option<&DirectiveLocation<'t>> {
        self.location.as_ref()
    }

    /// Gets the contents
    pub fn contents(&self) -> &str {
        (*self.contents).as_ref()
    }

    pub fn leading_whitespace(&self) -> &str {
        (*self.leading_space).as_ref()
    }

    pub fn set_leading_whitespace(&mut self, space: impl AsRef<str> + Sync + Send + 'static) {
        self.leading_space = Box::new(space);
    }

    pub fn first_line(&self) -> &str {
        (*self.contents).as_ref().lines().next().unwrap().trim()
    }

    /// All raw trimmed lines within the directive
    pub fn sub_directives(&self) -> impl Iterator<Item = &str> {
        (*self.contents).as_ref().trim().lines().skip(1).map(|l| l.trim())
    }
}

impl<'t> PartialEq for RawDirective<'t> {
    fn eq(&self, other: &Self) -> bool {
        self.location == other.location
    }
}

impl<'t> Eq for RawDirective<'t> {}

impl<'t> Ord for RawDirective<'t> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.location.cmp(&other.location)
    }
}

impl<'t> PartialOrd for RawDirective<'t> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<'t> fmt::Display for RawDirective<'t> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", (*self.leading_space).as_ref(), (*self.contents).as_ref())
    }
}
impl<'t> fmt::Debug for RawDirective<'t> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(self, f)
    }
}*/

/*
#[derive(Debug, Eq)]
pub struct ParsedDirective<'h, T: 'h> {
    raw: TextBlock<'h>,
    item: T,
}

impl<'h, T> ParsedDirective<'h, T> {
    pub fn new(raw: TextBlock<'h>, item: T) -> Self {
        Self { raw, item }
    }

    pub fn raw(&self) -> &TextBlock<'h> {
        &self.raw
    }

    pub fn item(&self) -> &T {
        &self.item
    }

    pub fn item_mut(&mut self) -> &mut T {
        &mut self.item
    }

    pub fn into_inner(self) -> T {
        self.item
    }
}

impl<'h, T> PartialEq for ParsedDirective<'h, T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.item == other.item
    }
}

impl<'h, T> fmt::Display for ParsedDirective<'h, T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.item)
    }
}

macro_rules! impl_partial_cmp_pd {
    ($type:ty) => {
        impl<'h> PartialOrd for ParsedDirective<'h, $type> {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }
    };
}
macro_rules! impl_cmp_pd {
    ($type:ty) => {
        impl<'h> Ord for ParsedDirective<'h, $type> {
            fn cmp(&self, other: &Self) -> Ordering {
                self.item.cmp(&other.item)
            }
        }
    };
}
impl_partial_cmp_pd!(JournalEntry<'h>);
impl_cmp_pd!(JournalEntry<'h>);
impl_partial_cmp_pd!(Arc<Price<'h>>);
impl_cmp_pd!(Arc<Price<'h>>);
impl_partial_cmp_pd!(Arc<Account<'h>>);
impl_cmp_pd!(Arc<Account<'h>>);
impl_partial_cmp_pd!(&Unit<'_>);
impl_cmp_pd!(&Unit<'_>);
impl_partial_cmp_pd!(Arc<String>);
impl_cmp_pd!(Arc<String>);
impl PartialOrd for ParsedDirective<'_, Arc<Tz>> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.item.name().partial_cmp(other.item.name())
    }
}
impl<T> Deref for ParsedDirective<'_, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.item
    }
}
impl<T> DerefMut for ParsedDirective<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.item
    }
}

/// Directives and their components may have whitespace before the start of their content.
pub trait WhitespacePrefixed {
    fn leading_whitespace(&self) -> &'static str;
}*/
