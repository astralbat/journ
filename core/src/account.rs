/*
 * Copyright (c) 2019-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::configuration::{AccountFilter, Filter};
use crate::ext::StrExt;
use crate::journal_context::JContext;
use crate::metadata::Metadata;
use crate::report::table2::fmt::CellFormatter;
use crate::report::table2::{
    Cell, CellRef, CellWidth, ColumnWidth, EllipsisCell, EllipsisPolicy, PolicyWrappingCell,
    StyledCell, WrapEase, WrapPolicy,
};
use crate::report::term_style::{Colour, Style};
use crate::unit::Unit;
use smallvec::{SmallVec, smallvec};
use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::sync::Arc;
use std::{cmp, fmt, iter};

#[derive(Clone, Eq)]
pub struct Account<'h> {
    /// The full name of the account, including any parents
    name: String,
    /// All codes this account is known by, including this one.
    aliases: Vec<String>,
    /// The parent account will never be virtual
    parent: Option<Arc<Account<'h>>>,
    metadata: Vec<Metadata<'h>>,
    /// Unit definitions for parsed primary amounts on this account
    units: HashSet<&'h Unit<'h>>,
}

impl<'h> Account<'h> {
    pub fn new(
        name: String,
        parent: Option<Arc<Account<'h>>>,
        metadata: Vec<Metadata<'h>>,
        units: HashSet<&'h Unit<'h>>,
    ) -> Account<'h> {
        Account { aliases: vec![name.clone()], name, parent, metadata, units }
    }

    pub fn units(&self) -> &HashSet<&'h Unit<'h>> {
        &self.units
    }

    pub fn push_unit(&mut self, unit: &'h Unit<'h>) {
        self.units.insert(unit);
    }

    /// Gets the parent name of the account specified.
    ///
    /// # Examples
    /// ```
    /// use journ_core::account::Account;
    ///
    /// assert_eq!(Account::parent_str("A"), None);
    /// assert_eq!(Account::parent_str("A:B"), Some(String::from("A")));
    /// assert_eq!(Account::parent_str("(A:B:C)"), Some(String::from("(A:B)")));
    /// assert_eq!(Account::parent_str("[A:B:C:D]"), Some(String::from("[A:B:C]")));
    /// ```
    pub fn parent_str(full_name: &str) -> Option<String> {
        let round_brackets = full_name.starts_with('(') && full_name.ends_with(')');
        let square_brackets = full_name.starts_with('[') && full_name.ends_with(']');

        full_name.rfind(':').map(|pos| {
            let mut ret = full_name[0..pos].to_string();
            if round_brackets {
                ret += ")";
            } else if square_brackets {
                ret += "]";
            }
            ret
        })
    }

    /// The exact name of the account, including any brackets
    pub fn name_exact(&self) -> &str {
        &self.name
    }

    /// Gets the full name of the account
    pub fn name(&self) -> &str {
        if self.is_virtual() { &self.name[1..self.name.len() - 1] } else { &self.name }
    }

    /// Gets the primary code, i.e. the first alias.
    pub fn primary_name(&self) -> &str {
        self.aliases.first().unwrap()
    }

    /// Gets all known aliases for this account, which is a list of other account names, including the name of this account as well.
    /// The first alias will always be the "primary" name; the first name defined for this account.
    pub fn aliases(&self) -> impl Iterator<Item = &str> {
        self.aliases.iter().map(|s| s.as_str())
    }

    /// Sets all known aliases for this account. The aliases must include this account's name.
    pub fn set_aliases(&mut self, aliases: Vec<String>) {
        assert!(
            self.name.is_empty() || aliases.contains(&self.name),
            "Aliases must contain the primary name"
        );
        assert_eq!(
            aliases.iter().collect::<HashSet<_>>().len(),
            aliases.len(),
            "Aliases must be unique"
        );

        self.aliases = aliases
    }

    /// Gets the last, non-parent part of the account name.
    pub fn last_part(&self) -> &str {
        match &self.parent {
            Some(parent) => &self.name[parent.name.len() + 1..],
            None => self.name.as_str(),
        }
    }

    pub fn matches(&self, pattern: &str) -> bool {
        AccountFilter::new(iter::once(pattern)).is_included(self)
    }

    pub fn parent(&self) -> Option<&Arc<Account<'h>>> {
        self.parent.as_ref()
    }

    pub fn is_real(&self) -> bool {
        !self.is_virtual()
    }

    pub fn is_balanced(&self) -> bool {
        !self.is_virtual() || self.is_virtual_balanced()
    }

    pub fn is_virtual(&self) -> bool {
        self.is_virtual_balanced() || self.is_virtual_unbalanced()
    }

    pub fn is_virtual_unbalanced(&self) -> bool {
        self.name.starts_with('(') && self.name.ends_with(')')
    }

    pub fn is_virtual_balanced(&self) -> bool {
        self.name.starts_with('[') && self.name.ends_with(']')
    }

    pub fn lazy_metadata(&self) -> &Vec<Metadata<'h>> {
        &self.metadata
    }

    /// Gets an iterator over the parents of this account, starting with the top-most parent.
    fn parents_from_root(&self) -> impl Iterator<Item = &Arc<Account<'h>>> {
        let parents_to_root: SmallVec<[&Arc<Account<'h>>; 8]> =
            iter::successors(self.parent.as_ref(), |parent| parent.parent.as_ref()).collect();
        parents_to_root.into_iter().rev()
    }

    /// Gets an iterator over all metadata for this account and its parents, starting with the top-most parent
    /// and working down.
    pub fn metadata<'a>(&'a self) -> impl Iterator<Item = &'a Metadata<'h>> + 'a {
        self.parents_from_root()
            .flat_map(|parent| parent.metadata.iter())
            .chain(self.metadata.iter())
    }

    /// Gets whether this account or a parent has the specified metadata key.
    pub fn has_metadata_key<K: AsRef<str>>(&self, key: &K) -> bool {
        if let Some(parent) = &self.parent
            && parent.has_metadata_key(key)
        {
            return true;
        }
        self.metadata.iter().any(|m| m.key() == key.as_ref())
    }

    /// Gets the metadata for a _single_ key from a set of possible `keys`. First searches this account for
    /// all `keys` before searching the parent account in the same way recursively.
    pub fn metadata_by_keys<K>(&self, keys: &[&K]) -> SmallVec<[&Metadata<'h>; 4]>
    where
        K: AsRef<str>,
    {
        for key in keys {
            let md_key = key.as_ref();
            if self.metadata.iter().filter(|m| m.key() == md_key).count() > 0 {
                return self
                    .metadata
                    .iter()
                    .filter(|m| m.key() == md_key)
                    //.map(|m| m.get_or_init())
                    .collect();
            }
        }
        if let Some(parent) = &self.parent {
            return parent.metadata_by_keys(keys);
        }
        smallvec![]
    }

    /// Searches for all metadata for this account matching the specified `key`, and if not found,
    /// proceeding to search the parent.
    pub fn metadata_by_key(&self, key: &str) -> SmallVec<[&Metadata<'h>; 4]> {
        if self.metadata.iter().filter(|m| m.key() == key).count() > 0 {
            return self
                .metadata
                .iter()
                .filter(|m| m.key() == key)
                //.map(|m| m.get_or_init())
                .collect();
        }
        if let Some(parent) = &self.parent {
            return parent.metadata_by_key(key);
        }
        smallvec![]
    }

    /// Gets the longest account that `self` and `other` share.
    pub fn common_parent<'a>(
        left: &'a Arc<Account<'h>>,
        right: &'a Arc<Account<'h>>,
    ) -> Option<&'a Arc<Account<'h>>> {
        if left.name == right.name {
            Some(left)
        } else if left.name.len() > right.name.len() {
            left.parent.as_ref().and_then(|parent| Account::common_parent(parent, right))
        } else {
            right.parent.as_ref().and_then(|parent| Account::common_parent(parent, left))
        }
    }

    pub fn into_cell(self: Arc<Account<'h>>) -> Box<dyn Cell + 'h> {
        let wrap = JContext::get().cmd().table_fmt_cmd().wrap();
        let boxed: Box<dyn Cell> = if wrap {
            let mut wrapping_account =
                PolicyWrappingCell::new(Box::new(self), WrapPolicy::AfterStr(":"));
            wrapping_account.set_wrap_ease(WrapEase::Reluctant);
            Box::new(wrapping_account)
        } else {
            Box::new(EllipsisCell::new(Box::new(self), EllipsisPolicy::AfterMidSep(':')))
        };

        Box::new(StyledCell::new(CellRef::Owned(boxed), Style::default().with_fg(Colour::Blue)))
    }
}

impl fmt::Display for Account<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.pad(&self.name)
    }
}

impl fmt::Debug for Account<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.pad(&self.name)
    }
}

impl PartialEq for Account<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.primary_name().eq_ignore_ascii_case(other.primary_name())
    }
}

impl PartialEq<str> for Account<'_> {
    fn eq(&self, other: &str) -> bool {
        self.aliases.iter().any(|a| a.eq_ignore_ascii_case(other))
    }
}

impl Ord for Account<'_> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        if self == other {
            return cmp::Ordering::Equal;
        }
        self.name().cmp_ignore_case_ascii(other.name())
    }
}

impl PartialOrd for Account<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Hash for Account<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.primary_name().hash(state)
    }
}

impl<'t, S> From<S> for Account<'t>
where
    S: AsRef<str>,
{
    fn from(s: S) -> Account<'t> {
        let parent = Account::parent_str(s.as_ref()).map(|s| Arc::new(Account::from(s)));
        Account::new(s.as_ref().to_string(), parent, vec![], HashSet::new())
    }
}

macro_rules! cell_from_account {
    ($t:ty) => {
        impl Cell for $t {
            fn print<'format>(
                &self,
                f: &mut dyn CellFormatter,
                line: usize,
                _width: Option<ColumnWidth>,
            ) -> fmt::Result {
                if line == 0 { write!(f, "{}", self.name()) } else { Err(fmt::Error) }
            }

            fn width(&self) -> CellWidth {
                CellWidth::Leaf(self.name().chars().count())
            }

            fn height(&self) -> usize {
                // An account cannot have a newline.
                1
            }
        }
    };
}
cell_from_account!(&Account<'_>);
cell_from_account!(Arc<Account<'_>>);

#[cfg(test)]
mod tests {}
