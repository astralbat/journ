/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use journ_core::account::Account;
use journ_core::amount::Amount;
use journ_core::amounts::Amounts;
use journ_core::configuration::{AccountFilter, Filter};
use journ_core::date_and_time::{JDate, JDateTime};
use journ_core::reporting::table::Cell;
use journ_core::reporting::table2::{BLANK_CELL, CellRef, EllipsisCell, MultiLineCell};
use journ_core::unit::Unit;
use rust_decimal::Decimal;
use smartstring::alias::String as SS;
use std::iter::Sum;
use std::sync::Arc;
use std::{fmt, iter, mem, slice};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub enum ColumnValue<'h> {
    #[default]
    Undefined,
    Boolean(bool),
    Account(Arc<Account<'h>>),
    Unit(&'h Unit<'h>),
    Description(&'h str),
    String(SS),
    StringRef(&'h str),
    Date(JDate<'h>),
    Datetime(JDateTime<'h>),
    Amount(Amount<'h>),
    List(Vec<ColumnValue<'h>>),
}

impl<'h> ColumnValue<'h> {
    pub fn as_bool(&self) -> Option<bool> {
        if let ColumnValue::Boolean(b) = self { Some(*b) } else { None }
    }

    pub fn as_amount(&self) -> Option<Amount<'h>> {
        match self {
            ColumnValue::Amount(a) => Some(*a),
            _ => None,
        }
    }

    pub fn as_account(&self) -> Option<&Arc<Account<'h>>> {
        if let ColumnValue::Account(a) = self { Some(a) } else { None }
    }

    pub fn as_unit(&self) -> Option<&'h Unit<'h>> {
        match self {
            ColumnValue::Unit(u) => Some(u),
            ColumnValue::Amount(a) => Some(a.unit()),
            _ => None,
        }
    }

    pub fn as_amount_mut(&mut self) -> Option<&mut Amount<'h>> {
        match self {
            ColumnValue::Amount(a) => Some(a),
            _ => None,
        }
    }

    pub fn as_undefined(&self) -> Option<()> {
        if self.is_undefined() { Some(()) } else { None }
    }

    pub fn is_undefined(&self) -> bool {
        matches!(self, ColumnValue::Undefined)
    }

    pub fn as_list(&self) -> &[ColumnValue<'h>] {
        match self {
            ColumnValue::List(v) => v,
            other => slice::from_ref(other),
        }
    }

    pub fn as_list_mut(&mut self) -> &mut [ColumnValue<'h>] {
        match self {
            ColumnValue::List(v) => v,
            other => slice::from_mut(other),
        }
    }

    pub fn into_list(self) -> Vec<ColumnValue<'h>> {
        match self {
            ColumnValue::List(v) => v,
            other => vec![other],
        }
    }

    /*
    pub fn as_amounts(&self) -> Option<&[Amount<'h>]> {
        match self {
            ColumnValue::Amounts(amounts) => Some(amounts),
            ColumnValue::Amount(amount) => Some(std::slice::from_ref(amount)),
            _ => None,
        }
    }*/

    pub fn as_str(&self) -> Option<&str> {
        match self {
            ColumnValue::StringRef(s) => Some(s),
            ColumnValue::String(s) => Some(s.as_str()),
            ColumnValue::Account(a) => Some(a.name()),
            _ => None,
        }
    }

    pub fn into_string(self) -> Option<SS> {
        match self {
            ColumnValue::String(s) => Some(s),
            ColumnValue::StringRef(s) => Some(SS::from(s)),
            _ => None,
        }
    }

    pub fn as_date(&self) -> Option<JDate<'h>> {
        match self {
            ColumnValue::Datetime(dt) => Some(dt.date()),
            ColumnValue::Date(date) => Some(*date),
            _ => None,
        }
    }

    pub fn as_datetime(&self) -> Option<JDateTime<'h>> {
        if let ColumnValue::Datetime(dt) = self { Some(*dt) } else { None }
    }

    pub fn as_number(&self) -> Option<Decimal> {
        match self {
            ColumnValue::Amount(num) => Some(num.quantity()),
            _ => None,
        }
    }

    pub fn cmp(&self, other: &ColumnValue<'h>) -> Option<std::cmp::Ordering> {
        self.as_number()
            .and_then(|a| other.as_number().map(|b| a.cmp(&b)))
            .or_else(|| self.as_datetime().and_then(|a| other.as_datetime().map(|b| a.cmp(&b))))
            .or_else(|| self.as_date().and_then(|a| other.as_date().map(|b| a.cmp(&b))))
            .or_else(|| self.as_str().and_then(|a| other.as_str().map(|b| a.cmp(b))))
            .or_else(|| {
                if let (ColumnValue::List(a), ColumnValue::List(b)) = (self, other) {
                    // Everything in the two lists must compare the same way
                    let mut combined = a.iter().zip(b);
                    combined
                        .next()
                        .and_then(|(a, b)| a.cmp(b))
                        .filter(|&res| combined.all(|(a, b)| a.cmp(b) == Some(res)))
                } else {
                    None
                }
            })
    }

    pub fn matches(&self, other: &ColumnValue<'h>) -> Option<bool> {
        match (self, other) {
            (ColumnValue::Account(a), ColumnValue::String(b)) => {
                AccountFilter::new(slice::from_ref(b).iter()).is_included(a).into()
            }
            _ => None,
        }
    }

    /// Creates a flattened iterator over matching pairs of the same type for operations.
    pub fn map<'a>(
        &'a self,
        other: &'a ColumnValue<'h>,
    ) -> Box<dyn Iterator<Item = (Option<&'a ColumnValue<'h>>, Option<&'a ColumnValue<'h>>)> + 'a>
    {
        match (self, other) {
            (ColumnValue::List(list_a), ColumnValue::List(list_b)) => {
                let mut mapped = Vec::with_capacity(list_a.len());
                for a in list_a.iter() {
                    match list_b.iter().find(|b| a.map(b).all(|(x, y)| x.is_some() && y.is_some()))
                    {
                        // Found a, b pairing
                        Some(b) => mapped.push((Some(a), Some(b))),
                        // No match for a
                        None => mapped.push((Some(a), None)),
                    }
                }
                for b in list_b.iter() {
                    match list_a.iter().find(|a| b.map(a).all(|(x, y)| x.is_some() && y.is_some()))
                    {
                        Some(_) => {}
                        // No match for b
                        None => mapped.push((None, Some(b))),
                    }
                }
                Box::new(mapped.into_iter())
            }
            (a, ColumnValue::List(list_b)) => Box::new(list_b.iter().flat_map(|b| a.map(b))),
            (ColumnValue::List(list_a), b) => Box::new(list_a.iter().flat_map(|a| a.map(b))),
            (ColumnValue::Amount(a), ColumnValue::Amount(b)) => {
                if a.unit() == b.unit() || a.unit().is_none() || b.unit().is_none() {
                    Box::new(iter::once((Some(self), Some(other))))
                } else {
                    Box::new(iter::once((None, None)))
                }
            }
            (a, b) if mem::discriminant(a) == mem::discriminant(b) => {
                Box::new(iter::once((Some(a), Some(b))))
            }
            _ => Box::new(iter::once((None, None))),
        }
    }

    pub fn into_cell_ref(self, show_zeros: bool) -> CellRef<'h> {
        match self {
            ColumnValue::Undefined => CellRef::Borrowed(&BLANK_CELL),
            ColumnValue::Boolean(b) => CellRef::Owned(Box::new(b.to_string())),
            ColumnValue::String(s) => CellRef::Owned(Box::new(s)),
            ColumnValue::StringRef(s) => CellRef::Owned(Box::new(s)),
            ColumnValue::Description(s) => {
                CellRef::Owned(Box::new(EllipsisCell::new(CellRef::Owned(Box::new(s)))))
            }
            ColumnValue::Date(date) => CellRef::Owned(Box::new(date)),
            ColumnValue::Datetime(dt) => CellRef::Owned(Box::new(dt)),
            ColumnValue::Account(acc) => CellRef::Owned(Box::new(acc)),
            ColumnValue::Unit(unit) => CellRef::Owned(Box::new(unit.to_string())),
            ColumnValue::Amount(amount) => CellRef::Owned(amount.into_cell(amount.unit().format())),
            ColumnValue::List(mut values) => {
                values.sort();

                // Don't include zero amounts in the list
                CellRef::Owned(Box::new(MultiLineCell::new(
                    values
                        .into_iter()
                        .filter(|cv| {
                            cv.as_amount().map(|a| !a.is_zero() || show_zeros).unwrap_or(true)
                        })
                        .map(|a| a.into_cell_ref(show_zeros)),
                )))
            }
        }
    }
}

impl fmt::Display for ColumnValue<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ColumnValue::Undefined => write!(f, "UNDEFINED"),
            ColumnValue::Boolean(b) => write!(f, "{}", b),
            ColumnValue::String(s) => write!(f, "{}", s),
            ColumnValue::StringRef(s) | ColumnValue::Description(s) => write!(f, "{}", s),
            ColumnValue::Account(acc) => write!(f, "{}", acc),
            ColumnValue::Unit(unit) => write!(f, "{}", unit),
            ColumnValue::Date(date) => write!(f, "{}", date),
            ColumnValue::Datetime(dt) => write!(f, "{}", dt),
            ColumnValue::Amount(amount) => write!(f, "{}", amount),
            ColumnValue::List(values) => {
                write!(f, "{}", values.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", "))
            }
        }
    }
}

impl<'a> From<ColumnValue<'a>> for Cell<'a> {
    fn from(value: ColumnValue<'a>) -> Self {
        match value {
            ColumnValue::Undefined => Cell::from("UNDEFINED"),
            ColumnValue::Boolean(b) => Cell::from(b.to_string()),
            ColumnValue::String(s) => Cell::from(s.to_string()),
            ColumnValue::StringRef(s) => Cell::from(s),
            ColumnValue::Description(s) => Cell::from(s),
            ColumnValue::Date(date) => Cell::from(date),
            ColumnValue::Datetime(dt) => Cell::from(dt),
            ColumnValue::Account(acc) => Cell::from(acc.to_string()),
            ColumnValue::Unit(unit) => Cell::from(unit.to_string()),
            ColumnValue::Amount(amount) => Cell::from(amount),
            ColumnValue::List(values) => Cell::from(format!(
                "[{}]",
                values.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", ")
            )),
        }
    }
}

impl<'h, A: Amounts<'h>> From<A> for ColumnValue<'h> {
    fn from(amounts: A) -> Self {
        if amounts.is_empty() {
            return ColumnValue::Amount(Amount::nil());
        }
        if amounts.len() == 1 {
            return ColumnValue::Amount(amounts.as_slice()[0]);
        }
        let vec: Vec<ColumnValue> =
            amounts.as_slice().iter().map(|a| ColumnValue::Amount(*a)).collect();
        ColumnValue::List(vec)
    }
}

impl Sum for ColumnValue<'_> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut total: Option<Amount> = None;
        for cv in iter {
            if let Some(amount) = cv.as_amount() {
                if let Some(t) = &mut total {
                    *t += amount;
                } else {
                    total = Some(amount);
                }
            }
        }
        if let Some(t) = total { ColumnValue::Amount(t) } else { ColumnValue::Undefined }
    }
}

impl<'a, 'h> IntoIterator for &'a ColumnValue<'h> {
    type Item = &'a ColumnValue<'h>;
    type IntoIter = ColumnValueIter<'a, 'h>;
    fn into_iter(self) -> Self::IntoIter {
        ColumnValueIter::new(self)
    }
}

impl<'h> IntoIterator for ColumnValue<'h> {
    type Item = ColumnValue<'h>;
    type IntoIter = std::vec::IntoIter<ColumnValue<'h>>;
    fn into_iter(self) -> Self::IntoIter {
        match self {
            ColumnValue::List(v) => v.into_iter(),
            other => vec![other].into_iter(),
        }
    }
}

impl<'h, V: Into<ColumnValue<'h>>> FromIterator<V> for ColumnValue<'h> {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        let vec: Vec<ColumnValue> = iter.into_iter().map(|v| v.into()).collect();
        if vec.len() == 1 { vec.into_iter().next().unwrap() } else { ColumnValue::List(vec) }
    }
}

pub struct ColumnValueIter<'a, 'h> {
    values: &'a ColumnValue<'h>,
    index: usize,
}
impl<'a, 'h> ColumnValueIter<'a, 'h> {
    pub fn new(values: &'a ColumnValue<'h>) -> Self {
        Self { values, index: 0 }
    }
}
impl<'a, 'h> Iterator for ColumnValueIter<'a, 'h> {
    type Item = &'a ColumnValue<'h>;
    fn next(&mut self) -> Option<Self::Item> {
        let list = self.values.as_list();
        if self.index < list.len() {
            let value = &list[self.index];
            self.index += 1;
            Some(value)
        } else {
            None
        }
    }
}
