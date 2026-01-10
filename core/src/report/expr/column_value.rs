/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::account::Account;
use crate::amount::Amount;
use crate::amounts::Amounts;
use crate::configuration::{AccountFilter, Filter};
use crate::datetime::{DateTimePrecision, JDate, JDateTime, JDateTimeRange};
use crate::error::JournResult;
use crate::report::command::arguments::Cmd;
use crate::report::table2::{BLANK_CELL, CellRef, EllipsisCell, MultiLineCell};
use crate::unit::{NumberFormat, Unit};
use crate::valued_amount::ValuedAmount;
use crate::{err, eval_identifier};
use chrono::MappedLocalTime;
use rust_decimal::Decimal;
use smartstring::alias::String as SS;
use std::cmp::Ordering;
use std::iter::Sum;
use std::sync::Arc;
use std::{fmt, iter, mem, slice};
use yaml_rust2::Yaml;

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
    Date(JDate),
    Datetime(JDateTime),
    DatetimeRange(JDateTimeRange),
    Number(Decimal),
    Amount(Amount<'h>),
    ValuedAmount(ValuedAmount<'h>),
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
            ColumnValue::Description(d) => Some(d),
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

    /// Like [Self::into_string()], except that this will return a string for any
    /// value, formatted in the canonical way.
    pub fn as_reporting_string(&self) -> String {
        match self {
            ColumnValue::Datetime(dt) => {
                let dtf = Cmd::args().datetime_cmd.datetime_format_or_default();
                let tz = Cmd::args().datetime_cmd.timezone().unwrap_or(dt.timezone());
                // format with max precision to avoid inconsistent formats in the reporting output
                format!("{}", dt.with_timezone(tz).format_with_precision(dtf, dtf.max_precision()))
            }
            ColumnValue::DatetimeRange(range) => {
                let dtf = Cmd::args().datetime_cmd.datetime_format_or_default();
                let mut s = String::new();
                range.write(&mut s, dtf).unwrap();
                s
            }
            ColumnValue::Date(date) => {
                let df = Cmd::args().datetime_cmd.date_format_or_default();
                format!("{}", date.format(df))
            }
            col => col.to_string(),
        }
    }

    pub fn as_date(&self) -> Option<JDate> {
        match self {
            ColumnValue::Datetime(dt) => Some(dt.date()),
            ColumnValue::Date(date) => Some(*date),
            _ => None,
        }
    }

    pub fn as_datetime(&self) -> Option<JDateTime> {
        match self {
            ColumnValue::Datetime(dt) => Some(*dt),
            ColumnValue::Date(dt) => {
                // Set as midnight in the configured timezone
                let tz = Cmd::get().datetime_fmt_cmd().timezone_or_default();
                let dt = match dt.and_hms_nano_opt(0, 0, 0, 0)?.and_local_timezone(tz) {
                    MappedLocalTime::None => None,
                    MappedLocalTime::Single(t) => Some(t),
                    MappedLocalTime::Ambiguous(t, _) => Some(t),
                }?;
                Some(JDateTime::new(dt, DateTimePrecision::Day))
            }
            _ => None,
        }
    }

    pub fn as_datetime_range(&self) -> Option<JDateTimeRange> {
        if let ColumnValue::DatetimeRange(range) = self { Some(*range) } else { None }
    }

    pub fn as_number(&self) -> Option<Decimal> {
        match self {
            ColumnValue::Amount(num) => Some(num.quantity()),
            ColumnValue::Number(num) => Some(*num),
            _ => None,
        }
    }

    pub fn cmp(&self, other: &ColumnValue<'h>) -> JournResult<Ordering> {
        self.as_amount()
            .and_then(|a| {
                other.as_amount().map(|b| {
                    if a.unit() != b.unit() {
                        Err(err!("Units must be the same when comparing amounts"))
                    } else {
                        Ok(a.cmp(&b))
                    }
                })
            })
            .transpose()?;

        let try_cmp = self
            .as_number()
            .and_then(|a| other.as_number().map(|b| a.cmp(&b)))
            .or_else(|| self.as_datetime().and_then(|a| other.as_datetime().map(|b| a.cmp(&b))))
            .or_else(|| {
                self.as_datetime_range().and_then(|a| other.as_datetime_range().map(|b| a.cmp(&b)))
            })
            .or_else(|| self.as_date().and_then(|a| other.as_date().map(|b| a.cmp(&b))))
            .or_else(|| self.as_str().and_then(|a| other.as_str().map(|b| a.cmp(b))))
            .or_else(|| self.as_undefined().and_then(|a| other.as_undefined().map(|b| a.cmp(&b))));

        match try_cmp {
            Some(cmp) => Ok(cmp),
            None => {
                if let (ColumnValue::List(a), ColumnValue::List(b)) = (self, other) {
                    // Compare like-wise until one element is no equal
                    let combined = a.iter().zip(b);
                    for (a, b) in combined {
                        let cmp = a.cmp(b)?;
                        if cmp != Ordering::Equal {
                            return Ok(cmp);
                        }
                    }
                    Ok(a.len().cmp(&b.len()))
                } else {
                    Err(err!("Unable to compare {} with {}", self, other))
                }
            }
        }
    }

    pub fn matches(&self, other: &ColumnValue<'h>) -> JournResult<bool> {
        match (self, other) {
            (ColumnValue::Account(a), ColumnValue::String(b)) => {
                Ok(AccountFilter::new(slice::from_ref(b).iter()).is_included(a))
            }
            _ => Err(err!("Cannot match {} with {}", self, other)),
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
            ColumnValue::Date(_date) => CellRef::Owned(Box::new(self.as_reporting_string())),
            ColumnValue::Datetime(_dt) => CellRef::Owned(Box::new(self.as_reporting_string())),
            ColumnValue::DatetimeRange(_range) => {
                CellRef::Owned(Box::new(self.as_reporting_string()))
            }
            ColumnValue::Account(acc) => CellRef::Owned(Box::new(acc)),
            ColumnValue::Unit(unit) => CellRef::Owned(Box::new(unit.to_string())),
            ColumnValue::Amount(amount) => CellRef::Owned(amount.into_cell(amount.unit().format())),
            // Format numbers similar to amounts using a number format
            ColumnValue::Number(qty) => {
                ColumnValue::Amount(Amount::nil().with_quantity(qty)).into_cell_ref(show_zeros)
            }
            ColumnValue::ValuedAmount(va) => CellRef::Owned(va.as_cell()),
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

    pub fn into_yaml(self, show_zeros: bool) -> Yaml {
        match self {
            ColumnValue::Undefined => Yaml::Null,
            ColumnValue::Boolean(b) => Yaml::Boolean(b),
            ColumnValue::String(s) => Yaml::String(s.to_string()),
            ColumnValue::StringRef(s) | ColumnValue::Description(s) => Yaml::String(s.to_string()),
            ColumnValue::Date(_) => Yaml::String(self.as_reporting_string()),
            ColumnValue::Datetime(_) => Yaml::String(self.as_reporting_string()),
            ColumnValue::DatetimeRange(_) => Yaml::String(self.as_reporting_string()),
            ColumnValue::Account(acc) => Yaml::String(acc.to_string()),
            ColumnValue::Unit(unit) => Yaml::String(unit.to_string()),
            ColumnValue::Amount(amount) => Yaml::String(amount.to_string()),
            // Don't format with thousands separators so that tools can parse it.
            ColumnValue::Number(qty) => {
                let mut s = String::new();
                let nf = NumberFormat::default();
                nf.write(qty, true, &mut s).unwrap();
                Yaml::String(s)
            }
            ColumnValue::ValuedAmount(va) => Yaml::String(va.to_string()),
            ColumnValue::List(mut values) => {
                values.sort();

                Yaml::Array(
                    values
                        .into_iter()
                        .filter(|cv| {
                            cv.as_amount().map(|a| !a.is_zero() || show_zeros).unwrap_or(true)
                        })
                        .map(|a| a.into_yaml(show_zeros))
                        .collect(),
                )
            }
        }
    }

    /// Evaluates a property identifier or sub-value on this `ColumnValue`.
    /// For example: "quantity" on an `Amount` value type.
    ///
    /// Returns `None` to indicate no such property exists.
    pub fn eval_identifier(self, identifier: &str) -> Option<ColumnValue<'h>> {
        // This will go unresolved without this
        if identifier.is_empty() {
            return Some(self);
        }

        use ColumnValue::*;
        eval_identifier!(identifier, ColumnValue<'h>, self,
            Amount(a) if "quantity" => Number(a.quantity()),
            Amount(a) if "unit" => Unit(a.unit()),
            Datetime(dt) if "date" => Date(dt.date()),
            DatetimeRange(range) if "start" => Datetime(range.start()),
            DatetimeRange(range) if "end" => Datetime(range.end()),
            // Fallback to code if name unavailable
            Unit(u) if "name" => u.name().map(|s| String(s.into())).unwrap_or(String(u.code().into())),
            Unit(u) if "format" => {
                let mut s = SS::new();
                u.format().write_spec(&mut s).unwrap();
                String(s)
            },
            Unit(u) if "rounding" => String(u.rounding_strategy().to_string().into()),
            Undefined => Undefined
        )
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
            ColumnValue::Date(_date) => write!(f, "{}", self.as_reporting_string()),
            ColumnValue::Datetime(_dt) => write!(f, "{}", self.as_reporting_string()),
            ColumnValue::DatetimeRange(_dt) => write!(f, "{}", self.as_reporting_string()),
            ColumnValue::Amount(amount) => write!(f, "{}", amount),
            ColumnValue::Number(qty) => write!(f, "{}", qty),
            ColumnValue::ValuedAmount(va) => write!(f, "{}", va),
            ColumnValue::List(values) => {
                write!(f, "{}", values.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", "))
            }
        }
    }
}

/*
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
            ColumnValue::ValuedAmount(va) => Cell::from(va),
            ColumnValue::List(values) => Cell::from(format!(
                "[{}]",
                values.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", ")
            )),
        }
    }
}*/

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

/// A simple sort algorithm for sorting ColumnValues when comparing them
/// might result in an error.
pub fn try_sort<'h>(v: &mut [ColumnValue<'h>]) -> JournResult<()> {
    for i in 1..v.len() {
        let mut j = i;
        while j > 0 {
            // Only call cmp, propagate errors with `?`
            if v[j - 1].cmp(&v[j])? != Ordering::Greater {
                break;
            }
            v.swap(j - 1, j);
            j -= 1;
        }
    }
    Ok(())
}
