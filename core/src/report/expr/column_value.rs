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
use crate::journal_context::JContext;
use crate::report::table2::{BLANK_CELL, CellRef, EllipsisCell, MultiLineCell, StyledCell};
use crate::report::term_style::{Colour, Style};
use crate::unit::{NumberFormat, Unit};
use crate::valued_amount::ValuedAmount;
use crate::{err, eval_identifier};
use chrono::MappedLocalTime;
use rust_decimal::Decimal;
use smartstring::alias::String as SS;
use std::cmp::Ordering;
use std::iter::Sum;
use std::ops::{Add, AddAssign};
use std::sync::Arc;
use std::{fmt, iter, mem, slice};
use yaml_rust2::Yaml;

#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
pub enum ColumnValue<'h> {
    #[default]
    Undefined,
    Boolean(bool),
    Account(Arc<Account<'h>>),
    Unit(&'h Unit<'h>),
    Description(SS),
    String(SS),
    StringRef(&'h str),
    Date(JDate),
    Datetime(JDateTime),
    DatetimeRange(JDateTimeRange),
    Number(Decimal),
    /// An amount and whether to format precisely
    Amount(Amount<'h>, bool),
    ValuedAmount(ValuedAmount<'h>),
    List(Vec<ColumnValue<'h>>),
}

impl<'h> ColumnValue<'h> {
    pub fn as_bool(&self) -> Option<bool> {
        if let ColumnValue::Boolean(b) = self { Some(*b) } else { None }
    }

    /// Generously interprets a value as a boolean, returning `false` for undefined values and `true` for any other non-boolean value.
    pub fn as_lenient_bool(&self) -> bool {
        match self {
            ColumnValue::Boolean(b) => *b,
            ColumnValue::Undefined => false,
            _ => true,
        }
    }

    pub fn as_amount(&self) -> Option<(Amount<'h>, bool)> {
        match self {
            ColumnValue::Amount(a, precise) => Some((*a, *precise)),
            _ => None,
        }
    }

    pub fn as_valued_amount(&self) -> Option<&ValuedAmount<'h>> {
        match self {
            ColumnValue::ValuedAmount(a) => Some(a),
            _ => None,
        }
    }

    pub fn as_account(&self) -> Option<&Arc<Account<'h>>> {
        if let ColumnValue::Account(a) = self { Some(a) } else { None }
    }

    pub fn as_unit(&self) -> Option<&'h Unit<'h>> {
        match self {
            ColumnValue::Unit(u) => Some(u),
            ColumnValue::Amount(a, _) => Some(a.unit()),
            _ => None,
        }
    }

    pub fn as_amount_mut(&mut self) -> Option<&mut Amount<'h>> {
        match self {
            ColumnValue::Amount(a, _) => Some(a),
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
                let dtf = JContext::get().cmd().datetime_fmt_cmd().datetime_format_or_default();
                let tz =
                    JContext::get().cmd().datetime_fmt_cmd().timezone().unwrap_or(dt.timezone());
                // format with max precision to avoid inconsistent formats in the reporting output
                format!("{}", dt.with_timezone(tz).format_with_precision(dtf, dtf.max_precision()))
            }
            ColumnValue::DatetimeRange(range) => {
                let dtf = JContext::get().cmd().datetime_fmt_cmd().datetime_format_or_default();
                let tz =
                    JContext::get().cmd().datetime_fmt_cmd().timezone().unwrap_or(range.timezone());
                let mut s = String::new();
                range.with_timezone(tz).write(&mut s, dtf).unwrap();
                s
            }
            ColumnValue::Date(date) => {
                let df = JContext::get().cmd().datetime_fmt_cmd().datetime_format_or_default();
                format!("{}", date.format(df))
            }
            col => col.to_string(),
        }
    }

    pub fn as_type_string(&self) -> String {
        match self {
            ColumnValue::Undefined => "UNDEFINED".to_string(),
            ColumnValue::Boolean(b) => format!("Boolean({})", b),
            ColumnValue::Account(a) => format!("Account({})", a),
            ColumnValue::Unit(u) => format!("Unit({})", u),
            ColumnValue::Description(d) => format!("Description({})", d),
            ColumnValue::String(s) => format!("String({})", s),
            ColumnValue::StringRef(s) => format!("String({})", s),
            ColumnValue::Date(_dt) => format!("Date({})", self.as_reporting_string()),
            ColumnValue::Datetime(dt) => format!("Datetime({})", dt),
            ColumnValue::DatetimeRange(range) => format!("DatetimeRange({})", range),
            ColumnValue::Number(n) => format!("Number({})", n),
            ColumnValue::Amount(a, _) => format!("Amount({})", a),
            ColumnValue::ValuedAmount(a) => format!("ValuedAmount({})", a),
            ColumnValue::List(_l) => format!("List({})", self.as_reporting_string()),
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
                let tz = JContext::get().cmd().datetime_fmt_cmd().timezone_or_default();
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
            ColumnValue::Amount(num, true) => Some(num.quantity()),
            ColumnValue::Amount(num, false) => Some(num.rounded().quantity()),
            ColumnValue::Number(num) => Some(*num),
            _ => None,
        }
    }

    pub fn try_cmp(&self, other: &ColumnValue<'h>) -> JournResult<Ordering> {
        self.partial_cmp(other).ok_or_else(|| {
            err!("Unable to compare {} with {}", self.as_type_string(), other.as_type_string())
        })
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
            (ColumnValue::Amount(a, _), ColumnValue::Amount(b, _)) => {
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

    pub fn into_cell_ref(self, show_zeros: bool, show_multi: bool) -> CellRef<'h> {
        const DATE_COLOUR: Colour = Colour::Cyan;
        match self {
            ColumnValue::Undefined => CellRef::Borrowed(&BLANK_CELL),
            ColumnValue::Boolean(b) => CellRef::Owned(Box::new(b.to_string())),
            ColumnValue::String(s) => CellRef::Owned(Box::new(s)),
            ColumnValue::StringRef(s) => CellRef::Owned(Box::new(s)),
            ColumnValue::Description(s) => {
                CellRef::Owned(Box::new(EllipsisCell::new(CellRef::Owned(Box::new(s)))))
            }
            ColumnValue::Date(_date) => CellRef::Owned(Box::new(StyledCell::new(
                self.as_reporting_string(),
                Style::default().with_fg(DATE_COLOUR),
            ))),
            ColumnValue::Datetime(_dt) => CellRef::Owned(Box::new(StyledCell::new(
                self.as_reporting_string(),
                Style::default().with_fg(DATE_COLOUR),
            ))),
            ColumnValue::DatetimeRange(_range) => CellRef::Owned(Box::new(StyledCell::new(
                self.as_reporting_string(),
                Style::default().with_fg(DATE_COLOUR),
            ))),
            ColumnValue::Account(acc) => CellRef::Owned(acc.into_cell()),
            ColumnValue::Unit(unit) => CellRef::Owned(Box::new(unit.code())),
            ColumnValue::Amount(amount, false) => {
                CellRef::Owned(amount.into_cell(amount.unit().format()))
            }
            ColumnValue::Amount(amount, true) => {
                CellRef::Owned(amount.into_cell(&amount.unit().format().as_precise()))
            }
            // Format numbers similar to amounts using a number format
            ColumnValue::Number(qty) => ColumnValue::Amount(Amount::nil().with_quantity(qty), true)
                .into_cell_ref(show_zeros, show_multi),
            ColumnValue::ValuedAmount(va) => CellRef::Owned(va.as_cell()),
            ColumnValue::List(mut values) => {
                if show_multi {
                    values.sort_by(|a, b| a.partial_cmp(b).unwrap_or(Ordering::Equal));

                    // Don't include zero amounts in the list
                    CellRef::Owned(Box::new(MultiLineCell::new(
                        values
                            .into_iter()
                            .filter(|cv| {
                                cv.as_amount()
                                    .map(|(a, _)| !a.is_zero() || show_zeros)
                                    .unwrap_or(true)
                            })
                            .map(|a| a.into_cell_ref(show_zeros, show_multi)),
                    )))
                } else {
                    CellRef::Borrowed(&BLANK_CELL)
                }
            }
        }
    }

    pub fn into_yaml(self, show_zeros: bool) -> Yaml {
        match self {
            ColumnValue::Undefined => Yaml::Null,
            ColumnValue::Boolean(b) => Yaml::Boolean(b),
            ColumnValue::String(s) => Yaml::String(s.to_string()),
            ColumnValue::StringRef(s) => Yaml::String(s.to_string()),
            ColumnValue::Description(s) => Yaml::String(s.to_string()),
            ColumnValue::Date(_) => Yaml::String(self.as_reporting_string()),
            ColumnValue::Datetime(_) => Yaml::String(self.as_reporting_string()),
            ColumnValue::DatetimeRange(_) => Yaml::String(self.as_reporting_string()),
            ColumnValue::Account(acc) => Yaml::String(acc.to_string()),
            ColumnValue::Unit(unit) => Yaml::String(unit.to_string()),
            ColumnValue::Amount(amount, false) => Yaml::String(amount.to_string()),
            ColumnValue::Amount(amount, true) => Yaml::String(amount.format_precise().to_string()),
            // Don't format with thousands separators so that tools can parse it.
            ColumnValue::Number(qty) => {
                let mut s = String::new();
                let nf = NumberFormat::default();
                nf.write(qty, true, &mut s).unwrap();
                Yaml::String(s)
            }
            ColumnValue::ValuedAmount(va) => Yaml::String(va.to_string()),
            ColumnValue::List(mut values) => {
                values.sort_by(|a, b| a.partial_cmp(b).unwrap_or(Ordering::Equal));

                Yaml::Array(
                    values
                        .into_iter()
                        .filter(|cv| {
                            cv.as_amount().map(|(a, _)| !a.is_zero() || show_zeros).unwrap_or(true)
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
        // We will overflow the stack without this.
        if identifier.is_empty() {
            return Some(self);
        }

        use ColumnValue::*;
        eval_identifier!(identifier, ColumnValue<'h>, self,
            Account(acc) if "name" => Account(Arc::new(acc.last_part().into())),
            Account(acc) if "parent" => match acc.parent() {
                Some(parent) => Account(Arc::clone(parent)),
                None => Undefined
            },
            Account(acc) => |ident: &str| {
                if ident.starts_with('+') {
                        acc.metadata()
                        .find(|m| m.key() == &identifier[1..])
                        .map(|m| m.value().map(|v| Some(String(SS::from(v)))).unwrap_or(Some(String(SS::new()))))
                        .unwrap_or(Some(Undefined))
                } else {
                    None
                }
            },
            Amount(a, true) if "quantity" => Number(a.quantity()),
            Amount(a, false) if "quantity" => Number(a.rounded().quantity()),
            Amount(a, _) if "unit" => Unit(a.unit()),
            Datetime(dt) if "date" => Date(dt.date()),
            DatetimeRange(range) if "start" => Datetime(range.start()),
            DatetimeRange(range) if "end" => Datetime(range.end()),
            DatetimeRange(range) if "mid" => Datetime(range.average()),
            // Fallback to code if name unavailable
            Unit(u) if "name" => u.name().map(|s| String(s.into())).unwrap_or(String(u.code().into())),
            Unit(u) if "format" => {
                let mut s = SS::new();
                u.format().write_spec(&mut s).unwrap();
                String(s)
            },
            Unit(u) if "rounding" => String(u.rounding_strategy().to_string().into()),
            Unit(u) => |ident: &str| {
                if ident.starts_with('+') {
                    u.metadata().iter()
                    .find(|m| m.key() == &ident[1..])
                        .map(|m| m.value().map(StringRef).map(Some).unwrap_or(Some(StringRef(""))))
                        .unwrap_or(Some(Undefined))
                } else {
                    None
                }
            },
            Undefined => |_| Some(Undefined),
            _ => |ident: &str| {
                if ident.is_empty() {
                    Some(self)
                } else {
                    None
                }
            }
        )
    }
}

impl fmt::Display for ColumnValue<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ColumnValue::Undefined => write!(f, "UNDEFINED"),
            ColumnValue::Boolean(b) => write!(f, "{}", b),
            ColumnValue::String(s) => write!(f, "{}", s),
            ColumnValue::StringRef(s) => write!(f, "{}", s),
            ColumnValue::Description(s) => write!(f, "{}", s),
            ColumnValue::Account(acc) => write!(f, "{}", acc),
            // Calling code() directly means we don't get quotes. Quotes aren't necessary and take column space.
            ColumnValue::Unit(unit) => write!(f, "{}", unit.code()),
            ColumnValue::Date(_date) => write!(f, "{}", self.as_reporting_string()),
            ColumnValue::Datetime(_dt) => write!(f, "{}", self.as_reporting_string()),
            ColumnValue::DatetimeRange(_dt) => write!(f, "{}", self.as_reporting_string()),
            ColumnValue::Amount(amount, _) => write!(f, "{}", amount),
            ColumnValue::Number(qty) => write!(f, "{}", qty),
            ColumnValue::ValuedAmount(va) => write!(f, "{}", va),
            ColumnValue::List(values) => {
                write!(f, "{}", values.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", "))
            }
        }
    }
}

impl PartialOrd for ColumnValue<'_> {
    /// The comparison of `ColumnValues` may result in a `None` when either is `undefined`.
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.is_undefined() || other.is_undefined() {
            return None;
        }

        match (self, other) {
            (ColumnValue::Undefined, _) | (_, ColumnValue::Undefined) => None,
            (ColumnValue::List(a), ColumnValue::List(b)) => a.partial_cmp(b),
            (ColumnValue::Boolean(a), ColumnValue::Boolean(b)) => Some(a.cmp(b)),
            (ColumnValue::Amount(a, _), ColumnValue::Amount(b, _)) => Some(a.cmp(b)),
            (ColumnValue::Number(a), ColumnValue::Number(b)) => Some(a.cmp(b)),
            (ColumnValue::Amount(a, _), ColumnValue::Number(b)) => Some(a.quantity().cmp(b)),
            (ColumnValue::Number(a), ColumnValue::Amount(b, _)) => Some(a.cmp(&b.quantity())),
            (ColumnValue::Datetime(a), ColumnValue::Datetime(b)) => Some(a.cmp(b)),
            (ColumnValue::DatetimeRange(a), ColumnValue::DatetimeRange(b)) => Some(a.cmp(b)),
            (ColumnValue::ValuedAmount(a), ColumnValue::ValuedAmount(b)) => {
                Some(a.amount().cmp(&b.amount()))
            }
            (a, b) if a.as_str().is_some() && b.as_str().is_some() => {
                Some(a.as_str().unwrap().cmp(b.as_str().unwrap()))
            }
            (a, b) if a.as_date().is_some() && b.as_date().is_some() => {
                Some(a.as_date().unwrap().cmp(&b.as_date().unwrap()))
            }
            (a, b) => Some(a.as_reporting_string().cmp(&b.as_reporting_string())),
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
            return ColumnValue::Amount(Amount::nil(), false);
        }
        if amounts.len() == 1 {
            return ColumnValue::Amount(amounts.as_slice()[0], false);
        }
        let vec: Vec<ColumnValue> =
            amounts.as_slice().iter().map(|a| ColumnValue::Amount(*a, false)).collect();
        ColumnValue::List(vec)
    }
}

impl<'h> Add for &ColumnValue<'h> {
    type Output = Option<ColumnValue<'h>>;

    /// Gets whether the left and right can be added without creating a new list.
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ColumnValue::Undefined, ColumnValue::Undefined) => Some(ColumnValue::Undefined),
            (ColumnValue::Number(a), ColumnValue::Number(b)) => Some(ColumnValue::Number(a + b)),
            (ColumnValue::Amount(a, a_precise), ColumnValue::Amount(b, b_precise))
                if a.unit() == b.unit() =>
            {
                Some(ColumnValue::Amount(a + b, *a_precise || *b_precise))
            }
            (ColumnValue::List(a), ColumnValue::List(b)) => {
                let mut res = vec![];
                'next_r: for r in b {
                    for l in a.iter() {
                        if let Some(sum) = l + r {
                            res.push(sum);
                            continue 'next_r;
                        } else {
                            res.push(l.clone());
                        }
                    }
                    res.push(r.clone());
                }
                Some(ColumnValue::List(res))
            }
            (ColumnValue::List(list), other) | (other, ColumnValue::List(list)) => {
                let mut res = list.clone();
                res.push(other.clone());
                Some(ColumnValue::List(res))
            }
            _ => None,
        }
    }
}

impl AddAssign for ColumnValue<'_> {
    /// Adds one `ColumnValue` to another. We defer to [ColumnValue::add] to sum in simple cases and
    /// whether one value is add-compatible with another - otherwise a list is formed.
    fn add_assign(&mut self, rhs: Self) {
        // Optimise list handling to avoid unnecessary allocation, otherwise defer to add()
        match (self, rhs) {
            (left, rhs) if left.is_undefined() => *left = rhs,
            (_, ColumnValue::Undefined) => {}
            (ColumnValue::List(left), ColumnValue::List(right)) => {
                'next_r: for r in right {
                    for l in left.iter_mut() {
                        if let Some(sum) = &*l + &r {
                            *l = sum;
                            continue 'next_r;
                        }
                    }
                    left.push(r);
                }
            }
            (ColumnValue::List(left), right) => {
                for l in left.iter_mut() {
                    if let Some(sum) = &*l + &right {
                        *l = sum;
                        return;
                    }
                }
                // Avoid duplicates in the list
                if !left.contains(&right) {
                    left.push(right);
                }
            }
            (l, r) => match &*l + &r {
                Some(sum) => *l = sum,
                // Avoid duplicates in the list
                None if *l == r => {}
                None => *l = ColumnValue::List(vec![l.clone(), r]),
            },
        }
    }
}

impl Sum for ColumnValue<'_> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut total: Option<Amount> = None;
        // Make precise if any of the amounts are precise
        let mut make_precise = false;
        for cv in iter {
            match cv {
                ColumnValue::Amount(amount, precise) => {
                    if let Some(t) = &mut total {
                        *t += amount;
                    } else {
                        total = Some(amount);
                    }
                    make_precise = make_precise || precise;
                }
                _ => {}
            }
        }
        if let Some(t) = total {
            ColumnValue::Amount(t, make_precise)
        } else {
            ColumnValue::Undefined
        }
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
    let err = |a: &ColumnValue<'h>, b: &ColumnValue<'h>| {
        err!("Unable to compare {} with {}", a.as_type_string(), b.as_type_string())
    };

    for i in 1..v.len() {
        let mut j = i;
        while j > 0 {
            // Only call cmp, propagate errors with `?`
            if v[j - 1].partial_cmp(&v[j]).ok_or_else(|| err(&v[j - 1], &v[j]))?
                != Ordering::Greater
            {
                break;
            }
            v.swap(j - 1, j);
            j -= 1;
        }
    }
    Ok(())
}
