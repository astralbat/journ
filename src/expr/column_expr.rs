/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::functions::{concat, cosum, max, min, neg, num, round, sum, sumif, text, value};
use crate::grouping::GroupKey;
use crate::report::{BinOp, CompareOp};
use journ_core::account::Account;
use journ_core::amount::Amount;
use journ_core::amounts::Amounts;
use journ_core::configuration::{AccountFilter, Filter};
use journ_core::date_and_time::{JDate, JDateTime};
use journ_core::err;
use journ_core::error::{JournError, JournResult};
use journ_core::journal::Journal;
use journ_core::journal_entry::JournalEntry;
use journ_core::posting::Posting;
use journ_core::reporting::table::Cell;
use journ_core::reporting::table2::{CellRef, EllipsisCell, MultiLineCell};
use journ_core::unit::Unit;
use journ_core::valuer::{SystemValuer, Valuer};
use rust_decimal::Decimal;
use smallvec::{SmallVec, smallvec};
use smartstring::alias::String as SS;
use std::ascii::AsciiExt;
use std::fmt::Write;
use std::{fmt, slice};

#[derive(Debug, Clone, PartialEq)]
pub enum ColumnExpr<'s> {
    Literal(&'s str),
    Column(&'s str),
    Function { name: &'s str, args: Vec<ColumnExpr<'s>> },
    BinaryOp { left: Box<ColumnExpr<'s>>, op: BinOp, right: Box<ColumnExpr<'s>> },
    CompareExpr { left: Box<ColumnExpr<'s>>, op: CompareOp, right: Box<ColumnExpr<'s>> },
    Number(Decimal),
    Parenthesized(Box<ColumnExpr<'s>>),
    Aliased(Box<ColumnExpr<'s>>, &'s str),
}

pub struct PostingGroup<'h, 'a> {
    /// Entries in this group, which are JournalEntries that share the same group key.
    entries: Box<dyn Fn() -> Box<dyn Iterator<Item = &'h JournalEntry<'h>> + 'a> + 'a>,
    postings: Box<
        dyn Fn() -> Box<dyn Iterator<Item = (&'h JournalEntry<'h>, &'a Posting<'h>)> + 'a> + 'a,
    >,
    group_key: Option<GroupKey<'h, 'a>>,
}

impl<'h, 'a> PostingGroup<'h, 'a> {
    pub fn new(
        entries: Box<dyn Fn() -> Box<dyn Iterator<Item = &'h JournalEntry<'h>> + 'a> + 'a>,
        postings: Box<
            dyn Fn() -> Box<dyn Iterator<Item = (&'h JournalEntry<'h>, &'a Posting<'h>)> + 'a> + 'a,
        >,
        group_key: Option<GroupKey<'h, 'a>>,
    ) -> Self {
        PostingGroup { entries, postings, group_key }
    }

    /// Gets all filtered entries for this group.
    pub fn entries(&self) -> impl Iterator<Item = &'h JournalEntry<'h>> {
        (self.entries)()
    }

    /// Gets all the filtered postings for this group, along with their parent entry.
    pub fn postings(&self) -> impl Iterator<Item = (&'h JournalEntry<'h>, &'a Posting<'h>)> {
        (self.postings)()
    }

    pub fn evaluate<F>(&self, mut f: F) -> JournResult<()>
    where
        F: FnMut(DataContext<'h, 'a>) -> JournResult<()>,
        'h: 'a,
    {
        for (entry, pst) in self.postings() {
            {
                f(DataContext::Single(entry, pst))?;
            }
        }
        Ok(())
    }
}

pub enum DataContext<'h, 'a> {
    Group(PostingGroup<'h, 'a>),
    Single(&'a JournalEntry<'h>, &'a Posting<'h>),
}

pub struct EvalContext<'h, 'a> {
    journal: &'a Journal<'h>,
    data_context: DataContext<'h, 'a>,
    variables: SmallVec<[(&'a str, ColumnValue<'h, 'a>); 1]>,
}

impl<'h, 'a> EvalContext<'h, 'a> {
    pub fn new(journal: &'a Journal<'h>, data_context: DataContext<'h, 'a>) -> Self {
        EvalContext { journal, data_context, variables: SmallVec::new() }
    }

    pub fn add_variable(&mut self, name: &'a str, value: ColumnValue<'h, 'a>) {
        self.variables.push((name, value));
    }

    pub fn journal(&self) -> &'a Journal<'h> {
        self.journal
    }

    pub fn data_context(&self) -> &DataContext<'h, 'a> {
        &self.data_context
    }

    /// Gets the group of entries for this context, if we are in a group context.
    pub fn entries(&self) -> Option<impl Iterator<Item = &'h JournalEntry<'h>>> {
        if let DataContext::Group(group) = &self.data_context {
            Some(group.entries())
        } else {
            None
        }
    }

    pub fn current_entry(&self) -> Option<&'a JournalEntry<'h>> {
        if let DataContext::Single(entry, _) = self.data_context { Some(entry) } else { None }
    }

    pub fn current_posting(&self) -> Option<&'a Posting<'h>> {
        if let DataContext::Single(_, posting) = self.data_context { Some(posting) } else { None }
    }
}

#[derive(Debug, Clone)]
pub enum ColumnValue<'h, 'a> {
    Boolean(bool),
    Account(&'a Account<'h>),
    Description(&'a str),
    String(SS),
    StringRef(&'a str),
    Date(JDate<'h>),
    Datetime(JDateTime<'h>),
    //Number(Decimal),
    Amount { amount: Amount<'h>, is_valuation: bool },
    List(Vec<ColumnValue<'h, 'a>>),
    // Like amount, but indicates this being a valued amount, to be formatted with the value formatter.
    //Value(Amount<'h>),
    //Amounts(Vec<Amount<'h>>),
    //Values(Vec<Amount<'h>>),
}

impl<'h, 's, 'a> ColumnValue<'h, 'a> {
    pub const fn nan() -> Self {
        ColumnValue::StringRef("NaN")
    }

    pub fn as_bool(&self) -> Option<bool> {
        if let ColumnValue::Boolean(b) = self { Some(*b) } else { None }
    }

    pub fn as_amount(&self) -> Option<Amount<'h>> {
        match self {
            ColumnValue::Amount { amount: a, is_valuation: _ } => Some(*a),
            _ => None,
        }
    }

    pub fn as_amount_mut(&mut self) -> Option<&mut Amount<'h>> {
        match self {
            ColumnValue::Amount { amount: a, is_valuation: _ } => Some(a),
            _ => None,
        }
    }

    pub fn is_nan(&self) -> bool {
        matches!(self, ColumnValue::StringRef("NaN"))
    }

    pub fn as_list(&self) -> &[ColumnValue<'h, 'a>] {
        match self {
            ColumnValue::List(v) => v,
            other => slice::from_ref(other),
        }
    }

    pub fn as_list_mut(&mut self) -> &mut [ColumnValue<'h, 'a>] {
        match self {
            ColumnValue::List(v) => v,
            other => slice::from_mut(other),
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

    pub fn as_date(&self) -> Option<JDate> {
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
            ColumnValue::Amount { amount: num, is_valuation: _ } => Some(num.quantity()),
            _ => None,
        }
    }

    pub fn cmp(&self, other: &ColumnValue<'h, 'a>) -> Option<std::cmp::Ordering> {
        self.as_number()
            .and_then(|a| other.as_number().map(|b| a.cmp(&b)))
            .or_else(|| self.as_datetime().and_then(|a| other.as_datetime().map(|b| a.cmp(&b))))
            .or_else(|| self.as_date().and_then(|a| other.as_date().map(|b| a.cmp(&b))))
            .or_else(|| self.as_str().and_then(|a| other.as_str().map(|b| a.cmp(b))))
            .or_else(|| {
                if let (ColumnValue::List(a), ColumnValue::List(b)) = (self, other) {
                    // Everything in the two lists must compare the same way
                    let mut combined = a.iter().zip(b);
                    if let Some(res) = combined.next().and_then(|(a, b)| a.cmp(b)) {
                        if combined.all(|(a, b)| a.cmp(&b) == Some(res)) { Some(res) } else { None }
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
    }

    pub fn matches(&self, other: &ColumnValue<'h, 'a>) -> Option<bool> {
        match (self, other) {
            (ColumnValue::Account(a), ColumnValue::String(b)) => {
                AccountFilter::new(std::slice::from_ref(b)).is_included(a).into()
            }
            _ => None,
        }
    }
}

impl fmt::Display for ColumnValue<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ColumnValue::Boolean(b) => write!(f, "{}", b),
            ColumnValue::String(s) => write!(f, "{}", s),
            ColumnValue::StringRef(s) | ColumnValue::Description(s) => write!(f, "{}", s),
            ColumnValue::Account(acc) => write!(f, "{}", acc),
            ColumnValue::Date(date) => write!(f, "{}", date),
            ColumnValue::Datetime(dt) => write!(f, "{}", dt),
            ColumnValue::Amount { amount, is_valuation } => write!(f, "{}", amount),
            ColumnValue::List(values) => {
                write!(f, "{}", values.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", "))
            }
        }
    }
}

impl<'s, 'a> From<ColumnValue<'_, 'a>> for Cell<'a> {
    fn from(value: ColumnValue<'_, 'a>) -> Self {
        match value {
            ColumnValue::Boolean(b) => Cell::from(b.to_string()),
            ColumnValue::String(s) => Cell::from(s.to_string()),
            ColumnValue::StringRef(s) => Cell::from(s),
            ColumnValue::Description(s) => Cell::from(s),
            ColumnValue::Date(date) => Cell::from(date),
            ColumnValue::Datetime(dt) => Cell::from(dt),
            ColumnValue::Account(acc) => Cell::from(acc.to_string()),
            ColumnValue::Amount { amount, .. } => Cell::from(amount),
            ColumnValue::List(values) => Cell::from(format!(
                "[{}]",
                values.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", ")
            )),
        }
    }
}

impl<'s, 'a, 'h, 'val> From<ColumnValue<'h, 'a>> for CellRef<'a>
where
    'h: 's,
    'a: 's,
{
    fn from(value: ColumnValue<'h, 'a>) -> Self {
        match value {
            ColumnValue::Boolean(b) => CellRef::Owned(Box::new(b.to_string())),
            ColumnValue::String(s) => CellRef::Owned(Box::new(s)),
            ColumnValue::StringRef(s) => CellRef::Owned(Box::new(s)),
            ColumnValue::Description(s) => {
                CellRef::Owned(Box::new(EllipsisCell::new(CellRef::Owned(Box::new(s)))))
            }
            ColumnValue::Date(date) => CellRef::Owned(Box::new(date)),
            ColumnValue::Datetime(dt) => CellRef::Owned(Box::new(dt)),
            ColumnValue::Account(acc) => CellRef::Owned(acc.into()),
            ColumnValue::Amount { amount, is_valuation } if !is_valuation => {
                CellRef::Owned(amount.into_cell(amount.unit().format()))
            }
            ColumnValue::Amount { amount, is_valuation } if is_valuation => {
                CellRef::Owned(amount.into_cell(amount.unit().value_format()))
            }
            ColumnValue::Amount { .. } => unreachable!(),
            ColumnValue::List(values) => {
                CellRef::Owned(Box::new(MultiLineCell::new(values.into_iter().map(|a| a.into()))))
            }
        }
    }
}

impl<'h, A: Amounts<'h>> From<A> for ColumnValue<'h, '_> {
    fn from(amounts: A) -> Self {
        if amounts.is_empty() {
            return ColumnValue::Amount { amount: Amount::nil(), is_valuation: false };
        }
        if amounts.len() == 1 {
            return ColumnValue::Amount { amount: amounts.as_slice()[0], is_valuation: false };
        }
        let vec: Vec<ColumnValue> = amounts
            .as_slice()
            .into_iter()
            .map(|a| ColumnValue::Amount { amount: *a, is_valuation: false })
            .collect();
        ColumnValue::List(vec)
    }
}

impl<'h, 'a> ColumnExpr<'a> {
    pub fn eval(&self, eval_context: &mut EvalContext<'h, 'a>) -> JournResult<ColumnValue<'h, 'a>>
    where
        'h: 'a,
    {
        let invalid_identifier = |name: &str| err!("Invalid identifier: '{}'", name,);
        let invalid_identifier_in_group = |name: &str, group_key: &GroupKey| {
            err!("Invalid group identifier: {}. Did you mean to enclose it in quotes?", name)
        };
        let identifier_not_valid_in_context =
            |name: &str| err!("Identifier '{}' is not valid in this context", name);
        match self {
            ColumnExpr::Literal(lit) => Ok(ColumnValue::StringRef(*lit)),
            ColumnExpr::Column(name) => match eval_context.data_context() {
                DataContext::Group(group) => {
                    if name.eq_ignore_ascii_case("account") {
                        match group.group_key {
                            Some(GroupKey::Account(acc)) => Ok(ColumnValue::Account(acc)),
                            _ => Err(identifier_not_valid_in_context("account")),
                        }
                    } else if name.eq_ignore_ascii_case("date") {
                        match group.group_key {
                            Some(GroupKey::Date(date)) => Ok(ColumnValue::Date(date)),
                            _ => Err(identifier_not_valid_in_context("date")),
                        }
                    } else if name.eq_ignore_ascii_case("description") {
                        match group.group_key {
                            Some(GroupKey::Description(desc)) => Ok(ColumnValue::Description(desc)),
                            _ => Err(identifier_not_valid_in_context("description")),
                        }
                    } else {
                        eval_context
                            .variables
                            .iter()
                            .find(|(s, v)| *s == *name)
                            .map(|(_, v)| v.clone())
                            .ok_or_else(|| match &group.group_key {
                                Some(key) => invalid_identifier_in_group(name, key),
                                None => invalid_identifier(name),
                            })
                    }
                }
                DataContext::Single(entry, pst) => {
                    if name.eq_ignore_ascii_case("account") {
                        Ok(ColumnValue::Account(pst.account()))
                    } else if name.eq_ignore_ascii_case("date") {
                        Ok(ColumnValue::Date(entry.date_and_time().datetime_range().start().date()))
                    } else if name.eq_ignore_ascii_case("datetime_from") {
                        Ok(ColumnValue::Datetime(entry.date_and_time().datetime_range().start()))
                    } else if name.eq_ignore_ascii_case("datetime_to") {
                        Ok(ColumnValue::Datetime(entry.date_and_time().datetime_range().end()))
                    } else if name.eq_ignore_ascii_case("datetime_mid") {
                        Ok(ColumnValue::Datetime(entry.date_and_time().average()))
                    } else if name.eq_ignore_ascii_case("description") {
                        Ok(ColumnValue::Description(entry.description()))
                    } else if name.eq_ignore_ascii_case("amount") {
                        Ok(ColumnValue::Amount { amount: pst.amount(), is_valuation: true })
                    } else {
                        eval_context
                            .variables
                            .iter()
                            .find(|(s, v)| *s == *name)
                            .map(|(_, v)| v.clone())
                            .ok_or_else(|| invalid_identifier(name))
                    }
                }
            },
            ColumnExpr::Function { name, args } => Self::eval_function(*name, args, eval_context),
            ColumnExpr::BinaryOp { left, op, right } => {
                let left_value = left.eval(eval_context)?;
                let right_value = right.eval(eval_context)?;

                Self::eval_binary_op(left_value, right_value, *op)
            }
            ColumnExpr::Number(num) => Ok(ColumnValue::Amount {
                amount: Amount::nil().with_quantity(*num),
                is_valuation: false,
            }),
            ColumnExpr::Parenthesized(inner) => inner.eval(eval_context),
            ColumnExpr::Aliased(inner, alias) => {
                let mut value = inner.eval(eval_context)?;
                eval_context.add_variable(alias, value.clone());
                Ok(value)
            }
            ColumnExpr::CompareExpr { left, op, right } => {
                let left_value = left.eval(eval_context)?;
                let right_value = right.eval(eval_context)?;

                let compare_res = match op {
                    CompareOp::Match => left_value.matches(&right_value),
                    CompareOp::NotMatch => left_value.matches(&right_value).map(|b| !b),
                    CompareOp::Eq => {
                        left_value.cmp(&right_value).map(|o| o == std::cmp::Ordering::Equal)
                    }
                    CompareOp::Neq => {
                        left_value.cmp(&right_value).map(|o| o != std::cmp::Ordering::Equal)
                    }
                    CompareOp::Lt => {
                        left_value.cmp(&right_value).map(|o| o == std::cmp::Ordering::Less)
                    }
                    CompareOp::Lte => {
                        left_value.cmp(&right_value).map(|o| o != std::cmp::Ordering::Greater)
                    }
                    CompareOp::Gt => {
                        left_value.cmp(&right_value).map(|o| o == std::cmp::Ordering::Greater)
                    }
                    CompareOp::Gte => {
                        left_value.cmp(&right_value).map(|o| o != std::cmp::Ordering::Less)
                    }
                };
                compare_res.map(ColumnValue::Boolean).ok_or_else(|| {
                    err!("Comparison operation '{}' is not supported for these types", op)
                })
            }
        }
    }

    fn eval_binary_op(
        left: ColumnValue<'h, 'a>,
        right: ColumnValue<'h, 'a>,
        op: BinOp,
    ) -> JournResult<ColumnValue<'h, 'a>> {
        macro_rules! binary_op {
            ($a:expr, $b:expr) => {{
                match op {
                    BinOp::Add => Ok($a + $b),
                    BinOp::Sub => Ok($a - $b),
                    BinOp::Mul => Ok($a * $b),
                    BinOp::Mod => {
                        if $b.is_zero() {
                            Err(err!("Modulo by zero in expression"))
                        } else {
                            Ok($a % $b)
                        }
                    }
                    BinOp::Div => {
                        if $b.is_zero() {
                            Err(err!("Division by zero in expression"))
                        } else {
                            Ok($a / $b)
                        }
                    }
                }
            }};
        }

        match (left, right) {
            (
                ColumnValue::Amount { amount: a, is_valuation: a_is_valuation },
                ColumnValue::Amount { amount: b, is_valuation: b_is_valuation },
            ) => Ok(binary_op!(a, b)
                .map(|amount| ColumnValue::Amount {
                    amount,
                    is_valuation: a_is_valuation && b_is_valuation,
                })
                .unwrap_or_else(|_: JournError| ColumnValue::nan())),
            (v, _) | (_, v) if v.is_nan() => Ok(ColumnValue::nan()),
            (ColumnValue::List(va), ColumnValue::List(vb)) => {
                let mut vec_res = Vec::with_capacity(va.len());
                // Should be safe to assume lists are normalized - each unit appears only once.
                // Lists do not have to have the same units, just operate where units match.
                for cv_a in va.into_iter() {
                    match cv_a.as_amount() {
                        Some(mut a) => {
                            let is_valuation =
                                matches!(cv_a, ColumnValue::Amount { is_valuation: true, .. });
                            if let Some(b) = vb
                                .iter()
                                .filter_map(ColumnValue::as_amount)
                                .filter(|b| a.unit() == b.unit())
                                .next()
                            {
                                binary_op!(a, b)
                                    .map(|c| {
                                        vec_res
                                            .push(ColumnValue::Amount { amount: c, is_valuation });
                                    })
                                    .unwrap_or_else(|_: JournError| {
                                        vec_res.push(ColumnValue::nan());
                                    });
                            }
                        }
                        None => {
                            return Err(err!(
                                "Binary operations are only supported for `Amount` type"
                            ));
                        }
                    }
                }
                Ok(ColumnValue::List(vec_res))
            }
            (left, right) => {
                Err(err!("Binary operations are only supported for `Amount` type").with_source(
                    err!("Left operand: {}, Right operand: {}, Operation: {}", left, right, op),
                ))
            }
        }
    }

    fn eval_function(
        name: &'a str,
        args: &[ColumnExpr<'a>],
        eval_context: &mut EvalContext<'h, 'a>,
    ) -> JournResult<ColumnValue<'h, 'a>> {
        if name.eq_ignore_ascii_case("value") {
            value(args, eval_context)
        } else if name.eq_ignore_ascii_case("sum") {
            sum(args, eval_context)
        } else if name.eq_ignore_ascii_case("sumif") {
            sumif(args, eval_context)
        } else if name.eq_ignore_ascii_case("cosum") {
            cosum(args, eval_context)
        } else if name.eq_ignore_ascii_case("-") {
            neg(args, eval_context)
        } else if name.eq_ignore_ascii_case("round") {
            round(args, eval_context)
        } else if name.eq_ignore_ascii_case("min") {
            min(args, eval_context)
        } else if name.eq_ignore_ascii_case("max") {
            max(args, eval_context)
        } else if name.eq_ignore_ascii_case("num") {
            num(args, eval_context)
        } else if name.eq_ignore_ascii_case("concat") {
            concat(args, eval_context)
        } else if name.eq_ignore_ascii_case("text") {
            text(args, eval_context)
        } else {
            Err(err!("Unknown function: '{}'", name))
        }
    }
}

impl fmt::Display for ColumnExpr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ColumnExpr::Literal(lit) => write!(f, "\"{}\"", lit),
            ColumnExpr::Column(name) => write!(f, "{}", name),
            ColumnExpr::Function { name, args } => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            ColumnExpr::BinaryOp { left, op, right } => {
                write!(f, "({} {} {})", left, op, right)
            }
            ColumnExpr::Number(num) => write!(f, "{}", num),
            ColumnExpr::Parenthesized(inner) => write!(f, "({})", inner),
            ColumnExpr::Aliased(inner, alias) => write!(f, "{}", alias),
            ColumnExpr::CompareExpr { left, op, right } => {
                write!(f, "({} {} {})", left, op, right)
            }
        }
    }
}
