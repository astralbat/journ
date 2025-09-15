/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::grouping::GroupKey;
use crate::report::BinOp;
use journ_core::account::Account;
use journ_core::amount::Amount;
use journ_core::configuration::{AccountFilter, Filter};
use journ_core::date_and_time::{JDate, JDateTime};
use journ_core::err;
use journ_core::error::JournResult;
use journ_core::journal::Journal;
use journ_core::journal_entry::JournalEntry;
use journ_core::posting::Posting;
use journ_core::reporting::table::Cell;
use journ_core::reporting::table2;
use journ_core::reporting::table2::{CellRef, EllipsisCell, MultiLineCell};
use journ_core::unit::Unit;
use journ_core::valuer::{SystemValuer, Valuer};
use rust_decimal::Decimal;
use smallvec::SmallVec;
use std::fmt;
use std::fmt::Write;

#[derive(Debug, Clone, PartialEq)]
pub enum ColumnExpr<'s> {
    Literal(&'s str),
    Column(&'s str),
    Function { name: &'s str, args: Vec<ColumnExpr<'s>> },
    BinaryOp { left: Box<ColumnExpr<'s>>, op: BinOp, right: Box<ColumnExpr<'s>> },
    Number(Decimal),
    Parenthesized(Box<ColumnExpr<'s>>),
}

pub struct PostingGroup<'h, 'a> {
    /// Entries in this group, which are JournalEntries that share the same group key.
    entries: Box<dyn Fn() -> Box<dyn Iterator<Item = &'h JournalEntry<'h>> + 'a> + 'a>,
    unit_filter: Box<dyn Filter<'a, Unit<'h>> + 'a>,
    account_filter: Box<dyn Filter<'a, Account<'h>> + 'a>,
    group_key: Option<GroupKey<'h, 'a>>,
}

impl<'h, 'a> PostingGroup<'h, 'a> {
    pub fn new(
        entries: Box<dyn Fn() -> Box<dyn Iterator<Item = &'h JournalEntry<'h>> + 'a> + 'a>,
        unit_filter: Box<dyn Filter<'a, Unit<'h>> + 'a>,
        account_filter: Box<dyn Filter<'a, Account<'h>> + 'a>,
        group_key: Option<GroupKey<'h, 'a>>,
    ) -> Self {
        PostingGroup { entries, unit_filter, account_filter, group_key }
    }

    pub fn entries(&self) -> impl Iterator<Item = &'h JournalEntry<'h>> {
        (self.entries)()
    }

    pub fn evaluate<F>(&self, mut f: F) -> JournResult<()>
    where
        F: FnMut(DataContext<'h, 'a>) -> JournResult<()>,
        'h: 'a,
    {
        for entry in self.entries() {
            for pst in entry
                .postings()
                .filter(|pst| self.unit_filter.is_included(pst.unit()))
                .filter(|pst| self.account_filter.is_included(pst.account()))
            {
                f(DataContext::Scalar(entry, pst))?;
            }
        }
        Ok(())
    }
}

pub enum DataContext<'h, 'a> {
    Group(PostingGroup<'h, 'a>),
    Scalar(&'a JournalEntry<'h>, &'a Posting<'h>),
}

pub struct EvalContext<'h, 'a> {
    journal: &'a Journal<'h>,
    data_context: DataContext<'h, 'a>,
    variables: SmallVec<[(&'static str, ColumnValue<'h, 'a, 'static>); 1]>,
}

impl<'h, 'a> EvalContext<'h, 'a> {
    pub fn new(journal: &'a Journal<'h>, data_context: DataContext<'h, 'a>) -> Self {
        EvalContext { journal, data_context, variables: SmallVec::new() }
    }

    pub fn add_variable(&mut self, name: &'static str, value: ColumnValue<'h, 'a, 'static>) {
        self.variables.push((name, value));
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
        if let DataContext::Scalar(entry, _) = self.data_context { Some(entry) } else { None }
    }

    pub fn current_posting(&self) -> Option<&'a Posting<'h>> {
        if let DataContext::Scalar(_, posting) = self.data_context { Some(posting) } else { None }
    }
}

#[derive(Debug, Clone)]
pub enum ColumnValue<'h, 'a, 's> {
    Account(&'a Account<'h>),
    Description(&'s str),
    String(String),
    StringRef(&'s str),
    Date(JDate<'h>),
    Datetime(JDateTime<'h>),
    Number(Decimal),
    Amount(Amount<'h>),
    // Like amount, but indicates this being a valued amount, to be formatted with the value formatter.
    Value(Amount<'h>),
    Amounts(Vec<Amount<'h>>),
    Values(Vec<Amount<'h>>),
}

impl<'h, 's, 'a> ColumnValue<'h, 'a, 's> {
    pub fn as_amount(&self) -> Option<Amount<'h>> {
        match self {
            ColumnValue::Amount(a) | ColumnValue::Value(a) => Some(*a),
            _ => None,
        }
    }

    pub fn as_amounts(&self) -> Option<&[Amount<'h>]> {
        match self {
            ColumnValue::Amounts(amounts) => Some(amounts),
            ColumnValue::Amount(amount) => Some(std::slice::from_ref(amount)),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<&str> {
        match self {
            ColumnValue::StringRef(s) => Some(s),
            ColumnValue::String(s) => Some(s.as_str()),
            _ => None,
        }
    }

    pub fn into_string(self) -> Option<String> {
        if let ColumnValue::String(s) = self { Some(s) } else { None }
    }

    pub fn as_date(&self) -> Option<JDate> {
        if let ColumnValue::Date(date) = self { Some(*date) } else { None }
    }

    pub fn as_datetime(&self) -> Option<JDateTime<'h>> {
        if let ColumnValue::Datetime(dt) = self { Some(*dt) } else { None }
    }

    pub fn as_number(&self) -> Option<Decimal> {
        if let ColumnValue::Number(num) = self { Some(*num) } else { None }
    }
}

impl fmt::Display for ColumnValue<'_, '_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ColumnValue::String(s) => write!(f, "{}", s),
            ColumnValue::StringRef(s) | ColumnValue::Description(s) => write!(f, "{}", s),
            ColumnValue::Account(acc) => write!(f, "{}", acc),
            ColumnValue::Date(date) => write!(f, "{}", date),
            ColumnValue::Datetime(dt) => write!(f, "{}", dt),
            ColumnValue::Number(num) => write!(f, "{}", num),
            ColumnValue::Amount(amount) => write!(f, "{}", amount),
            ColumnValue::Amounts(amounts) => write!(
                f,
                "{}",
                amounts.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", ")
            ),
            ColumnValue::Value(value) => write!(f, "{}", value),
            ColumnValue::Values(values) => {
                write!(f, "{}", values.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", "))
            }
        }
    }
}

impl<'s, 'a> From<ColumnValue<'_, 'a, 's>> for Cell<'s> {
    fn from(value: ColumnValue<'_, 'a, 's>) -> Self {
        match value {
            ColumnValue::String(s) => Cell::from(s),
            ColumnValue::StringRef(s) => Cell::from(s),
            ColumnValue::Description(s) => Cell::from(s),
            ColumnValue::Date(date) => Cell::from(date),
            ColumnValue::Datetime(dt) => Cell::from(dt),
            ColumnValue::Account(acc) => Cell::from(acc.to_string()),
            ColumnValue::Number(num) => Cell::from(num),
            ColumnValue::Amount(amount) => Cell::from(amount),
            ColumnValue::Amounts(amounts) => Cell::from(format!(
                "[{}]",
                amounts.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", ")
            )),
            ColumnValue::Value(value) => Cell::from(value),
            ColumnValue::Values(values) => Cell::from(format!(
                "[{}]",
                values.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", ")
            )),
        }
    }
}

impl<'s, 'a, 'h, 'val> From<ColumnValue<'h, 'a, 's>> for CellRef<'s>
where
    'h: 's,
    'a: 's,
{
    fn from(value: ColumnValue<'h, 'a, 's>) -> Self {
        match value {
            ColumnValue::String(s) => CellRef::Owned(Box::new(s)),
            ColumnValue::StringRef(s) => CellRef::Owned(Box::new(s)),
            ColumnValue::Description(s) => {
                CellRef::Owned(Box::new(EllipsisCell::new(CellRef::Owned(Box::new(s)))))
            }
            ColumnValue::Date(date) => CellRef::Owned(Box::new(date)),
            ColumnValue::Datetime(dt) => CellRef::Owned(Box::new(dt)),
            ColumnValue::Number(num) => CellRef::Owned(Box::new(num)),
            ColumnValue::Account(acc) => CellRef::Owned(acc.into()),
            ColumnValue::Amount(amount) => CellRef::Owned(amount.into_cell(amount.unit().format())),
            ColumnValue::Amounts(amounts) => CellRef::Owned(Box::new(MultiLineCell::new(
                amounts.into_iter().map(|a| CellRef::Owned(a.into_cell(a.unit().format()))),
            ))),
            ColumnValue::Value(value) => {
                CellRef::Owned(value.into_cell(value.unit().value_format()))
            }
            ColumnValue::Values(values) => CellRef::Owned(Box::new(MultiLineCell::new(
                values.into_iter().map(|a| CellRef::Owned(a.into_cell(a.unit().value_format()))),
            ))),
        }
    }
}

impl<'h, 'a, 's> ColumnExpr<'s> {
    pub fn eval(&self, eval_context: &EvalContext<'h, 'a>) -> JournResult<ColumnValue<'h, 'a, 's>>
    where
        'h: 's,
        'a: 's,
    {
        let invalid_identifier = |name: &str| err!("Invalid identifier: '{}'", name,);
        let invalid_identifier_in_group = |name: &str, group_key: &GroupKey| {
            err!("Invalid identifier: '{}' for grouping key: '{}'", name, group_key)
        };
        let identifier_not_valid_in_context =
            |name: &str| err!("Identifier '{}' is not valid in this context", name);
        match self {
            ColumnExpr::Literal(lit) => Ok(ColumnValue::StringRef(*lit)),
            ColumnExpr::Column(name) => match eval_context.data_context() {
                DataContext::Group(group) => match *name {
                    "account" => match group.group_key {
                        Some(GroupKey::Account(acc)) => Ok(ColumnValue::Account(acc)),
                        _ => Err(identifier_not_valid_in_context("account")),
                    },
                    "date" => match group.group_key {
                        Some(GroupKey::Date(date)) => Ok(ColumnValue::Date(date)),
                        _ => Err(identifier_not_valid_in_context("date")),
                    },
                    "description" => match group.group_key {
                        Some(GroupKey::Description(desc)) => Ok(ColumnValue::Description(desc)),
                        _ => Err(identifier_not_valid_in_context("description")),
                    },
                    _ => match &group.group_key {
                        Some(key) => Err(invalid_identifier_in_group(name, key)),
                        None => Err(invalid_identifier(name)),
                    },
                },
                DataContext::Scalar(entry, pst) => match *name {
                    "account" => Ok(ColumnValue::Account(pst.account())),
                    "date" => {
                        Ok(ColumnValue::Date(entry.date_and_time().datetime_range().start().date()))
                    }
                    "datetime_from" => {
                        Ok(ColumnValue::Datetime(entry.date_and_time().datetime_range().start()))
                    }
                    "datetime_to" => {
                        Ok(ColumnValue::Datetime(entry.date_and_time().datetime_range().end()))
                    }
                    "datetime_mid" => Ok(ColumnValue::Datetime(entry.date_and_time().average())),
                    "description" => Ok(ColumnValue::Description(entry.description())),
                    "amount" => Ok(ColumnValue::Amount(pst.amount())),
                    _ => eval_context
                        .variables
                        .iter()
                        .find(|(s, v)| *s == *name)
                        .map(|(_, v)| v.clone())
                        .ok_or_else(|| invalid_identifier(name)),
                },
            },
            ColumnExpr::Function { name, args } => Self::eval_function(*name, args, eval_context),
            ColumnExpr::BinaryOp { left, op, right } => {
                let left_value = left.eval(eval_context)?;
                let right_value = right.eval(eval_context)?;

                todo!("Implement binary operation logic for {:?}", op);
            }
            ColumnExpr::Number(num) => Ok(ColumnValue::Number(*num)),
            ColumnExpr::Parenthesized(inner) => inner.eval(eval_context),
        }
    }

    fn eval_function(
        name: &'s str,
        args: &[ColumnExpr<'s>],
        eval_context: &EvalContext<'h, 'a>,
    ) -> JournResult<ColumnValue<'h, 'a, 's>> {
        match name {
            "value" => {
                if args.len() == 0 || args.len() > 3 {
                    return Err(err!(
                        "Function 'value()' requires one to three arguments: a unit, an identifier, and an optional date. E.g. value(€, amount, 2023-01-01)."
                    ));
                }
                // The first argument is a string representing the unit. This is looked up in the journal or created if unknown.
                let quote_unit = args[0].eval(eval_context)?.as_string().ok_or(err!(
                    "Function 'value()' requires the first argument to be a string representing a unit"
                )).map(|s| eval_context.journal.config().get_unit(s).unwrap_or(eval_context.journal.allocator().alloc(Unit::new(s))))?;

                // The second argument is evaluated as the base amount that needs to be valued.
                let base_amount_col = args.get(1).unwrap_or(&ColumnExpr::Column("amount"));
                let base_amount = base_amount_col.eval(eval_context)?.as_amount().ok_or(err!(
                    "Function 'value()' requires the second argument to be an `Amount` type"
                ))?;

                // The third argument is an optional datetime. If not provided, the current entry's datetime is used.
                let datetime_col = args.get(2).unwrap_or(&ColumnExpr::Column("datetime_mid"));
                let datetime = datetime_col.eval(eval_context)?.as_datetime().ok_or(err!(
                    "Function 'value' requires the third argument to be a `Datetime` type"
                ))?;

                // If we are using the 'datetime_mid' column, we can proceed to use the entry to value.
                // By getting this far, we know that we are in a scalar context.
                let value = if datetime_col == &ColumnExpr::Column("datetime_mid") {
                    SystemValuer::from(eval_context.current_entry().unwrap())
                        .value(base_amount, quote_unit)?
                } else {
                    SystemValuer::on_date(eval_context.journal.config().clone(), datetime)
                        .value(base_amount, quote_unit)?
                }
                .ok_or_else(|| {
                    err!(
                        "Could not value amount {} in unit {} on date {}",
                        base_amount,
                        quote_unit.code(),
                        datetime
                    )
                })?;
                Ok(ColumnValue::Value(value))
            }
            "sum" => match eval_context.data_context() {
                DataContext::Group(group) => {
                    let mut totals = vec![];
                    if args.is_empty() {
                        return Err(err!("Function 'sum' requires one argument"));
                    }
                    let mut as_values = false;
                    group.evaluate(|context| {
                        let cv = args[0].eval(&EvalContext::new(eval_context.journal, context))?;
                        if matches!(cv, ColumnValue::Value(_)) {
                            as_values = true;
                        }
                        totals += cv.as_amount().ok_or(err!(
                            "Function 'sum' requires an `Amount` or `Value` type argument"
                        ))?;
                        Ok(())
                    })?;
                    if as_values {
                        Ok(ColumnValue::Values(totals))
                    } else {
                        Ok(ColumnValue::Amounts(totals))
                    }
                }
                _ => Err(err!(
                    "Function 'sum' is not valid in scalar context, only in group context"
                )),
            },
            "cosum" => {
                let accounts = args.iter().map(|arg| arg.eval(eval_context)?.into_string().ok_or(err!("cosum() requires arguments of type `String`, representing account patterns to match"))).collect::<Result<Vec<_>, _>>()?;
                let account_filter = AccountFilter::new(&accounts);
                match eval_context.data_context() {
                    DataContext::Group(group) => {
                        let mut total = Amount::nil();
                        for entry in group.entries() {
                            if entry
                                .postings()
                                .filter(|pst| account_filter.is_included(pst.account()))
                                .count()
                                == 0
                            {
                                continue;
                            }

                            for pst in entry.postings() {
                                if account_filter.is_included(pst.account()) {
                                    total += pst.amount();
                                }
                            }
                        }
                        Ok(ColumnValue::Amount(total))
                    }
                    _ => Err(err!(
                        "Function 'cosum' is not valid in scalar context, only in group context"
                    )),
                }
            }
            _ => Err(err!("Unknown function: {}", name)),
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
        }
    }
}
