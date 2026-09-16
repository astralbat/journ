/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::datetime::JDateTime;
use crate::err;
use crate::error::{JournError, JournResult};
use crate::report::expr::functions::abs::abs;
use crate::report::expr::functions::iferror::iferror;
use crate::report::expr::functions::*;
use crate::report::expr::parser::{BinOp, CompareOp, LogicalOp, UnaryOp, scalar_expr};
use crate::report::expr::{ColumnValue, IdentifierContext, parser};
use chrono::Duration;
use nom::Finish;
use nom::error::convert_error;
use rust_decimal::Decimal;
use smartstring::alias::String as SS;
use std::fmt;
use std::ops::Deref;
use std::str::FromStr;

#[derive(Debug, Clone, Eq)]
pub enum Expr {
    ScalarFunction { name: SS, args: Vec<Expr> },
    AggFunction(SS, Vec<Expr>, usize), // Index into separate list of unique aggregate functions
    Unary { op: UnaryOp, expr: Box<Expr> },
    BinaryOp { left: Box<Expr>, op: BinOp, right: Box<Expr> },
    Compare { left: Box<Expr>, op: CompareOp, right: Box<Expr> },
    LogicalOp { left: Box<Expr>, op: LogicalOp, right: Box<Expr> },
    Parenthesized(Box<Expr>),
    Literal(SS),
    Identifier(SS),
    Number(Decimal),
    Aliased(Box<Expr>, SS),
}

impl<'h> Expr {
    /// Gets whether this expression or any of its children are an aggregate expression.
    pub fn is_aggregate(&self) -> bool {
        matches!(self, Expr::AggFunction { .. })
            || self.children().any(|inner| matches!(inner, Expr::AggFunction { .. }))
    }

    /// Recursively searches for any Expr that is an aggregate function.
    pub fn find_aggregate(&self) -> Option<Expr> {
        if let Expr::AggFunction(_, _, _) = self {
            return Some(self.clone());
        }
        for child in self.children() {
            if let Some(agg) = child.find_aggregate() {
                return Some(agg);
            }
        }
        None
    }

    /// Gets whether this expression is a constant expression, meaning it contains no identifiers or aggregate functions and can be evaluated to the same value for every row.
    pub fn is_const(&self) -> bool {
        for child in self.children() {
            if !child.is_const() {
                return false;
            }
        }
        !matches!(self, Expr::AggFunction { .. } | Expr::Identifier(_))
    }

    pub fn eval<'a>(
        &self,
        context: &mut dyn IdentifierContext<'h, 'a>,
    ) -> JournResult<ColumnValue<'h>>
    where
        'h: 'a,
    {
        use Expr::*;
        match self {
            ScalarFunction { name, args } => eval_scalar_function(name, args, context),
            AggFunction(_, _, index) => context
                .eval_aggregate(*index)
                .ok_or_else(|| err!("Unable to evaluate aggregate function: {}", index)),
            BinaryOp { left, op, right } => {
                let left_value = left.eval(context)?;
                let right_value = right.eval(context)?;
                eval_binary_op(left_value, right_value, *op)
            }
            Parenthesized(inner) => inner.eval(context),
            Compare { left, op, right } => {
                let left_value = left.eval(context)?;
                let right_value = right.eval(context)?;
                eval_compare_op(left_value, right_value, *op)
            }
            Unary { op: UnaryOp::Not, expr } => match expr.eval(context)? {
                ColumnValue::Boolean(b) => Ok(ColumnValue::Boolean(!b)),
                // Be consistent with And logic as much as possible per De Morgan's law.
                ColumnValue::Undefined => Ok(ColumnValue::Undefined),
                _ => Ok(ColumnValue::Boolean(false)),
            },
            Unary { op: UnaryOp::Exists, expr } => match expr.eval(context)? {
                ColumnValue::Undefined => Ok(ColumnValue::Boolean(false)),
                _ => Ok(ColumnValue::Boolean(true)),
            },
            LogicalOp { left, op: parser::LogicalOp::And, right } => {
                match left.eval(context)? {
                    // Short-circuit evaluation for AND
                    ColumnValue::Boolean(false) => Ok(ColumnValue::Boolean(false)),
                    // (UNDEFINED AND FALSE) should be FALSE to be consistent with (FALSE AND UNDEFINED).
                    ColumnValue::Undefined => match right.eval(context)? {
                        ColumnValue::Boolean(false) => Ok(ColumnValue::Boolean(false)),
                        _ => Ok(ColumnValue::Undefined),
                    },
                    _ => right.eval(context),
                }
            }
            LogicalOp { left, op: parser::LogicalOp::Or, right } => {
                match left.eval(context)? {
                    // Short-circuit evaluation for OR
                    ColumnValue::Boolean(true) => Ok(ColumnValue::Boolean(true)),
                    // Evaluate the right-hand expression if false, but also allow a coalesce operation. That is,
                    // return the left hand side if defined.
                    ColumnValue::Boolean(false) | ColumnValue::Undefined => right.eval(context),
                    left => Ok(left),
                }
            }
            Literal(lit) => Ok(ColumnValue::String(lit.clone())),
            Identifier(name) => {
                if name.eq_ignore_ascii_case("true") {
                    Ok(ColumnValue::Boolean(true))
                } else if name.eq_ignore_ascii_case("false") {
                    Ok(ColumnValue::Boolean(false))
                } else {
                    context
                        .eval_identifier(name)
                        .ok_or_else(|| err!("Invalid identifier: '{}'", name))
                }
            }
            Number(num) => Ok(ColumnValue::Number(*num)),
            Aliased(inner, alias) => {
                let value = inner.eval(context)?;
                context.set_identifier(alias, value.clone());
                Ok(value)
            }
        }
    }

    /// Iterates over this expression and all sub-expressions in depth-first order.
    pub fn iter(&self) -> ExprIter<'_> {
        ExprIter { stack: vec![self] }
    }

    pub fn children(&self) -> impl Iterator<Item = &Expr> + '_ {
        self.iter().skip(1)
    }

    /// Compares two expressions for equality, treating aliased expressions as equal if either the inner expression or the alias matches.
    pub fn eq_expr_or_alias(&self, other: &Expr) -> bool {
        use Expr::*;
        match (self, other) {
            (Aliased(inner_a, alias_a), Aliased(inner_b, alias_b)) => {
                inner_a == inner_b || alias_a.eq_ignore_ascii_case(alias_b)
            }
            (Aliased(inner_a, _alias_a), other) => inner_a.deref() == other,
            (other, Aliased(inner_b, _alias_b)) => other == inner_b.deref(),
            _ => self == other,
        }
    }
}
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expr::*;
        match self {
            ScalarFunction { name, args } => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            AggFunction(name, args, _index) => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Unary { op, expr } => {
                write!(f, "({} {})", op, expr)
            }
            BinaryOp { left, op, right } => {
                write!(f, "({} {} {})", left, op, right)
            }
            Parenthesized(inner) => write!(f, "({})", inner),
            Compare { left, op, right } => {
                write!(f, "({} {} {})", left, op, right)
            }
            LogicalOp { left, op, right } => {
                write!(f, "({} {} {})", left, op, right)
            }
            Literal(lit) => write!(f, "{lit}"),
            Identifier(name) => write!(f, "{}", name),
            Number(num) => write!(f, "{}", num),
            Aliased(_inner, alias) => write!(f, "{}", alias),
        }
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        use Expr::*;
        match (self, other) {
            (
                ScalarFunction { name: name_a, args: args_a },
                ScalarFunction { name: name_b, args: args_b },
            ) => name_a.eq_ignore_ascii_case(name_b) && args_a == args_b,
            (AggFunction(_, _, index_a), AggFunction(_, _, index_b)) => index_a == index_b,
            (
                BinaryOp { left: left_a, op: op_a, right: right_a },
                BinaryOp { left: left_b, op: op_b, right: right_b },
            ) => op_a == op_b && left_a == left_b && right_a == right_b,
            (
                Compare { left: left_a, op: op_a, right: right_a },
                Compare { left: left_b, op: op_b, right: right_b },
            ) => op_a == op_b && left_a == left_b && right_a == right_b,
            (
                LogicalOp { left: left_a, op: op_a, right: right_a },
                LogicalOp { left: left_b, op: op_b, right: right_b },
            ) => op_a == op_b && left_a == left_b && right_a == right_b,
            (Parenthesized(inner_a), Parenthesized(inner_b)) => inner_a == inner_b,
            (Literal(lit_a), Literal(lit_b)) => lit_a == lit_b,
            (Identifier(name_a), Identifier(name_b)) => name_a.eq_ignore_ascii_case(name_b),
            (Number(num_a), Number(num_b)) => num_a == num_b,
            (Aliased(inner_a, alias_a), Aliased(inner_b, alias_b)) => {
                inner_a == inner_b && alias_a.eq_ignore_ascii_case(alias_b)
            }
            _ => false,
        }
    }
}

pub struct ExprIter<'a> {
    stack: Vec<&'a Expr>,
}
impl<'a> Iterator for ExprIter<'a> {
    type Item = &'a Expr;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(expr) = self.stack.pop() {
            use Expr::*;
            match expr {
                AggFunction(_name, args, _index) => {
                    for arg in args.iter().rev() {
                        self.stack.push(arg);
                    }
                }
                ScalarFunction { args, .. } => {
                    for arg in args.iter().rev() {
                        self.stack.push(arg);
                    }
                }
                BinaryOp { left, right, .. } => {
                    self.stack.push(right);
                    self.stack.push(left);
                }
                Compare { left, right, .. } => {
                    self.stack.push(right);
                    self.stack.push(left);
                }
                LogicalOp { left, right, .. } => {
                    self.stack.push(right);
                    self.stack.push(left);
                }
                Parenthesized(inner) => {
                    self.stack.push(inner);
                }
                Aliased(inner, _) => {
                    self.stack.push(inner);
                }
                _ => {}
            }
            Some(expr)
        } else {
            None
        }
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        for _ in 0..n {
            self.next()?;
        }
        self.next()
    }
}

fn eval_scalar_function<'h, 'a>(
    name: &str,
    args: &[Expr],
    context: &mut dyn IdentifierContext<'h, 'a>,
) -> JournResult<ColumnValue<'h>>
where
    'h: 'a,
{
    if name.eq_ignore_ascii_case("amount") {
        amount(args, context)
    } else if name.eq_ignore_ascii_case("bal") {
        bal(args, context)
    } else if name.eq_ignore_ascii_case("value") {
        value(args, context)
    } else if name.eq_ignore_ascii_case("-") {
        neg(args, context)
    } else if name.eq_ignore_ascii_case("round") {
        round(args, context)
    } else if name.eq_ignore_ascii_case("num") {
        num(args, context)
    } else if name.eq_ignore_ascii_case("abs") {
        abs(args, context)
    } else if name.eq_ignore_ascii_case("greatest") {
        greatest(args, context)
    } else if name.eq_ignore_ascii_case("least") {
        least(args, context)
    } else if name.eq_ignore_ascii_case("concat") {
        concat(args, context)
    } else if name.eq_ignore_ascii_case("text") {
        text(args, context)
    } else if name.eq_ignore_ascii_case("date") {
        date(args, context)
    } else if name.eq_ignore_ascii_case("datevalue") {
        datevalue(args, context)
    } else if name.eq_ignore_ascii_case("now") {
        now(args, context)
    } else if name.eq_ignore_ascii_case("if") {
        cond(args, context)
    } else if name.eq_ignore_ascii_case("iferror") {
        iferror(args, context)
    } else if name.eq_ignore_ascii_case("unichar") {
        unichar(args, context)
    } else if name.eq_ignore_ascii_case("isdefined") {
        isdefined(args, context)
    } else if name.eq_ignore_ascii_case("startswith") {
        startswith(args, context)
    } else if name.eq_ignore_ascii_case("endswith") {
        endswith(args, context)
    } else {
        Err(err!("Unknown function: '{}'", name))
    }
}

fn eval_binary_op<'h>(
    left: ColumnValue<'h>,
    right: ColumnValue<'h>,
    op: BinOp,
) -> JournResult<ColumnValue<'h>> {
    macro_rules! binary_op {
        ($a:expr, $b:expr, $op:ident) => {{
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

    //let mut results = Vec::new();
    match (left, right) {
        (ColumnValue::Number(a), ColumnValue::Number(b)) => {
            binary_op!(&a, &b, op).map(ColumnValue::Number)
        }
        // The number represents a duration in days. It may be a fraction of a day.
        (ColumnValue::Datetime(a), ColumnValue::Number(b))
        | (ColumnValue::Number(b), ColumnValue::Datetime(a)) => {
            let seconds: i64 = (b * dec!(60) * dec!(60) * dec!(24)).ceil().try_into().unwrap();
            match op {
                BinOp::Add => Ok(ColumnValue::Datetime(JDateTime::new(
                    a.datetime() + Duration::seconds(seconds),
                    a.precision(),
                ))),
                BinOp::Sub => Ok(ColumnValue::Datetime(JDateTime::new(
                    a.datetime() - Duration::seconds(seconds),
                    a.precision(),
                ))),
                _ => Err(err!("Datetime binary operation not supported: {} {} {}", a, op, b)),
            }
        }
        (ColumnValue::Amount(a, p_1), ColumnValue::Amount(b, _)) => binary_op!(&a, &b, op)
            .map(|amount| Ok(ColumnValue::Amount(amount, p_1)))
            .unwrap_or_else(|_: JournError| Ok(ColumnValue::Undefined)),
        (ColumnValue::Amount(a, p), ColumnValue::Number(b)) => binary_op!(a, b, op)
            .map(|amount| Ok(ColumnValue::Amount(amount, p)))
            .unwrap_or_else(|_: JournError| Ok(ColumnValue::Undefined)),
        // Mul and Add are commutative
        (ColumnValue::Number(a), ColumnValue::Amount(b, p))
            if op == BinOp::Mul || op == BinOp::Add =>
        {
            binary_op!(b, a, op)
                .map(|amount| Ok(ColumnValue::Amount(amount, p)))
                .unwrap_or_else(|_: JournError| Ok(ColumnValue::Undefined))
        }
        (ColumnValue::Number(a), ColumnValue::Amount(b, p)) => {
            binary_op!(b.with_quantity(a), b, op)
                .map(|amount| Ok(ColumnValue::Amount(amount, p)))
                .unwrap_or_else(|_: JournError| Ok(ColumnValue::Undefined))
        }
        (ColumnValue::List(mut a_list), ColumnValue::List(b_list)) => {
            for (a, b) in a_list.iter_mut().zip(b_list.iter()) {
                *a = eval_binary_op(a.clone(), b.clone(), op)?;
            }
            Ok(ColumnValue::List(a_list))
        }
        (ColumnValue::List(mut list), scalar) => {
            for item in list.iter_mut() {
                *item = eval_binary_op(item.clone(), scalar.clone(), op)?;
            }
            Ok(ColumnValue::List(list))
        }
        (scalar, ColumnValue::List(mut list)) => {
            for item in list.iter_mut() {
                *item = eval_binary_op(scalar.clone(), item.clone(), op)?;
            }
            Ok(ColumnValue::List(list))
        }
        (ColumnValue::Undefined, _) | (_, ColumnValue::Undefined) => Ok(ColumnValue::Undefined),
        (left, right) => Err(err!("Binary operation not supported: {:?} {} {:?}", left, op, right)),
    }
    /*pf
    if results.is_empty() {
        Ok(ColumnValue::Undefined)
    } else if results.len() == 1 {
        results.into_iter().next().unwrap()
    } else {
        Ok(ColumnValue::List(results.into_iter().collect::<Result<Vec<_>, _>>()?))
    }*/
}

fn eval_compare_op<'h, 'a>(
    left_value: ColumnValue<'h>,
    right_value: ColumnValue<'h>,
    op: CompareOp,
) -> JournResult<ColumnValue<'h>> {
    if left_value.is_undefined() || right_value.is_undefined() {
        return Ok(ColumnValue::Undefined);
    }
    let err = || {
        err!("Cannot compare {} and {}", left_value.as_type_string(), right_value.as_type_string())
    };

    let compare_res = match op {
        CompareOp::Match => left_value.matches(&right_value)?,
        CompareOp::NotMatch => !left_value.matches(&right_value)?,
        CompareOp::Eq => {
            left_value.partial_cmp(&right_value).ok_or_else(err)? == std::cmp::Ordering::Equal
        }
        CompareOp::Neq => {
            left_value.partial_cmp(&right_value).ok_or_else(err)? != std::cmp::Ordering::Equal
        }
        CompareOp::Lt => {
            left_value.partial_cmp(&right_value).ok_or_else(err)? == std::cmp::Ordering::Less
        }
        CompareOp::Lte => {
            left_value.partial_cmp(&right_value).ok_or_else(err)? != std::cmp::Ordering::Greater
        }
        CompareOp::Gt => {
            left_value.partial_cmp(&right_value).ok_or_else(err)? == std::cmp::Ordering::Greater
        }
        CompareOp::Gte => {
            left_value.partial_cmp(&right_value).ok_or_else(err)? != std::cmp::Ordering::Less
        }
    };
    Ok(ColumnValue::Boolean(compare_res))
}

/// A kind of `Expr` that is guaranteed to be scalar with no aggregate functions
/// within.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScalarExpr(pub(super) Expr);

impl TryFrom<Expr> for ScalarExpr {
    type Error = JournError;
    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        if let Some(agg) = expr.find_aggregate() {
            Err(err!(
                "Aggregate expression: '{}' contains an aggregate function that's not allowed here: '{}'",
                expr,
                agg
            ))
        } else {
            Ok(ScalarExpr(expr))
        }
    }
}

impl Deref for ScalarExpr {
    type Target = Expr;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl fmt::Display for ScalarExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for ScalarExpr {
    type Err = JournError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (rem, expr) = scalar_expr(s).finish().map_err(|e| convert_error(s, e))?;
        if !rem.is_empty() {
            return Err(err!("Unexpected input after parsing: '{}'", rem));
        }
        Ok(expr)
    }
}

/// An adapter type for parsing a list of scalar expressions from a comma-separated string.
#[derive(Debug, Clone)]
pub struct ScalarExprList(pub Vec<ScalarExpr>);

impl FromStr for ScalarExprList {
    type Err = JournError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parser::parse_non_aggregate(s).map(ScalarExprList)
    }
}
