/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::column_value::ColumnValue;
use crate::expr::context::IdentifierContext;
use crate::expr::functions::{
    concat, cond, date, datevalue, max, min, neg, num, round, text, value,
};
use crate::expr::parser::{BinOp, CompareOp, LogicalOp};
use journ_core::amount::Amount;
use journ_core::err;
use journ_core::error::{JournError, JournResult};
use rust_decimal::Decimal;
use smartstring::alias::String as SS;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Expr<'h> {
    ScalarFunction { name: SS, args: Vec<Expr<'h>> },
    AggFunction(&'h str, Vec<Expr<'h>>, usize), // Index into separate list of unique aggregate functions
    BinaryOp { left: Box<Expr<'h>>, op: BinOp, right: Box<Expr<'h>> },
    Compare { left: Box<Expr<'h>>, op: CompareOp, right: Box<Expr<'h>> },
    LogicalOp { left: Box<Expr<'h>>, op: LogicalOp, right: Box<Expr<'h>> },
    Parenthesized(Box<Expr<'h>>),
    Literal(&'h str),
    Identifier(SS),
    Number(Decimal),
    Aliased(Box<Expr<'h>>, &'h str),
}

impl<'h> Expr<'h> {
    pub fn eval(&self, context: &mut dyn IdentifierContext<'h>) -> JournResult<ColumnValue<'h>> {
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
            LogicalOp { left, op, right } => {
                let left_value = left.eval(context)?;
                let right_value = right.eval(context)?;
                eval_logical_op(left_value, right_value, *op)
            }
            Literal(lit) => Ok(ColumnValue::StringRef(lit)),
            Identifier(name) => {
                context.eval_identifier(name).ok_or_else(|| err!("Invalid identifier: '{}'", name))
            }
            Number(num) => Ok(ColumnValue::Amount(Amount::nil().with_quantity(*num))),
            Aliased(inner, alias) => {
                let value = inner.eval(context)?;
                context.set_identifier(alias, value.clone());
                Ok(value)
            }
        }
    }

    /// Iterates over this expression and all sub-expressions in depth-first order.
    pub fn iter(&self) -> ExprIter<'h, '_> {
        ExprIter { stack: vec![self] }
    }

    pub fn children(&self) -> impl Iterator<Item = &Expr<'h>> + '_ {
        self.iter().skip(1)
    }

    /// Compares two expressions for equality, treating aliased expressions as equal if either the inner expression or the alias matches.
    pub fn eq_expr_or_alias(&self, other: &Expr<'h>) -> bool {
        use Expr::*;
        match (self, other) {
            (Aliased(inner_a, alias_a), Aliased(inner_b, alias_b)) => {
                inner_a == inner_b || alias_a.eq_ignore_ascii_case(alias_b)
            }
            _ => self == other,
        }
    }
}
impl fmt::Display for Expr<'_> {
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
            Literal(lit) => write!(f, "\"{}\"", lit),
            Identifier(name) => write!(f, "{}", name),
            Number(num) => write!(f, "{}", num),
            Aliased(_inner, alias) => write!(f, "{}", alias),
        }
    }
}

impl PartialEq for Expr<'_> {
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

pub struct ExprIter<'h, 'a> {
    stack: Vec<&'a Expr<'h>>,
}
impl<'h, 'a> Iterator for ExprIter<'h, 'a> {
    type Item = &'a Expr<'h>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(expr) = self.stack.pop() {
            use Expr::*;
            match expr {
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
}

fn eval_scalar_function<'h, 'a>(
    name: &'a str,
    args: &[Expr<'h>],
    context: &mut dyn IdentifierContext<'h>,
) -> JournResult<ColumnValue<'h>> {
    if name.eq_ignore_ascii_case("value") {
        value(args, context)
    } else if name.eq_ignore_ascii_case("-") {
        neg(args, context)
    } else if name.eq_ignore_ascii_case("round") {
        round(args, context)
    } else if name.eq_ignore_ascii_case("num") {
        num(args, context)
    } else if name.eq_ignore_ascii_case("max") {
        max(args, context)
    } else if name.eq_ignore_ascii_case("min") {
        min(args, context)
    } else if name.eq_ignore_ascii_case("concat") {
        concat(args, context)
    } else if name.eq_ignore_ascii_case("text") {
        text(&args, context)
    } else if name.eq_ignore_ascii_case("date") {
        date(args, context)
    } else if name.eq_ignore_ascii_case("datevalue") {
        datevalue(args, context)
    } else if name.eq_ignore_ascii_case("if") {
        cond(args, context)
    } else {
        Err(err!("Unknown function: '{}'", name))
    }
}

fn eval_binary_op<'h, 'a>(
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

    let mut results = Vec::new();
    for mapping in left.map(&right) {
        match mapping {
            (Some(ColumnValue::Amount(a)), Some(ColumnValue::Amount(b))) => {
                results.push(
                    binary_op!(a, b, op)
                        .map(|amount| ColumnValue::Amount(amount))
                        .unwrap_or_else(|_: JournError| ColumnValue::Undefined),
                );
            }
            _ => {
                return Err(err!("Binary operations are only supported for `Amount` type"));
            }
        }
    }
    if results.is_empty() {
        Ok(ColumnValue::Undefined)
    } else if results.len() == 1 {
        Ok(results.into_iter().next().unwrap())
    } else {
        Ok(ColumnValue::List(results))
    }
}

fn eval_compare_op<'h, 'a>(
    left_value: ColumnValue<'h>,
    right_value: ColumnValue<'h>,
    op: CompareOp,
) -> JournResult<ColumnValue<'h>> {
    if left_value.is_undefined() || right_value.is_undefined() {
        return Ok(ColumnValue::Undefined);
    }

    let compare_res = match op {
        CompareOp::Match => left_value.matches(&right_value),
        CompareOp::NotMatch => left_value.matches(&right_value).map(|b| !b),
        CompareOp::Eq => left_value.cmp(&right_value).map(|o| o == std::cmp::Ordering::Equal),
        CompareOp::Neq => left_value.cmp(&right_value).map(|o| o != std::cmp::Ordering::Equal),
        CompareOp::Lt => left_value.cmp(&right_value).map(|o| o == std::cmp::Ordering::Less),
        CompareOp::Lte => left_value.cmp(&right_value).map(|o| o != std::cmp::Ordering::Greater),
        CompareOp::Gt => left_value.cmp(&right_value).map(|o| o == std::cmp::Ordering::Greater),
        CompareOp::Gte => left_value.cmp(&right_value).map(|o| o != std::cmp::Ordering::Less),
    };
    compare_res.map(ColumnValue::Boolean).ok_or_else(|| {
        err!("Comparison operation '{} {} {}' is not valid", left_value, op, right_value)
    })
}

fn eval_logical_op<'h, 'a>(
    left_value: ColumnValue<'h>,
    right_value: ColumnValue<'h>,
    op: LogicalOp,
) -> JournResult<ColumnValue<'h>> {
    if left_value.is_undefined() || right_value.is_undefined() {
        return Ok(ColumnValue::Undefined);
    }

    let left_bool = left_value
        .as_bool()
        .ok_or_else(|| err!("Left operand of logical operation '{}' is not a boolean", op))?;
    let right_bool = right_value
        .as_bool()
        .ok_or_else(|| err!("Right operand of logical operation '{}' is not a boolean", op))?;
    let result = match op {
        LogicalOp::And => left_bool && right_bool,
        LogicalOp::Or => left_bool || right_bool,
    };
    Ok(ColumnValue::Boolean(result))
}
