/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::column_value::ColumnValue;
use crate::expr::context::{EvalContext, IdentifierContext};
use crate::expr::functions::{concat, date, datevalue, max, min, neg, num, round, text, value};
use crate::expr::parser::{AggKind, BinOp, CompareOp, LogicalOp};
use journ_core::amount::Amount;
use journ_core::err;
use journ_core::error::{JournError, JournResult};
use journ_core::ext::VecLike;
use rust_decimal::Decimal;
use smallvec::SmallVec;
use smartstring::alias::String as SS;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Expr<'h> {
    ScalarFunction { name: SS, args: Vec<Expr<'h>> },
    AggFunction(&'h str, Vec<Expr<'h>>, usize), // Index into separate list of unique aggregate functions
    BinaryOp { left: Box<Expr<'h>>, op: BinOp, right: Box<Expr<'h>> },
    CompareExpr { left: Box<Expr<'h>>, op: CompareOp, right: Box<Expr<'h>> },
    LogicalOpExpr { left: Box<Expr<'h>>, op: LogicalOp, right: Box<Expr<'h>> },
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
            ScalarFunction { name, args } => {
                let values = args
                    .into_iter()
                    .map(|a| a.eval(context))
                    .collect::<Result<SmallVec<[ColumnValue; 4]>, _>>()?;
                eval_scalar_function(name, values, context)
            }
            AggFunction(_, _, index) => context
                .eval_aggregate(*index)
                .ok_or_else(|| err!("Unable to evaluate aggregate function: {}", index)),
            BinaryOp { left, op, right } => {
                let left_value = left.eval(context)?;
                let right_value = right.eval(context)?;
                eval_binary_op(left_value, right_value, *op)
            }
            Parenthesized(inner) => inner.eval(context),
            CompareExpr { left, op, right } => {
                let left_value = left.eval(context)?;
                let right_value = right.eval(context)?;
                eval_compare_op(left_value, right_value, *op)
            }
            LogicalOpExpr { left, op, right } => {
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

    pub fn children(&self) -> impl Iterator<Item = &Expr> {
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
            CompareExpr { left, op, right } => {
                write!(f, "({} {} {})", left, op, right)
            }
            LogicalOpExpr { left, op, right } => {
                write!(f, "({} {} {})", left, op, right)
            }
            Literal(lit) => write!(f, "\"{}\"", lit),
            Identifier(name) => write!(f, "{}", name),
            Number(num) => write!(f, "{}", num),
            Aliased(inner, alias) => write!(f, "{} as {}", inner, alias),
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
                CompareExpr { left: left_a, op: op_a, right: right_a },
                CompareExpr { left: left_b, op: op_b, right: right_b },
            ) => op_a == op_b && left_a == left_b && right_a == right_b,
            (
                LogicalOpExpr { left: left_a, op: op_a, right: right_a },
                LogicalOpExpr { left: left_b, op: op_b, right: right_b },
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
                CompareExpr { left, right, .. } => {
                    self.stack.push(right);
                    self.stack.push(left);
                }
                LogicalOpExpr { left, right, .. } => {
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

fn eval_scalar_function<'h, 'a, V: VecLike<ColumnValue<'h>>>(
    name: &'a str,
    mut args: V,
    context: &dyn IdentifierContext<'h>,
) -> JournResult<ColumnValue<'h>> {
    if name.eq_ignore_ascii_case("value") {
        value(args.as_mut_slice(), context)
    } else if name.eq_ignore_ascii_case("-") {
        neg(args.as_mut_slice())
    } else if name.eq_ignore_ascii_case("round") {
        round(args.as_mut_slice())
    } else if name.eq_ignore_ascii_case("num") {
        num(args.as_mut_slice())
    } else if name.eq_ignore_ascii_case("max") {
        max(args.as_mut_slice())
    } else if name.eq_ignore_ascii_case("min") {
        min(args.as_mut_slice())
    } else if name.eq_ignore_ascii_case("concat") {
        concat(args.as_mut_slice())
    } else if name.eq_ignore_ascii_case("text") {
        text(args.as_mut_slice())
    } else if name.eq_ignore_ascii_case("date") {
        date(args.as_mut_slice())
    } else if name.eq_ignore_ascii_case("datevalue") {
        datevalue(args.as_mut_slice())
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
        (ColumnValue::Amount(a), ColumnValue::Amount(b)) => Ok(binary_op!(a, b)
            .map(|amount| ColumnValue::Amount(amount))
            .unwrap_or_else(|_: JournError| ColumnValue::Undefined)),
        (v, _) | (_, v) if v.is_undefined() => Ok(ColumnValue::Undefined),
        (ColumnValue::List(va), ColumnValue::List(vb)) => {
            let mut vec_res = Vec::with_capacity(va.len());
            // Should be safe to assume lists are normalized - each unit appears only once.
            // Lists do not have to have the same units, just operate where units match.
            for cv_a in va.into_iter() {
                match cv_a.as_amount() {
                    Some(a) => {
                        if let Some(b) = vb
                            .iter()
                            .filter_map(ColumnValue::as_amount)
                            .filter(|b| a.unit() == b.unit())
                            .next()
                        {
                            binary_op!(a, b)
                                .map(|c| {
                                    vec_res.push(ColumnValue::Amount(c));
                                })
                                .unwrap_or_else(|_: JournError| {
                                    vec_res.push(ColumnValue::Undefined);
                                });
                        }
                    }
                    None => {
                        return Err(err!("Binary operations are only supported for `Amount` type"));
                    }
                }
            }
            Ok(ColumnValue::List(vec_res))
        }
        (left, right) => Err(err!("Binary operations are only supported for `Amount` type")
            .with_source(err!(
                "Left operand: {}, Right operand: {}, Operation: {}",
                left,
                right,
                op
            ))),
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
