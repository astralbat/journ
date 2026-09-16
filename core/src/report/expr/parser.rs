/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::err;
use crate::error::JournResult;
use crate::report::expr::aggregation::{
    AggState, CoSum, First, Last, Max, Min, Sum, SumIf, Unique,
};
use crate::report::expr::column_spec::ColumnSpec;
use crate::report::expr::plan::Plan;
use crate::report::expr::{Expr, ScalarExpr};
use nom::bytes::complete::{escaped_transform, tag_no_case};
use nom::combinator::{cut, map_res, value};
use nom::error::{VerboseError, context, convert_error};
use nom::{Err as NomErr, Parser};
use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, multispace0},
    combinator::{map, opt, recognize},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, tuple},
};
use rust_decimal::Decimal;
use smartstring::alias::String as SS;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum AggKind {
    Sum(Vec<Expr>),
    SumIf(Vec<Expr>),
    CoSum(Vec<Expr>),
    Min(Vec<Expr>),
    Max(Vec<Expr>),
    First(Vec<Expr>),
    Last(Vec<Expr>),
    Unique(Vec<Expr>),
}
impl<'h, 'a> AggKind {
    fn from_str_and_args(s: SS, args: Vec<Expr>) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "sum" => Some(AggKind::Sum(args)),
            "sumif" => Some(AggKind::SumIf(args)),
            "cosum" => Some(AggKind::CoSum(args)),
            "min" => Some(AggKind::Min(args)),
            "max" => Some(AggKind::Max(args)),
            "first" => Some(AggKind::First(args)),
            "last" => Some(AggKind::Last(args)),
            "unique" => Some(AggKind::Unique(args)),
            _ => None,
        }
    }

    pub fn make(&self) -> JournResult<Box<dyn AggState<'h, 'a> + 'h>>
    where
        'h: 'a,
    {
        match self {
            AggKind::Sum(args) => {
                Sum::new(args.clone()).map(|s| Box::new(s) as Box<dyn AggState<'h, 'a>>)
            }
            AggKind::SumIf(args) => {
                SumIf::new(args.clone()).map(|s| Box::new(s) as Box<dyn AggState<'h, 'a>>)
            }
            AggKind::CoSum(args) => {
                CoSum::new(args.clone()).map(|s| Box::new(s) as Box<dyn AggState<'h, 'a>>)
            }
            AggKind::Min(args) => {
                Min::new(args.clone()).map(|s| Box::new(s) as Box<dyn AggState<'h, 'a>>)
            }
            AggKind::Max(args) => {
                Max::new(args.clone()).map(|s| Box::new(s) as Box<dyn AggState<'h, 'a>>)
            }
            AggKind::First(args) => {
                First::new(args.clone()).map(|s| Box::new(s) as Box<dyn AggState<'h, 'a>>)
            }
            AggKind::Last(args) => {
                Last::new(args.clone()).map(|s| Box::new(s) as Box<dyn AggState<'h, 'a>>)
            }
            AggKind::Unique(args) => {
                Unique::new(args.clone()).map(|s| Box::new(s) as Box<dyn AggState<'h, 'a>>)
            }
        }
    }
}
impl fmt::Display for AggKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let join_args =
            |args: &Vec<Expr>| args.iter().map(ToString::to_string).collect::<Vec<_>>().join(",");
        match self {
            AggKind::Sum(args) => write!(f, "Sum({})", join_args(args)),
            AggKind::SumIf(args) => write!(f, "SumIf({})", join_args(args)),
            AggKind::CoSum(args) => write!(f, "CoSum({})", join_args(args)),
            AggKind::Min(args) => write!(f, "Min({})", join_args(args)),
            AggKind::Max(args) => write!(f, "Max({})", join_args(args)),
            AggKind::First(args) => write!(f, "First({})", join_args(args)),
            AggKind::Last(args) => write!(f, "Last({})", join_args(args)),
            AggKind::Unique(args) => write!(f, "Unique({})", join_args(args)),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
        };
        write!(f, "{}", symbol)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Exists,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            UnaryOp::Not => "NOT",
            UnaryOp::Exists => "EXISTS",
        };
        write!(f, "{}", symbol)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CompareOp {
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    Match,    // =~
    NotMatch, // !~
}

impl fmt::Display for CompareOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            CompareOp::Eq => "==",
            CompareOp::Neq => "!=",
            CompareOp::Lt => "<",
            CompareOp::Lte => "<=",
            CompareOp::Gt => ">",
            CompareOp::Gte => ">=",
            CompareOp::Match => "=~",
            CompareOp::NotMatch => "!~",
        };
        write!(f, "{}", symbol)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LogicalOp {
    And,
    Or,
}

impl fmt::Display for LogicalOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            LogicalOp::And => "AND",
            LogicalOp::Or => "OR",
        };
        write!(f, "{}", symbol)
    }
}

// Parse whitespace
fn ws<'a, F, O>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, VerboseError<&'a str>>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, VerboseError<&'a str>>,
{
    delimited(multispace0, inner, multispace0)
}

// Parse a string literal (e.g., "some text" or 'some text')
fn string_literal(input: &str) -> IResult<&str, SS, VerboseError<&str>> {
    let take_literal = |terminator: char, terminator_str: &'static str| {
        map(
            alt((
                escaped_transform(
                    take_while1(move |c| c != '\\' && c != terminator),
                    '\\',
                    alt((
                        value("\\", char('\\')),
                        value("\n", tag("n")),
                        value(terminator_str, char(terminator)),
                    )),
                ),
                // Match empty string
                value(String::new(), tag("")),
            )),
            SS::from,
        )
    };

    let (rem, s) = context(
        "string literal (use quotes, e.g. \"Assets..\")",
        alt((
            delimited(char('"'), take_literal('"', "\""), char('"')),
            delimited(char('\''), take_literal('\'', "'"), char('\'')),
        )),
    )(input)?;
    Ok((rem, SS::from(s)))
}

// Parse identifiers (column names, function names)
fn identifier(input: &str) -> IResult<&str, SS, VerboseError<&str>> {
    context(
        "identifier",
        map(
            escaped_transform(
                take_while1(|c: char| c != '\\' && c != '(' && c != ')' && c != ',' && c != ' '),
                '\\',
                alt((
                    value("\\", char('\\')),
                    value("(", char('(')),
                    value(")", char(')')),
                    value(",", char(',')),
                    value(" ", char(' ')),
                )),
            ),
            SS::from,
        ),
    )(input)
}

// Parse numbers
fn number(input: &str) -> IResult<&str, Decimal, VerboseError<&str>> {
    map(
        recognize(tuple((
            opt(char('-')),
            take_while1(|c: char| c.is_ascii_digit()),
            opt(pair(char('.'), take_while1(|c: char| c.is_ascii_digit()))),
        ))),
        |s: &str| s.parse().unwrap(),
    )(input)
}

fn unary_op<'h>(
    agg_functions: &RefCell<Vec<AggKind>>,
) -> impl FnMut(&'h str) -> IResult<&str, Expr, VerboseError<&str>> {
    move |input| match ws(alt((
        value(UnaryOp::Not, tag_no_case("NOT")),
        value(UnaryOp::Exists, tag_no_case("EXISTS")),
    )))(input)
    {
        Ok((rem, op)) => {
            map(ws(primary(agg_functions)), |expr| Expr::Unary { op, expr: Box::new(expr) })(rem)
        }
        Err(e) => Err(e),
    }
}

// Parse function arguments (comma-separated expressions)
fn function_args<'h>(
    agg_functions: &RefCell<Vec<AggKind>>,
) -> impl FnMut(&'h str) -> IResult<&'h str, Vec<Expr>, VerboseError<&'h str>> {
    move |input| {
        context(
            "function arguments",
            separated_list0(
                ws(char(',')),
                context("function argument expression", ws(expr(agg_functions))),
            ),
        )(input)
    }
}

fn aggregation_function(
    agg_functions: &RefCell<Vec<AggKind>>,
) -> impl FnMut(&str) -> IResult<&str, Expr, VerboseError<&str>> {
    move |input| {
        map_res(function(agg_functions), |(name, args)| {
            let kind = AggKind::from_str_and_args(name.clone(), args.clone()).ok_or_else(|| {
                NomErr::Error(VerboseError {
                    errors: vec![(
                        input,
                        nom::error::VerboseErrorKind::Context("unknown aggregation function"),
                    )],
                })
            })?;
            let mut agg_functions = agg_functions.borrow_mut();
            let index = agg_functions.iter().position(|k| *k == kind).unwrap_or_else(|| {
                agg_functions.push(kind.clone());
                agg_functions.len() - 1
            });
            Ok::<_, nom::Err<VerboseError<&str>>>(Expr::AggFunction(name, args, index))
        })(input)
    }
}

// Parse function calls: name(arg1, arg2, ...)
fn function<'h>(
    agg_functions: &RefCell<Vec<AggKind>>,
) -> impl FnMut(&'h str) -> IResult<&'h str, (SS, Vec<Expr>), VerboseError<&'h str>> {
    move |input| {
        map(
            tuple((
                identifier,
                preceded(
                    ws(char('(')),
                    cut(tuple((
                        function_args(agg_functions),
                        context(
                            "arguments should be valid identifiers or enclosed in quotes, terminating with a ')'",
                            ws(char(')')),
                        ),
                    ))),
                ),
            )),
            |(name, (args, _))| (name, args),
        )(input)
    }
}

// Parse primary expressions (atoms)
fn primary(
    agg_functions: &RefCell<Vec<AggKind>>,
) -> impl FnMut(&str) -> IResult<&str, Expr, VerboseError<&str>> {
    move |input| {
        context(
            "primary expression",
            alt((
                map(number, Expr::Number),
                map(string_literal, Expr::Literal),
                aggregation_function(agg_functions),
                map(function(agg_functions), |(name, args)| Expr::ScalarFunction {
                    name: SS::from(name),
                    args,
                }),
                unary_op(agg_functions),
                map(identifier, Expr::Identifier),
                map(delimited(ws(char('(')), expr(agg_functions), ws(char(')'))), |e| {
                    Expr::Parenthesized(Box::new(e))
                }),
            )),
        )(input)
    }
}

// Parse binary operators
fn binary_op(input: &str) -> IResult<&str, BinOp, VerboseError<&str>> {
    alt((
        map(char('+'), |_| BinOp::Add),
        map(char('-'), |_| BinOp::Sub),
        map(char('*'), |_| BinOp::Mul),
        map(char('/'), |_| BinOp::Div),
        map(char('%'), |_| BinOp::Mod),
    ))(input)
}

// Parse expressions with operator precedence (simplified)
fn binop_expr(
    agg_functions: &RefCell<Vec<AggKind>>,
) -> impl FnMut(&str) -> IResult<&str, Expr, VerboseError<&str>> {
    move |input| {
        let (input, left) = ws(primary(agg_functions))(input)?;

        // Try to parse as a sequence of binary operations
        let (input, bin_parts) = many0(tuple((ws(binary_op), ws(primary(agg_functions)))))(input)?;
        // Build left-associative expression tree
        let result = bin_parts.into_iter().fold(left, |acc, (op, right)| Expr::BinaryOp {
            left: Box::new(acc),
            op,
            right: Box::new(right),
        });

        Ok((input, result))
    }
}

fn compare<'h>(
    agg_functions: &RefCell<Vec<AggKind>>,
) -> impl FnMut(&'h str) -> IResult<&'h str, Expr, VerboseError<&'h str>> {
    move |input| {
        let (input, left) = ws(binop_expr(agg_functions))(input)?;
        match opt(tuple((
            ws(alt((
                map(tag("=="), |_| CompareOp::Eq),
                map(tag("!="), |_| CompareOp::Neq),
                map(tag("<="), |_| CompareOp::Lte),
                map(tag(">="), |_| CompareOp::Gte),
                map(char('<'), |_| CompareOp::Lt),
                map(char('>'), |_| CompareOp::Gt),
                map(tag("=~"), |_| CompareOp::Match),
                map(tag("!~"), |_| CompareOp::NotMatch),
            ))),
            ws(binop_expr(agg_functions)),
        )))(input)?
        {
            (input, Some((op, right))) => {
                Ok((input, Expr::Compare { left: Box::new(left), op, right: Box::new(right) }))
            }
            (input, None) => Ok((input, left)),
        }
    }
}

fn aliased_expr<'s, F>(f: F) -> impl FnMut(&'s str) -> IResult<&'s str, Expr, VerboseError<&'s str>>
where
    F: Parser<&'s str, Expr, VerboseError<&'s str>>,
{
    map(pair(f, opt(column_alias)), |(expr, alias)| match alias {
        Some(alias) => Expr::Aliased(Box::new(expr), SS::from(alias)),
        None => expr,
    })
}

pub(crate) fn expr<'s>(
    agg_functions: &RefCell<Vec<AggKind>>,
) -> impl FnMut(&'s str) -> IResult<&'s str, Expr, VerboseError<&'s str>> {
    move |input| {
        let (input, left) = ws(aliased_expr(compare(agg_functions)))(input)?;

        // Try to parse as a sequence of logical operations
        let (input, log_parts) = many0(tuple((
            ws(alt((
                map(tag_no_case("AND"), |_| LogicalOp::And),
                map(tag_no_case("OR"), |_| LogicalOp::Or),
            ))),
            ws(aliased_expr(compare(agg_functions))),
        )))(input)?;
        // Build left-associative expression tree
        let result = log_parts.into_iter().fold(left, |acc, (op, right)| Expr::LogicalOp {
            left: Box::new(acc),
            op,
            right: Box::new(right),
        });

        Ok((input, result))
    }
}

pub fn scalar_expr<'s>(input: &'s str) -> IResult<&'s str, ScalarExpr, VerboseError<&'s str>> {
    let agg_fns = RefCell::new(Vec::new());
    let (rem, expr) = expr(&agg_fns)(input)?;
    Ok((rem, ScalarExpr(expr)))
}

// Parse column alias: as "Alias Name"
fn column_alias(input: &str) -> IResult<&str, &str, VerboseError<&str>> {
    preceded(
        ws(tag("as")),
        alt((
            delimited(ws(char('"')), take_while1(|c: char| c != '"'), char('"')),
            delimited(ws(char('\'')), take_while1(|c: char| c != '\''), char('\'')),
            take_while1(|c: char| !c.is_whitespace() && c != ','),
        )),
    )(input)
}

// Parse a single column specification
fn column_spec<'s>(
    agg_functions: &RefCell<Vec<AggKind>>,
) -> impl FnMut(&'s str) -> IResult<&'s str, Expr, VerboseError<&'s str>> {
    expr(agg_functions)
}

fn parse_spec(input: &str) -> JournResult<(Vec<Expr>, Vec<AggKind>)> {
    let agg_functions = RefCell::new(Vec::new());
    match separated_list0(ws(char(',')), ws(column_spec(&agg_functions)))(input) {
        Ok(("", cols)) => Ok((cols, agg_functions.take())),
        Ok((remaining, _)) => Err(err!("Unexpected input after parsing: '{}'", remaining)),
        Err(NomErr::Error(e)) | Err(NomErr::Failure(e)) => Err(err!(convert_error(input, e))),
        Err(NomErr::Incomplete(_)) => Err(err!("Incomplete input while parsing")),
    }
}

// Parse the full column list: col1, col2, expr as "Alias", ...
pub fn parse_columns(input: &'_ str) -> JournResult<ColumnSpec> {
    parse_spec(input).map(|(exprs, agg_functions)| ColumnSpec::new(exprs, agg_functions))
}

/// Parses an expression that must not include aggregate functions.
pub fn parse_non_aggregate(input: &str) -> JournResult<Vec<ScalarExpr>> {
    let (exprs, _agg_functions) = parse_spec(input)?;
    exprs.into_iter().map(ScalarExpr::try_from).collect::<Result<Vec<_>, _>>()
}

pub fn parse_plan(
    column_spec: &str,
    where_conditions: Option<&str>,
    show_total: bool,
    group_by: Option<&str>,
    additional: HashMap<&'static str, &str>,
    sort_spec: Option<&str>,
    sort_ascending: bool,
    total_as: Option<&str>,
    grand_total_as: Option<&str>,
) -> JournResult<Plan> {
    let column_spec = parse_columns(column_spec)?;
    let where_conditions = where_conditions
        .map(|wc| {
            parse_non_aggregate(wc).map_err(|e| err!("Unable to parse --where").with_source(e))
        })
        .transpose()?;
    let group_by_expr = match group_by {
        Some(group_by) => Some(
            parse_non_aggregate(group_by)
                .map_err(|e| err!("Unable to parse --group-by").with_source(e))?,
        ),
        None => {
            // Auto-detect group-by based on the columns being output.
            let mut group_by_exprs = vec![];
            let mut found_agg = false;
            for expr in column_spec.exprs().iter() {
                match ScalarExpr::try_from(expr.clone()) {
                    Ok(expr) => group_by_exprs.push(expr),
                    Err(_) => found_agg = true,
                }
            }
            if !found_agg || group_by_exprs.is_empty() { None } else { Some(group_by_exprs) }
        }
    };
    let sort_exprs = sort_spec
        .map(|s| {
            parse_non_aggregate(s).map_err(|e| err!("Unable to parse --sort-by").with_source(e))
        })
        .transpose()?;

    let total_spec = total_as
        .map(|s| {
            parse_spec(s)
                .and_then(|(exprs, aggs)| {
                    match exprs.iter().find(|e| !e.is_aggregate() && !e.is_const()) {
                        Some(e) => Err(err!("Only aggregation and constant functions are allowed here, but found non-aggregate expression: {}", e)),
                        None => Ok(ColumnSpec::new(exprs, aggs)),
                    }
                })
                .map_err(|e| err!("Unable to parse --total-as").with_source(e))
        })
        .transpose()?
        .unwrap_or_else(|| column_spec.clone());

    let grand_total_spec = grand_total_as
        .map(|s| {
            parse_spec(s)
                .and_then(|(exprs, aggs)| {
                    match exprs.iter().find(|e| !e.is_aggregate() && !e.is_const()) {
                        Some(e) => Err(err!("Only aggregation and constant functions are allowed here, but found non-aggregate expression: {}", e)),
                        None => Ok(ColumnSpec::new(exprs, aggs)),
                    }
                })
                .map_err(|e| err!("Unable to parse --grand-total-as").with_source(e))
        })
        .transpose()?
        .unwrap_or_else(|| total_spec.clone());

    let mut additional_expr = HashMap::new();
    for (k, v) in additional.into_iter() {
        additional_expr.insert(k, parse_non_aggregate(v)?.remove(0));
    }
    let plan = Plan::new(
        column_spec,
        where_conditions.unwrap_or_default(),
        show_total,
        group_by_expr.unwrap_or_default(),
        additional_expr,
        sort_exprs.unwrap_or_default(),
        sort_ascending,
        total_spec,
        grand_total_spec,
    );
    plan.validate()?;
    Ok(plan)
}

#[cfg(test)]
mod tests {
    use crate::report::expr::Expr;
    use crate::report::expr::parser::{CompareOp, compare};
    use Expr::*;
    use std::cell::RefCell;

    #[test]
    fn test_compare() {
        let agg_funcs = RefCell::new(Vec::new());
        let res = compare(&agg_funcs)("iferror('abc', 'acb') < 'def'");
        assert!(res.is_ok());
        let res = res.unwrap();
        assert_eq!(
            res.1,
            Compare {
                left: Box::new(ScalarFunction {
                    name: "iferror".into(),
                    args: vec![Literal("abc".into()), Literal("acb".into())]
                }),
                op: CompareOp::Lt,
                right: Box::new(Literal("def".into()))
            }
        )
    }
}
