/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::err;
use crate::error::JournResult;
use crate::report::expr::Expr;
use crate::report::expr::aggregation::{
    AggState, CoSum, First, Last, Max, Min, Sum, SumIf, Unique,
};
use crate::report::expr::column_spec::ColumnSpec;
use crate::report::expr::plan::Plan;
use nom::Err as NomErr;
use nom::bytes::complete::{tag_no_case, take_while};
use nom::combinator::{cut, map_res};
use nom::error::{VerboseError, context, convert_error};
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
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum AggKind<'h> {
    Sum(Vec<Expr<'h>>),
    SumIf(Vec<Expr<'h>>),
    CoSum(Vec<Expr<'h>>),
    Min(Vec<Expr<'h>>),
    Max(Vec<Expr<'h>>),
    First(Vec<Expr<'h>>),
    Last(Vec<Expr<'h>>),
    Unique(Vec<Expr<'h>>),
}
impl<'h> AggKind<'h> {
    fn from_str_and_args(s: &str, args: Vec<Expr<'h>>) -> Option<Self> {
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

    pub fn make(&self) -> JournResult<Box<dyn AggState<'h> + 'h>> {
        match self {
            AggKind::Sum(args) => {
                Sum::new(args.clone()).map(|s| Box::new(s) as Box<dyn AggState<'h>>)
            }
            AggKind::SumIf(args) => {
                SumIf::new(args.clone()).map(|s| Box::new(s) as Box<dyn AggState<'h>>)
            }
            AggKind::CoSum(args) => {
                CoSum::new(args.clone()).map(|s| Box::new(s) as Box<dyn AggState<'h>>)
            }
            AggKind::Min(args) => {
                Min::new(args.clone()).map(|s| Box::new(s) as Box<dyn AggState<'h>>)
            }
            AggKind::Max(args) => {
                Max::new(args.clone()).map(|s| Box::new(s) as Box<dyn AggState<'h>>)
            }
            AggKind::First(args) => {
                First::new(args.clone()).map(|s| Box::new(s) as Box<dyn AggState<'h>>)
            }
            AggKind::Last(args) => {
                Last::new(args.clone()).map(|s| Box::new(s) as Box<dyn AggState<'h>>)
            }
            AggKind::Unique(args) => {
                Unique::new(args.clone()).map(|s| Box::new(s) as Box<dyn AggState<'h>>)
            }
        }
    }
}
impl fmt::Display for AggKind<'_> {
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
fn string_literal(input: &str) -> IResult<&str, &str, VerboseError<&str>> {
    context(
        "string literal (use quotes, e.g. \"Assets..\")",
        alt((
            delimited(char('"'), take_while(|c: char| c != '"'), char('"')),
            delimited(char('\''), take_while(|c: char| c != '\''), char('\'')),
        )),
    )(input)
}

// Parse identifiers (column names, function names)
fn identifier(input: &str) -> IResult<&str, &str, VerboseError<&str>> {
    context(
        "identifier",
        map(
            take_while1(|c: char| {
                // Metadata can have '-' in keys. E.g. "+CAG-Note"
                c.is_alphanumeric() || c == '_' || c == ':' || c == '.' || c == '+' || c == '-'
            }),
            |s: &str| s,
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

// Parse function arguments (comma-separated expressions)
fn function_args<'h>(
    agg_functions: &RefCell<Vec<AggKind<'h>>>,
) -> impl FnMut(&'h str) -> IResult<&'h str, Vec<Expr<'h>>, VerboseError<&'h str>> {
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

fn aggregation_function<'h>(
    agg_functions: &RefCell<Vec<AggKind<'h>>>,
) -> impl FnMut(&'h str) -> IResult<&'h str, Expr<'h>, VerboseError<&'h str>> {
    move |input| {
        map_res(function(agg_functions), |(name, args)| {
            let kind = AggKind::from_str_and_args(name, args.clone()).ok_or_else(|| {
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
    agg_functions: &RefCell<Vec<AggKind<'h>>>,
) -> impl FnMut(&'h str) -> IResult<&'h str, (&'h str, Vec<Expr<'h>>), VerboseError<&'h str>> {
    move |input| {
        map(
            tuple((
                alt((identifier, tag("-"))),
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
fn primary<'h>(
    agg_functions: &RefCell<Vec<AggKind<'h>>>,
) -> impl FnMut(&'h str) -> IResult<&'h str, Expr<'h>, VerboseError<&'h str>> {
    move |input| {
        context(
            "primary expression",
            alt((
                map(number, Expr::Number),
                map(string_literal, Expr::Literal),
                aggregation_function(agg_functions),
                map(function(agg_functions), |(name, args)| Expr::ScalarFunction {
                    name: name.into(),
                    args,
                }),
                map(identifier, |s| Expr::Identifier(s.into())),
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
fn binop_expr<'h>(
    agg_functions: &RefCell<Vec<AggKind<'h>>>,
) -> impl FnMut(&'h str) -> IResult<&'h str, Expr<'h>, VerboseError<&'h str>> {
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
    agg_functions: &RefCell<Vec<AggKind<'h>>>,
) -> impl FnMut(&'h str) -> IResult<&'h str, Expr<'h>, VerboseError<&'h str>> {
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

fn expr<'h>(
    agg_functions: &RefCell<Vec<AggKind<'h>>>,
) -> impl FnMut(&'h str) -> IResult<&'h str, Expr<'h>, VerboseError<&'h str>> {
    move |input| {
        let (input, left) = ws(compare(agg_functions))(input)?;

        // Try to parse as a sequence of logical operations
        let (input, log_parts) = many0(tuple((
            ws(alt((
                map(tag_no_case("AND"), |_| LogicalOp::And),
                map(tag_no_case("OR"), |_| LogicalOp::Or),
            ))),
            ws(compare(agg_functions)),
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
fn column_spec<'h>(
    agg_functions: &RefCell<Vec<AggKind<'h>>>,
) -> impl FnMut(&'h str) -> IResult<&'h str, Expr<'h>, VerboseError<&'h str>> {
    move |input| {
        map(tuple((expr(agg_functions), opt(column_alias))), |(expr, alias)| match alias {
            Some(alias) => Expr::Aliased(Box::new(expr), alias),
            None => expr,
        })(input)
    }
}

// Parse the full column list: col1, col2, expr as "Alias", ...
pub fn parse_columns(input: &'_ str) -> JournResult<ColumnSpec<'_>> {
    let agg_functions = RefCell::new(Vec::new());
    match separated_list0(ws(char(',')), ws(column_spec(&agg_functions)))(input) {
        Ok(("", cols)) => Ok(ColumnSpec::new(cols, agg_functions.take())),
        Ok((remaining, _)) => Err(err!("Unexpected input after parsing columns: '{}'", remaining)),
        Err(NomErr::Error(e)) | Err(NomErr::Failure(e)) => Err(err!(convert_error(input, e))),
        Err(NomErr::Incomplete(_)) => Err(err!("Incomplete input while parsing columns")),
    }
}

pub fn parse_non_aggregate<'h>(input: &'h str) -> JournResult<Vec<Expr<'h>>> {
    let agg_functions = RefCell::new(Vec::new());
    let res = match separated_list0(ws(char(',')), ws(column_spec(&agg_functions)))(input) {
        Ok(("", col)) => Ok(col),
        Ok((remaining, _)) => Err(err!("Unexpected input after parsing: '{}'", remaining)),
        Err(NomErr::Error(e)) | Err(NomErr::Failure(e)) => Err(err!(convert_error(input, e))),
        Err(NomErr::Incomplete(_)) => Err(err!("Incomplete input while parsing")),
    };
    if !agg_functions.borrow().is_empty() {
        Err(err!("Aggregation functions are not allowed here"))
    } else {
        res
    }
}

pub fn parse_plan<'h>(
    column_spec: &'h str,
    where_conditions: Option<&'h str>,
    show_total: bool,
    group_by: Option<&'h str>,
    additional: HashMap<&'static str, &'h str>,
    sort_spec: Option<&'h str>,
    sort_ascending: bool,
) -> JournResult<Plan<'h>> {
    let column_spec = parse_columns(column_spec)?;
    let where_conditions = where_conditions
        .map(|wc| {
            parse_non_aggregate(wc).map_err(|e| err!("Unable to parse --where").with_source(e))
        })
        .transpose()?;
    let group_by_expr = group_by
        .map(|s| {
            parse_non_aggregate(s).map_err(|e| err!("Unable to parse --group-by").with_source(e))
        })
        .transpose()?;
    let sort_exprs = sort_spec
        .map(|s| {
            parse_non_aggregate(s).map_err(|e| err!("Unable to parse --sort-by").with_source(e))
        })
        .transpose()?;

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
    );
    plan.validate()?;
    Ok(plan)
}
