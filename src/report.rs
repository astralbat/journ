/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::column_expr::ColumnExpr;
use journ_core::err;
use journ_core::error::JournResult;
use journ_core::reporting::table::Cell;
use nom::Err as NomErr;
use nom::combinator::cut;
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
use std::fmt;

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

#[derive(Debug, Clone, PartialEq)]
pub struct ColumnSpec<'s> {
    pub expr: ColumnExpr<'s>,
    //pub alias: Option<&'s str>,
}

impl<'s> ColumnSpec<'s> {
    pub fn alias(&self) -> Option<&'s str> {
        match &self.expr {
            ColumnExpr::Aliased(_, alias) => Some(*alias),
            _ => None,
        }
    }
}

impl From<&ColumnExpr<'_>> for Cell<'_> {
    fn from(spec: &ColumnExpr<'_>) -> Self {
        let s = spec.to_string();
        // Capitalize the first letter of the column name
        let s = if let Some(first) = s.chars().next() {
            first.to_uppercase().to_string() + &s[1..]
        } else {
            s
        };
        Cell::from(s)
    }
}

// Parse whitespace
fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, VerboseError<&'a str>>
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
            delimited(char('"'), take_while1(|c: char| c != '"'), char('"')),
            delimited(char('\''), take_while1(|c: char| c != '\''), char('\'')),
        )),
    )(input)
}

// Parse identifiers (column names, function names)
fn identifier(input: &str) -> IResult<&str, &str, VerboseError<&str>> {
    context(
        "identifier",
        map(
            take_while1(|c: char| c.is_alphanumeric() || c == '_' || c == ':' || c == '.'),
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
fn function_args(input: &str) -> IResult<&str, Vec<ColumnExpr>, VerboseError<&str>> {
    context(
        "function arguments",
        separated_list0(
            ws(char(',')),
            context("function argument expression", ws(expr)), // Now parsing full expressions, not just strings
        ),
    )(input)
}

// Parse function calls: name(arg1, arg2, ...)
fn function_call(input: &str) -> IResult<&str, ColumnExpr, VerboseError<&str>> {
    map(
        tuple((
            alt((identifier, tag("-"))),
            preceded(
                ws(char('(')),
                cut(tuple((
                    function_args,
                    context(
                        "arguments should be valid identifiers or enclosed in quotes, terminating with a ')'",
                        ws(char(')')),
                    ),
                ))),
            ),
        )),
        |(name, (args, _))| ColumnExpr::Function { name, args },
    )(input)
}

// Parse primary expressions (atoms)
fn primary(input: &str) -> IResult<&str, ColumnExpr, VerboseError<&str>> {
    context(
        "primary expression",
        alt((
            map(number, ColumnExpr::Number),
            map(string_literal, ColumnExpr::Literal),
            function_call,
            map(identifier, ColumnExpr::Column),
            map(delimited(ws(char('(')), expr, ws(char(')'))), |e| {
                ColumnExpr::Parenthesized(Box::new(e))
            }),
        )),
    )(input)
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
fn binop_expr(input: &str) -> IResult<&str, ColumnExpr, VerboseError<&str>> {
    let (input, left) = ws(primary)(input)?;

    // Try to parse as a sequence of binary operations
    let (input, bin_parts) = many0(tuple((ws(binary_op), ws(primary))))(input)?;
    // Build left-associative expression tree
    let result = bin_parts.into_iter().fold(left, |acc, (op, right)| ColumnExpr::BinaryOp {
        left: Box::new(acc),
        op,
        right: Box::new(right),
    });

    Ok((input, result))
}

fn expr(input: &str) -> IResult<&str, ColumnExpr, VerboseError<&str>> {
    let (input, left) = ws(binop_expr)(input)?;
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
        ws(binop_expr),
    )))(input)?
    {
        (input, Some((op, right))) => Ok((
            input,
            ColumnExpr::CompareExpr { left: Box::new(left), op, right: Box::new(right) },
        )),
        (input, None) => Ok((input, left)),
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
fn column_spec(input: &str) -> IResult<&str, ColumnExpr, VerboseError<&str>> {
    map(tuple((expr, opt(column_alias))), |(expr, alias)| match alias {
        Some(alias) => ColumnExpr::Aliased(Box::new(expr), alias),
        None => expr,
    })(input)
}

// Parse the full column list: col1, col2, expr as "Alias", ...
pub fn parse_columns(input: &str) -> JournResult<Vec<ColumnExpr>> {
    match separated_list0(ws(char(',')), ws(column_spec))(input) {
        Ok(("", cols)) => Ok(cols),
        Ok((remaining, _)) => Err(err!("Unexpected input after parsing columns: '{}'", remaining)),
        Err(NomErr::Error(e)) | Err(NomErr::Failure(e)) => Err(err!(convert_error(input, e))),
        Err(NomErr::Incomplete(_)) => Err(err!("Incomplete input while parsing columns")),
    }
}
