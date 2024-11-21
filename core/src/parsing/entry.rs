/*
 * Copyright (c) 2022-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::account::Account;
use crate::amount::AmountExpr;
use crate::date_and_time::{DateAndTime, JDate, JDateTime, JDateTimeRange, JTime};
use crate::error::parsing::{tag_err, IErrorMsg, IParseError};
use crate::journal_entry::{EntryObject, JournalEntry};
use crate::match_blocks;
use crate::metadata::Metadata;
use crate::parsing::amount::amount_expr;
use crate::parsing::text_block::block_remainder1;
use crate::parsing::text_input::{BlockInput, ConfigInput, LocatedInput, NodeInput, TextInput};
use crate::parsing::util::{
    blank_lines0, comment, recognize_rtrim, spaced_word, until_line_ending0,
};
use crate::parsing::{IParseResult, JParseResult};
use crate::posting::Posting;
use crate::valued_amount::{Valuation, ValuedAmount};
use chrono_tz::Tz;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{space0, space1};
use nom::combinator::{consumed, map, map_res, opt, recognize, rest};
use nom::multi::fold_many0;
use nom::sequence::{pair, preceded, tuple};
use nom::{Err as NomErr, Finish};
use std::sync::Arc;

pub fn date<'h, I>(input: I) -> IParseResult<'h, I, JDate<'h>>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    let df = input.config().as_herd_ref().date_format();
    JDate::parse(df)(input)
}

pub fn time<'h, I>(input: I) -> IParseResult<'h, I, JTime<'h>>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    let tf = input.config().as_herd_ref().time_format();
    JTime::parse(tf)(input)
}

/// Parser that reads the `date` and `time` separated by a single space.
pub fn datetime<'h, I>(tz: Tz) -> impl Fn(I) -> IParseResult<'h, I, JDateTime<'h>>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    move |input: I| {
        let df = input.config().as_herd_ref().date_format();
        let tf = input.config().as_herd_ref().time_format();
        JDateTime::parse(df, tf, tz)(input)
    }
}

pub fn date_and_time<'h, I>(input: I) -> IParseResult<'h, I, DateAndTime<'h>>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    let df = input.config().as_herd_ref().date_format();
    let tz = input.config().timezone();

    // Date from is required
    let (input, datetime_from) = datetime(tz)(input)?;

    // If the date range separator is present, a further datetime or time is required.
    let (input, sep) = opt(tag(".."))(input)?;
    let (input, datetime_range) = match sep {
        Some(_) => {
            let (input, datetime) = tag_err(
                IErrorMsg::DATE_OR_TIME,
                alt((
                    map(&mut datetime(tz), |dt| JDateTimeRange::new(datetime_from, Some(dt))),
                    map_res(time, |time_to| {
                        JDateTime::from_date_time(
                            JDate::new(datetime_from.naive_local().date(), df),
                            Some(time_to),
                            tz,
                        )
                        .map(|jdt| JDateTimeRange::new(datetime_from, Some(jdt)))
                    }),
                )),
            )(input)?;
            (input, datetime)
        }
        None => (input, JDateTimeRange::new(datetime_from, None)),
    };

    // Aux datetime is optional
    let (input, aux_date) = opt(preceded(tag("="), &mut datetime(tz)))(input)?;

    Ok((input, DateAndTime::new(datetime_range, aux_date)))
}

/// An account string must be prefixed by at least one space character (' ' or '\t').
fn account<'h, I>(input: I) -> IParseResult<'h, I, (&'h str, Arc<Account<'h>>)>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    let (input, (leading_space, account_str)) = pair(
        tag_err(IErrorMsg::INDENT_TOO_SMALL, space1),
        tag_err(IErrorMsg::ACCOUNT, spaced_word),
    )(input)?;

    let found_account = input.config().get_account(account_str.text().as_ref()).map(Arc::clone);
    let account = match found_account {
        Some(account) => account,
        None => input.config_mut().get_or_create_account(account_str.text()),
    };
    Ok((input, (leading_space.text(), account)))
}

/// Reads a valued amount.
/// # Examples
/// ```
/// # use journ_core::parsing::entry::{valued_amount};
/// # use journ_core::parsing::util::map_str;
/// # use journ_core::parse;
/// assert_eq!(parse!("€15", map_str(valued_amount)), Ok(("", "€15")));
/// assert_eq!(parse!("  ", map_str(valued_amount)), Err(("Unable to parse Decimal")));
/// ```
pub fn valued_amount<'h, I>(input: I) -> IParseResult<'h, I, ValuedAmount<'h>>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    let total_val = map(
        pair(recognize(tuple((space0::<I, _>, tag("@@"), space0))), amount_expr),
        |(pretext, expr)| Valuation::Total(expr.with_pretext(pretext.text()), false),
    );
    let unit_val = map(
        pair(recognize(tuple((space0::<I, _>, tag("@"), space0))), amount_expr),
        |(pretext, expr)| Valuation::Unit(expr.with_pretext(pretext.text())),
    );

    let allocator = input.config().allocator();
    let (rem, expr) = amount_expr(input)?;
    let va = || Ok(ValuedAmount::new_in(expr.clone(), allocator));
    let (rem, res) = fold_many0(consumed(alt((total_val, unit_val))), va, |va, (input, val)| {
        let mut acc = va?;

        if !val.value().is_positive() {
            return Err(NomErr::Error(IParseError::new(
                "Valuations should always be positive",
                input,
            )));
        }

        if acc.unit() == val.unit() {
            return Err(NomErr::Error(IParseError::new(
                "Valuation unit must be different from the amount unit",
                input,
            )));
        }

        acc.set_valuation(val);
        Ok(acc)
    })(rem.clone())?;
    Ok((rem, res?))
}

fn balance_assertion<'h, I>(input: I) -> IParseResult<'h, I, AmountExpr<'h>>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    map(
        pair(recognize(tuple((space0::<I, _>, tag("="), space0))), amount_expr),
        |(pretext, expr)| expr.with_pretext(pretext.text()),
    )(input)
}

pub(crate) fn posting<'h, I>(input: I) -> IParseResult<'h, I, Posting<'h>>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    let mut comment_str = None;
    let mut balance_assertion_expr = None;

    let (input, (account_leading_space, account)) = account(input)?;
    let (mut input, valued_amount) = opt(valued_amount)(input)?;

    // Try read comment
    if valued_amount.is_none() {
        (input, comment_str) = opt(map(comment, |i: I| i.text()))(input.clone())?;
    }

    if comment_str.is_none() {
        (input, balance_assertion_expr) = opt(balance_assertion)(input.clone())?;
        // Try read comment again
        (input, comment_str) = opt(map(comment, |i: I| i.text()))(input.clone())?;
    }

    Ok((
        input,
        Posting::new(
            account_leading_space,
            account,
            valued_amount.unwrap_or_else(ValuedAmount::nil),
            balance_assertion_expr,
            comment_str,
        ),
    ))
}

/// Parses metadata.
/// Metadata valued may be quoted, for example, to preserve leading spaces. The quotes can be escaped by doubling them.
pub(crate) fn metadata<'h, I: TextInput<'h> + BlockInput<'h>>(
    input: I,
) -> IParseResult<'h, I, Metadata<'h>> {
    let (input, (_pretext, _key_value)) = tuple((
        recognize(tuple((blank_lines0, space1, tag("+"), space0))),
        block_remainder1,
    ))(input)?;
    /*
    let mut md = Metadata::new(pretext.text(), key.text(), value.map(|s| s.text().trim_end()));
    md.set_raw_block(input.block());
     */
    let md = Metadata::from(input.block());
    Ok((input, md))
}

pub fn entry_date_and_remainder<'h, I>(input: I) -> IParseResult<'h, I, (DateAndTime<'h>, I)>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    let (input, (date_and_time, entry_rem)) = pair(date_and_time, rest)(input)?;
    Ok((input, (date_and_time, entry_rem)))
}

pub static E_ENTRY_EXPECTED: &str = "Entry expected";

pub fn entry<'h, 's, 'e, 'p, I>(
    input: I,
    date_and_time: DateAndTime<'h>,
) -> JParseResult<I, JournalEntry<'h>>
where
    'h: 'e,
    'e: 's,
    's: 'p,
    I: TextInput<'h>
        + ConfigInput<'h>
        + NodeInput<'h, 's, 'e, 'p>
        + BlockInput<'h>
        + LocatedInput<'h>,
{
    let parse_node = input.parse_node();

    let (input, description) =
        map(recognize_rtrim(until_line_ending0), |s: I| s.text())(input).unwrap();

    let mut entry_objs = Vec::with_capacity_in(2, input.config().allocator());
    let rem = match_blocks!(input.clone(),
        comment => |c: I| Ok(entry_objs.push(EntryObject::Comments(c.text()))),
        metadata => |md| Ok(entry_objs.push(EntryObject::Metadata(md))),
        posting => |pst| Ok(entry_objs.push(EntryObject::Posting(pst, false)))
    )
    .finish()
    .map_err(|e| NomErr::Failure(input.clone().into_err("Unable to parse entry").with_source(e)))?
    .0;

    // Create the entry after parsing to ensure configuration copy is up to date with the entry's
    // configuration.
    let mut je = JournalEntry::new(
        parse_node.node().id(),
        rem.config().clone(),
        date_and_time,
        description,
        entry_objs,
    );
    je.set_text_block(input.block());
    je.check().map_err(|e| NomErr::Failure(input.into_err("Entry check failed").with_source(e)))?;
    Ok((rem, je))
}

#[cfg(test)]
mod tests {
    use crate::parsing::entry::*;
    use crate::parsing::util::str_res;
    use crate::*;
    use chrono::{Datelike, Timelike};
    use indoc::indoc;
    use std::assert_eq;

    #[test]
    fn test_valued_amount() {
        let va = |s: &'static str| parse!(s, valued_amount).map(str_res);

        assert_eq!(va("€10"), Ok(("", "€10")));
        assert_eq!(va("-€10"), Ok(("", "-€10")));
        assert_eq!(va("€10 @@ $10"), Ok(("", "€10 @@ $10")));
        assert_eq!(va("€10 @ $1"), Ok(("", "€10 @ $1")));
        assert_eq!(va("€10 @ $1 @@ £10 + 2"), Ok(("", "€10 @ $1 @@ £10 + 2")));
        assert_eq!(va("€10    @@$10s"), Ok(("s", "€10    @@$10")));
    }

    #[test]
    fn test_datetime() {
        let mut config = config!(indoc! { r#"
            dateformat dd/mm/yyyy
            timeformat hh:mm:ss
            timezone Australia/Sydney
        "# });
        let mut dt = |s: &'static str| parse!(s, date_and_time, &mut config).0;

        let parsed = dt("01/01/2000 00:00:00").unwrap().1.datetime_from();
        assert_eq!(parsed.naive_utc().hour(), 13);
        assert_eq!(parsed.naive_utc().minute(), 0);
        assert_eq!(parsed.naive_utc().second(), 0);
        assert_eq!(parsed.naive_utc().day(), 31);
        assert_eq!(parsed.naive_utc().month(), 12);
        assert_eq!(parsed.naive_utc().year(), 1999);
    }

    #[test]
    pub fn test_datetime_z_suffix() {
        let mut config = config!(indoc! { r#"
            timezone Australia/Sydney
        "# });
        let mut dt = |s: &'static str| parse!(s, date_and_time, &mut config).0;

        let date = dt("2000-01-01 00:00:00Z").unwrap().1.datetime_from();
        assert_eq!(date.naive_utc().hour(), 0);
        assert_eq!(date.naive_utc().minute(), 0);
        assert_eq!(date.naive_utc().second(), 0);
        assert_eq!(date.naive_utc().day(), 1);
        assert_eq!(date.naive_utc().month(), 1);
        assert_eq!(date.naive_utc().year(), 2000);

        // These times are in a different timezone
        let date_and_time = dt("2000-01-01 00:00:00..23:59:59Z").unwrap().1;
        assert_eq!(date_and_time.datetime_from().naive_utc().hour(), 13);
        assert_eq!(date_and_time.datetime_from().naive_utc().minute(), 0);
        assert_eq!(date_and_time.datetime_from().naive_utc().second(), 0);
        assert_eq!(date_and_time.datetime_from().naive_utc().day(), 31);
        assert_eq!(date_and_time.datetime_from().naive_utc().month(), 12);
        assert_eq!(date_and_time.datetime_from().naive_utc().year(), 1999);
        assert_eq!(date_and_time.datetime_to().naive_utc().hour(), 23);
        assert_eq!(date_and_time.datetime_to().naive_utc().minute(), 59);
        assert_eq!(date_and_time.datetime_to().naive_utc().second(), 59);
        assert_eq!(date_and_time.datetime_to().naive_utc().day(), 1);
        assert_eq!(date_and_time.datetime_to().naive_utc().month(), 1);
        assert_eq!(date_and_time.datetime_to().naive_utc().year(), 2000);
    }

    #[test]
    fn test_posting() {
        let parse = |s: &'static str| parse!(s, posting).map(str_res);

        assert_eq!(parse(" ACC"), Ok(("", " ACC")));
        assert_eq!(parse("  ACC     $10"), Ok(("", "  ACC     $10")));
        assert_eq!(parse(" ACC; COMMENT"), Ok(("", " ACC; COMMENT")));
        assert_eq!(parse(" ACC  $10 ; COMMENT"), Ok(("", " ACC  $10 ; COMMENT")));
        assert_eq!(parse(" ACC  $10 = $20 ; COMMENT"), Ok(("", " ACC  $10 = $20 ; COMMENT")));
    }

    #[test]
    fn test_metadata() {
        let parse = |s: &'static str| parse!(s, metadata);
        let md = |p, k, i, v: Option<&'static str>| Metadata::new(p, k, i, v);

        assert_eq!(parse(" +k  v"), Ok(("", md(" +", "k", "  ", Some("v")))));
        assert_eq!(
            parse(" +key with spaces  v"),
            Ok(("", md(" +", "key with spaces", "  ", Some("v"))))
        );
        assert_eq!(parse(" +key  v"), Ok(("", md(" +", "key", "  ", Some("v")))));
        assert_eq!(
            parse(" +key  v\ndirective"),
            Ok(("directive", md(" +", "key", "  ", Some("v"))))
        );

        let metadata = parse(" +key  v\n   directive").unwrap().1;
        assert_eq!(metadata.properties().get(0), Some(&md("   ", "directive", "", None)));
    }
}
