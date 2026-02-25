/*
 * Copyright (c) 2022-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::account::Account;
use crate::amount::Amount;
use crate::datetime::DateAndTime;
use crate::datetime::{JDate, JDateTime, JDateTimeRange, JTime};
use crate::error::parsing::{IErrorMsg, IParseError, promote, tag_err};
use crate::journal_entry::{EntryObject, JournalEntry};
use crate::match_blocks;
use crate::metadata::Metadata;
use crate::parsing::amount::amount_expr;
use crate::parsing::input::{BlockInput, ConfigInput, LocatedInput, NodeInput, TextInput};
use crate::parsing::text_block::block_remainder1;
use crate::parsing::util::{
    blank_lines0, comment, recognize_rtrim, spaced_word, until_line_ending0,
};
use crate::parsing::{IParseResult, JParseResult};
use crate::posting::Posting;
use crate::valued_amount::{PostingValuation, ValuedAmount};
use chrono_tz::Tz;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{space0, space1};
use nom::combinator::{consumed, map, map_res, opt, recognize, rest};
use nom::multi::fold_many0;
use nom::sequence::{pair, preceded, tuple};
use nom::{Err as NomErr, Finish};
use std::sync::Arc;

pub fn date<'h, I>(input: I) -> IParseResult<'h, I, JDate>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    let df = input.config().date_format();
    JDate::parse(df)(input)
}

pub fn time<'h, I>(input: I) -> IParseResult<'h, I, JTime>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    let tf = input.config().time_format();
    JTime::parse(tf)(input)
}

/// Parser that reads the `date` and `time` in the configured DateTime format.
pub fn datetime<'h, I>(tz: Tz) -> impl Fn(I) -> IParseResult<'h, I, JDateTime>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    move |input: I| {
        let dtf = input.config().datetime_format();
        JDateTime::parse(dtf, tz)(input)
    }
}

pub fn date_and_time<'h, I>(input: I) -> IParseResult<'h, I, DateAndTime>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
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
                        datetime_from
                            .with_time(time_to)
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
        |(_pretext, expr)| PostingValuation::new_total(expr, false),
    );
    let unit_val = map(
        pair(recognize(tuple((space0::<I, _>, tag("@"), space0))), amount_expr),
        |(_pretext, expr)| PostingValuation::new_unit(expr),
    );

    let allocator = input.config().allocator();
    let (rem, expr) = amount_expr(input)?;
    let va = || Ok(ValuedAmount::new_in(expr.clone(), allocator));
    let (rem, res) =
        fold_many0(consumed(alt((total_val, unit_val))), va, |va, (input, mut val)| {
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

            // Set the sign of the total valuation to match the primary amount.
            // This is the internal representation for total valuations.
            if val.is_total() && expr.is_negative() {
                let negated = -val.value();
                val = PostingValuation::new_total(negated, false)
            }

            acc.set_valuation(val);
            Ok(acc)
        })(rem.clone())?;
    Ok((rem, res?))
}

fn balance_assertion<'h, I>(input: I) -> IParseResult<'h, I, Amount<'h>>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    map(
        pair(recognize(tuple((space0::<I, _>, tag("="), space0))), amount_expr),
        |(_pretext, expr)| expr,
    )(input)
}

pub(crate) fn posting<'h, I>(input: I) -> IParseResult<'h, I, Posting<'h>>
where
    I: TextInput<'h> + ConfigInput<'h> + BlockInput<'h>,
{
    let mut comment_str = None;
    let mut balance_assertion_expr = None;

    // Read the account
    let (input, (account_leading_space, account)) = account(input)?;

    // Read an optional valued amount
    let (mut input, mut valued_amount) = opt(valued_amount)(input)?;

    // Replace the `valued_amount` primary unit if specified on the account (overrides global config)
    if let Some(valued_amount) = &mut valued_amount
        && let Some(unit) = account.unit(valued_amount.unit().code())
    {
        valued_amount.set_amount(unit.with_quantity(valued_amount.amount().quantity()));
    }

    // Try read comment
    if valued_amount.is_none() {
        (input, comment_str) = opt(map(comment, |i: I| i.text()))(input.clone())?;
    }

    if comment_str.is_none() {
        (input, balance_assertion_expr) = opt(balance_assertion)(input.clone())?;
        // Try read comment again
        (input, comment_str) = opt(map(comment, |i: I| i.text()))(input.clone())?;
    }

    let block = input.block();
    Ok((
        input,
        Posting::new(
            Some(block),
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
pub(crate) fn metadata<'h, I: TextInput<'h> + BlockInput<'h> + ConfigInput<'h>>(
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
    let md = Metadata::lazy(input.config().clone(), input.block());
    Ok((input, md))
}

pub fn entry_date_and_remainder<'h, I>(input: I) -> IParseResult<'h, I, (DateAndTime, I)>
where
    I: TextInput<'h> + ConfigInput<'h>,
{
    let (input, (date_and_time, entry_rem)) = pair(date_and_time, rest)(input)?;
    Ok((input, (date_and_time, entry_rem)))
}

pub static E_ENTRY_EXPECTED: &str = "Entry expected";

pub fn entry<'h, 's, 'e, 'p, I>(input: I) -> JParseResult<I, JournalEntry<'h>>
where
    'h: 'e,
    'e: 's,
    's: 'p,
    I: TextInput<'h> + ConfigInput<'h> + NodeInput<'h, 's, 'p> + BlockInput<'h> + LocatedInput<'h>,
{
    let (_rem, (date, input)) =
        promote("Unable to read entry date", entry_date_and_remainder)(input)?;
    entry_with_date(input, date)
}

pub fn entry_with_date<'h, 's, 'e, 'p, I>(
    input: I,
    date_and_time: DateAndTime,
) -> JParseResult<I, JournalEntry<'h>>
where
    'h: 'e,
    'e: 's,
    's: 'p,
    I: TextInput<'h> + ConfigInput<'h> + NodeInput<'h, 's, 'p> + BlockInput<'h> + LocatedInput<'h>,
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
        let mut dt = |s: &'static str| parse!(s, date_and_time, &mut config);

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
        let mut dt = |s: &'static str| parse!(s, date_and_time, &mut config);

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
        let parse = |s: &'static str| {
            parse_block!(s, posting).map(|(rem, (_config, out))| (rem, out)).map(str_res)
        };

        assert_eq!(parse(" ACC"), Ok(("", " ACC")));
        assert_eq!(parse("  ACC     $10"), Ok(("", "  ACC     $10")));
        assert_eq!(parse(" ACC; COMMENT"), Ok(("", " ACC; COMMENT")));
        assert_eq!(parse(" ACC  $10 ; COMMENT"), Ok(("", " ACC  $10 ; COMMENT")));
        assert_eq!(parse(" ACC  $10 = $20 ; COMMENT"), Ok(("", " ACC  $10 = $20 ; COMMENT")));
    }

    #[test]
    fn test_metadata() {
        let parse = |s: &'static str| parse_block!(s, metadata);

        let res = parse(" +k  v");
        assert!(res.is_ok());
        let (rem, out) = res.unwrap();
        assert_eq!(rem.len(), 0);
        let (_config, md) = out;
        assert_eq!(md.key(), "k");
        assert_eq!(md.value(), Some("v"));

        let res = parse(" +key with spaces  v");
        assert!(res.is_ok());
        let (rem, out) = res.unwrap();
        assert_eq!(rem.len(), 0);
        let (_config, md) = out;
        assert_eq!(md.key(), "key with spaces");
        assert_eq!(md.value(), Some("v"));

        let res = parse(" +key  v\ndirective");
        assert!(res.is_ok());
        let (rem, out) = res.unwrap();
        assert_eq!(rem, "\ndirective");
        let (_config, md) = out;
        assert_eq!(md.key(), "key");
        assert_eq!(md.value(), Some("v"));

        let res = parse(" +key  v\n  directive");
        assert!(res.is_ok());
        let (rem, out) = res.unwrap();
        assert_eq!(rem, "\ndirective");
        let (_config, md) = out;
        assert_eq!(md.key(), "key");
        assert_eq!(md.value(), Some("v\n  directive"));
    }

    #[test]
    fn test_entry() {
        let ent = |s: &'static str| {
            let entry_res = parse_node!(s, entry);
            assert!(entry_res.is_ok());
            entry_res.unwrap().1
        };

        assert_eq!(ent("2000-01-01  desc").description(), "desc");
        assert_eq!(
            ent("2000-01-01  desc\n  AccB  £0").postings().next().unwrap().account().name(),
            "AccB"
        );

        // Entry config is updated
        assert!(entry!("2000-01-01\n  AccC  $0").config().get_unit("$").is_some());
    }
}
