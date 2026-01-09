/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::datetime::{DateFormatMode, DateTimePrecision};
use crate::error::parsing::IParseError;
use crate::error::{JournError, JournResult};
use crate::match_parsers;
use crate::parsing::input::TextInput;
use crate::parsing::{IParseResult, JParseResult};
use chrono::format::{DelayedFormat, Fixed, Item, Numeric, Pad};
use chrono::{NaiveDate, NaiveTime};
use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case, take};
use nom::character::complete::{none_of, one_of, space1};
use nom::combinator::{map, verify};
use nom::error::context;
use nom::sequence::pair;
use nom::{Err as NomErr, Finish};
use smartstring::alias::String as SS;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::fmt;
use std::fmt::Formatter;
use std::ops::{Deref, Range};
use std::str::FromStr;
use std::sync::{LazyLock, Mutex};

/// Date formats for `JDate`, `JTime` and `JDateTime` objects.
///
/// The "M" marker can stand for a minute or a month. This is disambiguated
/// following heuristics.
///
/// # Restrictions
/// * The format always begins with the date format and ends with the time format. This allows for optimal
/// parsing by always parsing the date items first. There isn't a valid use-case for why the user
/// would want the time first.
/// * The date format items can never be mixed with the time items.
///
/// # Examples (brackets show date/time ranges as parsed)
/// * (YYYY-MM-DD) (HH:MM:SS)
/// * (DD/HH/YY)  (HH:MM)
/// * (YYYY-MM-DD)T(HH:MM:SS)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DateTimeFormat<'h> {
    format_str: SS,
    items: Vec<Item<'h>>,
    date_item_range: Range<u8>,
    time_item_range: Range<u8>,
    spaces: usize,
}

/// Cache of leaked datetime formats.
static DATETIME_FORMATS: LazyLock<Mutex<HashMap<SS, &'static DateTimeFormat<'static>>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

impl<'h> DateTimeFormat<'h> {
    pub fn parse_format<I>(mode: DateFormatMode) -> impl FnMut(I) -> JParseResult<I, Self>
    where
        I: TextInput<'h>,
    {
        move |input: I| {
            let (rem, (items, date_range, time_range)) = match mode {
                DateFormatMode::Date => context(
                    "Invalid date format",
                    map(Self::date_items_parser(false), |items| {
                        let date_range = 0u8..items.len() as u8;
                        (items, date_range, Range::default())
                    }),
                )(input.clone())?,
                DateFormatMode::Time => context(
                    "Invalid time format",
                    map(Self::time_items_parser(false), |items| {
                        let time_range = 0u8..items.len() as u8;
                        (items, time_range, Range::default())
                    }),
                )(input.clone())?,
                DateFormatMode::DateTime => {
                    context("Invalid datetime format", Self::datetime_items)(input.clone())?
                }
            };
            let fmt = Self {
                format_str: SS::from(input.text()),
                spaces: Self::count_spaces(&items),
                items,
                date_item_range: date_range,
                time_item_range: time_range,
            };
            Ok((rem, fmt))
        }
    }

    pub fn parse_to_owned(
        s: &str,
        mode: DateFormatMode,
    ) -> JournResult<&'static DateTimeFormat<'static>> {
        DateTimeFormat::parse_format(mode)(s).finish().map(|(_, df)| df.into_owned())
    }

    fn date_items_parser<I>(
        stop_on_time_item: bool,
    ) -> impl FnMut(I) -> JParseResult<I, Vec<Item<'h>>>
    where
        I: TextInput<'h>,
    {
        move |input: I| {
            let mut items = vec![];

            let lit = |input: I| -> IParseResult<I, I> {
                none_of("\r\n")(input.clone())?;
                take(1usize)(input)
            };
            let lit_allowed = |input: &I| -> bool {
                !stop_on_time_item || one_of::<_, _, IParseError<_>>("HhSs")(input.clone()).is_err()
            };

            let input = match_parsers!(input,
                tag_no_case("YYYY") => |_| Ok(items.push(Item::Numeric(Numeric::Year, Pad::None))),
                tag_no_case("YY") => |_| Ok(items.push(Item::Numeric(Numeric::YearMod100, Pad::None))),
                tag_no_case("MMMM") => |_| Ok(items.push(Item::Fixed(Fixed::LongMonthName))),
                tag_no_case("MMM") => |_| Ok(items.push(Item::Fixed(Fixed::ShortMonthName))),
                tag_no_case("MM") => |_| Ok(items.push(Item::Numeric(Numeric::Month, Pad::Zero))),
                tag_no_case("M") => |_| Ok(items.push(Item::Numeric(Numeric::Month, Pad::None))),
                tag_no_case("DD") => |_| Ok(items.push(Item::Numeric(Numeric::Day, Pad::Zero))),
                tag_no_case("D") => |_| Ok(items.push(Item::Numeric(Numeric::Day, Pad::None))),
                space1 => |sp: I| Ok(items.push(Item::Space(sp.text()))),
                verify(lit, lit_allowed) => |lit: I| Ok(items.push(Item::Literal(lit.text()))),
                alt((tag("\r"), tag("\n"))) => |rest: I| Err(NomErr::Failure(rest.into_err("Date format must not span multiple lines")))
            )?.0;
            match items
                .iter()
                .filter(|i| matches!(i, Item::Numeric(_, _)))
                .all(|i| matches!(i, Item::Numeric(Numeric::Month, _)))
            {
                true => Err(NomErr::Error(
                    input
                        .into_err("No date items in the date format or format only contains month"),
                )),
                false => Ok((input, items)),
            }
        }
    }

    fn time_items_parser<I>(
        stop_on_date_item: bool,
    ) -> impl FnMut(I) -> JParseResult<I, Vec<Item<'h>>>
    where
        I: TextInput<'h>,
    {
        move |input| {
            let mut items = vec![];

            let lit = |input: I| -> IParseResult<'h, I, I> {
                none_of("\r\n")(input.clone())?;
                take(1usize)(input)
            };
            let lit_allowed = |input: &I| -> bool {
                !stop_on_date_item || one_of::<_, _, IParseError<_>>("DYdy")(input.clone()).is_err()
            };

            let input = match_parsers!(input,
                tag_no_case("HH") => |_| Ok(items.push(Item::Numeric(Numeric::Hour, Pad::Zero))),
                tag_no_case("H") => |_| Ok(items.push(Item::Numeric(Numeric::Hour, Pad::None))),
                tag_no_case("II") => |_| Ok(items.push(Item::Numeric(Numeric::Hour12, Pad::Zero))),
                tag_no_case("I") => |_| Ok(items.push(Item::Numeric(Numeric::Hour12, Pad::None))),
                tag_no_case("MM") => |_| Ok(items.push(Item::Numeric(Numeric::Minute, Pad::Zero))),
                tag_no_case("M") => |_| Ok(items.push(Item::Numeric(Numeric::Minute, Pad::None))),
                tag_no_case("SS") => |_| Ok(items.push(Item::Numeric(Numeric::Second, Pad::Zero))),
                tag_no_case("S") => |_| Ok(items.push(Item::Numeric(Numeric::Second, Pad::None))),
                tag("P") => |_| Ok(items.push(Item::Fixed(Fixed::UpperAmPm))),
                tag("p") => |_| Ok(items.push(Item::Fixed(Fixed::LowerAmPm))),
                space1 => |sp: I| Ok(items.push(Item::Space(sp.text()))),
                verify(lit, lit_allowed) => |lit: I| Ok(items.push(Item::Literal(lit.text()))),
                alt((tag("\r"), tag("\n"))) => |rest: I| Err(NomErr::Failure(rest.into_err("Time format must not span multiple lines")))
            )?
                .0;

            match items
                .iter()
                .filter(|i| matches!(i, Item::Numeric(_, _)))
                .all(|i| matches!(i, Item::Numeric(Numeric::Minute, _)))
            {
                true => Err(NomErr::Error(
                    input.into_err("No time items in time format or format only contains minute"),
                )),
                false => Ok((input, items)),
            }
        }
    }

    /// Parses the datetime input with the date always appearing before the time.
    pub fn datetime_items<I>(input: I) -> JParseResult<I, (Vec<Item<'h>>, Range<u8>, Range<u8>)>
    where
        I: TextInput<'h>,
    {
        let (rem, (mut date_items, mut time_items)) =
            pair(Self::date_items_parser(true), Self::time_items_parser(false))(input)?;

        // capture the position when the non-literal items end
        let date_end_pos = date_items
            .iter()
            .rposition(|i| matches!(i, Item::Fixed(_) | Item::Numeric(_, _)))
            .unwrap() as u8
            + 1;
        let date_items_range = 0u8..date_end_pos as u8;
        let time_items_range =
            date_items.len() as u8..date_items.len() as u8 + time_items.len() as u8;
        let mut all_items = Vec::with_capacity(date_items.len() + time_items.len());
        all_items.append(&mut date_items);
        all_items.append(&mut time_items);
        Ok((rem, (all_items, date_items_range, time_items_range)))
    }

    pub fn format_str(&self) -> &SS {
        &self.format_str
    }

    /// Replace the date section of this format with the date section of the provided.
    pub fn with_date_format(&self, df: &DateTimeFormat<'h>) -> Self {
        let mut new_df = self.clone();
        new_df.items.drain(self.date_item_range.start as usize..self.date_item_range.end as usize);
        for (i, item) in df.items
            [df.date_item_range.start as usize..df.date_item_range.end as usize]
            .iter()
            .enumerate()
        {
            new_df.items.insert(self.date_item_range.start as usize + i, item.clone());
        }
        new_df.date_item_range =
            self.date_item_range.start..self.date_item_range.start + df.date_item_range.len() as u8;
        if self.time_item_range.start > self.date_item_range.start {
            let offset = (new_df.date_item_range.len() - self.date_item_range.len()) as u8;
            new_df.time_item_range =
                new_df.time_item_range.start + offset..new_df.time_item_range.end + offset;
        }
        new_df.format_str = Self::format_str_from_items(&new_df.items);
        new_df.spaces = Self::count_spaces(&new_df.items);
        new_df
    }

    /// Replace the time section of this format with the time section of the provided.
    pub fn with_time_format(&self, df: &DateTimeFormat<'h>) -> Self {
        let mut new_df = self.clone();
        new_df.items.drain(self.time_item_range.start as usize..self.time_item_range.end as usize);
        for (i, item) in df.items
            [df.time_item_range.start as usize..df.time_item_range.end as usize]
            .iter()
            .enumerate()
        {
            new_df.items.insert(self.time_item_range.start as usize + i, item.clone());
        }
        new_df.time_item_range =
            self.time_item_range.start..self.time_item_range.start + df.time_item_range.len() as u8;
        if self.date_item_range.start > self.time_item_range.start {
            let offset = (new_df.time_item_range.len() - self.time_item_range.len()) as u8;
            new_df.date_item_range =
                new_df.date_item_range.start + offset..new_df.date_item_range.end + offset;
        }
        new_df.format_str = Self::format_str_from_items(&new_df.items);
        new_df.spaces = Self::count_spaces(&new_df.items);
        new_df
    }

    fn format_str_from_items(items: &[Item<'h>]) -> SS {
        items
            .iter()
            .map(|item| match item {
                Item::Literal(s) => SS::from(*s),
                Item::OwnedLiteral(s) => SS::from(s.deref()),
                Item::OwnedSpace(s) => SS::from(s.deref()),
                Item::Space(s) => SS::from(*s),
                Item::Numeric(numeric, pad) => match (numeric, pad) {
                    (Numeric::Year, _) => SS::from("YYYY"),
                    (Numeric::YearMod100, _) => SS::from("YY"),
                    (Numeric::Month, Pad::Zero) => SS::from("MM"),
                    (Numeric::Month, Pad::None) => SS::from("M"),
                    (Numeric::Day, Pad::Zero) => SS::from("DD"),
                    (Numeric::Day, Pad::None) => SS::from("D"),
                    (Numeric::Hour, Pad::Zero) => SS::from("HH"),
                    (Numeric::Hour, Pad::None) => SS::from("H"),
                    (Numeric::Hour12, Pad::Zero) => SS::from("II"),
                    (Numeric::Hour12, Pad::None) => SS::from("I"),
                    (Numeric::Minute, Pad::None) => SS::from("M"),
                    (Numeric::Minute, Pad::Zero) => SS::from("MM"),
                    (Numeric::Second, Pad::None) => SS::from("S"),
                    (Numeric::Second, Pad::Zero) => SS::from("SS"),
                    _ => panic!("Unhandled numeric type: {:?} with padding {:?}", numeric, pad),
                },
                Item::Fixed(fixed) => match fixed {
                    Fixed::ShortMonthName => SS::from("MMM"),
                    Fixed::LongMonthName => SS::from("MMMM"),
                    Fixed::LowerAmPm => SS::from("p"),
                    Fixed::UpperAmPm => SS::from("P"),
                    _ => panic!("Unhandled fixed type: {:?}", fixed),
                },
                ty => panic!("Unhandled type: {:?}", ty),
            })
            .collect()
    }

    fn count_spaces(items: &[Item<'h>]) -> usize {
        let mut total_spaces = 0;
        for item in items {
            match item {
                Item::OwnedLiteral(s) | Item::OwnedSpace(s) => {
                    total_spaces += s.chars().filter(|c| *c == ' ').count()
                }
                Item::Literal(s) | Item::Space(s) => {
                    total_spaces += s.chars().filter(|c| *c == ' ').count()
                }
                _ => {}
            }
        }
        total_spaces
    }

    /*
    pub fn set_date_format(&mut self, date_format: &'h DateFormat<'h>) {
        self.date_inner = date_format.inner.clone();
    }

    pub fn set_time_format(&mut self, time_format: &'h TimeFormat<'h>) {
        self.time_inner = time_format.inner.clone();
    }*/

    /*
    pub(super) fn translate<I>(input: I) -> JParseResult<I, Vec<Item<'h>>>
    where
        I: TextInput<'h>,
    {
        let mut items = vec![];

        let lit = |input: I| -> IParseResult<'h, I, I> {
            none_of("\r\n")(input.clone())?;
            take(1usize)(input)
        };

        let (rem, items) = permutation((DateFormat::translate, TimeFormat::translate))(input)?;

        let input = match_parsers!(input,
            tag_no_case("YYYY") => |_| Ok(items.push(Item::Numeric(Numeric::Year, Pad::None))),
            tag_no_case("YY") => |_| Ok(items.push(Item::Numeric(Numeric::YearMod100, Pad::None))),
            tag_no_case("MMMM") => |_| Ok(items.push(Item::Fixed(Fixed::LongMonthName))),
            tag_no_case("MMM") => |_| Ok(items.push(Item::Fixed(Fixed::ShortMonthName))),
            tag_no_case("MM") => |_| Ok(items.push(Item::Numeric(Numeric::Month, Pad::Zero))),
            tag_no_case("M") => |_| Ok(items.push(Item::Numeric(Numeric::Month, Pad::None))),
            tag_no_case("DD") => |_| Ok(items.push(Item::Numeric(Numeric::Day, Pad::Zero))),
            tag_no_case("D") => |_| Ok(items.push(Item::Numeric(Numeric::Day, Pad::None))),
            tag_no_case("HH") => |_| Ok(items.push(Item::Numeric(Numeric::Hour, Pad::Zero))),
            tag_no_case("H") => |_| Ok(items.push(Item::Numeric(Numeric::Hour, Pad::None))),
            tag_no_case("II") => |_| Ok(items.push(Item::Numeric(Numeric::Hour12, Pad::Zero))),
            tag_no_case("I") => |_| Ok(items.push(Item::Numeric(Numeric::Hour12, Pad::None))),
            tag_no_case("MM") => |_| Ok(items.push(Item::Numeric(Numeric::Minute, Pad::Zero))),
            tag_no_case("M") => |_| Ok(items.push(Item::Numeric(Numeric::Minute, Pad::None))),
            tag_no_case("SS") => |_| Ok(items.push(Item::Numeric(Numeric::Second, Pad::Zero))),
            tag_no_case("S") => |_| Ok(items.push(Item::Numeric(Numeric::Second, Pad::None))),
            tag("P") => |_| Ok(items.push(Item::Fixed(Fixed::UpperAmPm))),
            tag("p") => |_| Ok(items.push(Item::Fixed(Fixed::LowerAmPm))),
            space1 => |sp: I| Ok(items.push(Item::Space(sp.text()))),
            lit => |lit: I| Ok(items.push(Item::Literal(lit.text()))),
            rest => |rest: I| Err(NomErr::Failure(rest.into_err("Datetime format must not span multiple lines")))
        )?
            .0;
        Ok((input, items))
    }*/

    pub fn format<'a>(
        &'a self,
        date: Option<NaiveDate>,
        time: Option<NaiveTime>,
        precision: DateTimePrecision,
    ) -> DelayedFormat<impl Iterator<Item = &'a Item<'h>> + Clone + 'a> {
        DelayedFormat::new(date, time, self.items_to_precision(precision))
    }

    pub fn items(&self) -> impl Iterator<Item = &Item<'h>> + Clone + '_ {
        self.items.iter()
    }

    pub fn max_precision(&self) -> DateTimePrecision {
        self.items
            .iter()
            .map(|i| DateTimePrecision::try_from(i).unwrap_or(DateTimePrecision::Year))
            .max()
            .unwrap()
    }

    /// Take all items if the precision is the maximum of this format which will naturally
    /// include any trailing literals.
    ///
    /// Otherwise, take only `precision_to_and_including` items without trailing literals.
    pub fn items_to_precision(
        &self,
        precision_to_and_including: DateTimePrecision,
    ) -> impl Iterator<Item = &Item<'h>> + Clone + '_ {
        if precision_to_and_including == self.max_precision() {
            self.items.iter().take(self.items.len())
        } else {
            let mut to_take = 0;

            for (i, item) in self.items.iter().enumerate() {
                if let Ok(p) = DateTimePrecision::try_from(item)
                    && p <= precision_to_and_including
                {
                    to_take = i + 1;
                }
            }
            self.items.iter().take(to_take)
        }
    }

    pub fn date_items(&self) -> impl Iterator<Item = &Item<'h>> + Clone + '_ {
        self.items
            .iter()
            .enumerate()
            .filter(|(i, _)| self.date_item_range.contains(&(*i as u8)))
            .map(|(_, item)| item)
    }

    pub fn non_date_items(&self) -> impl Iterator<Item = &Item<'h>> + Clone + '_ {
        self.items
            .iter()
            .enumerate()
            .filter(|(i, _)| !self.date_item_range.contains(&(*i as u8)))
            .map(|(_, item)| item)
    }

    pub fn time_items(&self) -> impl Iterator<Item = &Item<'h>> + Clone + '_ {
        self.items
            .iter()
            .enumerate()
            .filter(|(i, _)| self.time_item_range.contains(&(*i as u8)))
            .map(|(_, item)| item)
    }

    /// Leaks the inner values to create a `DateTimeFormat`
    pub fn into_owned(self) -> &'static DateTimeFormat<'static> {
        let mut map = DATETIME_FORMATS.lock().unwrap();
        if let Some(dtf) = map.get(&self.format_str) {
            return dtf;
        }

        let mut items = vec![];
        for item in self.items.into_iter() {
            match item {
                Item::OwnedLiteral(s) => items.push(Item::OwnedLiteral(s.clone())),
                Item::Literal(s) => items.push(Item::OwnedLiteral(s.to_string().into_boxed_str())),
                Item::Numeric(num, pad) => items.push(Item::Numeric(num.clone(), pad)),
                Item::Fixed(fixed) => items.push(Item::Fixed(fixed.clone())),
                Item::Space(s) => items.push(Item::OwnedSpace(s.to_string().into_boxed_str())),
                Item::OwnedSpace(s) => items.push(Item::OwnedSpace(s.clone())),
                Item::Error => items.push(Item::Error),
            }
        }

        let dtf = DateTimeFormat { items, ..self };
        let key = dtf.format_str.clone();
        match map.entry(key) {
            Entry::Vacant(e) => e.insert(Box::leak(Box::new(dtf))),
            Entry::Occupied(e) => e.into_mut(),
        }
    }
}

impl<'h> fmt::Display for DateTimeFormat<'h> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format_str)
    }
}

impl FromStr for &'static DateTimeFormat<'_> {
    type Err = JournError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        DateTimeFormat::parse_format(DateFormatMode::DateTime)(s)
            .finish()
            .map(|(_, tf)| tf.into_owned())
    }
}

#[derive(Debug, Clone)]
pub struct DateFormat(&'static DateTimeFormat<'static>);
impl DateFormat {
    pub fn into_inner(self) -> &'static DateTimeFormat<'static> {
        self.0
    }
}
impl FromStr for DateFormat {
    type Err = JournError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        DateTimeFormat::parse_to_owned(s, DateFormatMode::Date).map(|df| DateFormat(df))
    }
}

#[derive(Debug, Clone)]
pub struct TimeFormat(&'static DateTimeFormat<'static>);
impl TimeFormat {
    pub fn into_inner(self) -> &'static DateTimeFormat<'static> {
        self.0
    }
}
impl FromStr for TimeFormat {
    type Err = JournError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        DateTimeFormat::parse_to_owned(s, DateFormatMode::Time).map(TimeFormat)
    }
}
/*

impl Default for &'static DateTimeFormat<'static> {
    fn default() -> Self {
        static DEFAULT_DATETIME_FORMAT: LazyLock<&'static DateTimeFormat<'static>> =
            LazyLock::new(|| "yyyy-mm-dd hh:mm:ss".parse().unwrap());

        *DEFAULT_DATETIME_FORMAT
    }
}*/

/*
impl<'h> From<&DateFormat<'h>> for &'static DateTimeFormat<'static> {
    /// Implements From<DateFormat>, creating a DateTimeFormat that only prints the date.
    fn from(value: &DateFormat<'h>) -> Self {
        let map = DATETIME_FORMATS.lock().unwrap();
        if let Some(dtf) = map.get(&(value.inner.format_str)) {
            return dtf;
        }

        DateTimeFormat {
            date_inner: value.inner.clone(),
            inter_literal: DateTimeFormatInner::empty(),
            time_inner: DateTimeFormatInner::empty(),
            date_first: true,
        }.into_owned()
    }
}

impl<'h> From<&TimeFormat<'h>> for &'static DateTimeFormat<'static> {
    /// Implements From<TimeFormat>, creating a DateTimeFormat that only prints the time.
    fn from(value: &TimeFormat<'h>) -> Self {
        let map = DATETIME_FORMATS.lock().unwrap();
        if let Some(dtf) = map.get(&(value.inner.format_str)) {
            return dtf;
        }

        DateTimeFormat {
            date_inner: DateTimeFormatInner::empty(),
            inter_literal: DateTimeFormatInner::empty(),
            time_inner: value.inner.clone(),
            date_first: true,
        }.into_owned()
    }
}*/

/*
impl<'h> From<(Option<&DateFormat<'h>>, Option<&TimeFormat<'h>>)>
    for &'static DateTimeFormat<'static>
{
    /// Implements From<(DateFormat, TimeFormat)>, creating a DateTimeFormat that prints
    /// date and time separated by a space.
    fn from(value: (Option<&DateFormat<'h>>, Option<&TimeFormat<'h>>)) -> Self {
        let map = DATETIME_FORMATS.lock().unwrap();
        match (value.0.as_ref().map(|v| &v.inner), value.1.as_ref().map(|v| &v.inner)) {
            (Some(df), Some(tf)) => {
                if let Some(dtf) = map.get(&(df.format_str.clone() + " " + tf.format_str.clone())) {
                    return dtf;
                }
            }
            (Some(inner), None) | (None, Some(inner)) => {
                if let Some(dtf) = map.get(&inner.format_str) {
                    return dtf;
                }
            }
            _ => {}
        }

        DateTimeFormat {
            date_inner: value.0.map(|v| v.inner.clone()).unwrap_or(DateTimeFormatInner::empty()),
            inter_literal: if value.0.is_some() && value.1.is_some() {
                DateTimeFormatInner {
                    format_str: SS::from(" "),
                    items: vec![Item::Space(" ")],
                    spaces: 1,
                }
            } else {
                DateTimeFormatInner::empty()
            },
            time_inner: value.1.map(|v| v.inner.clone()).unwrap_or(DateTimeFormatInner::empty()),
            date_first: true,
        }
        .into_owned()
    }
}*/
