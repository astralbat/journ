/*
 * Copyright (c) 2017-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::Amount;
use crate::error::parsing::promote;
use crate::error::JournError;
use crate::metadata::Metadata;
use crate::parsing;
use crate::price_db::PriceDatabase;
use crate::python::lambda::Lambda;
use crate::{err, parse};
use rust_decimal::Decimal;
use std::cell::RefCell;
use std::cmp;
use std::collections::HashSet;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::str::{self, FromStr};
use std::sync::{Arc, LazyLock};
use yaml_rust::Yaml;

pub static DEFAULT_UNIT_FORMAT: LazyLock<UnitFormat> = LazyLock::new(UnitFormat::default);

/// The unit used for nil amounts.
pub static NONE: Unit = Unit::none::<'_>();

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub enum RoundingStrategy {
    /// This is the default strategy
    #[default]
    HalfUp,
    HalfDown,
    HalfEven,
    AlwaysUp,
    AlwaysDown,
}

impl From<RoundingStrategy> for rust_decimal::RoundingStrategy {
    fn from(rs: RoundingStrategy) -> Self {
        match rs {
            RoundingStrategy::HalfUp => rust_decimal::RoundingStrategy::MidpointAwayFromZero,
            RoundingStrategy::HalfDown => rust_decimal::RoundingStrategy::MidpointTowardZero,
            RoundingStrategy::HalfEven => rust_decimal::RoundingStrategy::MidpointNearestEven,
            RoundingStrategy::AlwaysUp => rust_decimal::RoundingStrategy::AwayFromZero,
            RoundingStrategy::AlwaysDown => rust_decimal::RoundingStrategy::ToZero,
        }
    }
}

impl FromStr for RoundingStrategy {
    type Err = JournError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "halfup" => Ok(RoundingStrategy::HalfUp),
            "halfdown" => Ok(RoundingStrategy::HalfDown),
            "halfeven" => Ok(RoundingStrategy::HalfEven),
            "up" => Ok(RoundingStrategy::AlwaysUp),
            "down" => Ok(RoundingStrategy::AlwaysDown),
            other => Err(err!("Cannot parse rounding strategy: {}", other.to_string())),
        }
    }
}

impl fmt::Display for RoundingStrategy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RoundingStrategy::HalfUp => write!(f, "halfup"),
            RoundingStrategy::HalfDown => write!(f, "halfdown"),
            RoundingStrategy::HalfEven => write!(f, "halfeven"),
            RoundingStrategy::AlwaysUp => write!(f, "up"),
            RoundingStrategy::AlwaysDown => write!(f, "down"),
        }
    }
}

#[derive(Clone)]
pub struct Unit<'h> {
    code: String,
    name: Option<String>,
    /// All codes this unit is known by, including this one.
    aliases: Vec<String>,
    format: Option<UnitFormat>,
    conversion_expression: Option<Lambda>,
    prices: Option<Arc<PriceDatabase<'h>>>,
    rounding_strategy: RoundingStrategy,
    /// When valuing an entry in terms of a particular unit, if there are two or more other
    /// currencies used in postings, the entry may only need to value one of them to determine the
    /// other. When this happens, the currencies whose rank is lowest will be chosen.
    conversion_ranking: Option<usize>,
    metadata: Vec<Metadata<'h>>,
}

impl<'h> Unit<'h> {
    pub fn new<S: Into<String>>(code: S) -> Unit<'h> {
        let code = code.into();
        assert!(!code.is_empty(), "Unit code cannot be empty string");

        let mut aliases = Vec::new();
        aliases.push(code.clone());
        Unit {
            code,
            aliases,
            name: None,
            format: None,
            conversion_expression: None,
            prices: None,
            rounding_strategy: RoundingStrategy::default(),
            conversion_ranking: None,
            metadata: vec![],
        }
    }

    /// Gets a special unit that is empty, having no code. Amounts created with this unit
    /// will be written without any unit.
    pub const fn none<'a>() -> Unit<'a> {
        Unit {
            code: String::new(),
            name: None,
            aliases: vec![],
            format: None,
            conversion_expression: None,
            prices: None,
            rounding_strategy: RoundingStrategy::HalfUp,
            conversion_ranking: None,
            metadata: vec![],
        }
    }

    pub fn is_none(&self) -> bool {
        self.code.is_empty()
    }

    pub fn primary_code(&self) -> &str {
        self.aliases().next().unwrap_or("")
    }

    pub fn code(&self) -> &str {
        &self.code
    }

    pub fn set_code(&mut self, code: String) {
        self.code = code
    }

    pub fn conversion_ranking(&self) -> Option<usize> {
        self.conversion_ranking
    }

    pub fn set_conversion_ranking(&mut self, ranking: Option<usize>) {
        self.conversion_ranking = ranking
    }

    pub fn name(&self) -> Option<&String> {
        self.name.as_ref()
    }

    pub fn set_name(&mut self, name: Option<String>) {
        self.name = name
    }

    /// Gets all known aliases for this unit, which is a list of other unit codes, including the code of this unit as well.
    /// The first alias will always be the "primary" code; the first code defined for this unit.
    pub fn aliases(&self) -> impl Iterator<Item = &str> {
        self.aliases.iter().map(|s| s.as_str())
    }

    /// Sets all known aliases for this unit. The aliases must include this unit's code.
    pub fn set_aliases(&mut self, aliases: Vec<String>) {
        assert!(
            self.code.is_empty() || aliases.contains(&self.code),
            "Aliases must contain the primary code"
        );
        assert_eq!(
            aliases.iter().collect::<HashSet<_>>().len(),
            aliases.len(),
            "Aliases must be unique"
        );

        self.aliases = aliases
    }

    pub fn display_name(&self) -> &str {
        self.name().unwrap_or(&self.code)
    }

    pub fn rounding_strategy(&self) -> RoundingStrategy {
        self.rounding_strategy
    }

    pub fn set_rounding_strategy(&mut self, rounding_strategy: RoundingStrategy) {
        self.rounding_strategy = rounding_strategy;
    }

    /// Gets whether this unit is a recognised fiat unit
    pub fn is_fiat(&self) -> bool {
        self.code() == "USD" || self.code() == "GBP" || self.code() == "EUR"
    }

    pub fn with_quantity<D: Into<Decimal>>(&'h self, quantity: D) -> Amount<'h> {
        Amount::new(self, quantity.into())
    }

    pub fn money_str(&'static self, amount: &str) -> Result<Amount<'h>, rust_decimal::Error> {
        Decimal::from_str(amount).map(|d| Amount::new(self, d))
    }

    pub fn has_format(&self) -> bool {
        self.format.is_some()
    }

    /*
    /// Gets whether the code will appear on the left of the quantity when this unit is formatted.
    pub fn code_on_left(&self) -> bool {
        self.format().code_on_left(Some(&self.code))
    }*/

    pub fn number_format(&self) -> &NumberFormat {
        &self.format().number_format
    }

    pub fn format(&self) -> &UnitFormat {
        match &self.format {
            Some(cf) => cf,
            None => &DEFAULT_UNIT_FORMAT,
        }
    }

    pub fn set_format(&mut self, format: UnitFormat) {
        self.format = Some(format)
    }

    pub fn conversion_expression(&self) -> Option<&Lambda> {
        self.conversion_expression.as_ref()
    }

    pub fn set_conversion_expression(&mut self, conversion_expression: Option<Lambda>) {
        self.conversion_expression = conversion_expression
    }

    pub fn prices(&self) -> Option<&Arc<PriceDatabase<'h>>> {
        self.prices.as_ref()
    }

    pub fn set_prices(&mut self, prices: Option<Arc<PriceDatabase<'h>>>) {
        self.prices = prices
    }

    pub fn metadata(&self) -> &Vec<Metadata<'h>> {
        &self.metadata
    }

    pub fn set_metadata(&mut self, metadata: Vec<Metadata<'h>>) {
        self.metadata = metadata
    }

    /// Gets whether this account or a parent has the specified metadata key.
    pub fn has_tag(&self, tag: &str) -> bool {
        self.metadata.iter().any(|m| m.key() == tag)
    }
}

impl<'t> Hash for Unit<'t> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.primary_code().hash(state)
    }
}

impl<'t> PartialEq for Unit<'t> {
    /// Two units are always equal if they have the same code, since codes are expected to be unique.
    /// If the units do not share the same code, then perhaps they share the same name.
    fn eq(&self, other: &Self) -> bool {
        self.primary_code() == other.primary_code()
        /*
        if self.code == other.code {
            return true;
        }

        // Next, look for an alias in common which is more involved than naively comparing the code to
        // all the aliases of `other`. This is required because we can't guarantee that the
        // aliases are complete for a particular unit; more may have been subsequently added.
        // E.g.
        // unit €, EE
        //   ...
        // unit EUR, EE
        //   ...
        // Here, we say that € == EUR, because they have an alias in common: EE.
        for alias in iter::once(self.code.as_str()).chain(self.aliases.iter().copied()) {
            if other.aliases.contains(&alias) {
                return true;
            }
        }

        false*/
    }
}

impl<'h> Eq for Unit<'h> {}

impl<'h> PartialOrd for Unit<'h> {
    fn partial_cmp(&self, other: &Self) -> Option<::std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'h> Ord for Unit<'h> {
    fn cmp(&self, other: &Unit<'h>) -> cmp::Ordering {
        if self == other {
            return cmp::Ordering::Equal;
        }
        self.code.cmp(&other.code)
    }
}

impl<'h> fmt::Display for Unit<'h> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if self.code.chars().any(parsing::amount::illegal_unit_code_char) {
            write!(f, "\"{}\"", self.code)
        } else {
            write!(f, "{}", self.code)
        }
    }
}

impl<'h> fmt::Debug for Unit<'h> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.code)?;
        for (i, alias) in self.aliases.iter().enumerate() {
            if i == 0 {
                write!(f, "(")?;
            }
            write!(f, "{}", alias)?;
            if i == self.aliases.len() - 1 {
                write!(f, ")")?;
            } else {
                write!(f, ",")?;
            }
        }
        Ok(())
    }
}

impl From<&Unit<'_>> for Yaml {
    fn from(unit: &Unit<'_>) -> Self {
        // Unit code should not be needed as it would conventionally be the key for this map.
        let mut hash = yaml_rust::yaml::Hash::new();
        hash.insert(
            Yaml::String("name".to_string()),
            Yaml::String(unit.display_name().to_string()),
        );
        hash.insert(
            Yaml::String("aliases".to_string()),
            Yaml::Array(unit.aliases.iter().map(|a| Yaml::String(a.to_string())).collect()),
        );
        if let Some(format) = &unit.format {
            hash.insert(Yaml::String("format".to_string()), Yaml::String(format.to_string()));
        }
        hash.insert(
            Yaml::String("rounding".to_string()),
            Yaml::String(unit.rounding_strategy.to_string()),
        );
        Yaml::Hash(hash)
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum NegativeStyle {
    NegativeSign,
    Brackets,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct NumberFormat {
    negative_style: NegativeStyle,
    thousands_separator: u8,
    decimal_separator: u8,
    min_scale: Option<u8>,
    max_scale: Option<u8>,
}

thread_local!(static NUMBER_BUF: Rc<RefCell<String>> = Rc::new(RefCell::new(String::new())));
impl NumberFormat {
    pub fn decimal_separator(&self) -> u8 {
        self.decimal_separator
    }

    pub fn thousands_separator(&self) -> u8 {
        self.thousands_separator
    }

    pub fn negative_format(&self) -> NegativeStyle {
        self.negative_style
    }

    pub fn set_negative_style(&mut self, negative_style: NegativeStyle) {
        self.negative_style = negative_style
    }

    pub fn max_scale(&self) -> Option<u8> {
        self.max_scale
    }

    pub fn with_max_scale(self, max_scale: Option<u8>) -> Self {
        Self { max_scale, ..self }
    }

    pub fn parse(s: &str, definitive: bool) -> NumberFormat {
        // Assume a default for these unless unambiguous evidence to the contrary.
        let mut thousands_separator = b',';
        let mut decimal_separator = b'.';
        if s.splitn(2, '.').skip(1).flat_map(str::chars).any(|c| c == ',') {
            thousands_separator = b'.';
            decimal_separator = b',';
        }

        // Assume negative sign (e.g. -10.0) rather than brackets (e.g. (10.0)).
        let mut negative_format = NegativeStyle::NegativeSign;
        if s.starts_with('(') && s.ends_with(')') {
            negative_format = NegativeStyle::Brackets;
        }

        // The count of digits after the decimal point indicates the minimum scale. E.g. "10.00" rather than "10".
        // The special '#' character adds extra scale and indicates a maximum (numbers may be rounded). E.g. "10.00##" means "10.00" or "10.1234"
        let (min_scale, max_scale) = if let Some(dec_point_pos) = s.rfind(decimal_separator as char)
        {
            // If this is not definitive, it means that we are not parsing a format specification, but the first occurrence of an amount.
            // Therefore, we can't assert too much information from this amount, except that if it has trailing zeros in the fraction part, we can
            // at least assume the `min_scale`.
            if definitive {
                let min_scale =
                    s[dec_point_pos + 1..].chars().take_while(char::is_ascii_digit).count() as u8;
                let additional_scale =
                    s[dec_point_pos + 1..].chars().rev().filter(|c| *c == '#').count() as u8;
                (Some(min_scale), Some(min_scale + additional_scale))
            } else {
                let frac_digit_count =
                    s[dec_point_pos + 1..].chars().take_while(char::is_ascii_digit).count();
                let trailing_zero_count =
                    s[dec_point_pos + 1..].chars().rev().take_while(|c| c == &'0').count();
                if trailing_zero_count > 0 {
                    (Some(frac_digit_count as u8), None)
                } else {
                    (None, None)
                }
            }
        } else {
            (Some(0), Some(0))
        };

        NumberFormat {
            negative_style: negative_format,
            thousands_separator,
            decimal_separator,
            min_scale,
            max_scale,
        }
    }

    /// Formats a `quantity` ready for display.
    ///
    /// # Examples
    /// * `"-1,000.00"`
    /// * `"(1,000.00)"`
    /// * `"1.000,00"`
    pub fn format(&self, quantity: Decimal) -> String {
        // plain number may start with '-' if negative
        let mut num = quantity.to_string();
        Self::to_non_scientific(&mut num);

        if let Some(dec_point_pos) = num.rfind('.') {
            if let Some(max_scale) = self.max_scale {
                let num_decimals = (num.len() - (dec_point_pos + 1)) as u8;
                if num_decimals > max_scale {
                    // We should be all ascii digits, so we can truncate safely
                    num.truncate(dec_point_pos + 1 + max_scale as usize);
                }
            }
            // Trim any surplus 0's off the end
            if let Some(min_scale) = self.min_scale {
                let mut num_decimals = (num.len() - (dec_point_pos + 1)) as u8;
                while num_decimals > min_scale && num.ends_with('0') {
                    num.remove(num.len() - 1);
                    num_decimals -= 1;
                }
            }

            if num.ends_with('.') {
                num.pop();
            } else {
                // Replace decimal point
                num.remove(dec_point_pos);
                num.insert(dec_point_pos, self.decimal_separator as char);
            }
        }

        // Add thousand separators
        self.add_thousands_separators(&mut num);
        self.add_padding_zeros(&mut num);

        // Adjust negativity
        if num.starts_with('-') && self.negative_style == NegativeStyle::Brackets {
            num.remove(0);
            num.insert(0, '(');
            num.push(')');
        }
        num
    }

    fn add_thousands_separators<'a>(&self, num: &'a mut String) -> &'a mut String {
        let mut curr =
            num.bytes().position(|c| c == self.decimal_separator).unwrap_or(num.len()) as f64;

        curr -= 3.0;
        let first_digit_pos = if num.starts_with('-') { 1.0 } else { 0.0 };
        while curr > first_digit_pos {
            num.insert(curr as usize, self.thousands_separator as char);
            curr -= 3.0;
        }
        num
    }

    /// Modifies `decimal_s` by removing thousands separators.
    pub fn remove_thousands(&self, decimal_s: &str) -> Rc<RefCell<String>> {
        NUMBER_BUF.with(|buf| {
            let mut buf_borrow = buf.borrow_mut();
            buf_borrow.clear();
            buf_borrow.extend(decimal_s.chars().filter(|b| *b != self.thousands_separator as char));
            Rc::clone(buf)
        })
    }

    pub fn to_non_scientific(num: &mut String) -> &mut String {
        // 4E+1 = 40
        if let Some(i) = num.find('E') {
            let positive = num.chars().nth(i + 1).unwrap() == '+';
            let neg_num = num.starts_with('-');
            let change: usize = num[i + 2..].parse().unwrap();
            num.truncate(i);
            let mut dot_pos = num
                .find('.')
                .map(|pos| {
                    num.remove(pos);
                    pos
                })
                .unwrap_or(num.len());
            for _ in 0..change {
                if positive {
                    if dot_pos == num.len() {
                        num.push('0');
                    }
                    dot_pos += 1;
                } else if neg_num && dot_pos == 1 {
                    num.insert(1, '0');
                } else if dot_pos == 0 {
                    num.insert(0, '0');
                } else {
                    dot_pos -= 1;
                }
            }
            if dot_pos == 0 {
                num.insert_str(0, "0.");
            } else if neg_num && dot_pos == 1 {
                num.insert_str(1, "0.")
            } else if dot_pos != num.len() {
                num.insert(dot_pos, '.');
            }
        }
        num
    }

    /// Ensure at least `self.min_scale` number of zeros are on the end
    fn add_padding_zeros(&self, num: &mut String) {
        if let Some(min_scale) = self.min_scale {
            let how_many = match num.bytes().position(|c| c == self.decimal_separator) {
                Some(pos) => i16::from(min_scale) - (num.chars().count() - (pos + 1)) as i16,
                None => {
                    if min_scale > 0 {
                        num.push(self.decimal_separator as char);
                    }
                    i16::from(min_scale)
                }
            };
            for _ in 0..how_many {
                num.push('0');
            }
        }
    }
}

impl Default for NumberFormat {
    fn default() -> Self {
        Self {
            negative_style: NegativeStyle::NegativeSign,
            thousands_separator: b',',
            decimal_separator: b'.',
            min_scale: None,
            max_scale: None,
        }
    }
}

impl fmt::Display for NumberFormat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.negative_style == NegativeStyle::Brackets {
            write!(f, "(")?;
        } else {
            write!(f, "-")?;
        }
        write!(f, "1{}000", self.thousands_separator as char)?;

        let mut wrote_dec_sep = false;
        match self.min_scale {
            Some(min_scale) if min_scale > 0 => {
                write!(f, "{}", self.decimal_separator as char)?;
                wrote_dec_sep = true;
                for _ in 0..min_scale {
                    write!(f, "0")?;
                }
            }
            _ => {}
        }
        match self.max_scale {
            Some(max_scale) if max_scale > 0 => {
                if !wrote_dec_sep {
                    write!(f, "{}", self.decimal_separator as char)?;
                }
                for _ in self.min_scale.unwrap_or(0)..max_scale {
                    write!(f, "#")?;
                }
            }
            _ => {}
        }
        Ok(())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum NegativePosition {
    /// E.g. $-10
    QuantityAdjacent,
    /// E.g. -$10
    CodeAdjacent,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CodePosition {
    /// Left of the quantity. E.g. $10
    PaddedLeft(String),
    /// Right of the quantity. E.g. 10 USD
    PaddedRight(String),
    /// Left when a single char, right otherwise. E.g. $10, 10 USD
    Adaptive,
}

impl From<&CodePosition> for Yaml {
    fn from(cp: &CodePosition) -> Self {
        match cp {
            CodePosition::PaddedLeft(padding) => Yaml::String(format!("left: {}", padding)),
            CodePosition::PaddedRight(padding) => Yaml::String(format!("right: {}", padding)),
            CodePosition::Adaptive => Yaml::String("adaptive".to_string()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum CodeFormat {
    /// Always use the specified code when formatting.
    Literal(String),
    /// Never use a code when formatting. i.e. allowing unit-less amounts.
    Never,
    /// Use the code of the amount when formatting. This is the default.
    #[default]
    Unit,
}

impl CodeFormat {
    pub fn str_for_amount<'a>(&'a self, amount: Amount<'a>) -> &'a str {
        match self {
            CodeFormat::Literal(code) => code,
            CodeFormat::Never => "",
            CodeFormat::Unit => amount.unit().code(),
        }
    }
}

impl FromStr for CodeFormat {
    type Err = JournError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "" => Ok(CodeFormat::Never),
            "$UNIT$" => Ok(CodeFormat::Unit),
            other => Ok(CodeFormat::Literal(other.to_string())),
        }
    }
}

impl fmt::Display for CodeFormat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CodeFormat::Literal(code) => write!(f, "{}", code),
            CodeFormat::Never => write!(f, ""),
            CodeFormat::Unit => write!(f, "$UNIT$"),
        }
    }
}

/// Unit format
///
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnitFormat {
    number_format: NumberFormat,
    code: CodeFormat,
    code_position: CodePosition,
    negative_position: NegativePosition,
}

impl UnitFormat {
    pub fn new(
        number_format: NumberFormat,
        code: CodeFormat,
        code_position: CodePosition,
        negative_position: NegativePosition,
    ) -> Self {
        Self { number_format, code, code_position, negative_position }
    }

    pub fn number_format(&self) -> NumberFormat {
        self.number_format
    }

    pub fn negative_position(&self) -> NegativePosition {
        self.negative_position
    }

    pub fn format(&self, amount: Amount) -> String {
        self.formatted_amount_or_spec(Some(amount), self.number_format)
    }

    /// Gets whether the code should be on the left of the quantity.
    /// When adaptive, the code is on the left if its length <= 1 and doesn't contain any illegal characters that would
    /// require quotes; right otherwise.
    pub fn code_on_left(&self, code: &str) -> bool {
        match &self.code_position {
            CodePosition::PaddedLeft(_) => true,
            CodePosition::PaddedRight(_) => false,
            CodePosition::Adaptive => {
                !(code.len() > 1 || code.chars().any(parsing::amount::illegal_unit_code_char))
            }
        }
    }

    pub fn format_precise(&self, amount: Amount) -> String {
        self.formatted_amount_or_spec(Some(amount), self.number_format.with_max_scale(None))
    }

    /// Dual-purpose function for formatting an amount for display (if `Some`),
    /// or writing the format specification (if `None`).
    fn formatted_amount_or_spec(
        &self,
        amount: Option<Amount>,
        number_format: NumberFormat,
    ) -> String {
        let mut num = match amount {
            Some(amount) => number_format.format(amount.quantity()),
            None => number_format.to_string(),
        };

        let code_s = match (&self.code, amount) {
            (CodeFormat::Literal(s), _) => &**s,
            (CodeFormat::Never, _) => "",
            (CodeFormat::Unit, Some(amount)) => amount.unit().code(),
            (CodeFormat::Unit, None) => "$UNIT$",
        };

        //let code = self.code.str_for_amount(a).code.as_deref().or(amount.map(|a| a.unit().code()));
        let add_quotes = code_s.chars().any(parsing::amount::illegal_unit_code_char);
        let code_on_left = self.code_on_left(code_s);
        let padding = match &self.code_position {
            CodePosition::PaddedLeft(padding) => &**padding,
            CodePosition::PaddedRight(padding) => &**padding,
            CodePosition::Adaptive => {
                if code_on_left {
                    ""
                } else {
                    " "
                }
            }
        };

        if !code_s.is_empty() {
            if code_on_left {
                let insert_pos = if amount.map(|a| a < 0).unwrap_or(true)
                    && self.negative_position == NegativePosition::CodeAdjacent
                {
                    1
                } else {
                    0
                };
                num.insert_str(insert_pos, padding);
                if add_quotes {
                    num.insert(insert_pos, '"');
                    num.insert_str(insert_pos, code_s);
                    num.insert(insert_pos, '"');
                } else {
                    num.insert_str(insert_pos, code_s);
                }
            } else {
                num.push_str(padding);
                if add_quotes {
                    num.push('"');
                    num.push_str(code_s);
                    num.push('"');
                } else {
                    num.push_str(code_s);
                }
            }
        }
        num
    }
}

impl Default for UnitFormat {
    fn default() -> Self {
        UnitFormat {
            number_format: NumberFormat::default(),
            code: CodeFormat::default(),
            code_position: CodePosition::Adaptive,
            negative_position: NegativePosition::CodeAdjacent,
        }
    }
}

impl fmt::Display for UnitFormat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.formatted_amount_or_spec(None, self.number_format))
    }
}

impl FromStr for UnitFormat {
    type Err = JournError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse!(
            s,
            promote::<_, JournError, _, _>(
                "Cannot parse unit format",
                parsing::amount::unit_format(true)
            )
        )
        .map(|r| r.1)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Units<'h> {
    default_unit: Option<&'h Unit<'h>>,
}

impl<'h> Units<'h> {
    pub fn default_unit(&self) -> Option<&'h Unit<'h>> {
        self.default_unit
    }

    pub fn set_default_unit(&mut self, default: Option<&'h Unit<'h>>) {
        self.default_unit = default
    }
}

#[cfg(test)]
mod tests {
    use crate::parsing::amount::unit_format;
    use crate::unit::NumberFormat;
    use crate::*;

    #[test]
    fn test_to_non_scientific() {
        assert_eq!("40".to_string(), *NumberFormat::to_non_scientific(&mut "4E+1".to_string()));
    }

    #[test]
    fn test_thousands_separate() {
        let nf = NumberFormat::parse("1,000.00", true);
        assert_eq!("30,000".to_string(), *nf.add_thousands_separators(&mut "30000".to_string()));
        assert_eq!("7,000".to_string(), *nf.add_thousands_separators(&mut "7000".to_string()));
    }

    #[test]
    fn test_number_format() {
        assert_eq!(NumberFormat::parse("1.#", true).format(dec!(1.5)), "1.5".to_string());
    }

    #[test]
    fn test_unit_format() {
        let def_format = |s: &str| parse!(s, unit_format(true)).unwrap().1;
        let non_def_format = |s: &str| parse!(s, unit_format(false)).unwrap().1;

        assert_eq!(def_format("£ 1").format(amount!("£1")), "£ 1");
        assert_eq!(def_format("£1,000.00").format(amount!("£1,000")), "£1,000.00");
        assert_eq!(def_format("-£1,000.00").format(amount!("GBP-1,000")), "-£1,000.00");
        assert_eq!(def_format("(£1,000.00)").format(amount!("GBP-1,000")), "(£1,000.00)");

        assert_eq!(non_def_format("1 BTC").format(amount!("1 ABC")), "1 ABC");
        assert_eq!(def_format("1 BTC").format(amount!("1 ABC")), "1 BTC");

        assert_eq!(
            def_format("1,000.12345678 BTC").format(amount!("1,000 ABC")),
            "1,000.00000000 BTC"
        );
        assert_eq!(def_format("1,000.12###### BTC").format(amount!("1,000 ABC")), "1,000.00 BTC");
        assert_eq!(
            def_format("668,127.66219 BTC").format(amount!("668,127.66219 ABC")),
            "668,127.66219 BTC"
        );
        assert_eq!(def_format("-1,000.###kg Gold").format(amount!("0.001kg Gold")), "0.001kg Gold");
        assert_eq!(
            def_format("668,127 \"AB0-1\"").format(amount!("668,127 \"AB0-1\"")),
            "668,127 \"AB0-1\""
        );
        assert_eq!(
            def_format("-0.00000015 ABC").format(amount!("-0.00000015 ABC")),
            "-0.00000015 ABC"
        );
        assert_eq!(def_format("0.####### ABC").format(amount!("0 ABC")), "0 ABC");
    }
}
