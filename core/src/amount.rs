/*
 * Copyright (c) 2017-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table::Cell;
use crate::reporting::table::WrapPolicy;
use crate::reporting::table2::{AlignedCell, Alignment, CellRef, SpaceDistribution, StyledCell};
use crate::reporting::term_style::{Colour, Style};
use crate::reporting::{table, table2};
use crate::unit;
use crate::unit::{NegativeStyle, RoundingStrategy, Unit, UnitFormat};
use fmt::Write;
use rust_decimal::Decimal;
use rust_decimal::prelude::Zero;
use smartstring::alias::String as SS;
use std::borrow::Borrow;
use std::iter::Sum;
use std::ops::{Add, AddAssign, Bound, Deref, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};
use std::{cmp, fmt, mem};
use yaml_rust::Yaml;
use yaml_rust::yaml::Hash;

/// A quantity is a `Decimal` that forms the numeric part of an `Amount`. E.g. the '5' in '$5'.
pub type Quantity = Decimal;

/// Amounts are the basis of pretty much everything.
///
/// They are the 'money' values held by [Posting]s. Amounts within `Postings` are generally either credits
/// or debits with positive `Amounts` being Debits and negative `Amounts` being credits.
#[derive(Copy, Clone, PartialEq)]
pub struct Amount<'h> {
    /// The unit of the amount. This is the currency or other unit of the amount.
    unit: &'h Unit<'h>,
    /// The quantity of the amount. This is the numeric part of the amount.
    quantity: Quantity,
}

impl<'h> Amount<'h> {
    pub const MAX_SCALE: u32 = 27;

    pub fn new<D: Into<Decimal>>(unit: &'h Unit<'h>, quantity: D) -> Amount<'h> {
        let amount = quantity.into().normalize();
        Amount { unit, quantity: amount }
    }

    /// Gets a special amount that has is always zero and has no unit.
    pub fn nil() -> Amount<'h> {
        // Here we are shortening the 'static lifetime.
        // Safe since the lifetime parameter is only used for an Option type which is None.
        let none: &'h Unit<'h> = unsafe { mem::transmute(&unit::NONE) };

        Amount::new(none, Decimal::zero())
    }

    /// Gets whether the amount is nil, nothing: a zero amount with no unit.
    /// This is not the same as [Amount::is_zero()] which is always true when the quantity is zero.
    pub fn is_nil(&self) -> bool {
        self.unit.is_none()
    }

    pub fn quantity(&self) -> Quantity {
        self.quantity
    }

    pub fn with_quantity<D>(&self, quantity: D) -> Amount<'h>
    where
        D: Into<Decimal>,
    {
        let quantity = quantity.into();

        assert!(!self.unit.is_none() || quantity.is_zero(), "Can't set quantity on a nil amount");
        Amount::new(self.unit, quantity)
    }

    pub fn unit(&self) -> &'h Unit<'h> {
        self.unit
    }

    pub fn scale(&self) -> u32 {
        self.quantity.normalize().scale()
    }

    /// Rounds to the specified number of decimal places using the unit's rounding strategy.
    pub fn rounded_dec_places(&self, num_dec_places: u8) -> Amount<'h> {
        Amount::new(
            self.unit,
            self.quantity.round_dp_with_strategy(
                num_dec_places as u32,
                self.unit.rounding_strategy().into(),
            ),
        )
    }

    /// Rounds the amount to its maximum decimals using the unit's rounding strategy.
    pub fn rounded(&self) -> Amount<'h> {
        if let Some(max_scale) = self.unit.format().number_format().max_scale() {
            self.rounded_dec_places(max_scale)
        } else {
            *self
        }
    }

    pub fn rounded_with_strategy(&self, strategy: RoundingStrategy) -> Amount<'h> {
        if let Some(max_scale) = self.unit.format().number_format().max_scale() {
            Amount::new(
                self.unit,
                self.quantity.round_dp_with_strategy(max_scale as u32, strategy.into()),
            )
        } else {
            *self
        }
    }

    /// Assuming this amount value has been rounded, this gets the range of values the original,
    /// unrounded amount value could be depending on the rounding strategy used.
    /// Returns the range (min, max) of the possible values.
    pub fn rounding_error(&self) -> (Bound<Amount<'h>>, Bound<Amount<'h>>) {
        match self.unit.rounding_strategy() {
            RoundingStrategy::HalfUp => {
                let plus_minus_error = Decimal::new(5, self.quantity.scale() + 1);
                (Bound::Included(self - plus_minus_error), Bound::Excluded(self + plus_minus_error))
            }
            RoundingStrategy::HalfDown => {
                let plus_minus_error = Decimal::new(5, self.quantity.scale() + 1);
                (Bound::Excluded(self - plus_minus_error), Bound::Included(self + plus_minus_error))
            }
            RoundingStrategy::HalfEven => {
                let plus_minus_error = Decimal::new(5, self.quantity.scale() + 1);
                (Bound::Included(self - plus_minus_error), Bound::Included(self + plus_minus_error))
            }
            RoundingStrategy::AlwaysUp => {
                let plus_minus_error = Decimal::new(1, self.quantity.scale());
                (Bound::Excluded(self - plus_minus_error), Bound::Included(*self))
            }
            RoundingStrategy::AlwaysDown => {
                let plus_minus_error = Decimal::new(1, self.quantity.scale());
                (Bound::Included(*self), Bound::Excluded(self + plus_minus_error))
            }
        }
    }

    pub fn min(&self, other: Amount<'h>) -> Amount<'h> {
        if self.unit != other.unit {
            panic!("Currencies do not match")
        } else {
            Amount::new(self.unit, self.quantity.min(other.quantity))
        }
    }

    pub fn max(&self, other: &Amount<'h>) -> Amount<'h> {
        if self.unit != other.unit {
            panic!("Currencies do not match");
        } else {
            Amount::new(self.unit, self.quantity.max(other.quantity))
        }
    }

    pub fn is_zero(&self) -> bool {
        self.quantity == Decimal::zero()
    }

    /// Gets whether the amount can be considered negative. This is true if the quantity is zero or negative.
    pub fn is_negative(&self) -> bool {
        self.quantity == Decimal::zero() || self.quantity.is_sign_negative()
    }

    /// Gets whether the amount can be considered positive. This is true if the quantity is zero or positive.
    pub fn is_positive(&self) -> bool {
        self.quantity == Decimal::zero() || self.quantity.is_sign_positive()
    }

    pub fn negate(&self) -> Amount<'h> {
        Amount::new(self.unit, self.quantity.neg())
    }

    pub fn abs(&self) -> Amount<'h> {
        Amount::new(self.unit, self.quantity.abs())
    }

    pub fn format(&self) -> SS {
        self.unit.format().format(*self)
    }

    /// Same as format() but without loss of precision.
    pub fn format_precise(self) -> SS {
        self.unit.format().format_precise(self)
    }

    /// Splits the amount at `percent`, returning left and right (remainder) parts.
    /// The `percent` may be > 1 or < 0 if necessary.
    ///
    /// The amount is rounded to the scale of self, or the unit's max scale (if it has one), whichever is greater.
    pub fn split_percent(self, percent: Quantity) -> (Self, Self) {
        // In this simple implementation, the scale must be at least the precision of `self` to ensure no stray remainder can be created.
        // For example, if `self` is 100.6, we round up with a scale of 0 and `percent` is 1, then we would have (101, -0.4) rather than (100.6, 0).
        let rounding_scale =
            self.scale().max(self.unit.format().number_format().max_scale().unwrap_or(0) as u32)
                as u8;

        let l_amount = (self * percent).rounded_dec_places(rounding_scale);
        let r_amount = self - l_amount;
        (l_amount, r_amount)
    }

    /// Adds two amounts together, like `+` but with an additional check that
    /// the result is precise.
    /// Natively, `Decimal::checked_add` is not sufficient to guard against precision loss
    /// when the number of bits of the result exceeds 96. This check ensures precision by
    /// checking the scale of the result.
    /// Returns `None` if the addition would overflow or result in a loss of precision.
    ///
    /// # Panics
    /// If the units of the two amounts do not match.
    pub fn add_precise(&self, rhs: Amount<'h>) -> Option<Amount<'h>> {
        let scale = self.quantity.scale().max(rhs.quantity.scale());
        let result = self.quantity.checked_add(rhs.quantity)?;
        if result.scale() <= scale { Some(Amount::new(self.unit, result)) } else { None }
    }

    /// Formats the amount as a string, allowing a handler to process the
    /// different parts of the amount.
    pub fn handle_parts<H: AmountPartHandler>(&self, handler: &mut H, format: &UnitFormat) {
        struct AmountStrParser<'h, H: AmountPartHandler> {
            handler: &'h mut H,
            thousands_separator: char,
            decimal_separator: char,
            negative_style: NegativeStyle,
            in_quotes: bool,
            // Becomes true when the decimal point is found.
            found_decimal: bool,
            found_digits: bool,
            // Becomes true when a '-' or '+' is found.
            found_sign: bool,
        }
        impl<H: AmountPartHandler> Write for AmountStrParser<'_, H> {
            fn write_str(&mut self, s: &str) -> fmt::Result {
                let mut iter = s.chars().peekable();
                while let Some(c) = iter.next() {
                    // Two consecutive quotes are used to escape quotes in strings.
                    if c == '"' && iter.peek() != Some(&'"') {
                        self.in_quotes = !self.in_quotes;
                        self.handler.handle_part(AmountPart::Quote(c));
                        continue;
                    }
                    match c {
                        c if self.in_quotes => {
                            self.handler.handle_part(AmountPart::Code(c));
                        }
                        d if (d.is_ascii_digit() || d == self.thousands_separator)
                            && !self.found_decimal =>
                        {
                            self.found_digits = true;
                            self.handler.handle_part(AmountPart::DigitBeforeDecimal(c))
                        }
                        d if d.is_ascii_digit() => {
                            self.found_digits = true;
                            self.handler.handle_part(AmountPart::DigitAfterDecimal(c))
                        }
                        dp if dp == self.decimal_separator => {
                            self.found_decimal = true;
                            self.handler.handle_part(AmountPart::DecimalPoint(dp))
                        }
                        '+' if !self.found_digits && !self.found_sign => {
                            self.handler.handle_part(AmountPart::Sign(c));
                            self.found_sign = true
                        }
                        '-' if !self.found_digits
                            && !self.found_sign
                            && self.negative_style == NegativeStyle::NegativeSign =>
                        {
                            self.handler.handle_part(AmountPart::Sign(c));
                            self.found_sign = true;
                        }
                        '(' if !self.found_digits
                            && !self.found_sign
                            && self.negative_style == NegativeStyle::Brackets =>
                        {
                            self.handler.handle_part(AmountPart::Sign(c));
                            self.found_sign = true;
                        }
                        ')' if self.found_digits
                            && self.found_sign
                            && self.negative_style == NegativeStyle::Brackets =>
                        {
                            self.handler.handle_part(AmountPart::Sign(c));
                            self.found_sign = false;
                        }
                        _ => {
                            self.handler.handle_part(AmountPart::Code(c));
                        }
                    }
                }
                Ok(())
            }
        }
        let mut parser = AmountStrParser {
            handler,
            thousands_separator: self.unit.number_format().thousands_separator() as char,
            decimal_separator: self.unit.number_format().decimal_separator() as char,
            negative_style: self.unit.number_format().negative_style(),
            found_digits: false,
            in_quotes: false,
            found_decimal: false,
            found_sign: false,
        };
        format.write_amount(*self, &mut parser, None).unwrap();
    }

    pub fn into_cell(self, format: &UnitFormat) -> Box<dyn table2::Cell> {
        #[derive(Default)]
        struct AmountHandler {
            part_before_decimal: SS,
            part_after_decimal: SS,
            code_before_amount: SS,
            code_after_amount: SS,
            reached_decimal: bool,
            reached_integer_part: bool,
        }
        impl AmountPartHandler for AmountHandler {
            fn handle_part(&mut self, part: AmountPart) {
                match part {
                    AmountPart::DigitBeforeDecimal(_) => self.reached_integer_part = true,
                    AmountPart::Code(c) if !self.reached_integer_part => {
                        self.code_before_amount.push(c);
                        return;
                    }
                    AmountPart::Code(c) if self.reached_integer_part => {
                        self.code_after_amount.push(c);
                        return;
                    }
                    _ if self.reached_integer_part => self.reached_decimal = true,
                    _ => {}
                }
                if self.reached_decimal {
                    self.part_after_decimal.push(part.as_char());
                } else {
                    self.part_before_decimal.push(part.as_char());
                }
            }
        }
        let mut handler = AmountHandler::default();
        self.handle_parts(&mut handler, format);

        let left: Box<dyn table2::Cell> = if !handler.code_before_amount.is_empty() {
            let left = Box::new(AlignedCell::new(handler.code_before_amount, Alignment::Left))
                as Box<dyn table2::Cell>;
            let right = Box::new(AlignedCell::new(handler.part_before_decimal, Alignment::Right))
                as Box<dyn table2::Cell>;
            let mut cell = table2::BinaryCell::new(left, right);
            // The right integer part is right aligned, so give it all the extra space.
            cell.set_space_distribution(SpaceDistribution::Right);
            Box::new(cell)
        } else {
            Box::new(AlignedCell::new(handler.part_before_decimal, Alignment::Right))
        };
        let right = if !handler.code_after_amount.is_empty() {
            let left = Box::new(handler.part_after_decimal) as Box<dyn table2::Cell>;
            let right = Box::new(AlignedCell::new(handler.code_after_amount, Alignment::Right))
                as Box<dyn table2::Cell>;
            let mut cell = table2::BinaryCell::new(left, right);
            cell.set_space_distribution(SpaceDistribution::Left);
            Box::new(cell)
        } else {
            Box::new(handler.part_after_decimal) as Box<dyn table2::Cell>
        };
        let mut cell = table2::BinaryCell::new(left, right);
        // The left integer part is right aligned, so give it all the extra space - looks better.
        cell.set_space_distribution(SpaceDistribution::Left);

        if !self.is_positive() {
            Box::new(StyledCell::new(Box::new(cell), Style::default().with_fg(Colour::Red)))
        } else {
            Box::new(cell)
        }
    }

    /// Counts the number of characters in a normal string representation of the amount.
    /// Returns a tuple of:
    /// 0. the number of character counts in the prefixed code before any negative
    /// 1. the number of characters in the neg sign
    /// 2. the number of characters in the prefixed code following any negative sign
    /// 3. the number of digits before the decimal point
    /// 4. the number of digits after the decimal point
    /// 5. the number of characters in the suffixed code
    fn count_formatted_str_parts(self) -> (u16, u16, u16, u16, u16, u16) {
        struct Counter<'h> {
            amount: Amount<'h>,
            // Num of chars in the prefixed code before any negative sign, e.g. the '($' in '($5.00)'.
            prefixed_code_bef_sign: u16,
            // Num of chars in the prefixed code following any negative sign. E.g. the '$' in '-$5.00'.
            // Will be 0 in the absence of any negative sign.
            prefixed_code_aft_sign: u16,
            // Num of chars in the suffixed code, e.g. the ' AUD' in '5.00 AUD'.
            suffixed_code: u16,
            // The num of digits before the decimal point.
            before_decimal: u16,
            // The num of digits after the decimal point.
            after_decimal: u16,
            in_quotes: bool,
            // Becomes true when the decimal point is found.
            found_decimal: bool,
            // Becomes true when a '-' or '+' is found.
            found_sign: bool,
        }
        impl fmt::Write for Counter<'_> {
            fn write_str(&mut self, s: &str) -> fmt::Result {
                for c in s.chars() {
                    // This should also work for double quotes that are escaped ("").
                    if c == '"' {
                        self.in_quotes = !self.in_quotes;
                    }
                    match c {
                        d if !self.in_quotes
                            && (d.is_ascii_digit()
                                || d == self.amount.unit().number_format().thousands_separator()
                                    as char)
                            && !self.found_decimal =>
                        {
                            self.before_decimal += 1
                        }
                        d if !self.in_quotes && d.is_ascii_digit() => self.after_decimal += 1,
                        dp if !self.in_quotes
                            && dp
                                == self.amount.unit().number_format().decimal_separator()
                                    as char =>
                        {
                            self.found_decimal = true
                        }
                        '-' | '+' if !self.in_quotes => self.found_sign = true,
                        _ if self.before_decimal > 0 => self.suffixed_code += 1,
                        _ if !self.found_sign => self.prefixed_code_bef_sign += 1,
                        _ => self.prefixed_code_aft_sign += 1,
                    }
                }
                Ok(())
            }
        }
        let mut counter = Counter {
            amount: self,
            prefixed_code_bef_sign: 0,
            prefixed_code_aft_sign: 0,
            suffixed_code: 0,
            before_decimal: 0,
            after_decimal: 0,
            in_quotes: false,
            found_decimal: false,
            found_sign: false,
        };
        write!(&mut counter, "{}", self).unwrap();
        (
            counter.prefixed_code_bef_sign,
            if counter.found_sign { 1 } else { 0 },
            counter.prefixed_code_aft_sign,
            counter.before_decimal,
            counter.after_decimal,
            counter.suffixed_code,
        )
    }
}

impl Eq for Amount<'_> {}

impl<'h> PartialOrd for Amount<'h> {
    fn partial_cmp(&self, other: &Amount<'h>) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'h> Ord for Amount<'h> {
    fn cmp(&self, other: &Amount<'h>) -> cmp::Ordering {
        // partial_cmp will always be safe for non-NaN values
        self.unit.cmp(other.unit).then(self.quantity.partial_cmp(&other.quantity).unwrap())
    }
}

impl<'h, T> PartialEq<T> for Amount<'h>
where
    T: Into<Decimal> + Copy,
{
    fn eq(&self, other: &T) -> bool {
        self.quantity() == (*other).into()
    }
}

impl<'h, T> PartialOrd<T> for Amount<'h>
where
    T: Into<Decimal> + Copy,
{
    fn partial_cmp(&self, other: &T) -> Option<cmp::Ordering> {
        self.quantity().partial_cmp(&(*other).into())
    }
}

impl fmt::Debug for Amount<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.unit.format().format_precise(*self))
    }
}

impl fmt::Display for Amount<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if let Some(width) = f.width() {
            write!(f, "{:>width$}", self.unit.format().format(*self), width = width)
        } else {
            write!(f, "{}", self.unit.format().format(*self))
        }
    }
}

impl<'h, 'a> From<Amount<'h>> for Cell<'a> {
    fn from(amount: Amount<'h>) -> Self {
        let counts = amount.count_formatted_str_parts();
        let amount_str = amount.to_string();

        // If there is a code part before the amount, group it together with any negative sign
        // that precedes it.
        let offset_0 = if counts.0 as usize > 0 || counts.2 as usize > 0 {
            amount_str
                .char_indices()
                .nth(counts.0 as usize + counts.1 as usize + counts.2 as usize)
                .unwrap()
                .0
        // Otherwise, include the negative sign with the amount.
        } else {
            0
        };
        let part0 = &amount_str[..offset_0];

        // Digits before the decimal point: e.g. '5' in '5.00'.
        // Include any minus sign if there was no code part previous. It looks better.
        let mut digits_count = counts.3 as usize;
        if counts.2 == 0 && counts.1 > 0 {
            digits_count += counts.1 as usize;
        }
        let offset_3 = offset_0 + digits_count;
        let part3 = &amount_str[offset_0..offset_3];

        // Digits after the decimal point, including the decimal point: e.g. '.00' in '5.00'.
        let offset_4 = if counts.4 > 0 { offset_3 + counts.4 as usize + 1 } else { offset_3 };
        let part4 = &amount_str[offset_3..offset_4];

        // Code suffix. E.g. ' AUD' in '5.00 AUD'.
        let part5 = &amount_str[offset_4..];

        let mut amount_cell = Cell::from([
            Cell::new(part0.to_string()).with_alignment(table::Alignment::Right),
            Cell::new(part3.to_string()).with_alignment(table::Alignment::Right),
            Cell::new(part4.to_string()),
            Cell::new(part5.to_string()),
        ]);
        // Don't wrap amounts, they become unreadable
        amount_cell.set_wrap_policy(WrapPolicy::Never);
        if amount.is_negative() {
            amount_cell.set_foreground(Some(Colour::Red))
        }
        amount_cell
    }
}

impl<'h> From<&Amount<'h>> for Yaml {
    fn from(amount: &Amount<'h>) -> Self {
        // Converts the amount to a hash of unit and quantity.
        let mut hash = Hash::new();
        hash.insert(
            Yaml::String("unit".to_string()),
            Yaml::String(amount.unit().code().to_string()),
        );
        hash.insert(
            Yaml::String("quantity".to_string()),
            Yaml::String(amount.quantity().to_string()),
        );
        hash.insert(Yaml::String("display".to_string()), Yaml::String(amount.format().to_string()));
        Yaml::Hash(hash)
    }
}

impl<'h> Sum<Amount<'h>> for Amount<'h> {
    fn sum<I: Iterator<Item = Amount<'h>>>(iter: I) -> Amount<'h> {
        let mut sum: Amount<'h> = Amount::nil();
        for a in iter {
            sum += a
        }
        sum
    }
}

pub enum AmountPart {
    Quote(char),
    Code(char),
    Sign(char),
    DigitBeforeDecimal(char),
    DecimalPoint(char),
    DigitAfterDecimal(char),
}
impl AmountPart {
    pub fn as_char(&self) -> char {
        match self {
            AmountPart::Quote(c) => *c,
            AmountPart::Code(c) => *c,
            AmountPart::Sign(c) => *c,
            AmountPart::DigitBeforeDecimal(c) => *c,
            AmountPart::DecimalPoint(c) => *c,
            AmountPart::DigitAfterDecimal(c) => *c,
        }
    }
}
pub trait AmountPartHandler {
    fn handle_part(&mut self, part: AmountPart);
}

macro_rules! checked_op {
    ($amount_type:ty, $op_type:tt, $op_method:ident, $dec_op:path) => {
        impl<'h> $op_type<$amount_type> for $amount_type {
            type Output = Amount<'h>;
            fn $op_method(self, rhs: $amount_type) -> Self::Output {
                if self.unit != rhs.unit {
                    if !self.unit.is_none() && !rhs.unit.is_none() {
                        panic!("Units do not match: left: {}, right: {}", self.unit, rhs.unit);
                    }
                }
                Amount::new(
                    if self.unit.is_none() { rhs.unit } else { self.unit },
                    $dec_op(self.quantity, rhs.quantity).unwrap(),
                )
            }
        }
    };
}
checked_op!(Amount<'h>, Mul, mul, Decimal::checked_mul);
checked_op!(&Amount<'h>, Mul, mul, Decimal::checked_mul);
checked_op!(Amount<'h>, Add, add, Decimal::checked_add);
checked_op!(&Amount<'h>, Add, add, Decimal::checked_mul);
checked_op!(Amount<'h>, Sub, sub, Decimal::checked_sub);
checked_op!(&Amount<'h>, Sub, sub, Decimal::checked_sub);

macro_rules! op {
    ($amount_type:ty, $op_type:tt, $op_method:ident, $dec_op:path) => {
        impl<'h> $op_type<$amount_type> for $amount_type {
            type Output = Amount<'h>;
            fn $op_method(self, rhs: $amount_type) -> Amount<'h> {
                if self.unit != rhs.unit {
                    if !self.unit.is_none() && !rhs.unit.is_none() {
                        panic!("Units do not match: left: {}, right: {}", self.unit, rhs.unit);
                    }
                }
                Amount::new(
                    if self.unit.is_none() { rhs.unit } else { self.unit },
                    $dec_op(self.quantity, rhs.quantity),
                )
            }
        }
    };
}
op!(Amount<'h>, Div, div, Decimal::div);
op!(&Amount<'h>, Div, div, Decimal::div);

macro_rules! checked_op_assign {
    ($amount_type:ty, $op_type:tt, $op_method:tt, $dec_op:path) => {
        impl<'h> $op_type<$amount_type> for Amount<'h> {
            fn $op_method(&mut self, rhs: $amount_type) {
                if self.unit != rhs.unit {
                    if !self.unit.is_none() && !rhs.unit.is_none() {
                        panic!("Units do not match: left: {}, right: {}", self.unit, rhs.unit);
                    }
                }
                if self.unit.is_none() {
                    self.unit = rhs.unit;
                }
                self.quantity = $dec_op(self.quantity, rhs.quantity).unwrap();
            }
        }
    };
}
checked_op_assign!(Amount<'h>, AddAssign, add_assign, Decimal::checked_add);
checked_op_assign!(&Amount<'h>, AddAssign, add_assign, Decimal::checked_add);
checked_op_assign!(Amount<'h>, SubAssign, sub_assign, Decimal::checked_sub);
checked_op_assign!(&Amount<'h>, SubAssign, sub_assign, Decimal::checked_sub);
checked_op_assign!(Amount<'h>, MulAssign, mul_assign, Decimal::checked_mul);
checked_op_assign!(&Amount<'h>, MulAssign, mul_assign, Decimal::checked_mul);

macro_rules! op_assign {
    ($amount_type:ty, $op_type:tt, $op_method:tt, $dec_op:path) => {
        impl<'h> $op_type<$amount_type> for Amount<'h> {
            fn $op_method(&mut self, rhs: $amount_type) {
                if self.unit != rhs.unit {
                    if !self.unit.is_none() && !rhs.unit.is_none() {
                        panic!("Units do not match: left: {}, right: {}", self.unit, rhs.unit);
                    }
                }
                if self.unit.is_none() {
                    self.unit = rhs.unit;
                }
                self.quantity = $dec_op(self.quantity, rhs.quantity);
            }
        }
    };
}
op_assign!(Amount<'h>, DivAssign, div_assign, Decimal::div);
op_assign!(&Amount<'h>, DivAssign, div_assign, Decimal::div);

macro_rules! checked_op_decimal {
    ($amount_type:ty, $op_type:tt, $op_method:tt, $op:path, $unit_check:expr) => {
        impl<'h, D> $op_type<D> for $amount_type
        where
            D: Into<Decimal>,
        {
            type Output = Amount<'h>;
            fn $op_method(self, rhs: D) -> Self::Output {
                // Test whether $op is add or sub
                if $unit_check {
                    if self.unit.is_none() {
                        panic!("Attempt to operate on a nil amount")
                    }
                }
                Amount::new(self.unit, $op(self.quantity, rhs.into()).unwrap())
            }
        }
    };
}
checked_op_decimal!(Amount<'h>, Sub, sub, Decimal::checked_sub, true);
checked_op_decimal!(&Amount<'h>, Sub, sub, Decimal::checked_sub, true);
checked_op_decimal!(Amount<'h>, Add, add, Decimal::checked_add, true);
checked_op_decimal!(&Amount<'h>, Add, add, Decimal::checked_add, true);
checked_op_decimal!(Amount<'h>, Mul, mul, Decimal::checked_mul, false);
checked_op_decimal!(&Amount<'h>, Mul, mul, Decimal::checked_mul, false);

macro_rules! op_decimal {
    ($amount_type:ty, $op_type:tt, $op_method:tt, $op:path, $unit_check:expr) => {
        impl<'h, D> $op_type<D> for $amount_type
        where
            D: Into<Decimal>,
        {
            type Output = Amount<'h>;
            fn $op_method(self, rhs: D) -> Self::Output {
                // Test whether $op is add or sub
                if $unit_check {
                    if self.unit.is_none() {
                        panic!("Attempt to operate on a nil amount")
                    }
                }
                Amount::new(self.unit, $op(self.quantity, rhs.into()))
            }
        }
    };
}
op_decimal!(Amount<'h>, Div, div, Decimal::div, false);
op_decimal!(&Amount<'h>, Div, div, Decimal::div, false);

macro_rules! checked_op_assign_decimal {
    ($op_type:tt, $op_method:tt, $op:path, $unit_check:expr) => {
        impl $op_type<Decimal> for Amount<'_> {
            fn $op_method(&mut self, rhs: Decimal) {
                if $unit_check {
                    if self.unit.is_none() {
                        panic!("Attempt to operate on a nil amount")
                    }
                }
                use std::borrow::BorrowMut;
                *self.quantity.borrow_mut() = $op(self.quantity, rhs).unwrap()
            }
        }
    };
}

macro_rules! op_assign_decimal {
    ($op_type:tt, $op_method:tt, $op:path, $unit_check:expr) => {
        impl $op_type<Decimal> for Amount<'_> {
            fn $op_method(&mut self, rhs: Decimal) {
                if $unit_check {
                    if self.unit.is_none() {
                        panic!("Attempt to operate on a nil amount")
                    }
                }
                self.quantity = $op(self.quantity, rhs)
            }
        }
    };
}
checked_op_assign_decimal!(AddAssign, add_assign, Decimal::checked_add, true);
checked_op_assign_decimal!(SubAssign, sub_assign, Decimal::checked_sub, true);
checked_op_assign_decimal!(MulAssign, mul_assign, Decimal::checked_mul, false);
op_assign_decimal!(DivAssign, div_assign, Decimal::div, false);

/// An arithmetic expression of `Amounts`.
/// These are parsed from the journal files and stored so that they can be rewritten later.
#[derive(Clone)]
pub struct AmountExpr<'h> {
    amount: Amount<'h>,
    /// Preceding space and symbols
    pretext: &'h str,
    /// The parsed amount, excluding any surrounding space which may be an arithmetic expression.
    parsed: Option<&'h str>,
}

impl<'h> AmountExpr<'h> {
    pub fn new(amount: Amount<'h>, pretext: &'h str, parsed: Option<&'h str>) -> Self {
        Self { amount, pretext, parsed }
    }

    pub fn amount(&self) -> Amount<'h> {
        self.amount
    }

    /// Preceding space and symbols
    pub fn pretext(&self) -> &str {
        self.pretext
    }

    pub fn with_pretext(&self, pretext: &'h str) -> AmountExpr<'h> {
        AmountExpr::new(self.amount, pretext, self.parsed)
    }

    pub fn parsed_expression(&self) -> Option<&str> {
        self.parsed.as_ref().map(|p| p.borrow())
    }

    /// Once the amount is changed, it can no longer be written back as the expression it was. It will be an expression
    /// of a single Amount.
    pub fn set_amount(&mut self, amount: Amount<'h>) {
        if self.amount != amount {
            self.amount = amount;
            self.parsed = None;
        }
    }

    pub fn with_amount(&self, amount: Amount<'h>) -> Self {
        let mut expr = self.clone();
        expr.set_amount(amount);
        expr
    }

    pub fn write<W: fmt::Write>(&self, w: &mut W, precise: bool) -> fmt::Result {
        match self.parsed.as_ref() {
            Some(expr) => write!(w, "{}{}", self.pretext, expr),
            None => write!(
                w,
                "{}{}",
                self.pretext,
                if precise { self.amount.format_precise() } else { self.amount.format() }
            ),
        }
    }
}

impl fmt::Display for AmountExpr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write(f, false)
    }
}

impl fmt::Debug for AmountExpr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.parsed.as_ref() {
            Some(expr) => write!(f, "{}{}", self.pretext, expr),
            None => write!(f, "{}{:?}", self.pretext, self.amount),
        }
    }
}

impl<'h> Deref for AmountExpr<'h> {
    type Target = Amount<'h>;

    fn deref(&self) -> &Self::Target {
        &self.amount
    }
}

impl<'h> From<Amount<'h>> for AmountExpr<'h> {
    fn from(amount: Amount<'h>) -> Self {
        AmountExpr::new(amount, "", None)
    }
}

impl<'h> PartialEq<Amount<'h>> for AmountExpr<'h> {
    fn eq(&self, other: &Amount<'h>) -> bool {
        self.amount == *other
    }
}

#[cfg(test)]
mod tests {
    use crate::amount::Amount;
    use crate::unit::RoundingStrategy;
    use crate::*;
    use rust_decimal::Decimal;
    use std::str::FromStr;

    #[test]
    fn test_rounding() {
        let half_up = unit!("NULL");
        half_up.set_rounding_strategy(RoundingStrategy::HalfUp);
        let half_down = unit!("NULL");
        half_down.set_rounding_strategy(RoundingStrategy::HalfDown);
        let half_even = unit!("NULL");
        half_even.set_rounding_strategy(RoundingStrategy::HalfEven);

        assert_eq!(
            Amount::new(half_up, Decimal::from_str("1").unwrap()),
            Amount::new(half_up, Decimal::from_str("1.23").unwrap()).rounded_dec_places(0)
        );

        assert_eq!(
            Amount::new(half_up, Decimal::from_str("1.23").unwrap()),
            Amount::new(half_up, Decimal::from_str("1.234").unwrap()).rounded_dec_places(2)
        );

        assert_eq!(
            Amount::new(half_up, Decimal::from_str("1.24").unwrap()),
            Amount::new(half_up, Decimal::from_str("1.236").unwrap()).rounded_dec_places(2)
        );

        assert_eq!(
            Amount::new(half_up, Decimal::from_str("1.2346").unwrap()),
            Amount::new(half_up, Decimal::from_str("1.23456").unwrap()).rounded_dec_places(4)
        );

        // Half-even rounding
        assert_eq!(
            Amount::new(half_even, Decimal::from_str("1.234").unwrap()),
            Amount::new(half_even, Decimal::from_str("1.2345").unwrap()).rounded_dec_places(3)
        );
        assert_eq!(
            Amount::new(half_even, Decimal::from_str("1.234").unwrap()),
            Amount::new(half_even, Decimal::from_str("1.2335").unwrap()).rounded_dec_places(3)
        );

        // Half-down rounding
        assert_eq!(
            Amount::new(half_down, Decimal::from_str("2").unwrap()),
            Amount::new(half_down, Decimal::from_str("2.5").unwrap()).rounded_dec_places(0)
        );
        assert_eq!(
            Amount::new(half_down, Decimal::from_str("3").unwrap()),
            Amount::new(half_down, Decimal::from_str("2.501").unwrap()).rounded_dec_places(0)
        );
    }

    #[test]
    fn test_arithmetic() {
        assert_eq!(
            amount!("$445"),
            ((amount!("$1200") / (amount!("$1200") + amount!("$1500"))) * amount!("$1001"))
                .rounded()
        );
    }

    #[test]
    fn test_from_str() {
        let m = amount!("£10");
        assert_eq!(m.unit().code(), "£");
        assert_eq!(m.quantity(), 10i32.into());

        let m = amount!("10£");
        assert_eq!(m.unit().code(), "£");
        assert_eq!(m.quantity(), 10i32.into());

        let m = amount!("10 GBP");
        assert_eq!(m.unit().code(), "GBP");
        assert_eq!(m.quantity(), 10i32.into());

        let m = amount!("GBP 10");
        assert_eq!(m.unit().code(), "GBP");
        assert_eq!(m.quantity(), 10i32.into());

        let m = amount!("\"1 GBP 1\" 10");
        assert_eq!(m.unit().code(), "1 GBP 1");
        assert_eq!(m.quantity(), 10i32.into());

        let m = amount!("10,000.76 GBP");
        assert_eq!(m.unit().code(), "GBP");
        assert_eq!(m.quantity(), Decimal::from_str("10000.76").unwrap());

        let m = amount!("10 GBP 1");
        assert_eq!(m.unit().code(), "GBP");
        assert_eq!(m.quantity(), dec!(10));

        let m = amount!("£10 -");
        assert_eq!(m.unit().code(), "£");
        assert_eq!(m.quantity(), dec!(10));

        let m = amount!("-£1");
        assert_eq!(m.unit().code(), "£");
        assert_eq!(m.quantity(), dec!(-1));

        let m = amount!("1.1kg Gold");
        assert_eq!(m.unit().code(), "kg Gold");
        assert_eq!(m.quantity(), Decimal::from_str("1.1").unwrap());
    }
}
