/*
 * Copyright (c) 2022-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::alloc::HerdAllocator;
use crate::amount::{Amount, AmountExpr, Quantity};
use crate::error::JournResult;
use crate::money_util::MoneyPot;
use crate::reporting::table;
use crate::reporting::table::{Cell, WrapPolicy};
use crate::reporting::term_style::Colour;
use crate::unit::Unit;
use crate::valuer::Valuer;
use rust_decimal::Decimal;
use rust_decimal::prelude::{One, Zero};
use smallvec::{SmallVec, smallvec};
use std::iter::Sum;
use std::ops::{Add, Div, Mul, Sub};
use std::{fmt, iter};
use yaml_rust::Yaml;
use yaml_rust::yaml::Hash;

/// A holder for the total value and whether it was originally elided when this posting was parsed.
/// Valuations are always stored positively. I.e. negative valuations aren't possible.
#[derive(Clone)]
pub enum Valuation<'h> {
    Unit(AmountExpr<'h>),
    /// `Total(amount, elided)`
    Total(AmountExpr<'h>, bool),
}

impl<'h> Valuation<'h> {
    /// Creates a new unit valuation. The `amount` must not be negative.
    pub fn new_unit(amount: Amount<'h>) -> Valuation<'h> {
        assert!(amount.is_positive(), "unit valuations must be positive");

        Valuation::Unit(AmountExpr::new(amount, " @ ", None::<&'h str>))
    }

    /// Creates a new total valuation. The `amount` must not be negative.
    pub fn new_total(amount: Amount<'h>, elided: bool) -> Valuation<'h> {
        assert!(amount.is_positive(), "total valuations must be positive");

        Valuation::Total(AmountExpr::new(amount, " @@ ", None::<&'h str>), elided)
    }

    pub fn unit(&self) -> &'h Unit<'h> {
        match self {
            Valuation::Unit(m) => m.unit(),
            Valuation::Total(m, _) => m.unit(),
        }
    }
    pub fn value(&self) -> Amount<'h> {
        match self {
            Valuation::Unit(m) => **m,
            Valuation::Total(m, _) => **m,
        }
    }

    pub fn into_expr(self) -> AmountExpr<'h> {
        match self {
            Valuation::Unit(m) => m,
            Valuation::Total(m, _) => m,
        }
    }

    pub fn expr(&self) -> &AmountExpr<'h> {
        match self {
            Valuation::Unit(m) => m,
            Valuation::Total(m, _) => m,
        }
    }

    pub fn is_elided(&self) -> bool {
        match self {
            Valuation::Unit(_) => false,
            Valuation::Total(_, elided) => *elided,
        }
    }

    /// Write the valuation. We write the amount with full precision.
    fn write<W: fmt::Write>(&self, w: &mut W, include_elided: bool) -> fmt::Result {
        match self {
            Valuation::Unit(m) => m.write(w, true),
            Valuation::Total(m, elided) => {
                if include_elided || !elided {
                    m.write(w, true)
                } else {
                    Ok(())
                }
            }
        }
    }
}

impl PartialEq for Valuation<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Valuation::Unit(m1), Valuation::Unit(m2)) => m1.unit() == m2.unit(),
            (Valuation::Total(m1, _), Valuation::Total(m2, _)) => m1.unit() == m2.unit(),
            _ => false,
        }
    }
}

impl fmt::Display for Valuation<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write(f, false)
    }
}

impl fmt::Debug for Valuation<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Valuation::Unit(m) => write!(f, "{:?}", m),
            Valuation::Total(m, _elided) => {
                write!(f, "{:?}", m)
            }
        }
    }
}

impl<'h, 'a> From<Valuation<'h>> for table::Cell<'a> {
    fn from(value: Valuation<'h>) -> Self {
        value.expr().amount().into()
    }
}

impl<'h> From<&Valuation<'h>> for Yaml {
    fn from(val: &Valuation<'h>) -> Self {
        let mut map = Hash::new();
        match val {
            Valuation::Unit(m) => {
                map.insert(Yaml::String("type".to_string()), Yaml::String("unit".to_string()));
                map.insert(Yaml::String("amount".to_string()), (&**m).into());
            }
            Valuation::Total(m, _elided) => {
                map.insert(Yaml::String("type".to_string()), Yaml::String("total".to_string()));
                map.insert(Yaml::String("amount".to_string()), (&**m).into());
            }
        }
        Yaml::Hash(map)
    }
}

/// An amount with optional valuations.
#[derive(Clone)]
pub struct ValuedAmount<'h> {
    // First amount stored separately to avoid Vec allocation in common case
    // where only one amount is parsed.
    amount_expr: AmountExpr<'h>,
    // Store in an option only for the nil case.
    valuations: Option<Vec<Valuation<'h>, &'h HerdAllocator<'h>>>,
}

impl<'h> ValuedAmount<'h> {}

impl<'h> ValuedAmount<'h> {
    pub fn new_in(amount: AmountExpr<'h>, allocator: &'h HerdAllocator<'h>) -> Self {
        ValuedAmount { amount_expr: amount, valuations: Some(Vec::new_in(allocator)) }
    }

    /// A ValuedAmount with no units and a zero amount.
    pub fn nil() -> Self {
        ValuedAmount {
            amount_expr: AmountExpr::new(Amount::nil(), "", None::<&'h str>),
            valuations: None,
        }
    }

    pub fn zero(unit: &'h Unit<'h>, allocator: &'h HerdAllocator<'h>) -> Self {
        ValuedAmount {
            amount_expr: AmountExpr::new(unit.with_quantity(0), "", None::<&'h str>),
            valuations: Some(Vec::new_in(allocator)),
        }
    }

    pub fn from_valuations<I: IntoIterator<Item = Amount<'h>>>(
        values: I,
        allocator: &'h HerdAllocator<'h>,
    ) -> Self {
        let mut valuations = Vec::new_in(allocator);
        let mut amount = None;
        for (i, v) in values.into_iter().enumerate() {
            if i == 0 {
                amount = Some(AmountExpr::from(v));
            } else {
                valuations.push(Valuation::new_total(v.abs(), false));
            }
            /*
            if let Valuation::Total(t, ..) = v {
                amount = Some(t);
            } else {
                valuations.push(v);
            }*/
        }
        if amount.is_none() {
            if amount.is_none() && !valuations.is_empty() {
                panic!("There must be at least one total valuation to make a ValuedAmount")
            }
            ValuedAmount::nil()
        } else {
            let va = ValuedAmount { amount_expr: amount.unwrap(), valuations: Some(valuations) };
            assert!(
                !va.valuations.iter().flatten().any(|v| v.unit() == va.amount_expr.unit()),
                "Valuations must not contain the same unit as the amount"
            );
            va
        }
    }

    /// Gets the allocator for this ValuedAmount which will always be Some so as long
    /// as the ValuedAmount is not nil.
    pub fn allocator(&self) -> Option<&'h HerdAllocator<'h>> {
        self.valuations.as_ref().map(|v| v.allocator()).copied()
    }

    /// Divides at the threshold `quantity`, returning both segments: the first of `quantity`, and
    /// the second of the remainder.
    pub fn split(mut self, quantity: Quantity) -> (Self, Self) {
        // Shortcut to avoid divide by zero.
        if self.amount_expr.is_zero() {
            return (ValuedAmount::zero(self.unit(), self.allocator().unwrap()), self);
        }

        // Split by % for the valuations
        let split_percent = quantity / self.amount_expr.quantity();

        let saved_amount = self.amount();

        let mut right = self.clone();
        self.set_amount(self.amount_expr.with_quantity(quantity));
        right.set_amount(saved_amount - *self.amount_expr);
        for valuation in self.valuations.clone().into_iter().flatten() {
            match valuation {
                Valuation::Unit(v) => {
                    right.set_valuation(Valuation::Unit(v.clone()));
                    self.set_valuation(Valuation::Unit(v));
                }
                Valuation::Total(v, e) => {
                    let (l_amount, r_amount) = v.split_percent(split_percent);
                    self.set_valuation(Valuation::Total(v.with_amount(l_amount.abs()), e));
                    right.set_valuation(Valuation::Total(v.with_amount(r_amount.abs()), e));
                }
            }
        }
        (self, right)
    }

    /// Splits self in to `weights.len()` portions, each the proportional size of its `weight`,
    /// taking the `ith` one.
    pub fn split_weighted(&self, weights: &[Decimal], i: usize) -> ValuedAmount<'h> {
        assert!(i < weights.len());

        if self.is_nil() {
            return ValuedAmount::nil();
        }

        let split_total = |expr: &AmountExpr<'h>, pretext| {
            let mp = MoneyPot::new(**expr);
            AmountExpr::new(mp.split_weighted(weights).nth(i).unwrap(), pretext, None::<&'h str>)
        };

        let mut va = ValuedAmount::new_in(
            split_total(&self.amount_expr, ""),
            self.valuations.as_ref().unwrap().allocator(),
        );

        for val in self.valuations.iter().flatten() {
            match val {
                Valuation::Unit(_v) => va.valuations.as_mut().unwrap().push(val.clone()),
                Valuation::Total(v, e) => va
                    .valuations
                    .as_mut()
                    .unwrap()
                    .push(Valuation::Total(split_total(v, " @@ "), *e)),
            }
        }
        va
    }

    /*
    /// A clone of `self`, but using the global allocator.
    pub fn clone_in<A2: Allocator>(&self, allocator: A2) -> ValuedAmount<'h, A2> {
        let mut valuations = Vec::with_capacity_in(self.valuations.len(), allocator);
        valuations.extend(self.valuations.iter().cloned());
        ValuedAmount { amount_expr: self.amount_expr.clone(), valuations }
    }*/

    /// Gets whether the amount is zero.
    pub fn is_zero(&self) -> bool {
        self.amount_expr.is_zero()
    }

    /// Gets whether the amount is nil. This is similar, but not the same as
    /// [`Self::is_zero`]. All nil `ValuedAmounts` are zero, but not all
    /// zero `ValuedAmounts` are nil.
    pub fn is_nil(&self) -> bool {
        self.amount_expr.is_nil()
    }

    pub fn amount(&self) -> Amount<'h> {
        *self.amount_expr
    }

    /// Gets all effective 'amounts' for this valued amount. That is,
    /// the primary amount first, and then all valuations as total valuations.
    pub fn amounts(&self) -> impl Iterator<Item = Amount<'h>> + '_ {
        iter::once(*self.amount_expr).chain(self.totalled_valuations())
    }

    pub fn amount_expr(&self) -> &AmountExpr<'h> {
        &self.amount_expr
    }

    /// The unit of the primary amount.
    pub fn unit(&self) -> &'h Unit<'h> {
        self.amount_expr.unit()
    }

    pub fn set_amount(&mut self, amount: Amount<'h>) {
        assert!(!self.is_nil(), "Can't set an amount on nil, create a new ValuedAmount instead");

        self.amount_expr.set_amount(amount)
    }

    pub fn set_pretext(&mut self, pretext: &'h str) {
        self.amount_expr = self.amount_expr.with_pretext(pretext);
    }

    pub fn valuations(&self) -> impl Iterator<Item = &Valuation<'h>> + Clone {
        self.valuations.iter().flatten()
    }

    /*
    /// An iterator of all valuations, including the amount as a total valuation as the first yielded item.
    pub fn all_valuations(&self) -> impl Iterator<Item = Amount<'h>> + '_ {
        iter::once(Valuation::Total(self.amount_expr.clone(), false))
            .chain(self.valuations.iter().flatten().cloned())
    }*/

    /// Gets all the inner valuations as totals.
    pub fn totalled_valuations(&self) -> impl Iterator<Item = Amount<'h>> + '_ {
        self.valuations.iter().flatten().flat_map(move |v| match v {
            Valuation::Total(tot, _) => {
                if self.amount_expr.is_negative() {
                    Some((**tot).negate())
                } else {
                    Some(**tot)
                }
            }
            Valuation::Unit(factor) => Some(**factor * self.amount().quantity()),
        })
    }

    /// An iterator of all units.
    /// A nil `ValuedAmount` does not have any units.
    pub fn units(&self) -> impl Iterator<Item = &'h Unit<'h>> + '_ {
        let mut iter = iter::once(self.amount_expr.unit())
            .chain(self.valuations.iter().flatten().map(|v| v.unit()));
        iter::from_fn(move || if self.amount_expr.unit().is_none() { None } else { iter.next() })
    }

    /// Gets the price of the base unit in the quote unit.
    pub fn price(&self, base_unit: &'h Unit<'h>, quote_unit: &'h Unit<'h>) -> Option<Amount<'h>> {
        let base_amount = self.value_in(base_unit)?;
        let quote_amount = self.value_in(quote_unit)?;
        if base_amount.is_zero() {
            return None;
        }
        Some(quote_amount / base_amount.quantity())
    }

    /// Looks up any valuation in the specified `in_unit`.
    /// # Examples
    /// ```
    /// use journ_core::{amount, unit, val as va};
    ///
    /// assert_eq!(va!("$10").value_in(unit!("$")), Some(amount!("$10")));
    /// assert_eq!(va!("$10 @@ €10").value_in(unit!("€")), Some(amount!("€10")));
    /// assert_eq!(va!("$10 @ €1").value_in(unit!("€")), Some(amount!("€10")));
    /// assert_eq!(va!("$10").value_in(unit!("€")), None);
    /// ```
    pub fn value_in(&self, in_unit: &'h Unit<'h>) -> Option<Amount<'h>> {
        if self.amount_expr.unit() == in_unit {
            return Some(*self.amount_expr);
        }

        let found = self.valuations().find_map(|t| match t {
            Valuation::Total(mon, _) if mon.unit() == in_unit => {
                if !self.amount_expr.is_positive() { Some((**mon).negate()) } else { Some(**mon) }
            }
            Valuation::Unit(factor) if factor.unit() == in_unit => {
                Some(**factor * self.amount().quantity())
            }
            _ => None,
        });
        found.or_else(|| {
            if self.amount_expr.is_zero() { Some(in_unit.with_quantity(0)) } else { None }
        })
    }

    pub fn unit_valuer(&self) -> impl Valuer<'h> + '_ {
        move |amount: Amount<'h>, in_unit: &'h Unit<'h>| {
            for val in self.valuations() {
                if let Valuation::Unit(factor) = val {
                    if amount.unit() == self.unit() && factor.unit() == in_unit {
                        return Ok(Some(**factor * amount.quantity()));
                    } else if amount.unit() == in_unit && factor.unit() == self.unit() {
                        return Ok(Some(amount / **factor));
                    }
                }
            }
            Ok(None)
        }
    }

    pub fn value_with<V: Valuer<'h>>(
        &mut self,
        valuer: &mut V,
        quote_unit: &'h Unit<'h>,
    ) -> JournResult<Option<Amount<'h>>> {
        match valuer.value(self.amount(), quote_unit)? {
            Some(valuation) => {
                self.set_valuation(Valuation::Total(
                    AmountExpr::new(valuation.abs(), " @@ ", None::<&'static str>),
                    true,
                ));
                Ok(Some(valuation))
            }
            None => Ok(None),
        }
    }

    pub fn value_in_or_value_with<V: Valuer<'h>>(
        &mut self,
        in_unit: &'h Unit<'h>,
        valuer: &mut V,
    ) -> JournResult<Option<Amount<'h>>> {
        match self.value_in(in_unit) {
            Some(value) => Ok(Some(value)),
            None => self.value_with(valuer, in_unit),
        }
    }

    /// Looks for a valuation in `unit` and sets it as the amount, appending the original amount to
    /// its valuations.
    /// Returns the new amount if successfully found, otherwise the old amount on Err.
    pub fn in_terms_of(&mut self, unit: &'h Unit<'h>) -> bool {
        // Already in terms of unit
        if self.amount_expr.unit() == unit || self.is_nil() {
            return true;
        }
        match self.value_in(unit) {
            Some(amount) => {
                self.valuations.as_mut().unwrap().push(Valuation::new_total(
                    *self.amount_expr.with_amount(self.amount_expr.abs()),
                    false,
                ));
                self.valuations.as_mut().unwrap().retain(|v| v.unit() != unit);
                self.amount_expr.set_amount(amount);
                true
            }
            None => false,
        }
    }

    /// Gets the `ValuedAmount` that is the same as this one, but without the amount or valuation in the specified `unit`.
    /// If this `ValuedAmount` doesn't contain the `unit`, then it is returned unchanged.
    ///
    /// Caution: If the amount is negative, but the valuations are 0, then the original
    /// negativity is lost.
    pub fn without_unit(mut self, unit: &'h Unit<'h>) -> Self {
        if self.is_nil() {
            return self;
        }

        if self.amount_expr.unit() == unit {
            if self.valuations.as_ref().unwrap().is_empty() {
                return ValuedAmount::nil();
            } else {
                let is_neg = self.amount_expr.is_negative();
                match self.valuations.as_mut().unwrap().remove(0) {
                    Valuation::Total(amount_expr, _) => {
                        self.amount_expr =
                            AmountExpr::new(amount_expr.amount(), "", None::<&'h str>)
                    }
                    Valuation::Unit(unit) => {
                        self.amount_expr = AmountExpr::new(
                            (unit.amount() * self.amount_expr.quantity()).abs(),
                            "",
                            None::<&'h str>,
                        )
                    }
                }
                if is_neg {
                    self.negate();
                }
            }
        } else {
            self.valuations.as_mut().unwrap().retain(|v| v.unit() != unit);
        }
        self
    }

    pub const ERR_POSTING_VALUATION_UNIT_IS_AMOUNT_UNIT: &'static str =
        "Posting valuation unit is the same as the amount unit";
    pub const ERR_POSTING_VALUATION_IS_NEGATIVE: &'static str =
        "Valuations should always be positive";
    /// Adds or replaces an existing valuation with the same unit. The unit of the valuation specified must not
    /// be the same as the amount's unit.
    /// # Panics
    /// Panics if the amount is nil or negative or is the same unit as the main amount.
    pub fn set_valuation(&mut self, val: Valuation<'h>) {
        assert!(!self.is_nil(), "Cannot set valuation on nil valued amount");
        assert!(val.value().is_positive(), "Valuations should always be positive");
        assert_ne!(
            val.unit(),
            self.unit(),
            "Posting valuation unit is the same as the amount unit"
        );

        let existing_val = self.valuations.iter_mut().flatten().find(|t| t.unit() == val.unit());
        match existing_val {
            Some(t) => *t = val,
            None => {
                self.valuations.as_mut().unwrap().push(val);
            }
        }
    }

    /// Removes the valuation (never the amount) with the specified unit.
    pub fn remove_valuation(&mut self, unit: &'h Unit<'h>) {
        self.valuations.as_mut().unwrap().retain(|v| v.unit() != unit);
    }

    /// Converts any Unit valuations to Total valuations.
    pub fn to_totalled_valuations(&mut self) {
        let abs_amount = self.amount().abs();
        for val in self.valuations.iter_mut().flatten() {
            if let Valuation::Unit(v) = val {
                *val = Valuation::new_total(**v * abs_amount.quantity(), false);
            }
        }
    }

    pub fn negate(&mut self) {
        self.amount_expr = self.amount_expr.with_amount(self.amount().negate());
    }

    pub fn negated(mut self) -> Self {
        self.negate();
        self
    }

    pub fn abs(mut self) -> Self {
        self.amount_expr = self.amount_expr.with_amount(self.amount_expr.abs());
        self
    }

    /// Rounds amount and total valuations. This won't round unit valuations.
    pub fn round(&mut self) {
        self.amount_expr = self.amount_expr.with_amount(self.amount().rounded());
        self.round_total_valuations();
    }

    /// Rounds all Total valuations
    pub fn round_total_valuations(&mut self) {
        for val in self.valuations.iter_mut().flatten() {
            match val {
                Valuation::Total(expr, _) => {
                    *expr = expr.with_amount(expr.amount().rounded());
                }
                Valuation::Unit(_) => {}
            }
        }
    }

    pub fn rounded(mut self) -> Self {
        self.round();
        self
    }

    /// Gets an iterator of all the common units between multiple valued amounts, in order of [ValuedAmount::units()].
    /// # Examples
    /// ```
    /// # use journ_core::{unit, val};
    /// # use journ_core::valued_amount::ValuedAmount;
    /// # use journ_core::unit::Unit;
    /// # use std::vec::Vec;
    ///
    /// assert_eq!(ValuedAmount::common_units(&[&val!("£10 @@ $12 @@ €12"), &val!("$6 @@ £5")]).collect::<Vec<_>>(), vec![unit!("£"), unit!("$")]);
    /// assert_eq!(ValuedAmount::common_units(&[&ValuedAmount::nil(), &val!("£5")]).collect::<Vec<&Unit<'_>>>(), vec![unit!("£")]);
    /// ```
    pub fn common_units<'a>(
        valued_amounts: &'a [&ValuedAmount<'h>],
    ) -> impl Iterator<Item = &'h Unit<'h>> + 'a {
        let mut units: SmallVec<[&'h Unit<'h>; 2]> = smallvec![];
        iter::from_fn(move || {
            for va in valued_amounts {
                for unit in va.units().filter(|u| !units.contains(u)) {
                    if valued_amounts.iter().all(|va| va.value_in(unit).is_some()) {
                        units.push(unit);
                        return Some(unit);
                    }
                }
            }
            None
        })
    }

    /// Gets the relative weighting between the value of each of the `valued_amounts` in a common unit. This is normalized so
    /// that the sum of all the weights is 1 OR when all `valued_amounts` are zero, 0.
    /// Returns `None` if there are no common units between the `valued_amounts`.
    pub fn into_normalized_weights<'a>(
        iter: impl Iterator<Item = &'a ValuedAmount<'h>>,
    ) -> Option<SmallVec<[Decimal; 2]>>
    where
        'h: 'a,
    {
        let valued_amounts = iter.collect::<SmallVec<[&ValuedAmount; 4]>>();

        if valued_amounts.is_empty() {
            return Some(smallvec![]);
        } else if valued_amounts.len() == 1 {
            return Some(smallvec![Decimal::one()]);
        } else if valued_amounts.iter().all(|v| v.is_zero()) {
            return Some(smallvec![Decimal::zero(); valued_amounts.len()]);
        }

        let mut has_common_unit = false;
        for common_unit in ValuedAmount::common_units(&valued_amounts) {
            has_common_unit = true;
            let mut weights = smallvec![];

            // Sum the common unit
            let mut sum = Decimal::zero();
            for va in valued_amounts.iter() {
                let value = va.value_in(common_unit).unwrap();
                sum += value.quantity().abs();
            }
            // All values were zero. This common_unit is no good.
            if sum == Decimal::zero() {
                continue;
            }

            for va in valued_amounts.iter().take(valued_amounts.len() - 1) {
                weights.push(va.value_in(common_unit).unwrap().quantity().abs() / sum);
            }
            // Last one is a subtraction from 1.0 to guarantee the post condition that the
            // sum of all weights is 1.
            weights.push(Decimal::one() - weights.iter().sum::<Decimal>());
            return Some(weights);
        }

        // If there was at least one common unit, but we have not returned yet, it means that
        // all values in the common units were zero. Therefore, we can say that they do in fact all
        // weigh the same.
        if has_common_unit {
            let weight = Decimal::one() / Decimal::from(valued_amounts.len());
            return Some(smallvec![weight; valued_amounts.len()]);
        }

        None
    }

    pub fn write<W: fmt::Write>(&self, w: &mut W, include_elided: bool) -> fmt::Result {
        write!(w, "{}", self.amount_expr)?;
        for valuation in self.valuations.iter().flatten() {
            valuation.write(w, include_elided)?;
        }
        Ok(())
    }

    /// Similar to `+`, where common units are added, but units on the lhs are not removed when they do not exist on the rhs.
    pub fn add_from(&mut self, rhs: &Self) {
        // Add main amount
        if let Some(rhs_amount) = rhs.value_in(self.unit()) {
            self.amount_expr.set_amount(self.amount() + rhs_amount);
        }
        let is_neg = self.amount().is_negative();
        // Add total valuations.
        for value in self.valuations.iter_mut().flatten() {
            match value {
                Valuation::Total(lhs_tot, _e) => {
                    if let Some(rhs_tot) = rhs.value_in(lhs_tot.unit()) {
                        let lhs_amount = if is_neg { lhs_tot.negate() } else { lhs_tot.amount() };
                        *lhs_tot = lhs_tot.with_amount((lhs_amount + rhs_tot).abs());
                    }
                }
                Valuation::Unit(_) => {}
            }
        }
    }

    pub fn sub_from(&mut self, rhs: &Self) {
        self.add_from(&rhs.clone().negated())
    }
}

/// The valued amount can be considered equal no matter the order of the valuations, nor
/// whether they are unit or total. What matters are that they have the same number of valuations
/// and each valuation is equal.
impl PartialEq for ValuedAmount<'_> {
    fn eq(&self, other: &Self) -> bool {
        if self.valuations.as_ref().map(Vec::len) != other.valuations.as_ref().map(Vec::len) {
            return false;
        }
        for unit in self.units() {
            if self.value_in(unit) != other.value_in(unit) {
                return false;
            }
        }
        true
    }
}

impl Eq for ValuedAmount<'_> {}

impl PartialOrd for ValuedAmount<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ValuedAmount<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.valuations
            .as_ref()
            .map(Vec::len)
            .cmp(&other.valuations.as_ref().map(Vec::len))
            .then_with(|| {
                self.units()
                    .zip(other.units())
                    .map(|(u1, u2)| self.value_in(u1).cmp(&other.value_in(u2)))
                    .find(|o| *o != std::cmp::Ordering::Equal)
                    .unwrap_or(std::cmp::Ordering::Equal)
            })
    }
}

impl fmt::Display for ValuedAmount<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write(f, false)
    }
}

impl Default for ValuedAmount<'_> {
    fn default() -> Self {
        ValuedAmount::nil()
    }
}

macro_rules! to_cell {
    ($t:ty) => {
        impl<'a, 'h> From<$t> for table::Cell<'a> {
            fn from(value: $t) -> Self {
                if value.is_nil() {
                    return table::Cell::from(&"");
                }
                let mut vals: Vec<table::Cell<'a>> = Vec::with_capacity(
                    value.valuations.as_ref().map(|v| v.len() * 2 + 1).unwrap_or(1),
                );
                vals.push(value.amount_expr.amount().into());
                if let Some(valuations) = &value.valuations {
                    // Put the valuations in to an extra cell layer so that, when wrapped,
                    // they can align with the main amount.
                    for val in valuations.iter() {
                        match val {
                            Valuation::Unit(v) => {
                                vals.push(table::Cell::from(&" @ "));
                                vals.push([v.amount()].into());
                            }
                            Valuation::Total(v, _) => {
                                vals.push(table::Cell::from(&" @@ "));
                                vals.push([v.amount()].into())
                            }
                        }
                    }
                }
                let mut cell: table::Cell<'a> = vals.into();
                if value.amount().is_negative() {
                    cell.set_foreground(Some(Colour::Red));
                }
                cell.set_wrap_policy(WrapPolicy::With(|cell: &Cell| {
                    for (i, child) in cell.iter().enumerate() {
                        if *child == " @ " || *child == " @@ " {
                            if i == cell.as_branch().unwrap().len() - 1 {
                                return WrapPolicy::Never;
                            }
                            return WrapPolicy::Position(i + 1);
                        }
                    }
                    WrapPolicy::Any
                }));
                cell
            }
        }
    };
}
to_cell!(ValuedAmount<'h>);
to_cell!(&ValuedAmount<'h>);

impl<'h> From<&ValuedAmount<'h>> for Yaml {
    fn from(va: &ValuedAmount<'h>) -> Self {
        let mut map = Hash::new();
        if va.is_nil() {
            return Yaml::Hash(map);
        }

        //map.extend(Yaml::from(&va.amount()).into_hash().unwrap());
        //map.insert(Yaml::String("amount".to_string()), (&va.amount()).into());

        // Create a map of unit keys -> an `Amount` yaml map.
        // We filter out the unit key from the valuation map.
        //let mut units_map = Hash::new();
        for valuation in va.amounts() {
            let mut unit_map = Hash::new();
            unit_map.extend(
                Yaml::from(&valuation)
                    .into_hash()
                    .unwrap()
                    .into_iter()
                    .filter(|(k, _v)| k.as_str() != Some("unit")),
            );
            map.insert(Yaml::String(valuation.unit().code().to_string()), Yaml::Hash(unit_map));
        }

        //map.insert(Yaml::String("valuations".to_string()), Yaml::Hash(units_map));
        Yaml::Hash(map)
    }
}

impl fmt::Debug for ValuedAmount<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.amount_expr)?;
        for v in self.valuations.iter().flatten() {
            write!(f, "{:?}", v)?;
        }
        Ok(())
    }
}

impl<'h> Add<&ValuedAmount<'h>> for &ValuedAmount<'h> {
    type Output = Option<ValuedAmount<'h>>;

    /// Gets the sum of two ValuedAmounts.
    /// The valuations are summed for each unit only when both sides specify them.
    ///
    /// Return `None` if there are no units in common.
    ///
    /// # Examples
    /// ```
    /// use journ_core::val;
    /// use journ_core::valued_amount::ValuedAmount;
    ///
    /// assert_eq!(&val!("£10 @@ $12") + &val!("£5 @@ $6"), Some(val!("£15 @@ $18")));
    /// assert_eq!(&val!("£10 @@ $12") + &val!("£5"), Some(val!("£15")));
    /// assert_eq!(&val!("-£10 @@ $12") + &val!("£5 @@ $6"), Some(val!("-£5 @@ $6")));
    /// assert_eq!(&val!("0.2U @ $10") + &val!("0.8U @ $20"), Some(val!("1U @ $18")));
    /// assert_eq!(&val!("£0") + &val!("£5 @ $1.3 @@ €6"), Some(val!("£5 @ $1.3 @@ €6")));
    /// assert_eq!(&val!("£10 @@ $12") + &val!("£5 @ $1.3"), Some(val!("£15 @@ $18.5")));
    /// assert_eq!(&val!("£10 @@ $12") + &val!("$12 @@ £10"), Some(val!("£20 @@ $24")));
    /// assert_eq!(&val!("£10 @@ $12") + &val!("€21"), None);
    /// assert_eq!(&val!("£5") + &val!("€0"), Some(val!("£5")));
    /// assert_eq!(&val!("£0 @@ $0") + &val!("€0"), Some(val!("£0 @@ $0 @@ €0")));
    /// assert_eq!(&ValuedAmount::nil() + &val!(" €2"), Some(val!(" €2")));
    /// assert_eq!(&val!(" €1") + &ValuedAmount::nil(), Some(val!(" €1")));
    /// ```
    fn add(self, rhs: &ValuedAmount<'h>) -> Option<ValuedAmount<'h>> {
        if self.is_nil() {
            return Some(rhs.clone());
        }
        if rhs.is_nil() {
            return Some(self.clone());
        }

        let allocator = match (&self.valuations, &rhs.valuations) {
            (Some(vals), _) => vals.allocator(),
            (_, Some(vals)) => vals.allocator(),
            _ => unreachable!("At least one side must have valuations"),
        };

        let self_rhs_array = [&self, rhs];
        let mut unit_iter: Box<dyn Iterator<Item = &'h Unit<'h>>, _> =
            match (self.is_zero(), rhs.is_zero()) {
                // Note that this may cause duplicate units. We compensate below.
                (true, true) => Box::new_in(self.units().chain(rhs.units()), allocator),
                (true, false) => Box::new_in(rhs.units(), allocator),
                (false, true) => Box::new_in(self.units(), allocator),
                (false, false) => {
                    Box::new_in(ValuedAmount::common_units(&self_rhs_array), allocator)
                }
            };
        let initial = unit_iter.next();
        match initial {
            Some(initial) => {
                let mut va_accum = ValuedAmount::new_in(
                    AmountExpr::new(
                        self.value_in(initial).unwrap() + rhs.value_in(initial).unwrap(),
                        "",
                        None::<&'h str>,
                    ),
                    allocator,
                );

                let self_amount_val = Valuation::new_total(self.amount_expr.amount().abs(), false);
                let rhs_amount_val = Valuation::new_total(rhs.amount_expr.amount().abs(), false);
                for cu in unit_iter {
                    let lhs_val =
                        self.valuations.iter().flatten().find(|v| v.unit() == cu).or_else(|| {
                            if self.amount_expr.unit() == cu {
                                Some(&self_amount_val)
                            } else {
                                None
                            }
                        });
                    let rhs_val =
                        rhs.valuations.iter().flatten().find(|v| v.unit() == cu).or_else(|| {
                            if rhs.amount_expr.unit() == cu { Some(&rhs_amount_val) } else { None }
                        });
                    match (lhs_val, rhs_val) {
                        // When both sides have a unit valuation, we set the summed valuation to be the weighted sum.
                        (Some(Valuation::Unit(lhs_uv)), Some(Valuation::Unit(rhs_uv))) => {
                            let denom = (self.value_in(initial).unwrap().abs()
                                + rhs.value_in(initial).unwrap().abs())
                            .quantity();
                            // If the denominator is zero, it means both sides are zero in the initial unit.
                            // In that case, we'll average the two unit valuations.
                            let sum_uv = if denom.is_zero() {
                                (lhs_uv.amount() + rhs_uv.amount()) / 2
                            } else {
                                let l = lhs_uv.amount().abs()
                                    * self.value_in(initial).unwrap().quantity().abs()
                                    / denom;
                                let r = rhs_uv.amount().abs()
                                    * rhs.value_in(initial).unwrap().quantity().abs()
                                    / denom;
                                l + r
                            };
                            va_accum.set_valuation(Valuation::Unit(lhs_uv.with_amount(sum_uv)));
                        }
                        // When only one side has a unit valuation, we set it as the valuation.
                        (Some(Valuation::Unit(uv)), None) | (None, Some(Valuation::Unit(uv))) => {
                            va_accum.set_valuation(Valuation::Unit(uv.clone()));
                        }
                        // Otherwise we sum the total valuations.
                        (Some(Valuation::Total(expr, e)), _)
                        | (_, Some(Valuation::Total(expr, e))) => {
                            // Use `value_in` to account for potential negativity.
                            // Ignore the result as it may be a duplicate.
                            if cu != va_accum.unit() {
                                va_accum.set_valuation(Valuation::Total(
                                    expr.with_amount(
                                        (self.value_in(cu).unwrap() + rhs.value_in(cu).unwrap())
                                            .abs(),
                                    ),
                                    *e,
                                ));
                            }
                        }
                        (None, None) => unreachable!(),
                    }
                }
                Some(va_accum)
            }
            None => None,
        }
    }
}

impl<'h> Add for ValuedAmount<'h> {
    type Output = Option<ValuedAmount<'h>>;

    fn add(self, rhs: ValuedAmount<'h>) -> Option<ValuedAmount<'h>> {
        &self + &rhs
    }
}

impl<'h> Sub for &ValuedAmount<'h> {
    type Output = Option<ValuedAmount<'h>>;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn sub(self, rhs: Self) -> Self::Output {
        self + &rhs.clone().negated()
    }
}

impl<'h> Sub for ValuedAmount<'h> {
    type Output = Option<ValuedAmount<'h>>;

    fn sub(self, rhs: Self) -> Self::Output {
        &self - &rhs
    }
}

macro_rules! impl_sum {
    ($t:ty) => {
        impl<'h, 'a> Sum<$t> for Option<ValuedAmount<'h>> {
            /// Sums all the `ValuedAmount`s in the iterator, returning `None` if any of them could not be
            /// summed due to incompatible units.
            fn sum<I: Iterator<Item = $t>>(iter: I) -> Self {
                let mut sum: Option<ValuedAmount> = Some(ValuedAmount::nil());
                for va in iter {
                    match &sum.unwrap() + &va {
                        Some(s) => sum = Some(s),
                        None => return None,
                    }
                }
                sum
            }
        }
    };
}
impl_sum!(&'a ValuedAmount<'h>);
impl_sum!(ValuedAmount<'h>);

impl<'h> Mul<Quantity> for &ValuedAmount<'h> {
    type Output = ValuedAmount<'h>;

    fn mul(self, rhs: Quantity) -> Self::Output {
        if self.is_nil() {
            return self.clone();
        }

        let mut va = ValuedAmount::new_in(
            AmountExpr::new(self.amount_expr.amount() * rhs, "", None::<&'h str>),
            self.valuations.as_ref().unwrap().allocator(),
        );
        let mut va_valuations = Vec::new_in(*self.valuations.as_ref().unwrap().allocator());
        va_valuations.extend(self.valuations.as_ref().unwrap().iter().map(|v| match v {
            Valuation::Unit(unit) => Valuation::Unit(unit.clone()),
            Valuation::Total(tot, elided) => Valuation::new_total((**tot * rhs).abs(), *elided),
        }));
        va.valuations = Some(va_valuations);
        va
    }
}

impl<'h> Div<&ValuedAmount<'h>> for &ValuedAmount<'h> {
    type Output = Option<Decimal>;

    fn div(self, rhs: &ValuedAmount<'h>) -> Option<Decimal> {
        ValuedAmount::common_units(&[&self, rhs])
            .next()
            .map(|unit| (self.value_in(unit).unwrap() / rhs.value_in(unit).unwrap()).quantity())
    }
}
