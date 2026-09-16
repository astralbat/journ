/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
mod entry_valuer;
mod lambda_valuer;
mod linear_system_valuer;
mod price_db_valuer;
mod system_valuer;

use crate::amount::{Amount, Quantity};
use crate::error::{JournError, JournResult};
use crate::journal_entry::JournalEntry;
use crate::unit::Unit;
use crate::{err, valued_amount};
pub use entry_valuer::EntryValuer;
pub use lambda_valuer::LambdaValuer;
pub use linear_system_valuer::LinearSystemValuer;
pub use price_db_valuer::PriceDatabaseValuer;
use rust_decimal::prelude::One;
use smallvec::SmallVec;
use smartstring::alias::String as SS;
use std::borrow::Cow;
use std::cell::LazyCell;
use std::ops::{Add, Deref, DerefMut};
use std::{fmt, iter};
pub use system_valuer::SystemValuer;

#[derive(Default, Clone)]
pub struct Valuation<'h> {
    /// The valuation
    amount: Amount<'h>,
    /// This equivalent valuations this valuation was created from where the last
    /// valuation in the chain is the original base amount.
    via: Option<Box<Valuation<'h>>>,
    /// The sources of the valuation
    sources: SmallVec<[SS; 2]>,
}
impl<'h> Valuation<'h> {
    /// Creates a new valuation `from_amount`.
    pub fn binary(valuation: Amount<'h>, from_amount: Amount<'h>) -> Self {
        Self {
            amount: valuation,
            via: Some(Box::new(Valuation { amount: from_amount, ..Default::default() })),
            ..Default::default()
        }
    }

    /// The basis valuation is a valuation of itself.
    pub fn unary(valuation: Amount<'h>) -> Self {
        Self { amount: valuation, ..Default::default() }
    }

    pub fn rounded(&mut self) {
        self.amount = self.amount.rounded();
    }

    /// Sets the valuation unit, which must be the same as the current valuation unit.
    ///
    /// This is done to ensure the valuation unit is identical to the quote_unit passed in to the
    /// valuation call.
    pub fn set_unit(&mut self, unit: &'h Unit<'h>) {
        assert_eq!(unit, self.amount.unit(), "Cannot change valuation unit to a different unit");

        self.amount = unit.with_quantity(self.amount.quantity());
    }

    /// Gets the `quote/base` price in the valuation unit.
    pub fn price_with(&self, base_amount: Amount<'h>) -> Amount<'h> {
        self.amount / base_amount.quantity()
    }

    /// Gets the `base/quote` in the base unit.
    pub fn price_with_inverse(
        &self,
        base_amount: Amount<'h>,
        quote_unit: &'h Unit<'h>,
    ) -> Amount<'h> {
        if self.amount.is_zero() {
            quote_unit.with_quantity(dec!(0))
        } else {
            quote_unit
                .with_quantity(Quantity::one() / self.amount.quantity() * base_amount.quantity())
        }
    }

    /// Tries to invert the valuation, which will succeed for non-basis valuations.
    pub fn invert(&self) -> Option<Valuation<'h>> {
        let base_amount = self.via().last()?;
        Some(Valuation::binary(
            base_amount.unit().with_quantity(base_amount.quantity() / self.amount.quantity()),
            self.amount,
        ))
    }

    pub fn value(&self) -> Amount<'h> {
        self.amount
    }

    pub fn values(&self) -> impl DoubleEndedIterator<Item = Amount<'h>> {
        // Collect to make reverse iteration possible
        iter::once(self.amount)
            .chain(self.via().map(|v| v.amount))
            .collect::<SmallVec<[Amount; 3]>>()
            .into_iter()
    }

    pub fn contains_unit(&self, unit: &'h Unit<'h>) -> bool {
        self.amount.unit() == unit || self.via().any(|v| v.amount.unit() == unit)
    }

    /// Revalues this valuation or a component in the valuation chain. All
    /// components are revalued relative to the change.
    ///
    /// Returns `Err` when the `amount`'s unit does not appear in the valuation
    /// chain.
    pub fn revalue(&mut self, amount: Amount<'h>) -> JournResult<()> {
        let scalar = iter::chain(iter::once(&*self), self.via()).find_map(|via| {
            if amount.unit() == via.unit() {
                Some(amount.quantity() / via.quantity())
            } else {
                None
            }
        });
        match scalar {
            Some(scalar) => {
                self.amount *= scalar;
                let mut next_via = self.via.as_deref_mut();
                while let Some(via) = next_via {
                    via.amount *= scalar;
                    next_via = via.via.as_deref_mut();
                }
                Ok(())
            }
            None => Err(err!("Unable to revalue")),
        }
    }

    /// Gets the other valuations in the chain for traceability where the last valuation
    /// in the chain is the original base amount queried.
    ///
    /// This may be an empty `Iterator` in the case of a `basis` valuation (a base amount
    /// was valued in its own unit); or the end of the chain was reached.
    pub fn via(&self) -> impl Iterator<Item = &Valuation<'h>> + '_ {
        iter::successors(self.via.as_deref(), |next| next.via.as_deref())
    }

    pub fn via_values(&self) -> impl Iterator<Item = Amount<'h>> + '_ {
        self.via().map(|v| v.amount)
    }

    /// Sets the previous value in the Valuation chain.
    pub fn set_via(&mut self, via: Valuation<'h>) {
        if via.via().any(|v| v.unit() == self.amount.unit()) {
            panic!(
                "Cannot set valuation via a valuation that contains the same unit as the current valuation"
            );
        }
        self.via = Some(Box::new(via));
    }

    pub fn sources(&self) -> &[SS] {
        &self.sources
    }

    pub fn clear_sources(&mut self) {
        self.sources.clear();
    }

    pub fn into_sources(self) -> SmallVec<[SS; 2]> {
        self.sources
    }

    /// Adds the source if not already contained, returning `true` if added.
    pub fn add_source<S: Into<SS>>(&mut self, source: S) -> bool {
        let source: SS = source.into();
        if !self.sources.contains(&source) {
            self.sources.push(source);
            true
        } else {
            false
        }
    }
}

impl<'h> Deref for Valuation<'h> {
    type Target = Amount<'h>;
    fn deref(&self) -> &Self::Target {
        &self.amount
    }
}

impl fmt::Display for Valuation<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.values().rev().peekable();
        while let Some(value) = iter.next() {
            write!(f, "{}", value)?;
            if iter.peek().is_some() {
                write!(f, " ==> ")?;
            }
        }
        /*
        let mut iter = self.values().enumerate().peekable();
        while let Some((i, val)) = iter.next() {
            // Skip last (base amount)
            if iter.peek().is_none() {
                break;
            }
            if i > 0 {
                write!(f, " via ")?;
            }
            write!(f, "{}", val)?;
        }*/
        Ok(())
    }
}

impl fmt::Debug for Valuation<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.values().rev().peekable();
        while let Some(value) = iter.next() {
            write!(f, "{:?}", value)?;
            if iter.peek().is_some() {
                write!(f, " ==> ")?;
            }
        }
        Ok(())
    }
}

impl PartialEq for Valuation<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.amount == other.amount
    }
}

impl<'h> Add for &Valuation<'h> {
    type Output = Option<Valuation<'h>>;

    /// Adds two valuations. This will return `Some` if the valuations
    /// share the same unit; `None` otherwise.
    ///
    /// Note that this operation may lose traceability information if both sides'
    /// traceability is not unit compatible.
    fn add(self, rhs: Self) -> Option<Valuation<'h>> {
        if self.unit() != rhs.unit() {
            None
        } else {
            Some(Valuation {
                amount: self.amount + rhs.amount,
                sources: iter::chain(self.sources.iter(), rhs.sources.iter()).cloned().collect(),
                via: self.via.as_deref().and_then(|self_via| {
                    rhs.via.as_deref().and_then(|rhs_via| (self_via + rhs_via).map(Box::new))
                }),
            })
        }
    }
}

impl<'h> Valuer<'h> for Valuation<'h> {
    /// Values an `amount` in `quote_unit` by evaluating the valuation chain for both units.
    /// If they exist, the valuation can be returned.
    fn value(&mut self, quote_unit: &'h Unit<'h>, amount: Amount<'h>) -> ValuationResult<'h> {
        let mut new_value = self.clone();
        match new_value.revalue(amount) {
            Ok(()) => {
                if new_value.value().unit() == quote_unit {
                    new_value.set_unit(quote_unit);
                    return Ok(new_value);
                }
                new_value
                    .via()
                    .find(|v| v.unit() == quote_unit)
                    .cloned()
                    .map(|v| {
                        let mut v = v;
                        v.set_unit(quote_unit);
                        Ok(v)
                    })
                    .unwrap_or_else(|| {
                        Err(ValuationError::Undetermined(err!(
                            "Unable to value {} in {}",
                            amount,
                            quote_unit
                        )))
                    })
            }
            Err(e) => Err(ValuationError::EvalFailure(
                err!("Unable to revalue {} in {}", amount, quote_unit).with_source(e),
            )),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ValuationError {
    /// Price could not be found with this valuer at this time with reason provided.
    Undetermined(JournError),
    /// An error occurred during evaluation of the valuer
    EvalFailure(JournError),
}
impl std::error::Error for ValuationError {}
impl fmt::Display for ValuationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValuationError::Undetermined(reason) => write!(f, "{}", reason),
            ValuationError::EvalFailure(e) => write!(f, "{}", e),
        }
    }
}

impl From<ValuationError> for JournError {
    fn from(e: ValuationError) -> Self {
        match e {
            ValuationError::Undetermined(reason) => err!("{}", reason),
            ValuationError::EvalFailure(e) => e,
        }
    }
}

pub type ValuationResult<'h> = Result<Valuation<'h>, ValuationError>;

pub struct OrValuer<'h, V1, V2>
where
    V1: Valuer<'h>,
    V2: Valuer<'h>,
{
    first: V1,
    second: V2,
    _marker: std::marker::PhantomData<&'h ()>,
}
impl<'h, V1, V2> OrValuer<'h, V1, V2>
where
    V1: Valuer<'h>,
    V2: Valuer<'h>,
{
    pub fn new(first: V1, second: V2) -> Self {
        Self { first, second, _marker: std::marker::PhantomData }
    }
}
impl<'h, V1, V2> Valuer<'h> for OrValuer<'h, V1, V2>
where
    V1: Valuer<'h>,
    V2: Valuer<'h>,
{
    fn value(&mut self, quote_unit: &'h Unit<'h>, amount: Amount<'h>) -> ValuationResult<'h> {
        match self.first.value(quote_unit, amount) {
            Ok(v) => Ok(v),
            Err(ValuationError::Undetermined(_)) => self.second.value(quote_unit, amount),
            Err(e) => Err(e),
        }
    }
}

/// A trait for valuing amounts in different units.
pub trait Valuer<'h> {
    /// Values the specified `amount` in the `quote_unit`.
    ///
    /// # Returns
    /// The unrounded value of the `amount` in terms of the `quote_unit` passed in, or an error if the valuation
    /// could not be determined.
    fn value(&mut self, quote_unit: &'h Unit<'h>, amount: Amount<'h>) -> ValuationResult<'h>;

    /// Values the specified `amount` in the `quote_unit` via a third unit `via`. This is useful for
    /// valuing an amount in a unit that is not directly supported by the valuer, but can be valued via
    /// another unit. For example, units A and B may not value well against each other, but both can
    /// be valued in USD.
    ///
    /// The `via` valuation should be in a unit that is neither the `quote_unit` nor the unit of the `amount`,
    /// with the `via` valuation chain being searched for such a unit.
    ///
    /// # Panics
    /// If no valid unit third unit is found.
    fn value_via(
        &mut self,
        quote_unit: &'h Unit<'h>,
        amount: Amount<'h>,
        mut via: Valuation<'h>,
    ) -> ValuationResult<'h> {
        let via_unit = via
            .values()
            .find(|v| v.unit() != quote_unit && v.unit() != amount.unit())
            .map(|a| a.unit())
            .unwrap_or_else(|| panic!("Unable to value {} in {} via {}", amount, quote_unit, via));

        // The `via` may already be able to value, saving a potentially more  expensive valuation call.
        let intermediate_val =
            Valuer::value(&mut via, via_unit, amount).or_else(|_| self.value(via_unit, amount))?;
        let mut v = self.value(quote_unit, *intermediate_val)?;
        v.set_via(intermediate_val);
        Ok(v)
    }

    fn or(self, other: impl Valuer<'h>) -> OrValuer<'h, Self, impl Valuer<'h>>
    where
        Self: Sized,
    {
        OrValuer::new(self, other)
    }
}

impl<'h, F> Valuer<'h> for F
where
    F: FnMut(&'h Unit<'h>, Amount<'h>) -> ValuationResult<'h>,
{
    fn value(&mut self, unit: &'h Unit<'h>, amount: Amount<'h>) -> ValuationResult<'h> {
        self(unit, amount)
    }
}

impl<'h, T: Valuer<'h>> Valuer<'h> for LazyCell<T> {
    fn value(&mut self, quote_unit: &'h Unit<'h>, amount: Amount<'h>) -> ValuationResult<'h> {
        self.deref_mut().value(quote_unit, amount)
    }
}

pub enum ValueError<'h> {
    Err(JournError),
    ValuationNeeded(&'h Unit<'h>, Amount<'h>),
}
pub type ValueResult<'h, O> = Result<O, ValueError<'h>>;

impl fmt::Debug for ValueError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueError::Err(e) => write!(f, "Err({:?})", e),
            ValueError::ValuationNeeded(u, a) => {
                write!(f, "ValuationNeeded({}, {})", u, a)
            }
        }
    }
}

/*
pub enum ValueResult<'h, O> {
    Ok(O),
    Err(JournError),
    ValuationNeeded(&'h Unit<'h>, Amount<'h>),
}*/
/*
pub enum ValueResidual<'h> {
    Err(JournError),
    ValuationNeeded(&'h Unit<'h>, Amount<'h>),
}

impl<'h, O> Try for ValueResult<'h, O> {
    type Output = O;
    type Residual = ValueResidual<'h>;

    fn from_output(output: Self::Output) -> Self {
        ValueResult::Ok(output)
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            ValueResult::Ok(output) => ControlFlow::Continue(output),
            ValueResult::Err(e) => ControlFlow::Break(ValueResidual::Err(e)),
            ValueResult::ValuationNeeded(u1, u2) => {
                ControlFlow::Break(ValueResidual::ValuationNeeded(u1, u2))
            }
        }
    }
}
impl<'h, O> FromResidual<ValueResidual<'h>> for ValueResult<'h, O> {
    fn from_residual(residual: ValueResidual<'h>) -> Self {
        match residual {
            ValueResidual::Err(e) => ValueResult::Err(e),
            ValueResidual::ValuationNeeded(u1, u2) => ValueResult::ValuationNeeded(u1, u2),
        }
    }
}

impl<'h, O> FromResidual<Result<Infallible, JournError>> for ValueResult<'h, O> {
    fn from_residual(residual: Result<Infallible, JournError>) -> Self {
        match residual {
            Err(e) => ValueResult::Err(e),
            Ok(infallible) => match infallible {}, // This will never happen
        }
    }
}*/

impl From<JournError> for ValueError<'_> {
    fn from(e: JournError) -> Self {
        ValueError::Err(e)
    }
}

/// Executes a function that may return a `ValuationNeeded` result during its processing.
/// This will cause the valuation to be performed on the entry before retrying.
pub fn exec_optimistic<'h, F, O>(
    entry: &mut Cow<JournalEntry<'h>>,
    round_valuations: bool,
    f: F,
) -> JournResult<O>
where
    F: Fn(&JournalEntry<'h>) -> ValueResult<'h, O>,
{
    loop {
        match f(entry.as_ref()) {
            Ok(o) => return Ok(o),
            Err(ValueError::Err(e)) => return Err(e),
            Err(ValueError::ValuationNeeded(quote_unit, amount)) => {
                match SystemValuer::from(entry.as_ref()).value(quote_unit, amount) {
                    Ok(val) => {
                        let price = val.value() * (dec!(1) / amount.quantity());
                        let entry = entry.to_mut();
                        for pst in entry.postings_mut().filter(|pst| pst.unit() != quote_unit) {
                            if let Some(amount) = pst.amount_in(amount.unit()) {
                                // When the amount is small, use unit valuations for increased accuracy. This matters when using the LinearSystemValuer.
                                // We round in case the entry gets written out later.
                                let val = if round_valuations && amount.abs() < 1 {
                                    valued_amount::PostingValuation::new_unit(price.rounded())
                                } else {
                                    let mut total = price * amount.quantity();
                                    if round_valuations {
                                        total = total.rounded();
                                    }
                                    valued_amount::PostingValuation::new_total(total, false)
                                };
                                pst.set_valuation(val);
                            }
                        }
                    }
                    Err(e) => {
                        return Err(err!("Unable to value {} in {} on entry", amount, quote_unit)
                            .with_source(e));
                    }
                }
            }
        }
    }
}
#[cfg(test)]
mod test {
    use crate::valuer::{Valuation, Valuer};
    use crate::{amount, unit};

    #[test]
    fn test_revalue() {
        let mut v = Valuation::unary(amount!("$1"));
        assert!(v.revalue(amount!("$2")).is_ok());
        assert!(v.revalue(amount!("€1")).is_err());

        // Doubling B, should double A in the valuation chain.
        let mut via = Valuation::binary(amount!("10 A"), amount!("1 B"));
        assert!(via.revalue(amount!("2 B")).is_ok());
        assert_eq!(via.value(), amount!("20 A"));
    }

    #[test]
    fn test_value() {
        let mut via = Valuation::binary(amount!("10 A"), amount!("1 B"));
        assert_eq!(
            Valuer::value(&mut via, unit!("B"), amount!("1 A"))
                .map(|v| v.value())
                .map_err(|e| e.to_string()),
            Ok(amount!("0.1 B"))
        );
    }
}
