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

use crate::amount::Amount;
use crate::error::{JournError, JournResult};
use crate::journal_entry::JournalEntry;
use crate::unit::Unit;
use crate::{err, valued_amount};
pub use entry_valuer::EntryValuer;
pub use lambda_valuer::LambdaValuer;
pub use linear_system_valuer::LinearSystemValuer;
pub use price_db_valuer::PriceDatabaseValuer;
use smallvec::SmallVec;
use smartstring::alias::String as SS;
use std::borrow::Cow;
use std::cell::LazyCell;
use std::convert::Infallible;
use std::ops::{Add, ControlFlow, Deref, DerefMut, FromResidual, Try};
use std::{fmt, iter};
pub use system_valuer::SystemValuer;

#[derive(Debug, Default, Clone)]
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
    pub fn from_amount(valuation: Amount<'h>, from_amount: Amount<'h>) -> Self {
        Self {
            amount: valuation,
            via: Some(Box::new(Valuation { amount: from_amount, ..Default::default() })),
            ..Default::default()
        }
    }

    /// The basis valuation is a valuation of itself.
    pub fn basis(valuation: Amount<'h>) -> Self {
        Self { amount: valuation, ..Default::default() }
    }

    pub fn rounded(&mut self) {
        self.amount = self.amount.rounded();
    }

    /// Gets the `quote/base` price in the valuation unit.
    pub fn price_with(&self, base_amount: Amount<'h>) -> Amount<'h> {
        self.amount / base_amount.quantity()
    }

    /// Gets the `base/quote` in the base unit.
    pub fn price_with_inverse(&self, base_amount: Amount<'h>) -> Amount<'h> {
        base_amount / self.amount.quantity()
    }

    pub fn value(&self) -> Amount<'h> {
        self.amount
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

    /// Sets the previous value in the Valuation chain.
    pub fn set_via(&mut self, via: Valuation<'h>) {
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

#[derive(Debug)]
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
    /// Returns `Ok(Valuation)` if the operation succeeded, with the inner `Amount` being `Some` if the valuation lookup was successful,
    /// and `None` if the value could not be determined.
    /// An `Err(e)` is returned if a non-system error occurred while determining the value (user error).
    ///
    /// Returned values should never be rounded. It is the responsibility of the caller to round the value if necessary.
    ///
    /// It is allowed for the function to return a `Valuation` in a unit other than the `quote_unit`, but implementors should
    /// note this behaviour up front.
    fn value(&mut self, quote_unit: &'h Unit<'h>, amount: Amount<'h>) -> ValuationResult<'h>;

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

pub enum ValueResult<'h, O> {
    Ok(O),
    Err(JournError),
    ValuationNeeded(&'h Unit<'h>, Amount<'h>),
}
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
            ValueResult::Ok(o) => return Ok(o),
            ValueResult::Err(e) => return Err(e),
            ValueResult::ValuationNeeded(quote_unit, amount) => {
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
mod test {}
