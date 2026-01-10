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
use std::convert::Infallible;
use std::fmt;
use std::ops::{ControlFlow, Deref, FromResidual, Try};
pub use system_valuer::SystemValuer;
/*
pub trait Valuation<'h> {
    fn value(&self) -> Amount<'h>;

    /// Which other units (unique) were involved in the valuation.
    fn via(&self) -> impl Iterator<Item=&'h Unit<'h>>;

    fn sources(&self) -> impl Iterator<Item=&str>;
}*/

#[derive(Debug, Default, Clone)]
pub struct Valuation<'h> {
    /// The valuation
    amount: Amount<'h>,
    /// This valuation was via another valuation
    via: Option<Box<Valuation<'h>>>,
    /// The sources of the valuation
    sources: SmallVec<[SS; 2]>,
}
impl<'h> Valuation<'h> {
    pub fn new(amount: Amount<'h>) -> Self {
        Self { amount, ..Default::default() }
    }

    pub fn rounded(&mut self) {
        self.amount = self.amount.rounded();
    }

    pub fn value(&self) -> Amount<'h> {
        self.amount
    }

    pub fn with_value(&self, amount: Amount<'h>) -> Self {
        Self { amount, ..self.clone() }
    }

    pub fn via(&self) -> impl Iterator<Item = &Valuation<'h>> + '_ {
        std::iter::successors(self.via.as_deref(), |next| next.via.as_deref())
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
/*
impl<'h> Valuation for SingleValuation<'h> {
    fn value(&self) -> &Amount<'h> {
        &self.amount
    }

    fn via(&self) -> impl Iterator<Item=>  {
        &self.via
    }

    fn sources(&self) -> impl Iterator<Item=&str>  {
        self.sources.iter()
    }
}*/
impl<'h> Deref for Valuation<'h> {
    type Target = Amount<'h>;
    fn deref(&self) -> &Self::Target {
        &self.amount
    }
}
/*
impl<'h> Valuation<'h> for Vec<SingleValuation<'h>> {
    fn value(&self) -> &Amount<'h> {
        todo!()
    }

    fn via(&self) -> &[&'h Unit<'h>] {

    }

    fn sources(&self) -> &[SS] {
        todo!()
    }
}*/

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

pub enum ValueResult<'h, O> {
    Ok(O),
    Err(JournError),
    ValuationNeeded(&'h Unit<'h>, &'h Unit<'h>),
}
pub enum ValueResidual<'h> {
    Err(JournError),
    ValuationNeeded(&'h Unit<'h>, &'h Unit<'h>),
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
            ValueResult::ValuationNeeded(base, quote) => {
                match SystemValuer::from(entry.as_ref()).value(quote, base.with_quantity(1)) {
                    Ok(val) => {
                        let price = val.value();
                        let entry = entry.to_mut();
                        for pst in
                            entry.postings_mut().filter(|pst| pst.unit() != val.value().unit())
                        {
                            if let Some(amount) = pst.amount_in(base) {
                                // When the amount is small, use unit valuations for increased accuracy. This matters when using the LinearSystemValuer.
                                // We round in case the entry gets written out later.
                                let val = if round_valuations && amount.abs() < 1 {
                                    valued_amount::PostingValuation::new_unit(price.rounded())
                                } else {
                                    let mut total = val.value() * amount.quantity();
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
                        return Err(
                            err!("Unable to value {} in {} on entry", base, quote).with_source(e)
                        );
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use nalgebra::{DMatrix, DVector};

    #[test]
    fn test_linear() {
        // Case 1
        #[rustfmt::skip]
            let m = DMatrix::from_row_slice(3, 2, &[
            1.0, 0.0,
            10.0, -2.0,
            0.0, 0.0,
        ]);
        let b = DVector::from_row_slice(&[1.0, 0.0, 0.0]);
        let decomp = m.svd(true, true);
        let solved = decomp.solve(&b, 0.0).unwrap();
        println!("Case 1: {}", solved);

        // Case 2
        #[rustfmt::skip]
            let m = DMatrix::from_row_slice(2, 2, &[
            1.0, 0.0,
            10.0, -5.0,
        ]);
        let b = DVector::from_row_slice(&[1.0, 0.0]);
        let decomp = m.svd(true, true);
        let solved = decomp.solve(&b, 0.0).unwrap();
        println!("Case 2: {}", solved);

        // Case 3
        #[rustfmt::skip]
            let m = DMatrix::from_row_slice(2, 2, &[
            1.0, 0.0,
            12.0, -4.0
        ]);
        let b = DVector::from_row_slice(&[1.0, 0.0]);
        let decomp = m.svd(true, true);
        let solved = decomp.solve(&b, 0.0).unwrap();
        println!("Case 3: {}", solved);

        // Case 4
        #[rustfmt::skip]
            let m = DMatrix::from_row_slice(3, 3, &[
            1.0, 0.0, 0.0,
            0.0, -5.0, 2.0,
            10.0, 0.0, -2.0
        ]);
        let b = DVector::from_row_slice(&[1.0, 0.0, 0.0]);
        let decomp = m.svd(true, true);
        let solved = decomp.solve(&b, 0.0).unwrap();
        println!("Case 4: {}", solved);

        // Case 5
        #[rustfmt::skip]
            let m = DMatrix::from_row_slice(4, 4, &[
            1.0, 0.0, 0.0, 0.0,
            8.0, 0.0, 0.0, -12.0,
            0.0, 0.0, 16.0, -4.0,
            8.0, -4.0, 16.0, -8.0
        ]);
        let b = DVector::from_row_slice(&[1.0, 0.0, 0.0, 0.0]);
        let decomp = m.svd(true, true);
        let solved = decomp.solve(&b, 0.0).unwrap();
        println!("Case 5: {}", solved);

        // Case 6
        #[rustfmt::skip]
        let m = DMatrix::from_row_slice(5, 5, &[
            0.0, 1.0, 0.0, 0.0, 0.0,
            8.0, 0.0, 0.0, -12.0, 0.0,
            0.0, 0.0, 16.0, -4.0, 0.0,
            0.0, 0.0, -32.0, 0.0, 4.0,
            8.0, -4.0, 16.0, 0.0, -4.0,
        ]);
        let b = DVector::from_row_slice(&[1.0, 0.0, 0.0, 0.0, 0.0]);
        let solved = m.svd(true, true).solve(&b, 0.0).unwrap();
        println!("Case 6: {}", solved);

        // Case 7 (incomplete system)
        #[rustfmt::skip]
            let m = DMatrix::from_row_slice(5, 5, &[
            0.0, 1.0, 0.0, 0.0, 0.0,
            8.0, -16.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 16.0, -4.0, 0.0,
            0.0, 0.0, 16.0, 0.0, -16.0,
            0.0, 0.0, 0.0, 0.0, 0.0
        ]);
        let b = DVector::from_row_slice(&[1.0, 0.0, 0.0, 0.0, 0.0]);
        let solved = m.svd(true, true).solve(&b, 0.0).unwrap();
        println!("Case 7: {}", solved);

        // Case 8 (slightly different rates)
        #[rustfmt::skip]
            let m = DMatrix::from_row_slice(3, 2, &[
            0.0, 1.0,
            1.0, -1.111,
            10.0, -11.12,
        ]);
        let b = DVector::from_row_slice(&[1.0, 0.0, 0.0]);
        let solved = m.svd(true, true).solve(&b, 0.0).unwrap();
        println!("Case 8: {}", solved);
    }
}
