/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::Amount;
use crate::err;
use crate::journal_entry::JournalEntry;
use crate::unit::Unit;
use crate::valuer::{Valuation, ValuationError, ValuationResult, Valuer};
use nalgebra::{DMatrix, DVector};
use rust_decimal::Decimal;
use smallvec::{SmallVec, smallvec};
use std::ops::Range;

/// Valuer that derives valuations from a set of valued amounts in a specific quote unit.
pub struct LinearSystemValuer<'h> {
    data: Vec<f64>,
    units: SmallVec<[&'h Unit<'h>; 4]>,
    row_count: usize,
    zero_sum_row: Option<usize>,
}
impl<'h> LinearSystemValuer<'h> {
    /// Creates a new valuer from a set of valued amounts in a specific quote unit.
    /// `valued_amounts` should be a collection of X = Y known values.
    pub fn new(
        valued_amounts: impl Iterator<Item = (Amount<'h>, Amount<'h>)>,
    ) -> LinearSystemValuer<'h> {
        // Add valuations, with equations rearranged to be Amount - Value = 0.
        let data = Vec::with_capacity(8);
        //data.extend((0..units.len()).map(|_| 0.0));
        let mut vav =
            LinearSystemValuer { data, units: smallvec!(), row_count: 0, zero_sum_row: None };

        for (amount, val) in valued_amounts {
            vav.add_value((amount, val));
        }
        vav
    }

    fn ensure_has_unit(&mut self, unit: &'h Unit<'h>) {
        if !self.units.contains(&unit) {
            let units_len = self.units.len();
            self.units.push(unit);

            let mut i = units_len;
            while i <= self.data.len() {
                self.data.insert(i, 0.0);
                i += units_len + 1;
                if units_len == 0 {
                    break;
                }
            }
        }
    }

    pub fn add_value(&mut self, value: (Amount<'h>, Amount<'h>)) {
        self.ensure_has_unit(value.0.unit());
        self.ensure_has_unit(value.1.unit());

        let last_row = (self.row_count + 1) * self.units.len() - self.units.len();
        let amount_col = self.unit_col(value.0.unit());
        let val_col = self.unit_col(value.1.unit());
        // Make sure the last row is zeroed out. Gets used during value.
        self.data[last_row..last_row + self.units.len()].iter_mut().for_each(|c| *c = 0.0);
        // Make the value negative so that the equation is Amount + Value = 0.
        self.data[last_row + amount_col] = f64::try_from(value.0.quantity()).unwrap();
        self.data[last_row + val_col] = f64::try_from(value.1.quantity() * dec!(-1)).unwrap();
        // Keep the last row available for the Valuer impl.
        self.data.extend((0..self.units.len()).map(|_| 0.0));
        self.row_count += 1;
    }

    /// Adds a zero sum constraint to the valuer. This extra information can be useful in solving the linear system.
    /// the `amounts` should either sum to zero or the total considered value of them are zero if they are in more
    /// than one kind of unit.
    pub fn add_zero_sum(&mut self, amounts: impl Iterator<Item = Amount<'h>>) {
        // Pre-total the amounts in Decimal to make more accurate
        let mut total_amounts = vec![];
        for amount in amounts {
            total_amounts += amount
        }
        // All amounts are zero - there is no useful zero sum information to add
        // and attempting to do so will cause the system to be unsolvable.
        if total_amounts.iter().all(|a| a.is_zero()) {
            return;
        }

        for amount in total_amounts {
            self.ensure_has_unit(amount.unit());

            for (j, i) in self.row_indices(self.row_count).enumerate() {
                if j == self.unit_col(amount.unit()) {
                    self.data[i] = f64::try_from(amount.quantity()).unwrap();
                }
            }
        }

        // Keep the last row available for the Valuer impl.
        self.data.extend((0..self.units.len()).map(|_| 0.0));
        self.zero_sum_row = Some(self.row_count);
        self.row_count += 1;
    }

    fn unit_col(&self, unit: &'h Unit<'h>) -> usize {
        self.units.iter().position(|u| *u == unit).unwrap()
    }

    fn row_indices(&mut self, i: usize) -> Range<usize> {
        i * self.units.len()..(i + 1) * self.units.len()
    }
}

impl<'h> From<&JournalEntry<'h>> for LinearSystemValuer<'h> {
    fn from(entry: &JournalEntry<'h>) -> Self {
        // Iterator of all the valuations we are using.
        let total_valuations = move || {
            entry.balanced_postings().flat_map(move |p| {
                p.valuations().filter_map(move |v| {
                    if v.is_zero() { None } else { Some((p.amount(), v.value())) }
                })
            })
        };

        let mut vav = LinearSystemValuer::new(total_valuations());
        vav.add_zero_sum(entry.balanced_postings().map(|p| p.amount()));
        vav
    }
}

impl<'h> Valuer<'h> for LinearSystemValuer<'h> {
    fn value(&mut self, quote_unit: &'h Unit<'h>, amount: Amount<'h>) -> ValuationResult<'h> {
        // If the base unit is not in the system, we cannot value it.
        if self.units.iter().all(|u| *u != amount.unit()) {
            return Err(ValuationError::Undetermined(err!("Not derivable")));
        }

        // Ensure the quote unit is part of the system
        self.ensure_has_unit(quote_unit);

        let base_unit = amount.unit();
        // The last row is special in that 1.0 is set against the column of the base_curr and
        // 0 for all others. This matches the 1.0 in the b vector and defines the system's solution to be in terms of
        // the base unit.
        for (j, i) in self.row_indices(self.row_count).enumerate() {
            self.data[i] = if self.unit_col(base_unit) == j { 1.0 } else { 0.0 };
        }

        #[allow(non_snake_case)]
        let mut A = DMatrix::from_row_slice(self.row_count + 1, self.units.len(), &self.data);
        let mut b = DVector::from_fn(A.nrows(), |i, _| if i == self.row_count { 1.0 } else { 0.0 });

        // Compute a tolerance based on f64 machine accuracy.
        // We use this to check whether values ~0 are considered zero.
        let singular_values = A.clone().svd(false, false).singular_values;
        let epsilon = 2.22 * 10.0f64.powf(-16.0);
        let tol = epsilon * A.ncols().max(A.nrows()) as f64 * singular_values[0];

        // Reorder the columns of A so that the units of interest are first. This ensures they are retained
        // when we retain only those columns that are linearly independent.
        A.swap_columns(self.unit_col(base_unit), 0);
        A.swap_columns(self.unit_col(quote_unit), 1);

        // If the system is not full rank, we'll have to remove some columns below.
        // This means the zero sum row is no longer valid and will have to be removed.
        if A.rank(tol) < A.ncols()
            && let Some(zsr) = self.zero_sum_row
        {
            A = A.remove_row(zsr);
            b = b.remove_row(0);
        }

        // Decide on which columns of A are going to be included.
        // We only want those that are linearly independent i.e. they contribute to the rank of A.
        // If we don't do this the system will be underdetermined and we won't be able to solve it uniquely.
        let mut span_matrix = DMatrix::zeros(A.nrows(), 0);
        span_matrix = span_matrix.insert_column(0, 0.0);
        span_matrix.column_mut(0).copy_from(&A.column(0));
        for col_index in 1..A.ncols() {
            let candidate_column = A.column(col_index).clone_owned();

            // Create the augmented span matrix by adding the candidate column
            let augmented_matrix = {
                let mut augmented = span_matrix.clone();
                let augmented_cols = augmented.ncols();
                augmented = augmented.insert_column(augmented_cols, 0.0);
                augmented.column_mut(augmented_cols).copy_from(&candidate_column);
                augmented
            };

            // Compute the ranks
            let current_rank = span_matrix.rank(tol);
            let augmented_rank = augmented_matrix.rank(tol);

            // If the rank increases, the candidate column is linearly independent
            if augmented_rank > current_rank {
                span_matrix = augmented_matrix;
            } else if col_index == 1 {
                // If the quote column is not linearly independent, we can't solve the system.
                return Err(ValuationError::Undetermined(err!("Not derivable")));
            }
        }

        // We don't have our base/quote units included.
        if span_matrix.ncols() < 2 {
            return Err(ValuationError::Undetermined(err!("Not derivable")));
        }

        trace!("A = {}", span_matrix);
        trace!("b = {}", b);

        // Now we are ready to solve
        let svd = span_matrix.clone().svd(true, true);
        let x = svd.solve(&b, tol).unwrap();
        trace!("x = {}", x);

        let tol = 1e-10;
        //let base_rate_adj = 1.0 / x.column(0)[0];
        let rate = x.column(0)[1];
        // A rate of approximately zero means there is no solution. The power should not be set too high - as high as experience allows.
        //let zero_check = (rate * 10.0f64.powf(9.0)).round();
        if rate > tol {
            let mut valuation = Valuation::new(
                quote_unit.with_quantity(Decimal::try_from(1.0 / rate).unwrap_or_else(|e| {
                    panic!("Unable to convert {}, to Decimal: {}", 1.0 / rate, e)
                })) * amount.quantity(),
            );
            valuation.add_source("Entry (Derived)");
            return Ok(valuation);
        }

        Err(ValuationError::Undetermined(err!("Not derivable")))
    }
}

/*
#[derive(Debug)]
pub struct Valuation<'h> {
    /// The valuation
    amount: Amount<'h>,
    /// The sources of the valuation
    sources: SmallVec<[SS; 2]>,
}
impl<'h> Valuation<'h> {
    pub fn new(amount: Amount<'h>) -> Self {
        Self { amount, sources: smallvec![] }
    }

    pub fn value(&self) -> Amount<'h> {
        self.amount
    }

    pub fn rounded(&mut self) {
        self.amount = self.amount.rounded();
    }

    pub fn with_value(&self, amount: Amount<'h>) -> Self {
        Self { amount, sources: self.sources.clone() }
    }

    pub fn clear_sources(&mut self) {
        self.sources.clear();
    }

    pub fn sources(&self) -> &[SS] {
        &self.sources
    }

    pub fn into_sources(self) -> SmallVec<[SS; 2]> {
        self.sources
    }

    pub fn add_source<S: Into<SS>>(&mut self, source: S) {
        self.sources.push(source.into());
    }
}
impl<'h> Deref for Valuation<'h> {
    type Target = Amount<'h>;
    fn deref(&self) -> &Self::Target {
        &self.amount
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
}*/

/*
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
                        let price = val.amount;
                        let entry = entry.to_mut();
                        for pst in entry.postings_mut().filter(|pst| pst.unit() != val.unit()) {
                            if let Some(amount) = pst.amount_in(base) {
                                // When the amount is small, use unit valuations for increased accuracy. This matters when using the LinearSystemValuer.
                                // We round in case the entry gets written out later.
                                let val = if round_valuations && amount.abs() < 1 {
                                    valued_amount::PostingValuation::new_unit(price.rounded())
                                } else {
                                    let mut total = val.amount * amount.quantity();
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
}*/

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
