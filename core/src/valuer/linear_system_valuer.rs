/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::Amount;
use crate::err;
use crate::error::JournResult;
use crate::journal_entry::JournalEntry;
use crate::unit::Unit;
use crate::valuer::{Valuation, ValuationError, ValuationResult, Valuer};
use rust_decimal::Decimal;
use rust_decimal::prelude::{One, Zero};
use smallvec::{SmallVec, smallvec};
use std::ops::Range;

/// Valuer that derives valuations from a set of valued amounts in a specific quote unit.
#[derive(Clone)]
pub struct LinearSystemValuer<'h> {
    data: Vec<Decimal>,
    /// For each unit, its accumulated rounding error. This is used when checking the accuracy
    /// of the solution is within the rounding tolerance.
    epsilon: SmallVec<[Decimal; 16]>,
    units: SmallVec<[&'h Unit<'h>; 4]>,
    row_count: usize,
    zero_sum_row: Option<usize>,
    connectivity: Dsu,
}
impl<'h> LinearSystemValuer<'h> {
    /// Creates a new valuer from a set of valued amounts in a specific quote unit.
    /// `valued_amounts` should be a collection of X = Y known values.
    ///
    /// Only the first of X = Y pair shall be added to the system to prevent price contradictions
    /// later.
    pub fn new(
        valued_amounts: impl Iterator<Item = (Amount<'h>, Amount<'h>)>,
    ) -> LinearSystemValuer<'h> {
        // Add valuations, with equations rearranged to be Amount - Value = 0.
        let data = Vec::with_capacity(8);
        //data.extend((0..units.len()).map(|_| 0.0));
        let mut vav = LinearSystemValuer {
            data,
            epsilon: SmallVec::<[Decimal; 16]>::new(),
            units: smallvec!(),
            row_count: 0,
            zero_sum_row: None,
            connectivity: Dsu::new(),
        };

        for (amount, val) in valued_amounts {
            vav.add_value((amount, val));
        }
        vav
    }

    fn ensure_has_unit(&mut self, unit: &'h Unit<'h>) -> usize {
        match self.units.iter().position(|u| u == &unit) {
            Some(pos) => pos,
            None => {
                let units_len = self.units.len();
                self.units.push(unit);
                self.epsilon.push(Decimal::zero());

                let mut i = units_len;
                while i <= self.data.len() {
                    self.data.insert(i, Decimal::zero());
                    //self.epsilon.insert(i, Decimal::zero());
                    i += units_len + 1;
                    if units_len == 0 {
                        break;
                    }
                }
                let dsu_id = self.connectivity.make_set();
                debug_assert_eq!(dsu_id, self.units.len() - 1);
                self.units.len() - 1
            }
        }
    }

    fn find_mapping(&self, value: (Amount<'h>, Amount<'h>)) -> Option<usize> {
        let amount_col = self.units.iter().position(|&u| u == value.0.unit())?;
        let val_col = self.units.iter().position(|&u| u == value.1.unit())?;

        (0..self.row_count).into_iter().position(|i| {
            (0..self.units.len()).all(|j| {
                if j == amount_col || j == val_col {
                    self.data[i * self.units.len() + j] != Decimal::zero()
                } else {
                    self.data[i * self.units.len() + j] == Decimal::zero()
                }
            })
        })
    }

    pub fn has_value(&self, value: (Amount<'h>, Amount<'h>)) -> bool {
        self.find_mapping(value).is_some()
    }

    /// Adds an amount/value mapping to the system. If the mapping already exists
    /// it is added to.
    pub fn add_value(&mut self, value: (Amount<'h>, Amount<'h>)) {
        // Don't add zeros
        if value.0.is_zero() && value.1.is_zero() {
            return;
        }
        let amount_col = self.ensure_has_unit(value.0.unit());
        let val_col = self.ensure_has_unit(value.1.unit());

        match self.find_mapping(value) {
            Some(row) => {
                // Don't add if the addition would make the row zero. This makes it useless.
                if self.data[row * self.units.len() + amount_col] + value.0.quantity() != dec!(0) {
                    self.data[row * self.units.len() + amount_col] += value.0.quantity();
                    self.data[row * self.units.len() + val_col] += value.1.quantity() * dec!(-1);
                    /*
                    self.epsilon[row * self.units.len() + amount_col] +=
                        value.0.epsilon().max(value.0.unit().epsilon());
                    self.epsilon[row * self.units.len() + val_col] +=
                        value.1.epsilon().max(value.1.unit().epsilon());*/
                }
            }
            None => {
                // Record these two units as connected.
                self.connectivity.union(amount_col, val_col);

                let last_row = (self.row_count + 1) * self.units.len() - self.units.len();
                // Make sure the last row is zeroed out. Gets used during value.
                self.data[last_row..last_row + self.units.len()]
                    .iter_mut()
                    .for_each(|c| *c = Decimal::zero());
                /*
                self.epsilon[last_row..last_row + self.units.len()]
                    .iter_mut()
                    .for_each(|c| *c = Decimal::zero());*/

                // Make the value negative so that the equation is Amount + Value = 0.
                self.data[last_row + amount_col] = value.0.quantity();
                self.data[last_row + val_col] = value.1.quantity() * dec!(-1);
                /*
                self.epsilon[last_row + amount_col] =
                    value.0.epsilon().max(value.0.unit().epsilon());
                self.epsilon[last_row + val_col] = value.1.epsilon().max(value.1.unit().epsilon());

                 */
                // Keep the last row available for the Valuer impl.
                self.data.extend((0..self.units.len()).map(|_| Decimal::zero()));
                //self.epsilon.extend((0..self.units.len()).map(|_| Decimal::zero()));
                self.row_count += 1;
            }
        }
        self.epsilon[amount_col] += value.0.epsilon().max(value.0.unit().epsilon());
        self.epsilon[val_col] += value.1.epsilon().max(value.1.unit().epsilon());
    }

    /// Adds a zero sum constraint to the valuer. This extra information can be useful in solving the linear system.
    /// the `amounts` should either sum to zero or the total considered value of them are zero if they are in more
    /// than one kind of unit.
    pub fn add_zero_sum(&mut self, amounts: impl Iterator<Item = Amount<'h>> + Clone) {
        // Pre-total the amounts in Decimal to make more accurate
        let mut total_amounts = SmallVec::<[Amount<'h>; 4]>::new();
        for amount in amounts.clone() {
            total_amounts += amount
        }
        // All amounts are zero - there is no useful zero sum information to add
        // and attempting to do so will cause the system to be unsolvable.
        if total_amounts.iter().all(|a| a.is_zero()) {
            return;
        }

        // Only create a union if there are two sets remaining. If there are more don't create a union.
        // This keeps our DSU fairly strict to keep out false positives.
        if self.connectivity.count() == 2 {
            for i in 0..self.connectivity.len() - 1 {
                for j in i + 1..self.connectivity.len() {
                    if self.connectivity.find(i) != self.connectivity.find(j) {
                        self.connectivity.union(i, j);
                    }
                }
            }
        }

        // When there are only two non-zero amounts, we can connect them in the DSU.
        let mut iter = total_amounts.iter().filter(|a| !a.is_zero());
        let col_a = iter.next().map(|a| self.ensure_has_unit(a.unit()));
        let col_b = iter.next().map(|a| self.ensure_has_unit(a.unit()));
        if let Some(a) = col_a
            && let Some(b) = col_b
            && iter.next().is_none()
        {
            self.connectivity.union(a, b);
        }

        // We still iterate via `amounts` rather than `total_amounts` because we want to increase epsilon values
        // for each amount. Rounding errors are additive.
        for (unit_col, amount) in total_amounts
            .iter()
            .filter(|a| !a.is_zero())
            .flat_map(|a| {
                let col = self.ensure_has_unit(a.unit());
                amounts.clone().filter_map(move |amt| {
                    if amt.unit() == a.unit() { Some((col, amt)) } else { None }
                })
            })
            .collect::<SmallVec<[(usize, Amount<'h>); 4]>>()
        {
            for (j, i) in self.row_indices(self.row_count).enumerate() {
                if j == unit_col {
                    self.data[i] += amount.quantity();
                    self.epsilon[unit_col] += amount.epsilon().max(amount.unit().epsilon());
                }
            }
        }

        // Keep the last row available for the Valuer impl.
        self.data.extend((0..self.units.len()).map(|_| Decimal::zero()));
        //self.epsilon.extend((0..self.units.len()).map(|_| Decimal::zero()));
        self.zero_sum_row = Some(self.row_count);
        self.row_count += 1;
    }

    fn unit_col(&self, unit: &'h Unit<'h>) -> usize {
        self.units.iter().position(|u| *u == unit).unwrap()
    }

    fn row_indices(&self, i: usize) -> Range<usize> {
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

impl Default for LinearSystemValuer<'_> {
    fn default() -> Self {
        LinearSystemValuer {
            data: Vec::with_capacity(8),
            units: smallvec![],
            epsilon: smallvec![],
            row_count: 0,
            zero_sum_row: None,
            connectivity: Dsu::new(),
        }
    }
}

const NOT_DERIVABLE: &str = "Not derivable";

impl<'h> Valuer<'h> for LinearSystemValuer<'h> {
    fn value(&mut self, quote_unit: &'h Unit<'h>, amount: Amount<'h>) -> ValuationResult<'h> {
        // If the base unit or the quote unit is not in the system, we cannot value it.
        if self.units.iter().all(|u| *u != amount.unit()) || !self.units.contains(&quote_unit) {
            return Err(ValuationError::Undetermined(err!(NOT_DERIVABLE)));
        }

        let base_unit = amount.unit();

        // Ensure the quote unit is part of the system
        let quote_col = self.unit_col(quote_unit);
        let base_col = self.unit_col(base_unit);

        /*
        if !self.connectivity.connected(base_col, quote_col) {
            return Err(ValuationError::Undetermined(err!(NOT_CONNECTED)));
        }*/

        // The last row is special in that 1.0 is set against the column of the base_curr and
        // 0 for all others. This matches the 1.0 in the b vector and defines the system's solution to be in terms of
        // the base unit.
        for (j, i) in self.row_indices(self.row_count).enumerate() {
            self.data[i] = if self.unit_col(base_unit) == j { dec!(1.0) } else { dec!(0.0) };
            //self.epsilon[i] = Decimal::zero();
        }

        #[allow(non_snake_case)]
        let mut A = vec![];
        //let mut epsilon: SmallVec<[SmallVec<[Decimal; 4]>; 4]> = smallvec![];
        // Get the group id of the base_unit. We'll only include units that share
        // the same group in our linear system.
        //let base_root = self.connectivity.find(base_col);
        let mut a_base_col = 0;
        let mut a_quote_col = 0;
        for i in 0..self.row_count + 1 {
            let mut row = vec![];
            //let mut epsilon_row: SmallVec<[Decimal; 4]> = smallvec![];
            for j in (0..self.units.len())
            /*.filter(|&u| self.connectivity.find(u) == base_root)*/
            {
                if self.units[j] == base_unit {
                    a_base_col = row.len();
                } else if self.units[j] == quote_unit {
                    a_quote_col = row.len();
                }
                row.push(self.data[i * self.units.len() + j]);
                //epsilon_row.push(self.epsilon[i * self.units.len() + j]);
            }
            A.push(row);
            //epsilon.push(epsilon_row);
        }
        let mut b = vec![Decimal::zero(); A.len()];
        b[A.len() - 1] = Decimal::one();

        /*
        let mut epsilon = self
            .units
            .iter()
            .map(|u| {
                let unit_col = self.unit_col(u);
                let max_data_epsilon = A
                    .split_last()
                    .unwrap()
                    .1
                    .iter()
                    .map(|row| {
                        if row[unit_col] != Decimal::zero() {
                            u.with_quantity(row[unit_col]).epsilon()
                        } else {
                            Decimal::MIN
                        }
                    })
                    .max()
                    .unwrap();
                max_data_epsilon.max(u.epsilon())
            })
            .collect::<Vec<_>>();
         */
        let mut epsilon = self.epsilon.clone();

        // Reorder the columns of A so that the units of interest are first. This ensures they are retained
        // when we retain only those columns that are linearly independent.
        swap_columns(&mut A, a_base_col, 0);
        epsilon.swap(a_base_col, 0);
        if a_quote_col == 0 {
            a_quote_col = a_base_col
        }
        swap_columns(&mut A, a_quote_col, 1);
        epsilon.swap(a_quote_col, 1);
        a_quote_col = 1;

        // If the system is not full rank, we'll have to remove some columns below.
        // This means the zero sum row is no longer valid and will have to be removed.
        let res = analyze_and_solve(&mut A, &mut b, &epsilon)?;
        /*if res.rank < self.units.len()
            && let Some(_zsr) = self.zero_sum_row
        {
            // Not sure if this can happen
            unreachable!("Unexpected rank < units.len()");
            /*
            A.remove(zsr);
            b.remove(0);*/
        }*/

        if let Some(solution) = res.solution {
            // The original row positons may have been reordered during solving so
            // we need to find the correct row. It is the one whose a_quote_col is 1.
            let rate = 'rate: {
                for i in 0..A.len() {
                    if A[i][a_quote_col] == Decimal::one() {
                        break 'rate solution[i];
                    }
                }
                return Err(ValuationError::Undetermined(err!(NOT_DERIVABLE)));
            };
            let mut valuation = Valuation::binary(
                if rate == Decimal::zero() {
                    quote_unit.with_quantity(0)
                } else {
                    quote_unit.with_quantity(Decimal::one() / rate) * amount.quantity()
                },
                amount,
            );
            valuation.add_source("Entry (Derived)");
            return Ok(valuation);
        }

        Err(ValuationError::Undetermined(err!(NOT_DERIVABLE)))
    }
}

fn swap_columns(data: &mut [Vec<Decimal>], col_a: usize, col_b: usize) {
    for row in data {
        row.swap(col_a, col_b);
    }
}

/// A Disjoint Set Union based on simple indices.
///
/// This is used to connect unit nodes together when an exchange rate is known between them, forming
/// larger sets. Before computing the linear solution, we ask this structure whether the base unit and the
/// quote unit indices are in the same union set. If not, we know that the system is not derivable.
///
/// This point of this is to save us from interpreting near-zero values that in the solution that are intended
/// to be zero but are not (due to f64 accuracy) as a non-zero solution (false positive).
#[derive(Clone)]
struct Dsu {
    parent: Vec<usize>,
    rank: Vec<u8>,
    count: usize,
}

impl Dsu {
    fn new() -> Self {
        Self { parent: Vec::new(), rank: Vec::new(), count: 0 }
    }

    fn make_set(&mut self) -> usize {
        let id = self.parent.len();
        self.parent.push(id);
        self.rank.push(0);
        self.count += 1;
        id
    }

    fn len(&self) -> usize {
        self.parent.len()
    }

    /// Gets the number of disjoint sets
    fn count(&mut self) -> usize {
        self.count
    }

    fn find(&mut self, x: usize) -> usize {
        if self.parent[x] != x {
            let root = self.find(self.parent[x]);
            self.parent[x] = root;
        }
        self.parent[x]
    }

    fn union(&mut self, a: usize, b: usize) {
        let mut ra = self.find(a);
        let mut rb = self.find(b);
        if ra == rb {
            return;
        }

        if self.rank[ra] < self.rank[rb] {
            std::mem::swap(&mut ra, &mut rb);
        }
        self.parent[rb] = ra;
        if self.rank[ra] == self.rank[rb] {
            self.rank[ra] += 1;
        }
        self.count -= 1;
    }

    /// Gets whether two units are connected; i.e. in the same sub group.
    /// If the units are not connected, the system is definitely not derivable. If they are
    /// connected, the system _should_ be derivable since we only connect two rates at a time.
    fn connected(&mut self, a: usize, b: usize) -> bool {
        self.find(a) == self.find(b)
    }
}

pub struct MatrixResult {
    pub solution: Option<Vec<Decimal>>,
    pub rank: usize,
}

#[allow(clippy::needless_range_loop)]
fn analyze_and_solve(
    a: &mut [Vec<Decimal>],
    b: &mut [Decimal],
    epsilon: &[Decimal],
) -> Result<MatrixResult, ValuationError> {
    // Keep track of the original row indices so we can validate accuracy later.
    let original_a = a.iter().cloned().collect::<SmallVec<[_; 8]>>();
    let original_b = b.iter().cloned().collect::<SmallVec<[_; 8]>>();
    let mut row_ids: SmallVec<[usize; 8]> = (0..a.len()).collect();

    if a.is_empty() || a[0].is_empty() {
        return Ok(MatrixResult { solution: None, rank: 0 });
    }

    let rows = a.len();
    let cols = a[0].len();
    let mut pivot_row = 0;

    for j in 0..cols {
        if pivot_row >= rows {
            break;
        }

        // Find best pivot
        let mut best = pivot_row;
        for i in pivot_row + 1..rows {
            if a[i][j].abs() > a[best][j].abs() {
                best = i;
            }
        }

        if a[best][j].is_zero() {
            continue;
        }

        a.swap(pivot_row, best);
        b.swap(pivot_row, best);
        row_ids.swap(pivot_row, best);

        // Normalize pivot row (crucial for solution extraction)
        let pivot = a[pivot_row][j];
        for k in j..cols {
            a[pivot_row][k] /= pivot;
        }
        b[pivot_row] /= pivot;

        // Eliminate column in all OTHER rows
        for i in 0..rows {
            if i != pivot_row {
                let factor = a[i][j];
                let b_pivot = b[pivot_row];
                b[i] -= factor * b_pivot;
                for k in j..cols {
                    let a_pivot = a[pivot_row][k];
                    a[i][k] -= factor * a_pivot;
                }
            }
        }
        pivot_row += 1;
    }

    let rank = pivot_row;

    let mut solution = None;
    if rank >= 2 {
        if a[0][1..].iter().any(|&x| !x.is_zero()) || a[1][2..].iter().any(|&x| !x.is_zero()) {
            return Err(ValuationError::Undetermined(err!(NOT_DERIVABLE)));
        }
        let mut x = vec![Decimal::ZERO; rank];
        x.copy_from_slice(&b[..rank]);
        if x.iter().all(|&x| x.is_zero()) {
            return Err(ValuationError::Undetermined(err!(NOT_DERIVABLE)));
        }

        check_tolerance(original_a.as_ref(), row_ids.as_ref(), &x, original_b.as_ref(), epsilon)?;
        solution = Some(x);
    }

    Ok(MatrixResult { solution, rank })
}

/// Checks the tolerance of the solution by calculating a residual and tolerance for each row and comparing them.
/// If any residual exceeds its corresponding tolerance, an error is returned.
fn check_tolerance(
    original_a: &[Vec<Decimal>],
    row_ids: &[usize],
    x: &[Decimal],
    original_b: &[Decimal],
    epsilon: &[Decimal],
) -> Result<(), ValuationError> {
    for (i, &row_id) in row_ids.iter().enumerate() {
        let residual: Decimal =
            original_a[row_id].iter().zip(x.iter()).map(|(a_ij, &x_j)| a_ij * x_j).sum::<Decimal>()
                - original_b[row_id];

        let tolerance: Decimal =
            x.iter().zip(epsilon.iter()).map(|(x_i, &eps_ij)| x_i.abs() * eps_ij).sum();
        // Decimal still has to round during calculations, so we need to set a minimum tolerance to avoid false positives.
        let min_abs_tolerance = Decimal::new(1, 12);
        let tolerance = tolerance.max(min_abs_tolerance);

        if residual.abs() > tolerance {
            return Err(ValuationError::Undetermined(err!(
                "Valuation not within tolerance due to inconsistent values"
            )));
        }
    }
    Ok(())
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
    e
}*/

#[cfg(test)]
mod test {

    use super::*;
    use crate::{amount, unit};

    #[test]
    fn test_multiple_groups() {
        // C and D are unconnected to A and B but we can still value A and B.
        let mut lsv = LinearSystemValuer::new(
            vec![(amount!("1 C"), amount!("10 D")), (amount!("1 A"), amount!("10 B"))].into_iter(),
        );

        assert_eq!(lsv.value(unit!("B"), amount!("1 A")), Ok(Valuation::unary(amount!("10 B"))));
        assert_eq!(lsv.value(unit!("D"), amount!("1 C")), Ok(Valuation::unary(amount!("10 D"))));
        assert_eq!(
            lsv.value(unit!("B"), amount!("1 C")),
            Err(ValuationError::Undetermined(err!(NOT_DERIVABLE)))
        );
    }

    #[test]
    fn test_not_full_rank() {
        let mut lsv = LinearSystemValuer::new(vec![(amount!("1 A"), amount!("10 B"))].into_iter());
        lsv.add_zero_sum(vec![amount!("1 A"), amount!("10 B"), amount!("100 C")].into_iter());
        assert_eq!(lsv.value(unit!("B"), amount!("0.5 A")), Ok(Valuation::unary(amount!("5 B"))));
    }
}
