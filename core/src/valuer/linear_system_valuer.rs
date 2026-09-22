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
use std::collections::HashMap;
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
    /// Tracks which row holds the accumulated data for a given (amount_col, val_col) unit pair,
    /// keyed by the unordered pair of column indices. This is needed (rather than inferring the
    /// row from which columns are currently nonzero) because a pair's coefficients can
    /// legitimately become exactly zero once merged (e.g. two valuations of opposite sign that
    /// cancel, or a valuation whose stated value is itself zero), which would otherwise be
    /// indistinguishable from there being no row at all for that pair.
    row_for_pair: HashMap<(usize, usize), usize>,
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
        let mut vav = LinearSystemValuer {
            data,
            epsilon: SmallVec::<[Decimal; 16]>::new(),
            units: smallvec!(),
            row_count: 0,
            zero_sum_row: None,
            connectivity: Dsu::new(),
            row_for_pair: HashMap::new(),
        };

        for (amount, val) in valued_amounts {
            vav.add_value((amount, val));
        }
        vav
    }

    /// Checks the entry for inconsistent valuations - valuations not within rounding tolerances of the precision of
    /// the values provided.
    pub fn check(entry: &JournalEntry<'h>) -> JournResult<()> {
        let mut vav = LinearSystemValuer::from(entry);
        for pst in entry.balanced_postings() {
            for value_unit in pst.value_units() {
                match vav.value(value_unit, pst.amount()) {
                    Ok(_) => {}
                    Err(ve) => match ve {
                        ValuationError::Undetermined(err)
                            if err.contains_msg(&err!(VALUATION_NOT_WITHIN_TOLERANCE)) =>
                        {
                            return Err(err!("Inconsistent valuations",));
                        }
                        ValuationError::EvalFailure(err) => return Err(err),
                        _ => {}
                    },
                }
            }
        }
        Ok(())
    }

    fn ensure_has_unit(&mut self, unit: &'h Unit<'h>) -> usize {
        match self.units.iter().position(|u| u == &unit) {
            Some(pos) => pos,
            None => {
                let units_len = self.units.len();
                self.units.push(unit);

                let mut i = units_len;
                while i <= self.data.len() {
                    self.data.insert(i, Decimal::zero());
                    self.epsilon.insert(i, Decimal::zero());
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
        let key = if amount_col <= val_col { (amount_col, val_col) } else { (val_col, amount_col) };
        self.row_for_pair.get(&key).copied()
    }

    pub fn has_value(&self, value: (Amount<'h>, Amount<'h>)) -> bool {
        self.find_mapping(value).is_some()
    }

    /// Adds an amount/value mapping to the system. If the mapping already exists
    /// it is added to.
    pub fn add_value(&mut self, value: (Amount<'h>, Amount<'h>)) {
        self.add_value_with_epsilon(value, value.1.epsilon());
    }

    fn add_value_with_epsilon(&mut self, value: (Amount<'h>, Amount<'h>), value_epsilon: Decimal) {
        // Don't add zeros
        if value.0.is_zero() && value.1.is_zero() {
            return;
        }
        let amount_col = self.ensure_has_unit(value.0.unit());
        let val_col = self.ensure_has_unit(value.1.unit());

        match self.find_mapping(value) {
            Some(row) => {
                let amount_idx = row * self.units.len() + amount_col;
                let val_idx = row * self.units.len() + val_col;
                self.data[amount_idx] += value.0.quantity();
                self.data[val_idx] += value.1.quantity() * dec!(-1);
                // The posted amount is treated as an exact, transacted quantity (not a
                // rounded measurement), so it contributes no epsilon of its own here. All
                // rounding uncertainty in this equation comes from the stated valuation,
                // whose rounding errors are additive when summing independently-rounded
                // valuations.
                self.epsilon[val_idx] += value_epsilon;
            }
            None => {
                // Record these two units as connected.
                self.connectivity.union(amount_col, val_col);

                let last_row = (self.row_count + 1) * self.units.len() - self.units.len();
                // Make sure the last row is zeroed out. Gets used during value.
                self.data[last_row..last_row + self.units.len()]
                    .iter_mut()
                    .for_each(|c| *c = Decimal::zero());
                self.epsilon[last_row..last_row + self.units.len()]
                    .iter_mut()
                    .for_each(|c| *c = Decimal::zero());

                // Make the value negative so that the equation is Amount + Value = 0.
                self.data[last_row + amount_col] = value.0.quantity();
                self.data[last_row + val_col] = value.1.quantity() * dec!(-1);
                // The posted amount is an exact, transacted quantity; only the stated valuation
                // carries rounding uncertainty (see the merge branch above for more detail).
                self.epsilon[last_row + val_col] = value_epsilon;

                // Keep the last row available for the Valuer impl.
                self.data.extend((0..self.units.len()).map(|_| Decimal::zero()));
                self.epsilon.extend((0..self.units.len()).map(|_| Decimal::zero()));
                let key = if amount_col <= val_col { (amount_col, val_col) } else { (val_col, amount_col) };
                self.row_for_pair.insert(key, self.row_count);
                self.row_count += 1;
            }
        }
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

        // Posted quantities are exact transactions. Rounding uncertainty for primary amounts that
        // represent a quoted value is added by `From<&JournalEntry>` below.
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
                }
            }
        }

        // Keep the last row available for the Valuer impl.
        self.data.extend((0..self.units.len()).map(|_| Decimal::zero()));
        self.epsilon.extend((0..self.units.len()).map(|_| Decimal::zero()));
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
        let mut vav = LinearSystemValuer::default();
        // Count how many valuations exist per unordered unit-pair (posting unit, valuation unit)
        // so that a valuation whose stated value is zero can still be included when it is
        // corroborated by another valuation for the same pair (contributing useful rounding
        // tolerance), while a *lone* zero valuation for a pair continues to be dropped (it
        // carries no rate information on its own and would otherwise create a degenerate,
        // self-contradictory row).
        let mut pair_counts: HashMap<(&Unit, &Unit), usize> = HashMap::new();
        for posting in entry.balanced_postings() {
            for valuation in posting.posting_valuations() {
                let a = posting.unit();
                let b = valuation.unit();
                let key = if a.code() <= b.code() { (a, b) } else { (b, a) };
                *pair_counts.entry(key).or_insert(0) += 1;
            }
        }
        for posting in entry.balanced_postings() {
            for valuation in posting.posting_valuations().filter(|valuation| {
                if !valuation.value().is_zero() {
                    return true;
                }
                let a = posting.unit();
                let b = valuation.unit();
                let key = if a.code() <= b.code() { (a, b) } else { (b, a) };
                pair_counts.get(&key).copied().unwrap_or(0) > 1
            }) {
                let value = valuation.value_with_primary(posting.amount());
                let value_epsilon = if valuation.is_unit() {
                    valuation.expr().epsilon() * posting.amount().quantity().abs()
                } else {
                    value.epsilon()
                };
                vav.add_value_with_epsilon((posting.amount(), value), value_epsilon);
            }
        }
        vav.add_zero_sum(entry.balanced_postings().map(|p| p.amount()));
        if let Some(zero_sum_row) = vav.zero_sum_row {
            for posting in entry.balanced_postings().filter(|pst| !pst.amount().is_zero()) {
                let is_quoted_elsewhere = entry
                    .balanced_postings()
                    .flat_map(|posting| posting.posting_valuations())
                    .any(|valuation| valuation.unit() == posting.unit());
                if is_quoted_elsewhere {
                    let i = zero_sum_row * vav.units.len() + vav.unit_col(posting.unit());
                    vav.epsilon[i] += posting.amount().epsilon();
                }
            }
        }
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
            row_for_pair: HashMap::new(),
        }
    }
}

static NOT_DERIVABLE: &str = "Not derivable";
static VALUATION_NOT_WITHIN_TOLERANCE: &str = "Inconsistent values";

impl<'h> Valuer<'h> for LinearSystemValuer<'h> {
    fn value(&mut self, quote_unit: &'h Unit<'h>, amount: Amount<'h>) -> ValuationResult<'h> {
        // If the base unit or the quote unit is not in the system, we cannot value it.
        if self.units.iter().all(|u| *u != amount.unit()) || !self.units.contains(&quote_unit) {
            return Err(ValuationError::Undetermined(err!(NOT_DERIVABLE)));
        }

        let base_unit = amount.unit();
        if self.zero_sum_row.is_none()
            && !self.connectivity.connected(self.unit_col(base_unit), self.unit_col(quote_unit))
        {
            return Err(ValuationError::Undetermined(err!(NOT_DERIVABLE)));
        }

        // The last row is special in that 1.0 is set against the column of the base_curr and
        // 0 for all others. This matches the 1.0 in the b vector and defines the system's solution to be in terms of
        // the base unit.
        for (j, i) in self.row_indices(self.row_count).enumerate() {
            self.data[i] = if self.unit_col(base_unit) == j { dec!(1.0) } else { dec!(0.0) };
            self.epsilon[i] = Decimal::zero();
        }

        #[allow(non_snake_case)]
        let mut A = vec![];
        let mut epsilon = vec![];
        // Get the group id of the base_unit. We'll only include units that share
        // the same group in our linear system.
        let mut a_base_col = 0;
        let mut a_quote_col = 0;
        for i in 0..self.row_count + 1 {
            let mut row = vec![];
            let mut epsilon_row = vec![];
            for j in 0..self.units.len() {
                if self.units[j] == base_unit {
                    a_base_col = row.len();
                } else if self.units[j] == quote_unit {
                    a_quote_col = row.len();
                }
                row.push(self.data[i * self.units.len() + j]);
                epsilon_row.push(self.epsilon[i * self.units.len() + j]);
            }
            A.push(row);
            epsilon.push(epsilon_row);
        }
        let mut b = vec![Decimal::zero(); A.len()];
        b[A.len() - 1] = Decimal::one();

        // Reorder the columns of A so that the units of interest are first. This ensures they are retained
        // when we retain only those columns that are linearly independent.
        swap_columns(&mut A, a_base_col, 0);
        swap_columns(&mut epsilon, a_base_col, 0);
        if a_quote_col == 0 {
            a_quote_col = a_base_col
        }
        swap_columns(&mut A, a_quote_col, 1);
        swap_columns(&mut epsilon, a_quote_col, 1);
        a_quote_col = 1;

        // If the system is not full rank, we'll have to remove some columns below.
        // This means the zero sum row is no longer valid and will have to be removed.
        let res = analyze_and_solve(&mut A, &mut b, &epsilon, self.zero_sum_row)?;

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
}

/// Performs Gauss-Jordan elimination (with partial pivoting) on `a`, reducing it to reduced row
/// echelon form in place. `rhs` is carried alongside `a` and updated with the same linear
/// combinations used to reduce `a`, so that `rhs[i]` for a pivot row ends up holding the solved
/// value for the variable that row pivoted on. `row_ids` is permuted in lockstep with the rows
/// so that callers can map reduced row positions back to their original row.
///
/// When `worst_case` is `true`, `rhs` is treated as a vector of worst-case error bounds rather
/// than exact values: multiplications/accumulations that would normally allow errors of opposite
/// sign to cancel are instead done with absolute values, since two independent measurement
/// errors can't be assumed to offset each other. This lets the exact same elimination steps be
/// replayed to propagate per-row error budgets through to the solved variables (and to any
/// redundant/check rows), rather than just the nominal solution.
///
/// Returns the rank of `a`. `pivot_cols`, if provided, is cleared and filled (in pivot order)
/// with the original column index each successful pivot resolved, so callers can tell which
/// column ended up at each final row position — this can diverge from row position once any
/// earlier column fails to find a pivot (e.g. because its coefficients are all zero across the
/// remaining rows), since a later column then takes over that row slot instead.
#[allow(clippy::needless_range_loop)]
fn eliminate(
    a: &mut [Vec<Decimal>],
    rhs: &mut [Decimal],
    row_ids: &mut [usize],
    worst_case: bool,
    mut pivot_cols: Option<&mut Vec<usize>>,
) -> usize {
    if let Some(pivot_cols) = pivot_cols.as_deref_mut() {
        pivot_cols.clear();
    }
    if a.is_empty() || a[0].is_empty() {
        return 0;
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
        rhs.swap(pivot_row, best);
        row_ids.swap(pivot_row, best);

        // Normalize pivot row (crucial for solution extraction)
        let pivot = a[pivot_row][j];
        for k in j..cols {
            a[pivot_row][k] /= pivot;
        }
        rhs[pivot_row] =
            if worst_case { rhs[pivot_row] / pivot.abs() } else { rhs[pivot_row] / pivot };

        // Eliminate column in all OTHER rows
        for i in 0..rows {
            if i != pivot_row {
                let factor = a[i][j];
                let rhs_pivot = rhs[pivot_row];
                if worst_case {
                    rhs[i] += factor.abs() * rhs_pivot;
                } else {
                    rhs[i] -= factor * rhs_pivot;
                }
                for k in j..cols {
                    let a_pivot = a[pivot_row][k];
                    a[i][k] -= factor * a_pivot;
                }
            }
        }
        if let Some(pivot_cols) = pivot_cols.as_deref_mut() {
            pivot_cols.push(j);
        }
        pivot_row += 1;
    }

    pivot_row
}

#[allow(clippy::needless_range_loop)]
fn analyze_and_solve(
    a: &mut [Vec<Decimal>],
    b: &mut [Decimal],
    //epsilon: &[SmallVec<[Decimal; 4]>],
    epsilon: &[Vec<Decimal>],
    zero_sum_row: Option<usize>,
) -> Result<MatrixResult, ValuationError> {
    // Keep track of the original row indices so we can validate accuracy later.
    let original_a = a.iter().cloned().collect::<SmallVec<[_; 8]>>();
    let original_b = b.iter().cloned().collect::<SmallVec<[_; 8]>>();
    let mut row_ids: SmallVec<[usize; 8]> = (0..a.len()).collect();

    if a.is_empty() || a[0].is_empty() {
        return Ok(MatrixResult { solution: None });
    }

    let mut pivot_cols: Vec<usize> = Vec::new();
    let rank = eliminate(a, b, &mut row_ids, false, Some(&mut pivot_cols));

    // Column 1 is, by convention of the sole caller, the quote unit's column (after the
    // base/quote columns were swapped to the front). If it never received a pivot, its rate
    // is genuinely undetermined by the system (no combination of rows resolves it), so it
    // would be meaningless (and unsound, per the comment on `x_by_col` below) to run tolerance
    // checks against a "solution" that doesn't actually include it.
    if a[0].len() > 1 && !pivot_cols.iter().take(rank).any(|&c| c == 1) {
        return Ok(MatrixResult { solution: None });
    }

    let mut solution = None;
    if rank >= 2 {
        let mut x = vec![Decimal::ZERO; rank];
        x.copy_from_slice(&b[..rank]);

        // `x` is indexed by *final row/pivot position*, which only lines up with the original
        // column order when every column from left to right finds a pivot. Once some column
        // fails to pivot (e.g. its coefficients are all zero at that point), a later column
        // takes over that row slot instead, shifting the correspondence. Rebuild a
        // column-indexed view (0 for any column that was never pivoted, i.e. left undetermined)
        // so that every subsequent computation that combines `x` with per-column data
        // (`a_row`/`eps_row`, both still in original column order) lines up correctly.
        let cols = a[0].len();
        let mut x_by_col = vec![Decimal::ZERO; cols];
        for (pos, &col) in pivot_cols.iter().enumerate() {
            x_by_col[col] = x[pos];
        }

        // Each row that was actually used to *pivot* (i.e. solve for one of the variables in `x`)
        // has its own local error budget: the worst-case change to its residual if its own
        // coefficients were off by their recorded epsilon, holding the solution `x` fixed. That
        // budget is exactly the uncertainty that got baked into the corresponding solved
        // variable, so we seed it as the starting error for that row and propagate it through
        // the identical sequence of eliminations used to derive `x`.
        //
        // The zero-sum row has its own rounding budget. Seed it even when redundant so the
        // tolerance survives the same eliminations as the solution.
        let is_pivot_row: SmallVec<[bool; 8]> = {
            let mut flags = smallvec![false; original_a.len()];
            for &row_id in row_ids.iter().take(rank) {
                flags[row_id] = true;
            }
            if let Some(row_id) = zero_sum_row {
                flags[row_id] = true;
            }
            flags
        };
        let local_tolerance: Vec<Decimal> = original_a
            .iter()
            .zip(epsilon.iter())
            .map(|(a_row, eps_row)| {
                x_by_col
                    .iter()
                    .zip(a_row.iter().zip(eps_row.iter()))
                    .map(|(x_j, (_, &eps_ij))| x_j.abs() * eps_ij)
                    .sum()
            })
            .collect();
        let mut prop_row_ids: SmallVec<[usize; 8]> = (0..original_a.len()).collect();
        let mut prop_a: Vec<Vec<Decimal>> = original_a.iter().cloned().collect();
        let mut prop_rhs: Vec<Decimal> = local_tolerance
            .iter()
            .enumerate()
            .map(|(row_id, &tolerance)| {
                if !is_pivot_row[row_id] {
                    return Decimal::ZERO;
                }
                tolerance
            })
            .collect();
        eliminate(&mut prop_a, &mut prop_rhs, &mut prop_row_ids, true, None);

        // `prop_rhs` is now indexed the same way `row_ids`/`b` ended up after the main
        // elimination (since it was derived from an identical copy of `a` and so pivots
        // identically), giving each original row's propagated tolerance in its final position.
        let mut tolerance = vec![Decimal::ZERO; original_a.len()];
        for (pos, &row_id) in prop_row_ids.iter().enumerate() {
            tolerance[row_id] = prop_rhs[pos]
                + if is_pivot_row[row_id] { Decimal::ZERO } else { local_tolerance[row_id] };
        }

        check_tolerance(
            original_a.as_ref(),
            row_ids.as_ref(),
            &x_by_col,
            original_b.as_ref(),
            &tolerance,
        )?;
        solution = Some(x);
    }

    Ok(MatrixResult { solution })
}

/// Checks the tolerance of the solution by calculating a residual and tolerance for each row and comparing them.
/// If any residual exceeds its corresponding tolerance, an error is returned.
fn check_tolerance(
    original_a: &[Vec<Decimal>],
    row_ids: &[usize],
    x: &[Decimal],
    original_b: &[Decimal],
    tolerance: &[Decimal],
) -> Result<(), ValuationError> {
    for &row_id in row_ids.iter() {
        let residual: Decimal =
            original_a[row_id].iter().zip(x.iter()).map(|(a_ij, &x_j)| a_ij * x_j).sum::<Decimal>()
                - original_b[row_id];

        // Decimal still has to round during calculations, so we need to set a minimum tolerance to avoid false positives.
        let min_abs_tolerance = Decimal::new(1, 12);
        let tolerance = tolerance[row_id].max(min_abs_tolerance);
        if residual.abs() > tolerance {
            return Err(ValuationError::Undetermined(err!(VALUATION_NOT_WITHIN_TOLERANCE)));
        }
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::test_util::{entry, with_entry};
    use crate::{amount, entry, unit};
    use indoc::indoc;

    #[test]
    fn test_zero_value() {
        with_entry(
            indoc! {r#"
            2000-01-01  Entry 1
                A  100 ABC
                B  100 XYZ @@ $0
                C  -100 DEF @@ $100
            "#},
            |entry| {
                let mut valuer = LinearSystemValuer::from(entry);
                assert_eq!(
                    valuer.value(unit!("ABC"), amount!("100 DEF")),
                    Ok(amount!("100 ABC").into())
                );
            },
        );
    }

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

    #[test]
    fn test_tolerance1() {
        let res = entry(indoc! {r#"
            2000-01-01  Entry 1
                ACC_A  -100 B @@ $1.00
                ACC_B  100 A @@ $0.99
            "#});
        assert!(res.is_err());

        let res = entry(indoc! {r#"
            2000-01-01  Entry 1
                ACC_A  -100 B @@ $1.00
                ACC_B  100 A @@ $0.995
            "#});
        assert!(res.is_ok());
    }

    #[test]
    fn test_tolerance2() {
        // This looks like it should fail as $100.00 != $90.00 + $9.99, but the rounding error assumes
        // worst case and so is additive for $90.00 and $9.99, which is enough to cover the $0.01 difference.
        let res = entry(indoc! {r#"
            2000-01-01  Entry 1
                ACC_A  -100 A @@ $100.00
                ACC_B  90 B @@ $90.00
                ACC_C  10 B @@ $9.99
            "#});
        assert!(res.is_ok());

        // This then, should be the failure boundary.
        let res = entry(indoc! {r#"
            2000-01-01  Entry 1
                ACC_A  -100 A @@ $100.00
                ACC_B  90 B @@ $90.00
                ACC_C  10 B @@ $9.98
            "#});
        assert!(res.is_err());
    }

    #[test]
    fn test_tolerance3() {
        let res = entry(indoc! {r#"
            2000-01-01  Entry1
                ACC_A  1,000 A @@ £1,000.00
                ACC_B  -1,000.00500000 A @@ £1,000.01
                ACC_C  -1,000.00060000 B @@ £500.00
                ACC_D  1,000 B @@ £500.00
                ACC_E  0.00500000 A @@ £0.01
                ACC_F  0.00060000 B @@ £0.00
            "#});
        assert!(res.is_ok());

        let res = entry(indoc! {r#"
            2000-01-01  Entry1
                ACC_A  -0.007305 A @@ £0.01
                ACC_B  -0.004305 B @@ £0.00
                ACC_C  0.004305 A @@ £0.01
                ACC_D  0.007305 B @@ £0.00
            "#});
        assert!(res.is_ok());
    }

    #[test]
    fn test_tolerance_with_unit_vals() {
        // The unit value is rounded.
        let res = entry(indoc! {r#"
            2000-01-01  Entry 1
                ACC_A  -100 A @ £1.23345
                ACC_B  £123.35
            "#});
        assert!(res.is_ok());

        // These two outside the rounding tolerance
        let res = entry(indoc! {r#"
            2000-01-01  Entry 1
                ACC_A  -100 A @ £1.23345
                ACC_B  £123.33
            "#});
        assert!(res.is_err());
        let res = entry(indoc! {r#"
            2000-01-01  Entry 1
                ACC_A  -100 A @ £1.23345
                ACC_B  £123.36
            "#});
        assert!(res.is_err());
    }

    #[test]
    fn test_tolerance_with_multiple_vals() {
        // The unit value is rounded.
        let res = entry(indoc! {r#"
            2000-01-01  Entry 1
                ACC_A  -100 A @ £1.23345 @@ $100.00
                ACC_B  £30.84 @@ $25.00
                ACC_C  £92.51 @@ $75.00
            "#});
        assert!(res.is_ok());
    }
}
