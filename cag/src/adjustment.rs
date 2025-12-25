/*
 * Copyright (c) 2022-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use journ_core::alloc::HerdAllocator;
use journ_core::amount::{Amount, Quantity};
use journ_core::date_and_time::JDateTimeRange;
use journ_core::error::JournResult;
use journ_core::journal_entry::{EntryId, JournalEntry};
use journ_core::metadata::Metadata;
use journ_core::reporting::table::{Cell, WrapPolicy};
use journ_core::unit::Unit;
use journ_core::valued_amount::{PostingValuation, ValuedAmount};
use log::trace;
use rust_decimal::Decimal;
use rust_decimal::prelude::One;
use smallvec::SmallVec;
use std::fmt;
use std::fmt::Formatter;
use yaml_rust::Yaml;
use yaml_rust::yaml::Hash;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct AdjustmentId<'h> {
    entry_id: EntryId<'h>,
    position: u32,
}

impl<'h> AdjustmentId<'h> {
    pub(crate) fn new(entry_id: EntryId<'h>, position: u32) -> Self {
        Self { entry_id, position }
    }
}

#[derive(Clone)]
pub enum AmountAdjustment<'h> {
    /// Add an amount to a named pool
    Add(Amount<'h>),
    Scale(Amount<'h>),
    Set(Amount<'h>),
}

impl<'h> AmountAdjustment<'h> {
    pub fn amount(&self) -> Amount<'h> {
        match self {
            AmountAdjustment::Add(amount) => *amount,
            AmountAdjustment::Scale(amount) => *amount,
            AmountAdjustment::Set(amount) => *amount,
        }
    }

    pub fn is_additive(&self) -> bool {
        matches!(self, AmountAdjustment::Add(_) | AmountAdjustment::Set(_))
    }

    /// Splits the adjustment on `qty` threshold if this is an additive adjustment.
    /// Scalar adjustments are returned as is.
    /// `qty` may be positive or negative. In any case, the returned adjustments will
    /// always sum to the original.
    ///
    /// # Panics
    /// If the adjustment is a `Set` adjustment.
    pub fn split(self, qty: Quantity) -> (Self, Self) {
        match self {
            AmountAdjustment::Add(amount) => {
                let left = amount.with_quantity(qty);
                let right = amount - qty;
                trace!(
                    "Splitting additive adjustment {} into {}, {}",
                    amount.format_precise(),
                    left.format_precise(),
                    right.format_precise()
                );
                (AmountAdjustment::Add(left), AmountAdjustment::Add(right))
            }
            AmountAdjustment::Scale(scalar) => {
                (AmountAdjustment::Scale(scalar), AmountAdjustment::Scale(scalar))
            }
            AmountAdjustment::Set(_) => panic!("Cannot split a set-to-quantity adjustment"),
        }
    }

    /// Applies the adjustment to the given amount.
    pub fn apply(&self, amount: Amount<'h>) -> Amount<'h> {
        match self {
            AmountAdjustment::Add(add_amount) => amount + *add_amount,
            AmountAdjustment::Scale(scalar) => amount * *scalar,
            AmountAdjustment::Set(_) => panic!("Cannot apply a set-to-quantity adjustment"),
        }
    }

    pub fn inverse(&self) -> Self {
        match self {
            AmountAdjustment::Add(amount) => AmountAdjustment::Add(*amount * Decimal::from(-1)),
            AmountAdjustment::Scale(scalar) => AmountAdjustment::Scale(
                scalar.unit().with_quantity(Decimal::one() / scalar.quantity()),
            ),
            AmountAdjustment::Set(_) => panic!("Cannot invert a set-to-quantity adjustment"),
        }
    }
}

impl fmt::Display for AmountAdjustment<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            AmountAdjustment::Add(amount) if amount.is_positive() => {
                write!(f, "+{}", self.amount().format_precise())
            }
            AmountAdjustment::Add(_) => write!(f, "{}", self.amount().format_precise()),
            AmountAdjustment::Scale(_) => write!(f, "*{}", self.amount().format_precise()),
            AmountAdjustment::Set(_) => write!(f, "={}", self.amount().format_precise()),
        }
    }
}

impl fmt::Debug for AmountAdjustment<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            AmountAdjustment::Add(amount) if amount.is_positive() => {
                write!(f, "+{:?}", amount)
            }
            AmountAdjustment::Add(_) => write!(f, "{:?}", self.amount()),
            AmountAdjustment::Scale(amount) => write!(f, "*{:?}", amount),
            AmountAdjustment::Set(amount) => write!(f, "={:?}", amount),
        }
    }
}

impl From<&AmountAdjustment<'_>> for Yaml {
    fn from(amount_adj: &AmountAdjustment) -> Self {
        let mut hash = Hash::new();
        match amount_adj {
            AmountAdjustment::Add(amount) if amount.is_positive() => {
                hash.insert(Yaml::String("op".to_string()), Yaml::String("+".to_string()));
                hash.insert(Yaml::String("amount".to_string()), amount.into());
            }
            AmountAdjustment::Add(amount) => {
                hash.insert(Yaml::String("op".to_string()), Yaml::String("-".to_string()));
                hash.insert(Yaml::String("amount".to_string()), (&amount.abs()).into());
            }
            AmountAdjustment::Scale(q) => {
                hash.insert(Yaml::String("op".to_string()), Yaml::String("*".to_string()));
                hash.insert(Yaml::String("quantity".to_string()), Yaml::String(q.to_string()));
            }
            AmountAdjustment::Set(q) => {
                hash.insert(Yaml::String("op".to_string()), Yaml::String("=".to_string()));
                hash.insert(Yaml::String("quantity".to_string()), Yaml::String(q.to_string()));
            }
        }
        Yaml::Hash(hash)
    }
}

/// An adjustment to a specific pool, or all pools.
///
/// There are various scenarios to be aware of here. The easiest kind of adjustment
/// to consider is the scalar one. This is easy to reason with, and can be pushed
/// to all pools without much further consideration, aside perhaps, on whether the
/// pool is empty.
///
/// The more complex adjustment to consider is the additive one. There is no easy and
/// consistent approach to apply this to all pools (CAG-Adjust). For example, consider
/// when the net balance of all pools is zero. Therefore, we disallow such adjustments
/// unless they are applied to a specific pool (CAG-AdjustPool) instead.
#[derive(Clone)]
pub struct Adjustment<'h> {
    id: AdjustmentId<'h>,
    /// If set, only apply the adjustment to the specified pool
    pool: Option<&'h str>,
    entry: &'h JournalEntry<'h>,
    metadata: SmallVec<[Metadata<'h>; 4]>,
    datetime: JDateTimeRange<'h>,
    amount_adjustments: Vec<AmountAdjustment<'h>>,
    properties: Vec<Metadata<'h>>,
    allocator: &'h HerdAllocator<'h>,
}

impl<'h> Adjustment<'h> {
    pub fn new(
        position: u32,
        entry: &'h JournalEntry<'h>,
        metadata: SmallVec<[Metadata<'h>; 4]>,
        pool: Option<&'h str>,
        amount_adjustments: Vec<AmountAdjustment<'h>>,
    ) -> Self {
        assert!(
            !amount_adjustments.is_empty(),
            "An adjustment must have at least one amount adjustment"
        );

        if amount_adjustments.iter().any(|amnt_adj| amnt_adj.is_additive()) {
            assert!(pool.is_some(), "Additive adjustments must be applied to a specific pool");
        }

        let entry_id = entry.id();
        let datetime = entry.date_and_time().datetime_range();
        Adjustment {
            datetime,
            entry,
            metadata,
            pool,
            amount_adjustments,
            id: AdjustmentId::new(entry_id, position),
            properties: vec![],
            allocator: entry.config().allocator(),
        }
    }

    /// Gets whether this identity is the same as the identity adjustment.
    pub fn is_identity(&self) -> bool {
        self.amount_adjustments.iter().all(|amnt_adj| match amnt_adj {
            AmountAdjustment::Scale(q) => q.quantity() == Decimal::one(),
            AmountAdjustment::Add(amount) => amount.is_zero(),
            AmountAdjustment::Set(_) => false,
        })
    }

    /// Gets whether all adjustments are scalar.
    pub fn is_scalar(&self) -> bool {
        self.amount_adjustments
            .iter()
            .all(|amnt_adj| matches!(amnt_adj, AmountAdjustment::Scale(_)))
    }

    pub fn id(&self) -> AdjustmentId<'h> {
        self.id
    }

    pub fn datetime(&self) -> JDateTimeRange<'h> {
        self.datetime
    }

    /// The primary unit that's being reorganised.
    pub fn unit(&self) -> &'h Unit<'h> {
        self.amount_adjustments[0].amount().unit()
    }

    pub fn pool(&self) -> Option<&'h str> {
        self.pool
    }

    pub fn with_pool(mut self, pool: &'h str) -> Self {
        self.pool = Some(pool);
        self
    }

    pub fn amount_adjustments(&self) -> &[AmountAdjustment<'h>] {
        &self.amount_adjustments
    }

    pub fn entry(&self) -> &'h JournalEntry<'h> {
        self.entry
    }

    pub fn allocator(&self) -> &'h HerdAllocator<'h> {
        self.allocator
    }

    pub fn properties(&self) -> &Vec<Metadata<'h>> {
        &self.properties
    }

    pub fn set_properties(&mut self, properties: Vec<Metadata<'h>>) {
        self.properties = properties
    }

    /// Convert `AmountAdjustment::Set` to `AmountAdjustment::Add` by calculating the difference between the set amount and the amount within the `balance` provided.
    /// If the balance does not have a value in the same unit as the set amount, the set amount is converted to an add adjustment with the whole set amount.
    pub fn convert_set_to_add(&mut self, balance: &ValuedAmount<'h>) {
        self.make_canonical();

        for amnt_adj in &mut self.amount_adjustments {
            if let AmountAdjustment::Set(set_amount) = amnt_adj {
                match balance.value_in(set_amount.unit()) {
                    Some(bal_amount) => {
                        let diff = *set_amount - bal_amount;
                        *amnt_adj = AmountAdjustment::Add(diff);
                    }
                    None => {
                        *amnt_adj = AmountAdjustment::Add(*set_amount);
                    }
                }
            }
        }
    }

    pub(crate) fn set_amount_adjustments(&mut self, amount_adjs: Vec<AmountAdjustment<'h>>) {
        self.amount_adjustments = amount_adjs;
    }

    /// Converts the adjustment to canonical form by combining adjustments that have the same unit.
    pub fn make_canonical(&mut self) {
        trace!("Making adjustment canonical {}", self);
        'restart: loop {
            for i in 0..self.amount_adjustments.len() {
                for j in i + 1..self.amount_adjustments.len() {
                    if self.amount_adjustments[i].amount().unit()
                        == self.amount_adjustments[j].amount().unit()
                    {
                        self.amount_adjustments[i] = match self.amount_adjustments[j] {
                            AmountAdjustment::Add(amount) => AmountAdjustment::Add(
                                self.amount_adjustments[i].amount().add_precise(amount).unwrap(),
                            ),
                            AmountAdjustment::Scale(amount) => AmountAdjustment::Scale(
                                self.amount_adjustments[i].amount() * amount,
                            ),
                            AmountAdjustment::Set(amount) => AmountAdjustment::Set(amount),
                        };
                        self.amount_adjustments.remove(j);
                        continue 'restart;
                    }
                }
            }
            break;
        }
        trace!("Canonical adjustment is {}", self);
    }

    /// Splits the adjustment into two parts.
    /// The `amount_percent` can be more than 1, or less than 0.
    pub fn split(self, amount_percent: Decimal) -> (Self, Self) {
        let (l_adjs, r_adjs) = self
            .amount_adjustments
            .into_iter()
            .map(|amnt_adj| match amnt_adj {
                AmountAdjustment::Add(amount) => {
                    let (l_amount, r_amount) = amount.split_percent(amount_percent);
                    (AmountAdjustment::Add(l_amount), AmountAdjustment::Add(r_amount))
                }
                AmountAdjustment::Scale(scalar) => {
                    (AmountAdjustment::Scale(scalar), AmountAdjustment::Scale(scalar))
                }
                AmountAdjustment::Set(_) => panic!("Cannot split a set-to-quantity adjustment"),
            })
            .unzip();
        (
            Adjustment {
                id: self.id,
                entry: self.entry,
                metadata: self.metadata.clone(),
                pool: self.pool,
                datetime: self.datetime,
                amount_adjustments: l_adjs,
                properties: self.properties.clone(),
                allocator: self.allocator,
            },
            Adjustment {
                id: self.id,
                entry: self.entry,
                metadata: self.metadata,
                pool: self.pool,
                datetime: self.datetime,
                amount_adjustments: r_adjs,
                properties: self.properties.clone(),
                allocator: self.allocator,
            },
        )
    }

    /// Applies the adjustment to the given valued amount.
    /// The adjustment components are applied in turn to the relevant amount/valuation of the given `valued_amount`.
    ///
    /// Return an error if the adjustment would make the considered value negative.
    /// # Panics
    /// If any of the amount adjustments are `SetQuantity`. These should be converted to `AddAmount` before applying.
    pub fn apply(&self, valued_amount: &mut ValuedAmount<'h>) -> JournResult<()> {
        assert_eq!(valued_amount.unit(), self.unit());
        debug_assert!(valued_amount.posting_valuations().all(PostingValuation::is_total));

        trace!("Applying adjustment {} to {}", self, valued_amount);

        // Make canonical first to ensure the logic proceeds correctly
        let mut canonical_self = self.clone();
        canonical_self.make_canonical();

        let originally_positive = valued_amount.amount().is_positive();
        valued_amount
            .set_amount(canonical_self.amount_adjustments[0].apply(valued_amount.amount()));

        for amnt_adj in &canonical_self.amount_adjustments[1..] {
            let mut existing_amount =
                valued_amount.value_in(amnt_adj.amount().unit()).unwrap_or(Amount::nil());
            // If the sign got flipped during the primary amount adjustment, the value_in() function is now going to return the inverse
            // of what we want. So flip it back.
            if valued_amount.amount() != 0
                && valued_amount.amount().is_positive() != originally_positive
            {
                existing_amount = -existing_amount;
            }

            let new_amount = amnt_adj.apply(existing_amount);

            // The signs need to match. It does not make sense to have an adjustment that causes the
            // valued amount to have conflicting signs. E.g. "10 Units @@ -$5" implies that each unit
            // has a negative value which, even if it makes sense in some situations, cannot be modelled currently.
            //if new_amount.is_positive() != valued_amount.amount().is_positive() {
            //    return Err(err!("Adjustment would change the sign of the considered value")
            //        .with_source(err!("{} on {}", amnt_adj, existing_amount.format_precise())));
            //}
            valued_amount.set_valuation(PostingValuation::new_total(new_amount, false));
        }
        Ok(())
    }
}

impl PartialEq for Adjustment<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Adjustment<'_> {}

impl PartialOrd for Adjustment<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Adjustment<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl fmt::Display for Adjustment<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.amount_adjustments[0])?;
        for adj in &self.amount_adjustments[1..] {
            write!(f, ", {}", adj)?;
        }
        Ok(())
    }
}

impl fmt::Debug for Adjustment<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.amount_adjustments[0])?;
        for adj in &self.amount_adjustments[1..] {
            write!(f, ", {:?}", adj)?;
        }
        Ok(())
    }
}

impl<'a, 'h, 'b> From<&'a Adjustment<'h>> for Cell<'b>
where
    'b: 'a,
{
    fn from(value: &'a Adjustment<'h>) -> Self {
        let mut cells = vec![];
        cells.push(match &value.amount_adjustments[0] {
            AmountAdjustment::Add(amount) => Cell::from(*amount),
            AmountAdjustment::Scale(amount) => Cell::from(*amount),
            AmountAdjustment::Set(amount) => Cell::from(*amount),
        });
        for adj in &value.amount_adjustments[1..] {
            cells.push(Cell::from(&","));
            cells.push(match adj {
                // The array wrapper is necessary to ensure these cells line up vertically with valuations in ValuedAmounts.
                AmountAdjustment::Add(amount) => Cell::from([*amount]),
                AmountAdjustment::Scale(amount) => Cell::from([*amount]),
                AmountAdjustment::Set(amount) => Cell::from([*amount]),
            });
        }
        let mut cell: Cell = cells.into();
        cell.set_wrap_policy(WrapPolicy::With(|cell| {
            for (i, child) in cell.iter().enumerate().take(cell.len() - 1) {
                if *child == "," {
                    return WrapPolicy::Position(i + 1);
                }
            }
            WrapPolicy::Never
        }));
        cell
    }
}

impl From<&Adjustment<'_>> for Yaml {
    fn from(adj: &Adjustment) -> Self {
        let mut hash = Hash::new();
        hash.insert(Yaml::String("datetime".to_string()), Yaml::String(adj.datetime().to_string()));

        let adjustments = adj.amount_adjustments.iter().map(Yaml::from).collect();
        hash.insert(Yaml::String("adjustments".to_string()), Yaml::Array(adjustments));
        Yaml::Hash(hash)
    }
}
