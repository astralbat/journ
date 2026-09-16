/*
 * Copyright (c) 2022-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjusted_value::AdjustedValue;
use journ_core::alloc::HerdAllocator;
use journ_core::amount::{Amount, Quantity};
use journ_core::datetime::JDateTimeRange;
use journ_core::err;
use journ_core::error::JournResult;
use journ_core::journal_entry::JournalEntry;
use journ_core::metadata::Metadata;
use journ_core::tree_id::TreeId;
use journ_core::unit::Unit;
use log::trace;
use rust_decimal::Decimal;
use rust_decimal::prelude::One;
use smallvec::SmallVec;
use std::fmt;
use std::fmt::Formatter;
use yaml_rust2::Yaml;
use yaml_rust2::yaml::Hash;

pub type AdjustmentId = TreeId;

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
    id: TreeId,
    /// If set, only apply the adjustment to the specified pool
    pool: Option<&'h str>,
    entry: &'h JournalEntry<'h>,
    metadata: SmallVec<[Metadata<'h>; 4]>,
    datetime: JDateTimeRange,
    amount_adjustments: Vec<AmountAdjustment<'h>>,
    properties: Vec<Metadata<'h>>,
    allocator: &'h HerdAllocator<'h>,
}

impl<'h> Adjustment<'h> {
    pub fn new(
        position: usize,
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

        let datetime = entry.datetime_range();
        Adjustment {
            datetime,
            entry,
            metadata,
            pool,
            amount_adjustments,
            id: entry.id().branch(position + 1),
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

    pub fn id(&self) -> &AdjustmentId {
        &self.id
    }

    pub fn datetime(&self) -> JDateTimeRange {
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

    /// There will always be at least one amount adjustment.
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
    pub fn convert_set_to_add(&mut self, adj_value: &AdjustedValue<'h>) {
        self.make_canonical();

        for amnt_adj in &mut self.amount_adjustments {
            if let AmountAdjustment::Set(set_amount) = amnt_adj {
                if set_amount.unit() == adj_value.amount().unit() {
                    let diff = *set_amount - adj_value.amount();
                    *amnt_adj = AmountAdjustment::Add(diff);
                } else if set_amount.unit() == adj_value.value().unit() {
                    let diff = *set_amount - adj_value.value();
                    *amnt_adj = AmountAdjustment::Add(diff);
                } else {
                    *amnt_adj = AmountAdjustment::Add(*set_amount);
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

    /*
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
                id: self.id.clone(),
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
    }*/

    /// Applies the adjustment to the given valued amount.
    /// The adjustment components are applied in turn to the relevant amount/valuation of the given `valued_amount`.
    ///
    /// Return an error if the adjustment would make the considered value negative.
    /// # Panics
    /// If any of the amount adjustments are `SetQuantity`. These should be converted to `AddAmount` before applying.
    pub fn apply(&self, adj_value: &mut AdjustedValue<'h>) -> JournResult<()> {
        assert_eq!(adj_value.amount().unit(), self.unit());

        trace!("Applying adjustment {} to {}", self, adj_value);

        // Make canonical first to ensure the logic proceeds correctly
        let mut canonical_self = self.clone();
        canonical_self.make_canonical();

        adj_value.set_amount(canonical_self.amount_adjustments[0].apply(adj_value.amount()));

        // Only apply the adjustment component that matches the unit of account of the adjusted value.
        canonical_self
            .amount_adjustments
            .iter()
            .find(|amnt_adj| amnt_adj.amount().unit() == adj_value.unit_of_account())
            .map(|amnt_adj| {
                let new_value = amnt_adj.apply(adj_value.value());
                if new_value < 0 {
                    return Err(err!(
                        "Adjustment {} would make value negative: {}",
                        amnt_adj,
                        new_value.format_precise()
                    ));
                }
                adj_value.set_value(new_value);
                Ok(())
            })
            .transpose()?;
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
