/*
 * Copyright (c) 2023-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjustment::{Adjustment, AmountAdjustment};
use crate::cgt_configuration::MatchMethod;
use crate::deal::{Deal, DealId};
use crate::deal_group::DealGroup;
use crate::deal_holding::DealHolding::*;
use crate::pool::PoolBalance;
use chrono::DateTime;
use chrono_tz::Tz;
use journ_core::alloc::HerdAllocator;
use journ_core::amount::{Amount, Quantity};
use journ_core::configuration::Configuration;
use journ_core::date_and_time::{JDateTime, JDateTimeRange};
use journ_core::err;
use journ_core::error::{JournError, JournResult};
use journ_core::ext::NumExt;
use journ_core::unit::{RoundingStrategy, Unit};
use journ_core::valued_amount::{Valuation, ValuedAmount};
use journ_core::valuer::{SystemValuer, Valuer};
use linked_hash_set::LinkedHashSet;
use log::trace;
use std::collections::VecDeque;
use std::fmt::Debug;
use std::ops::Add;
use std::{fmt, iter, mem};
use yaml_rust::Yaml;

/// A unifying type for the different kinds of deal holdings (containers). A DealHolding is a tree structure with
/// [Deal] types at its leaves, with the root of the tree being managed by a [`Pool`].
/// Thus, separate DealHoldings are required for each asset unit.
///
/// A DealHolding can never be empty; it will always have at least one deal, which itself may be zero.
#[derive(Clone, PartialEq, Eq)]
pub enum DealHolding<'h> {
    //Single(Deal<'h>),
    Group(DealGroup<'h>),
    Sequence(SequenceDealHolding<'h>),
    Average(AverageDealHolding<'h>),
}

impl<'h> DealHolding<'h> {
    /// Gets the adjusted balance of the holding consisting of the total amount and its value in a particular
    /// tax unit.
    pub fn total(&self) -> &PoolBalance<'h> {
        match &self {
            //Single(deal) => deal.total(),
            Group(group) => group.total(),
            Sequence(sh) => sh.total(),
            Average(ah) => ah.total(),
        }
    }

    /*
    fn abs_total(&self) -> Amount<'h> {
        match self {
            //Single(deal) => deal.total().amount().abs(),
            Group(group) => group.total().amount().abs(),
            Sequence(seq) => seq.abs_total(),
            Average(avg) => avg.total().amount().abs(),
        }
    }*/

    /// Gets the deal totals without expenses.
    pub fn total_before_expenses(&self) -> ValuedAmount<'h> {
        match self {
            //Single(deal) => deal.total_before_expenses(),
            Group(group) => group.total_before_expenses().clone(),
            Sequence(seq) => seq.total_before_expenses(),
            Average(avg) => avg.valued_amount.clone(),
        }
    }

    pub fn unit(&self) -> &'h Unit<'h> {
        match self {
            //Single(deal) => deal.unit(),
            Group(group) => group.unit(),
            Sequence(seq) => seq.sequence.front().unwrap().unit(),
            Average(avg) => avg.valued_amount.amount().unit(),
        }
    }

    pub fn datetime(&self) -> JDateTimeRange<'h> {
        match self {
            //Single(deal) => deal.datetime(),
            Group(group) => group.datetime(),
            Sequence(seq) => seq.datetime(),
            Average(avg) => avg.datetime,
        }
    }

    /*
    pub fn first_deal<'a>(&'a self) -> &'a Deal<'h> {
        let root = self.root_parent();
        match root {
            Single(deal) => deal,
            Sequence(seq) => seq.sequence.front().unwrap().first_deal(),
            _ => panic!("Expect root parent to be a single deal"),
        }
    }*/

    /*
    pub fn first_deal_mut<'a>(&'a mut self) -> Option<&'a mut Deal<'h>> {
        match self {
            Single(deal) => Some(deal),
            Group(_group) => self.root_parent().first_deal_mut(),
            Sequence(seq) => seq.sequence.front_mut().unwrap().first_deal_mut(),
            Average(_) => self.root_parent().first_deal_mut(),
        }
    }*/

    pub fn last_deal<'a>(&'a self) -> Option<&'a Deal<'h>> {
        match self {
            //Single(deal) => Some(deal),
            Group(group) => Some(group.last_deal()),
            Sequence(seq) => seq.sequence.back().unwrap().last_deal(),
            Average(_) => None,
        }
    }

    /*
    pub fn deal(&self, i: usize) -> Option<&Deal<'h>> {
        match self {
            Single(deal) => {
                if i == 0 {
                    Some(deal)
                } else {
                    None
                }
            }
            Group(group) => panic!("Cannot get deal from a deal group"),
            Sequence(seq) => seq.sequence.get(i).and_then(|dh| dh.deal(i)),
            Average(_) => panic!("Cannot get deal from an average holding"),
        }
    }*/

    pub fn deal_iter(&self) -> Box<dyn Iterator<Item = &Deal<'h>> + '_> {
        match self {
            //Single(deal) => Box::new(iter::once(deal)),
            Group(group) => group.deal_iter(),
            Sequence(seq) => seq.deal_iter(),
            Average(_avg) => Box::new(iter::empty()),
        }
    }

    /*
    pub fn deal_iter_mut(&mut self) -> Box<dyn Iterator<Item = &mut Deal<'h>> + '_> {
        match self {
            Single(deal) => Box::new(std::iter::once(deal)),
            Group(group) => Box::new(std::iter::empty()),
            Sequence(seq) => Box::new(seq.sequence.iter_mut().flat_map(|dh| dh.deal_iter_mut())),
            Average(_) => Box::new(std::iter::empty()),
        }
    }*/

    /// Gets the origin of the deals within the holding. The origin is not necessarily an original deal from
    /// a journal entry, only the set of split parents of the deals contained within.
    pub fn split_parent(&self) -> Option<&DealGroup<'h>> {
        match self {
            Group(group) => group.split_parent(),
            _ => None,
        }
    }

    pub fn root_parent(&self) -> Option<&DealGroup<'h>> {
        match self.split_parent() {
            Some(parent) => Some(parent.root_parent()),
            None => None,
        }
    }

    /// Gets the expenses of deals within the holding. Note that this will
    /// always return `None` for average holdings.
    pub fn expenses(&self) -> ValuedAmount<'h> {
        match self {
            //Single(deal) => deal.expenses().clone(),
            Group(group) => group.expenses().clone(),
            Sequence(seq) => seq.expenses(),
            Average(avg) => avg.expenses().clone(),
        }
    }

    /*
    pub fn adjustments(&self) -> impl Iterator<Item = &Adjustment<'h>> + '_ {
        match self {
            //Single(deal) => deal.adjustments(),
            Group(group) => group.adjustments(),
            Sequence(seq) => seq.adjustments(),
            Average(avg) => &avg.adjustments,
        }
    }*/

    /// Gets the total explicit taxable gain of all the deals in the holding, if any of them
    /// has an explicitly set taxable gain.
    pub fn explicit_taxable_gain(&self) -> Option<ValuedAmount<'h>> {
        let explicit_gain: Option<ValuedAmount> = self
            .deal_iter()
            .map(|d| d.taxable_gain().cloned().unwrap_or(ValuedAmount::nil()))
            .sum();
        match explicit_gain {
            Some(va) if !va.is_nil() => Some(va),
            _ => None,
        }
    }

    pub fn set_unit_of_account(
        &mut self,
        uoa: &'h Unit<'h>,
        config_and_value_date: Option<(&Configuration<'h>, DateTime<Tz>)>,
    ) -> Result<(), JournError> {
        match self {
            /*Single(deal) => match config_and_value_date {
                Some((config, date)) => {
                    deal.set_unit_of_account_with_value_date(uoa, config, date)?
                }
                None => deal.set_unit_of_account(uoa)?,
            },*/
            Group(group) => group.set_unit_of_account(uoa, config_and_value_date)?,
            Sequence(seq) => seq.set_unit_of_account(uoa, config_and_value_date)?,
            Average(avg) => avg.set_unit_of_account(uoa, config_and_value_date)?,
        }
        Ok(())
    }

    /*
    pub fn push_deal(&mut self, deal: Deal<'h>) {
        let holding = mem::replace(self, Single(Deal::zero(deal.unit(), deal.entry())));

        *self = match holding {
            Single(d) => {
                let allocator = d.allocator();
                let split_parent = d.split_parent().cloned();
                Sequence(SequenceDealHolding::from_iterator(
                    [Single(d), Single(deal)],
                    split_parent.map(|p| Box::new_in(p, allocator)),
                    allocator,
                ))
            }
            Group(g) => {
                let allocator = g.allocator();
                let split_parent = g.split_parent().cloned();
                Sequence(SequenceDealHolding::from_iterator(
                    [Group(g), Single(deal)],
                    split_parent.map(|p| Box::new_in(p, allocator)),
                    allocator,
                ))
            }
            Sequence(mut seq) => {
                seq.insert(deal);
                Sequence(seq)
            }
            Average(avg) => {
                let allocator = avg.valued_amount.allocator().unwrap();
                let split_parent = avg.split_parent().cloned();
                Sequence(SequenceDealHolding::from_iterator(
                    [Average(avg), Single(deal)],
                    split_parent.map(|p| Box::new_in(p, allocator)),
                    allocator,
                ))
            }
        };
    }*/

    /// Pushes a deal on to the holding.
    pub fn push_group(&mut self, group: DealGroup<'h>) {
        let holding = mem::replace(
            self,
            Average(AverageDealHolding::from(&Deal::zero(
                group.unit(),
                group.entries().next().unwrap(),
            ))),
        );

        *self = match holding {
            /*Single(d) => {
                Sequence(SequenceDealHolding::from_iterator([Single(d), Group(group)], allocator))
            }*/
            Group(g) => {
                let allocator = g.allocator();
                Sequence(SequenceDealHolding::from_iterator([Group(g), Group(group)], allocator))
            }
            Sequence(mut seq) => {
                seq.insert(group);
                Sequence(seq)
            }
            Average(avg) => {
                let allocator = avg.valued_amount.allocator().unwrap();
                Sequence(SequenceDealHolding::from_iterator(
                    [Average(avg), Group(group)],
                    allocator,
                ))
            }
        }
    }

    /// Splits the holding on the specified amount.
    /// `split_amount` must be in the same pos/neg sign as the holding's balance to be successful.
    ///
    /// On success the holding will be split into the matched and remaining portions where
    /// the matched part will only contain a single deal (if the holding is or contains a sequence).
    /// This should be called repeatedly until either the `split_amount` has been reached, or an
    /// `Err` is returned.
    pub fn split_max(
        self,
        split_amount: Quantity,
        method: MatchMethod,
    ) -> Result<(DealHolding<'h>, Option<DealHolding<'h>>), DealHolding<'h>> {
        match self {
            /*Single(deal) => deal
            .split_max(split_amount)
            .map(|(l, r)| (Single(l), r.map(Single)))
            .map_err(Single),*/
            Group(group) => {
                group.split_max(split_amount).map(|(l, r)| (Group(l), r.map(Group))).map_err(Group)
            }
            Sequence(seq_holding) => seq_holding.split_max(split_amount, method).map_err(Sequence),
            Average(avg_holding) => {
                avg_holding
                    .split_max(split_amount)
                    .map(|(l, r)| (Average(l), r.map(Average)))
                    .map_err(Average)
                /*
                match method {
                MatchMethod::Average => avg_holding.split_max(split_amount),
                MatchMethod::Fifo | MatchMethod::Lifo => {
                    let allocator = avg_holding.valued_amount.allocator().unwrap();
                    let split_parent = avg_holding.split_parent().cloned();
                    let seq_holding = SequenceDealHolding::from_iterator(
                        [Average(avg_holding)],
                        split_parent.map(|p| Box::new_in(p, allocator)),
                        allocator,
                    );
                    Self::split_max(Sequence(seq_holding), split_amount, method)
                }*/
            }
        }
    }

    /// Removes the deal sub-holding with the given `deal_id` from the holding.
    /// The deal itself cannot be returned as it may have been adjusted, in which case it will still be wrapped
    /// in the adjustment.
    ///
    /// Returns the portion of the holding extracted, along with the remainder of the holding, if any.
    /// # Panics
    /// If the holding is or contains an average holding.
    pub fn extract(self, deal_id: DealId<'h>) -> Result<(DealGroup<'h>, Option<Self>), Self> {
        match self {
            /*Single(deal) => Err(Single(deal)),*/
            Group(group) => {
                if group.id() == deal_id {
                    Ok((group, None))
                } else {
                    Err(Group(group))
                }
            }
            Sequence(seq) => {
                let (extracted, new_seq) = seq.extract(deal_id)?;
                match new_seq {
                    Some(seq) => Ok((extracted, Some(Sequence(seq)))),
                    None => Ok((extracted, None)),
                }
            }
            Average(_) => {
                panic!("Cannot convert an average holding back into dealing events")
            }
        }
    }

    pub fn add_adjustment(&mut self, adj: Adjustment<'h>) -> JournResult<()> {
        match self {
            //Single(deal) => deal.add_adjustment(adj),
            Group(group) => group.add_adjustment(adj),
            Sequence(seq) => seq.add_adjustment(adj),
            Average(avg) => avg.add_adjustment(adj),
        }
    }

    /*
    pub fn extract_first(self) -> (DealHolding<'h>, Option<DealHolding<'h>>) {
        match self {
            Single(deal) => (Single(deal), None),
            Sequence(seq) => {
                let (extracted, new_seq) = seq.extract_first();
                (extracted, new_seq.map(Sequence))
            }
            Adjusted(adj) => {
                let (extracted, rem_adj) = adj.extract_first();
                (extracted, rem_adj.map(Adjusted))
            }
            Average(avg) => (Average(avg), None),
        }
    }*/

    /*
    /// Removes all inner holding leaves in ascending date order.
    pub fn extract_all(self) -> impl Iterator<Item = DealHolding<'h>> {
        let mut remaining_dh = Some(self);
        std::iter::from_fn(move || match remaining_dh.take() {
            Some(dh) => {
                let (extracted, remainder) = dh.extract_first();
                remaining_dh = remainder;
                Some(extracted)
            }
            None => None,
        })
    }*/

    pub fn description(&self) -> LinkedHashSet<&'h str> {
        match self {
            /*Single(deal) => {
                let mut desc = LinkedHashSet::new();
                desc.insert(deal.entry().description().trim_start());
                desc
            }*/
            Group(group) => group.entry_description(),
            Sequence(seq) => seq.sequence.front().unwrap().description(),
            Average(_) => LinkedHashSet::new(),
        }
    }

    pub fn metadata(&self, tag: &str) -> LinkedHashSet<String> {
        match self {
            //Single(deal) => deal.entry().metadata_tag_values(tag),
            Group(group) => group.metadata(tag),
            Sequence(seq) => {
                let mut tag_vals = LinkedHashSet::new();
                for dh in &seq.sequence {
                    for val in dh.metadata(tag) {
                        tag_vals.insert_if_absent(val);
                    }
                }
                tag_vals
            }
            Average(_avg) => LinkedHashSet::new(),
        }
    }

    pub fn allocator(&self) -> &'h HerdAllocator<'h> {
        match self {
            //Single(deal) => deal.allocator(),
            Group(group) => group.allocator(),
            Sequence(seq) => seq.sequence.allocator(),
            Average(avg) => avg.valued_amount.allocator().unwrap(),
        }
    }
}

/*
impl<'h> TryFrom<DealHolding<'h>> for Vec<DealingEvent<'h>> {
    type Error = JournError;

    fn try_from(value: DealHolding<'h>) -> Result<Self, Self::Error> {
        match value {
            Single(deal) => Ok(vec![DealingEvent::Deal(deal)]),
            Sequence(seq_holding) => seq_holding.try_into(),
            Average(_) => {
                Err(err!("Cannot convert an average holding back into dealing events"))
            }
            Adjusted(adj_holding) => adj_holding.try_into(),
        }
    }
}*/

impl Debug for DealHolding<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            //Single(deal) => write!(f, "{}", deal),
            Group(group) => write!(f, "{:?}", group),
            Sequence(seq) => write!(f, "{:?}", seq),
            Average(avg) => write!(f, "{:?}", avg),
        }
    }
}

impl<'h> Add for DealHolding<'h> {
    type Output = Result<DealHolding<'h>, (DealHolding<'h>, DealHolding<'h>)>;

    fn add(self, rhs: DealHolding<'h>) -> Self::Output {
        // Both need to be sign compatible to add. This is usually used to indicate
        // we're adding a long position to a short position or vice versa.
        // Avoiding this means we're not 'destroying' how much was acquired or disposed of in total.
        if self.total().amount().is_negative() != rhs.total().amount().is_negative() {
            return Err((self, rhs));
        }

        let combined =
            Sequence(SequenceDealHolding::from_iterator([self.clone(), rhs], self.allocator()));
        Ok(combined)
    }
}

impl From<&DealHolding<'_>> for Yaml {
    fn from(value: &DealHolding<'_>) -> Self {
        let mut map = yaml_rust::yaml::Hash::new();
        let expenses = value.expenses();
        let total = value.total().valued_amount().clone();
        let total_before_expenses = value.total_before_expenses();

        map.insert(Yaml::String("total".to_string()), (&total).into());
        map.insert(Yaml::String("expenses".to_string()), (&expenses).into());
        map.insert(
            Yaml::String("total_before_expenses".to_string()),
            (&total_before_expenses).into(),
        );
        map.insert(Yaml::String("datetime".to_string()), value.datetime().into());
        map.insert(
            Yaml::String("remainder".to_string()),
            Yaml::Boolean(match value.root_parent() {
                Some(parent) => parent.total().amount() != value.total().amount(),
                None => false,
            }),
        );
        Yaml::Hash(map)
    }
}

/// A deal holding that displays absolute values
pub struct AbsDealHolding<'h, 'd>(pub &'d DealHolding<'h>);

impl From<AbsDealHolding<'_, '_>> for Yaml {
    fn from(value: AbsDealHolding<'_, '_>) -> Self {
        let mut map = yaml_rust::yaml::Hash::new();
        let expenses = value.0.expenses();
        let total = value.0.total().valued_amount().clone();
        let unadjusted_total = value.0.total_before_expenses();

        map.insert(Yaml::String("adjusted_total".to_string()), (&total.abs()).into());
        map.insert(Yaml::String("expenses".to_string()), (&expenses.abs()).into());
        map.insert(Yaml::String("unadjusted_total".to_string()), (&unadjusted_total.abs()).into());
        Yaml::Hash(map)
    }
}

/// A sequence of deal holdings. The sequence will never be empty.
#[derive(Clone, PartialEq, Eq)]
pub struct SequenceDealHolding<'h> {
    sequence: VecDeque<DealHolding<'h>, &'h HerdAllocator<'h>>,
    balance: PoolBalance<'h>,
    oldest: JDateTime<'h>,
    newest: JDateTime<'h>,
}

impl<'h> SequenceDealHolding<'h> {
    pub fn new(sequence: VecDeque<DealHolding<'h>, &'h HerdAllocator<'h>>) -> Self {
        assert!(!sequence.is_empty());

        let balance = Self::calc_balance(&sequence);
        let oldest = sequence.iter().map(|dh| dh.datetime().start()).min().unwrap();
        let newest = sequence.iter().map(|dh| dh.datetime().end()).max().unwrap();
        Self { sequence, balance, oldest, newest }
    }

    pub fn from_iterator<I: IntoIterator<Item = DealHolding<'h>>>(
        seq: I,
        allocator: &'h HerdAllocator<'h>,
    ) -> Self {
        let mut vd = VecDeque::with_capacity_in(0, allocator);
        vd.extend(seq);

        let balance = Self::calc_balance(&vd);
        let oldest = vd.iter().map(|dh| dh.datetime().start()).min().unwrap();
        let newest = vd.iter().map(|dh| dh.datetime().end()).max().unwrap();
        let sdh = Self { sequence: vd, balance, oldest, newest };
        assert!(!sdh.sequence.is_empty());
        sdh
    }

    pub fn allocator(&self) -> &'h HerdAllocator<'h> {
        self.sequence.allocator()
    }

    pub fn first(&self) -> &DealHolding<'h> {
        self.sequence.front().unwrap()
    }

    pub fn iter(&self) -> impl Iterator<Item = &DealHolding<'h>> {
        self.sequence.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut DealHolding<'h>> {
        self.sequence.iter_mut()
    }

    pub fn datetime(&self) -> JDateTimeRange<'h> {
        // The sequence won't necessarily be in date order due to the way rule logic
        // may operate.
        JDateTimeRange::new(self.oldest, Some(self.newest))
    }

    pub fn total(&self) -> &PoolBalance<'h> {
        &self.balance
    }

    pub fn total_before_expenses(&self) -> ValuedAmount<'h> {
        self.sequence
            .iter()
            .map(|dh| dh.total_before_expenses())
            .sum::<Option<ValuedAmount<'h>>>()
            .unwrap()
    }

    pub fn expenses(&self) -> ValuedAmount<'h> {
        self.sequence.iter().map(|dh| dh.expenses()).sum::<Option<ValuedAmount<'h>>>().unwrap()
    }

    /*
    pub fn adjustments(&self) -> impl Iterator<Item = &Adjustment<'h>> + '_ {
        self.sequence.iter().flat_map(|dh| dh.adjustments())
    }*/

    /*
    fn abs_total(&self) -> Amount<'h> {
        let mut total = self.sequence.iter().next().unwrap().unit().with_quantity(0);
        for dh in &self.sequence {
            total += dh.abs_total();
        }
        total
    }*/

    fn calc_balance(
        sequence: &VecDeque<DealHolding<'h>, &'h HerdAllocator<'h>>,
    ) -> PoolBalance<'h> {
        let mut total = sequence.iter().next().unwrap().total().clone();
        for bal in sequence.iter().skip(1).map(|dh| dh.total()) {
            total += &bal;
        }
        total
    }

    pub fn set_unit_of_account(
        &mut self,
        uoa: &'h Unit<'h>,
        config_and_value_date: Option<(&Configuration<'h>, DateTime<Tz>)>,
    ) -> Result<(), JournError> {
        for dh in self.sequence.iter_mut() {
            dh.set_unit_of_account(uoa, config_and_value_date)?
        }
        self.balance = Self::calc_balance(&self.sequence);
        Ok(())
    }

    pub fn deal_iter(&self) -> Box<dyn Iterator<Item = &Deal<'h>> + '_> {
        Box::new(self.sequence.iter().flat_map(|dh| dh.deal_iter()))
    }

    pub fn split_max(
        mut self,
        amount: Quantity,
        method: MatchMethod,
    ) -> Result<(DealHolding<'h>, Option<DealHolding<'h>>), Self> {
        if !amount.is_sign_compatible(self.balance.amount().quantity()) {
            return Err(self);
        }
        if amount.is_zero() {
            return Err(self);
        }

        let split_parent = self.clone();

        // If we need to split using the average method, we convert the sequence to an average holding, perform the split and set
        // the split parent to be this holding, allowing the returned average holdings to be able to link back to the original deals.
        if method == MatchMethod::Average {
            // Commented out to make consistent with Deal, Avg holding.
            // Whole amount taken. Return early to avoid average conversion.
            //if amount.abs() >= self.balance.amount().abs().quantity() {
            //    return Ok((Sequence(self), None));
            //}

            return match Average(AverageDealHolding::from(self)).split_max(amount, method) {
                Ok((left, right)) => {
                    /*
                    match_then!(&mut left, Average(avg) => avg.split_parent = Some(Box::new_in(Sequence(split_parent.clone()), allocator)));
                    if let Some(right) = &mut right {
                        match_then!(right, Average(avg) => avg.split_parent = Some(Box::new_in(Sequence(split_parent), allocator)));
                    }*/
                    Ok((left, right))
                }
                Err(_avg) => Err(split_parent),
            };
        }

        //let mut split_amount_rem = amount;

        let reverse = matches!(method, MatchMethod::Lifo);
        let mut i = if reverse { self.sequence.len() - 1 } else { 0 };
        while let Some(target_part) = self.sequence.remove(i) {
            match target_part.split_max(amount, method) {
                Ok((left, right)) => {
                    //split_amount_rem -= left.total().amount().quantity();
                    if let Some(right) = right {
                        self.sequence.insert(i, right);
                    }
                    return if self.sequence.is_empty() {
                        Ok((left, None))
                    } else {
                        self.balance = Self::calc_balance(&self.sequence);
                        Ok((left, Some(Sequence(self))))
                    };
                }
                Err(holding) => {
                    self.sequence.insert(i, holding);
                    if reverse {
                        if i == 0 {
                            break;
                        }
                        i -= 1;
                    } else {
                        if i == self.sequence.len() - 1 {
                            break;
                        }
                        i += 1;
                    }
                }
            }
            //if split_amount_rem.is_zero() {
            //    break;
            //}
        }

        Err(self)
    }

    /// Adds an adjustment to a sequence of holdings. The way this is done is by breaking
    /// up the `adj` in to a series of 'mini adjustments' that are applied to each holding
    /// in the sequence.
    /// In doing this, the nature of FIFO/LIFO holdings is preserved so that the newly adjusted
    /// holdings are still eligible for matching. The alternative would be to convert the entire
    /// sequence in to some kind of Average holding which may not be desired.
    ///
    /// The mini adjustments are calculated proportionally, and in such a way that their sum total will equal exactly
    /// the original adjustment.
    /// Furthermore, the mini adjustments are rounded so that when they are matched, there are no odd
    /// fractions lying about. This could potentially be changed in the future to depend on the `round_deals`
    /// parameter if there is a case for not rounding.
    pub fn add_adjustment(&mut self, orig_adj: Adjustment<'h>) -> JournResult<()> {
        // First make sure that the whole adj can be applied to this holding. We expect any validation
        // would have already occurred.
        assert!(
            orig_adj.apply(&mut self.total().valued_amount().clone()).is_ok(),
            "Adjustment cannot be applied to holding"
        );

        let self_total = self.total().clone();
        let seq_len = self.sequence.len();
        let bal_before = self.balance.clone();
        // Keep track of a remainder after applying each mini adjustment. We'll apply this to the last element.
        let mut rem_adj = orig_adj.clone();
        let mut rem_amount_adjustments = rem_adj.amount_adjustments().iter().cloned().collect();
        for (i, dh) in self.sequence.iter_mut().enumerate() {
            if i == seq_len - 1 {
                rem_adj.set_amount_adjustments(rem_amount_adjustments);
                dh.add_adjustment(rem_adj)?;
                break;
            } else {
                // Create the mini adjustment for this holding
                let mut mini_adj_amount_adjustments = vec![];
                let mut sign_positive = true;
                for (j, amount_adj) in orig_adj.amount_adjustments().iter().cloned().enumerate() {
                    if matches!(amount_adj, AmountAdjustment::Scale(_)) {
                        mini_adj_amount_adjustments.push(amount_adj.clone());
                        continue;
                    }

                    // Calculate amount adjustments only for those in common
                    if let (Some(dh_amount_total), Some(amount_total)) = (
                        dh.total().valued_amount().value_in(amount_adj.amount().unit()),
                        self_total.valued_amount().value_in(amount_adj.amount().unit()),
                    ) {
                        let adj_amount_unrounded =
                            amount_adj.amount() * (dh_amount_total / amount_total);

                        // The first amount adjustment is the primary amount. We can determine the sign of the
                        // holding. This is important as subsequent adjustments need to match on the same sign which is an implied
                        // requirement for ValuedAmounts. In certain edge cases, the rounding might change the sign, so here we try
                        // different rounding strategies to ensure the sign is maintained.
                        let adj_amount_rounded = adj_amount_unrounded.rounded();
                        let mut adj_amount = if j == 0 {
                            sign_positive = (dh_amount_total + adj_amount_rounded).is_positive();
                            adj_amount_rounded
                        } else {
                            if (dh_amount_total + adj_amount_rounded).is_positive() != sign_positive
                            {
                                if (dh_amount_total
                                    + adj_amount_unrounded
                                        .rounded_with_strategy(RoundingStrategy::AlwaysUp))
                                .is_positive()
                                    != sign_positive
                                {
                                    adj_amount_unrounded
                                        .rounded_with_strategy(RoundingStrategy::AlwaysDown)
                                } else {
                                    adj_amount_unrounded
                                        .rounded_with_strategy(RoundingStrategy::AlwaysUp)
                                }
                            } else {
                                adj_amount_rounded
                            }
                        };
                        // We force some imprecision here to _try_ to give wiggle room so that the mini adjustments
                        // can be added precisely with `Amount::add_precise`. If we don't do this, we may fail on the post-condition assertion.
                        if adj_amount.scale() > 18 {
                            adj_amount = adj_amount.rounded_dec_places(18);
                        }

                        let (left, _right) = amount_adj.split(adj_amount.quantity());
                        mini_adj_amount_adjustments.push(left.clone());
                        rem_amount_adjustments.push(left.inverse());
                    }
                }
                let mut mini_adj = orig_adj.clone();
                mini_adj.set_amount_adjustments(mini_adj_amount_adjustments);
                // Apply the mini adjustment to the holding
                dh.add_adjustment(mini_adj)?;
            };

            self.oldest = self.oldest.min(dh.datetime().start());
            self.newest = self.newest.max(dh.datetime().end());
        }
        self.balance = Self::calc_balance(&self.sequence);

        // Check the post-condition; the sum total of all mini-adjustments (the balance) should equal the original adjustment.
        let mut adjusted_va = bal_before.into_valued_amount();
        orig_adj.apply(&mut adjusted_va)?;
        assert_eq!(
            &adjusted_va,
            self.balance.valued_amount(),
            "The sum total of all mini adjustments should equal the original adjustment"
        );

        Ok(())

        /*

        // Use the abs total. This ensures accuracy when splitting the adjustment.
        let mut curr_abs_total = self.abs_total().quantity();
        let len = self.sequence.len() as i64;
        for dh in &mut self.sequence {
            let (adj_l, adj_r) = if curr_abs_total.is_zero() {
                adj.split(Decimal::one() / Decimal::new(len, 6))
            } else {
                adj.split(dh.abs_total().quantity() / curr_abs_total)
            };
            debug!("Adj L: {:?},\nAdj R: {:?}", adj_l, adj_r);
            adj = adj_r;
            curr_abs_total -= dh.abs_total().quantity();

            match dh {
                //Single(deal) => deal.add_adjustment(adj_l.clone())?,
                Group(group) => group.add_adjustment(adj_l)?,
                Sequence(seq) => seq.add_adjustment(adj_l)?,
                Average(avg) => avg.add_adjustment(adj_l)?,
            };
            self.oldest = self.oldest.min(dh.datetime().start());
            self.newest = self.newest.max(dh.datetime().end());
        }
        self.balance = Self::calc_balance(&self.sequence);
        Ok(())*/
    }

    pub fn extract(self, group_id: DealId<'h>) -> Result<(DealGroup<'h>, Option<Self>), Self> {
        let mut new_seq = VecDeque::new_in(*self.sequence.allocator());
        let mut extracted_deal = None;
        for dh in self.sequence {
            match dh.extract(group_id) {
                Ok((extracted, remainder)) => {
                    extracted_deal = Some(extracted);
                    if let Some(remainder) = remainder {
                        new_seq.push_back(remainder);
                    }
                }
                Err(dh) => new_seq.push_back(dh),
            }
        }
        match extracted_deal {
            Some(extracted) => {
                Ok((extracted, if new_seq.is_empty() { None } else { Some(Self::new(new_seq)) }))
            }
            None => Err(Self::new(new_seq)),
        }
    }

    pub fn insert<DH: Into<DealHolding<'h>>>(&mut self, holding: DH) {
        let holding = holding.into();
        assert_ne!(holding.total().amount(), 0);

        //let insert_pos =
        //    self.sequence.partition_point(|dh| dh.datetime_range() <= holding.datetime_range());
        //self.sequence.insert(insert_pos, holding);
        self.oldest = self.oldest.min(holding.datetime().start());
        self.newest = self.newest.max(holding.datetime().end());
        self.sequence.push_back(holding);
        self.balance = Self::calc_balance(&self.sequence);
    }
}

impl Debug for SequenceDealHolding<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for (i, dh) in self.sequence.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            write!(f, "{:?}", dh)?;
        }
        write!(f, "]")
    }
}

impl From<&SequenceDealHolding<'_>> for Yaml {
    fn from(value: &SequenceDealHolding<'_>) -> Self {
        let mut seq = Vec::with_capacity(value.sequence.len());
        for dh in &value.sequence {
            seq.push(dh.into());
        }
        Yaml::Array(seq)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct AverageDealHolding<'h> {
    /// The datetime range of dealing events contained within
    datetime: JDateTimeRange<'h>,
    valued_amount: ValuedAmount<'h>,
    expenses: ValuedAmount<'h>,
    balance: PoolBalance<'h>,
}

impl<'h> AverageDealHolding<'h> {
    pub fn allocator(&self) -> &'h HerdAllocator<'h> {
        self.valued_amount.allocator().unwrap()
    }

    pub fn amount(&self) -> Amount<'h> {
        self.valued_amount.amount()
    }

    /// The datetime range of dealing events contained within.
    pub fn datetime_range(&self) -> JDateTimeRange<'h> {
        self.datetime
    }

    pub fn calculate_balance(&self) -> PoolBalance<'h> {
        let mut total = self.total_before_expenses().clone();
        total.add_from(&self.expenses);
        PoolBalance::new(total)
    }

    pub fn set_unit_of_account(
        &mut self,
        uoa: &'h Unit<'h>,
        config_and_value_date: Option<(&Configuration<'h>, DateTime<Tz>)>,
    ) -> Result<(), JournError> {
        match config_and_value_date {
            Some((config, date)) => {
                match SystemValuer::on_date(
                    config.clone(),
                    JDateTime::from_datetime(
                        date,
                        Some(config.as_herd_ref().date_format()),
                        Some(config.as_herd_ref().time_format()),
                    ),
                )
                .value(self.valued_amount.amount(), uoa)?
                {
                    Some(val) => {
                        self.valued_amount
                            .set_valuation(Valuation::new_total(val.rounded(), false));
                    }
                    None => {
                        self.valued_amount.value_in(uoa).ok_or_else(|| {
                            err!("Unable to adjust unit of account. Unable to value holding")
                        })?;
                    }
                }
            }
            None => {
                self.valued_amount.value_in(uoa).ok_or_else(|| {
                    err!("Unable to adjust unit of account for an average holding without revaluing first")
                })?;
            }
        }
        self.balance = self.calculate_balance();
        Ok(())
    }

    pub fn total(&self) -> &PoolBalance<'h> {
        &self.balance
    }

    pub fn total_before_expenses(&self) -> &ValuedAmount<'h> {
        &self.valued_amount
    }

    pub fn expenses(&self) -> &ValuedAmount<'h> {
        &self.expenses
    }

    pub fn value_with<V: Valuer<'h>>(
        &mut self,
        valuer: &mut V,
        quote_unit: &'h Unit<'h>,
    ) -> JournResult<()> {
        self.valued_amount.value_with(valuer, quote_unit)?;
        self.expenses.value_with(valuer, quote_unit)?;
        self.balance = self.calculate_balance();
        Ok(())
    }

    pub fn value_in_or_value_with<V: Valuer<'h>>(
        &mut self,
        in_unit: &'h Unit<'h>,
        valuer: &mut V,
    ) -> JournResult<()> {
        self.valued_amount
            .value_in_or_value_with(in_unit, valuer)
            .and_then(|_| self.expenses.value_in_or_value_with(in_unit, valuer))?
            .ok_or_else(|| err!("Unable to value holding"))?;
        self.balance = self.calculate_balance();
        Ok(())
    }

    pub fn set_in_terms_of_unit_of_account(&mut self, uoa: &'h Unit<'h>) {
        self.expenses.in_terms_of(uoa);
    }

    pub fn split_max(self, amount: Quantity) -> Result<(Self, Option<Self>), Self> {
        if amount.is_sign_positive() != self.valued_amount.amount().quantity().is_sign_positive() {
            return Err(self);
        }
        if self.valued_amount.is_zero() {
            return Err(self);
        }

        let split_amount = amount.min_abs(self.valued_amount.amount().quantity());
        //let split_percent = split_amount / self.valued_amount.amount().quantity();
        //if split_amount.is_zero() {
        //    return Err(Average(self));
        //}
        //let split_parent = self.clone();

        // Use the balance for the split and round that. This is more accurate than splitting the
        // valued amount and expenses components, rounding them and adding them back together.
        let (bal_split, bal_rem) = self.balance.into_valued_amount().split(split_amount);

        let (left_va, rem_va) = self.valued_amount.split(split_amount);

        // Split expenses.
        let mut left_exp = (&bal_split - &left_va).unwrap();
        let mut right_exp = (&bal_rem - &rem_va).unwrap();
        // If the main expense amount is 0, but there are non-zero values, then remove the main amount.
        for exp in [&mut left_exp, &mut right_exp] {
            let unit = if exp.amount().is_zero() {
                if let Some(amount) = exp.totalled_valuations().find(|v| !v.is_zero()) {
                    Some(amount.unit())
                } else {
                    None
                }
            } else {
                None
            };
            if let Some(unit) = unit {
                let primary_unit = exp.unit();
                exp.in_terms_of(unit);
                exp.remove_valuation(primary_unit);
            }
        }

        let right = if rem_va.amount().is_zero() {
            None
        } else {
            Some(AverageDealHolding {
                balance: PoolBalance::new(bal_rem),
                datetime: self.datetime,
                valued_amount: rem_va,
                expenses: right_exp,
            })
        };
        trace!("Averagesplit_max: left_va: {}, right: {:?}", left_va, right.as_ref());
        Ok((
            AverageDealHolding {
                balance: PoolBalance::new(bal_split),
                datetime: self.datetime,
                valued_amount: left_va,
                expenses: left_exp,
            },
            right,
        ))
    }

    pub fn add_deal(&mut self, deal: &Deal<'h>) -> JournResult<()> {
        self.valued_amount = (&self.valued_amount + deal.valued_amount())
            .expect("Attempt to add incompatible amount");
        self.expenses =
            (&self.expenses + deal.expenses()).expect("Attempt to add incompatible expenses");
        self.datetime.set_start(self.datetime.start().min(deal.datetime().start()));
        self.datetime.set_end(self.datetime.end().max(deal.datetime().end()));
        self.balance = self.calculate_balance();
        Ok(())
    }

    pub fn add_adjustment(&mut self, adj: Adjustment<'h>) -> JournResult<()> {
        adj.apply(&mut self.valued_amount)?;
        self.balance = self.calculate_balance();
        Ok(())
    }
}

impl Debug for AverageDealHolding<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Avg({:?})", self.valued_amount)
    }
}

impl<'h> From<SequenceDealHolding<'h>> for AverageDealHolding<'h> {
    fn from(seq: SequenceDealHolding<'h>) -> Self {
        let datetime = seq.datetime();
        let balance = seq.total().clone();
        let valued_amount = seq.total_before_expenses();

        Self { valued_amount, expenses: seq.expenses(), datetime, balance }
    }
}

impl<'h> From<&Deal<'h>> for AverageDealHolding<'h> {
    fn from(deal: &Deal<'h>) -> Self {
        Self {
            valued_amount: deal.valued_amount().clone(),
            expenses: deal.expenses().clone(),
            datetime: deal.datetime(),
            balance: deal.total().clone(),
        }
    }
}

/*
impl<'h> From<Deal<'h>> for DealHolding<'h> {
    fn from(deal: Deal<'h>) -> Self {
        Single(deal)
    }
}*/

impl<'h> From<DealGroup<'h>> for DealHolding<'h> {
    fn from(group: DealGroup<'h>) -> Self {
        Group(group)
    }
}

impl<'h> From<SequenceDealHolding<'h>> for DealHolding<'h> {
    fn from(mut seq: SequenceDealHolding<'h>) -> Self {
        if seq.sequence.len() == 1 {
            seq.sequence.pop_back().unwrap()
        } else {
            Sequence(seq)
        }
    }
}

impl<'h> From<AverageDealHolding<'h>> for DealHolding<'h> {
    fn from(avg: AverageDealHolding<'h>) -> Self {
        Average(avg)
    }
}
