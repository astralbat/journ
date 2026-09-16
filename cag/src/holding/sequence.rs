/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjusted_value::AdjustedValue;
use crate::adjustment::{Adjustment, AmountAdjustment};
use crate::cag_configuration::MatchMethod;
use crate::deal::Deal;
use crate::holding::deal_holding::DealHolding::{Average, Sequence};
use crate::holding::deal_holding::{DEAL_HOLDING_ID_COUNTER, DealHolding};
use crate::holding::{AverageDealHolding, DealHoldingSummary};
use chrono::DateTime;
use chrono_tz::Tz;
use journ_core::alloc::HerdAllocator;
use journ_core::amount::{Amount, Quantity};
use journ_core::configuration::Configuration;
use journ_core::datetime::{JDateTime, JDateTimeRange};
use journ_core::error::{JournError, JournResult};
use journ_core::ext::NumExt;
use journ_core::journal_context::JContext;
use journ_core::journal_entry::JournalEntry;
use journ_core::unit::Unit;
use linked_hash_set::LinkedHashSet;
use std::collections::VecDeque;
use std::fmt;
use std::fmt::Debug;
use std::rc::Rc;

/// A sequence of deal holdings. The sequence will never be empty.
#[derive(PartialEq, Eq)]
pub struct SequenceDealHolding<'h> {
    id: usize,
    sequence: VecDeque<DealHolding<'h>, &'h HerdAllocator<'h>>,
    adjusted_value: AdjustedValue<'h>,
    oldest: JDateTime,
    newest: JDateTime,
    split_parent: Option<Rc<DealHoldingSummary<'h>>>,
}

impl<'h> SequenceDealHolding<'h> {
    pub fn new(sequence: VecDeque<DealHolding<'h>, &'h HerdAllocator<'h>>) -> Self {
        assert!(!sequence.is_empty());

        let id = DEAL_HOLDING_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let balance = Self::calc_adj_amount(&sequence);
        let oldest = sequence.iter().map(|dh| dh.datetime().start()).min().unwrap();
        let newest = sequence.iter().map(|dh| dh.datetime().end()).max().unwrap();
        Self { id, sequence, adjusted_value: balance, oldest, newest, split_parent: None }
    }

    pub fn from_iterator<I: IntoIterator<Item = DealHolding<'h>>>(seq: I) -> Self {
        let mut vd = VecDeque::with_capacity_in(0, JContext::get().allocator());
        vd.extend(seq);

        let id = DEAL_HOLDING_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let balance = Self::calc_adj_amount(&vd);
        let oldest = vd.iter().map(|dh| dh.datetime().start()).min().unwrap();
        let newest = vd.iter().map(|dh| dh.datetime().end()).max().unwrap();
        let sdh =
            Self { id, sequence: vd, adjusted_value: balance, oldest, newest, split_parent: None };
        assert!(!sdh.sequence.is_empty());
        sdh
    }

    pub fn id(&self) -> usize {
        self.id
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

    pub fn datetime(&self) -> JDateTimeRange {
        // The sequence won't necessarily be in date order due to the way rule logic
        // may operate.
        JDateTimeRange::new(self.oldest, Some(self.newest))
    }

    pub fn adjusted_value(&self) -> AdjustedValue<'h> {
        self.adjusted_value
    }

    pub fn amount(&self) -> Amount<'h> {
        self.adjusted_value.amount()
    }

    pub fn value(&self) -> Amount<'h> {
        self.adjusted_value.value()
    }

    pub fn consideration(&self) -> Amount<'h> {
        self.adjusted_value.consideration()
    }

    pub fn expenses(&self) -> Amount<'h> {
        self.adjusted_value.expenses()
    }

    pub fn split_parent(&self) -> Option<&Rc<DealHoldingSummary<'h>>> {
        self.split_parent.as_ref()
    }

    pub fn entries(&self) -> Box<dyn Iterator<Item = &'h JournalEntry<'h>> + '_> {
        Box::new(self.sequence.iter().flat_map(|dh| dh.entries()))
    }

    /// Calculates the balance.
    ///
    fn calc_adj_amount(
        sequence: &VecDeque<DealHolding<'h>, &'h HerdAllocator<'h>>,
    ) -> AdjustedValue<'h> {
        let mut total = sequence.iter().next().unwrap().adjusted_value();
        for bal in sequence.iter().skip(1).map(|dh| dh.adjusted_value()) {
            total += bal;
        }
        total
    }

    pub fn set_value_on_date(
        &mut self,
        uoa: &'h Unit<'h>,
        config: &Configuration<'h>,
        date: DateTime<Tz>,
    ) -> Result<(), JournError> {
        for dh in self.sequence.iter_mut() {
            dh.set_value_on_date(uoa, config, date)?
        }
        self.adjusted_value = Self::calc_adj_amount(&self.sequence);
        Ok(())
    }

    pub fn ensure_valued(&mut self, uoa: &'h Unit<'h>) -> Result<(), JournError> {
        for dh in self.sequence.iter_mut() {
            dh.ensure_valued(uoa)?
        }
        self.adjusted_value = Self::calc_adj_amount(&self.sequence);
        Ok(())
    }

    pub fn into_deal_iter(self) -> Box<dyn Iterator<Item = Deal<'h>> + 'h> {
        Box::new(self.sequence.into_iter().flat_map(|dh| dh.into_deal_iter()))
    }

    pub fn deal_iter(&self) -> Box<dyn Iterator<Item = &Deal<'h>> + '_> {
        Box::new(self.sequence.iter().flat_map(|dh| dh.deal_iter()))
    }

    pub fn description(&self) -> LinkedHashSet<&'h str> {
        let mut descs = LinkedHashSet::new();
        for dh in self.sequence.iter() {
            descs.extend(dh.description());
        }
        descs
    }

    pub fn split_max(
        mut self,
        amount: Quantity,
        method: MatchMethod,
    ) -> Result<(DealHolding<'h>, Option<DealHolding<'h>>), Self> {
        if !amount.is_sign_compatible(self.adjusted_value.amount().quantity()) {
            return Err(self);
        }
        if amount.is_zero() {
            return Err(self);
        }

        // If we need to split using the average method, we convert the sequence to an average holding, perform the split and set
        // the split parent to be this holding, allowing the returned average holdings to be able to link back to the original deals.
        if method == MatchMethod::Average {
            return if !self.adjusted_value.amount().is_zero() {
                let (left, right) =
                    Average(AverageDealHolding::from(self)).split_max(amount, method).unwrap();
                Ok((left, right))
            } else {
                Err(self)
            };
        }

        let as_holding = Sequence(self);
        let self_snapshot = DealHoldingSummary::from(&as_holding);
        self = as_holding.into_sequence().unwrap();

        let reverse = matches!(method, MatchMethod::Lifo);
        let mut i = if reverse { self.sequence.len() - 1 } else { 0 };
        while let Some(target_part) = self.sequence.remove(i) {
            match target_part.split_max(amount, method) {
                Ok((left, right)) => {
                    if let Some(right) = right {
                        self.sequence.insert(i, right);
                    }
                    return if self.sequence.is_empty() {
                        Ok((left, None))
                    } else {
                        self.adjusted_value = Self::calc_adj_amount(&self.sequence);
                        self.split_parent = Some(Rc::new(self_snapshot));
                        Ok((left, Some(Sequence(self))))
                    };
                }
                Err(holding) => {
                    self.sequence.insert(i, *holding);
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
    /// The mini adjustments are calculated proportionally, and in such a way that their sum total will equal approximately
    /// the original adjustment. See the `balance` function for precision.
    ///
    /// Furthermore, the mini adjustments are rounded so that when they are matched, there are no odd
    /// fractions lying about. This could potentially be changed in the future to depend on the `round_deals`
    /// parameter if there is a case for not rounding.
    pub fn add_adjustment(&mut self, orig_adj: Adjustment<'h>) -> JournResult<()> {
        let self_total = self.adjusted_value;
        let seq_len = self.sequence.len();
        // Keep track of a remainder after applying each mini adjustment. We'll apply this to the last element.
        let mut rem_adj = orig_adj.clone();
        let mut rem_amount_adjustments = rem_adj.amount_adjustments().to_vec();
        for (i, dh) in self.sequence.iter_mut().enumerate() {
            if i == seq_len - 1 {
                rem_adj.set_amount_adjustments(rem_amount_adjustments);
                dh.add_adjustment(rem_adj)?;
                break;
            } else {
                // Create the mini adjustment for this holding
                let mut mini_adj_amount_adjustments = vec![];
                for amount_adj in orig_adj.amount_adjustments().iter().cloned() {
                    if matches!(amount_adj, AmountAdjustment::Scale(_)) {
                        mini_adj_amount_adjustments.push(amount_adj);
                        continue;
                    }

                    // Calculate amount adjustments only for those in common
                    let (dh_amount_total, amount_total) =
                        if amount_adj.amount().unit() == self_total.amount().unit() {
                            (dh.amount(), self_total.amount())
                        } else if amount_adj.amount().unit() == self_total.value().unit() {
                            (dh.value(), self_total.value())
                        } else {
                            continue;
                        };

                    let adj_amount_unrounded =
                        amount_adj.amount() * (dh_amount_total / amount_total);

                    // The first amount adjustment is the primary amount. We can determine the sign of the
                    // holding. This is important as subsequent adjustments need to match on the same sign which is an implied
                    // requirement for ValuedAmounts. In certain edge cases, the rounding might change the sign, so here we try
                    // different rounding strategies to ensure the sign is maintained.
                    // We round to the same number of decimal places as the holding's amount. This is important to ensure that
                    // the sign stays consistent.
                    let adj_amount_rounded =
                        adj_amount_unrounded.rounded_dec_places(dh_amount_total.scale() as u8);

                    let (left, _right) = amount_adj.split(adj_amount_rounded.quantity());
                    mini_adj_amount_adjustments.push(left.clone());
                    rem_amount_adjustments.push(left.inverse());
                }
                let mut mini_adj = orig_adj.clone();
                mini_adj.set_amount_adjustments(mini_adj_amount_adjustments);
                // Apply the mini adjustment to the holding
                dh.add_adjustment(mini_adj)?;
            };

            self.oldest = self.oldest.min(dh.datetime().start());
            self.newest = self.newest.max(dh.datetime().end());
        }
        self.adjusted_value = Self::calc_adj_amount(&self.sequence);

        Ok(())
    }

    pub fn extract(
        self,
        holding_id: usize,
    ) -> Result<(DealHolding<'h>, Option<DealHolding<'h>>), Self> {
        let mut new_seq = VecDeque::new_in(*self.sequence.allocator());
        let mut extracted_deal = None;
        for dh in self.sequence {
            match dh.extract(holding_id) {
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
                let rem = match new_seq.len() {
                    0 => None,
                    1 => new_seq.pop_front(),
                    _ => Some(Sequence(Self::new(new_seq))),
                };
                Ok((extracted, rem))
            }
            None => Err(Self::new(new_seq)),
        }
    }

    pub fn insert<DH: Into<DealHolding<'h>>>(&mut self, holding: DH) {
        let holding = holding.into();
        assert_ne!(holding.amount(), 0);

        //let insert_pos =
        //    self.sequence.partition_point(|dh| dh.datetime_range() <= holding.datetime_range());
        //self.sequence.insert(insert_pos, holding);
        self.oldest = self.oldest.min(holding.datetime().start());
        self.newest = self.newest.max(holding.datetime().end());
        self.sequence.push_back(holding);
        self.adjusted_value = Self::calc_adj_amount(&self.sequence);
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

impl<'h> From<SequenceDealHolding<'h>> for DealHolding<'h> {
    fn from(mut seq: SequenceDealHolding<'h>) -> Self {
        if seq.sequence.len() == 1 { seq.sequence.pop_back().unwrap() } else { Sequence(seq) }
    }
}

/*
impl From<&SequenceDealHolding<'_>> for Yaml {
    fn from(value: &SequenceDealHolding<'_>) -> Self {
        let mut seq = Vec::with_capacity(value.sequence.len());
        for dh in &value.sequence {
            seq.push(dh.into());
        }
        Yaml::Array(seq)
    }
}*/
