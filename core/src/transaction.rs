/*
 * Copyright (c) 2019-2023. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::Amount;
use crate::posting::Posting;
use crate::posting_valuation::Valuation;
use crate::unit::Unit;
use rust_decimal_macros::*;
use smallvec::{smallvec, SmallVec};
use std::borrow::{Borrow, BorrowMut};
use std::collections::HashSet;
use std::mem;

/// A transaction consists of two posting groups where one group is net positive (the target)
/// and the other, net negative (the source). Both sides must balance to zero and this
/// can always be verified when both the source and target groups share the same unit. In a trade
/// both sides will have different currencies, but if valued in a particular unit, these values will then need to balance.
#[derive(Debug, Clone)]
pub struct Transaction<T> {
    source: PostingGroup<T>,
    target: PostingGroup<T>,
}
pub struct TransactionMut {}
impl<T> Transaction<T> {
    pub fn new(source: PostingGroup<T>, target: PostingGroup<T>) -> Transaction<T> {
        Transaction { source, target }
    }

    /// Gets the net negative group - where amounts have moved from
    pub fn source(&self) -> &PostingGroup<T> {
        &self.source
    }

    pub fn source_and_target_mut(&mut self) -> (&mut PostingGroup<T>, &mut PostingGroup<T>) {
        (&mut self.source, &mut self.target)
    }

    /// Gets the net positive group - where the amounts are moving to
    pub fn target(&self) -> &PostingGroup<T> {
        &self.target
    }

    pub fn currencies(&self) -> HashSet<&'static Unit>
    where
        T: Borrow<Posting>,
    {
        let mut currs = HashSet::new();
        for pst in self.postings() {
            currs.insert(pst.borrow().unit());
        }
        currs
    }
    /*
    pub fn acquisitions(&self, include_asset_transfer: bool) -> Vec<&T>
    where
        T: Borrow<Posting>,
    {
        let mut aqs = vec![];
        if self.is_trade() || include_asset_transfer {
            // It is must be that any associated expenses are related to the trade and are being spent after the trade has taken place. Therefore, they are included as
            // an acquisition. If this is not the case, the entry can't be balanced in the first place.
            aqs.append(
                &mut self
                    .target
                    .postings()
                    .into_iter()
                    .filter(|p| {
                        (*p).borrow().account().is_asset()
                            || (((*p).borrow().account().is_expense() || (*p).borrow().account().is_income())
                                && (*p).borrow().amount().is_positive()
                                && !(*p).borrow().is_tagged(TAG_DIS_FEE)
                                && !(*p).borrow().is_tagged(TAG_ACQ_FEE))
                    })
                    .collect(),
            );
        }
        aqs.append(
            &mut self
                .source
                .postings()
                .into_iter()
                .filter(|p| !(*p).borrow().is_tagged(TAG_DIS_FEE) && !(*p).borrow().is_tagged(TAG_ACQ_FEE))
                .filter(|p| {
                    (*p).borrow().account().is_income()
                        || ((*p).borrow().account().is_expense() && (*p).borrow().amount().is_negative())
                })
                .collect(),
        );
        aqs
    }

    pub fn disposals(&self, include_asset_transfer: bool) -> Vec<&T>
    where
        T: Borrow<Posting>,
    {
        let mut disps = vec![];
        if self.is_trade() || include_asset_transfer {
            disps.append(
                &mut self
                    .source
                    .postings()
                    .into_iter()
                    .filter(|p| {
                        (*p).borrow().account().is_asset()
                            || (((*p).borrow().account().is_expense() || (*p).borrow().account().is_income())
                                && (*p).borrow().amount().is_negative()
                                && !(*p).borrow().is_tagged(TAG_DIS_FEE)
                                && !(*p).borrow().is_tagged(TAG_ACQ_FEE))
                    })
                    .collect(),
            );
        }
        disps.append(
            &mut self
                .target
                .postings()
                .into_iter()
                .filter(|p| !(*p).borrow().is_tagged(TAG_DIS_FEE) && !(*p).borrow().is_tagged(TAG_ACQ_FEE))
                .filter(|p| {
                    (*p).borrow().account().is_expense()
                        || ((*p).borrow().account().is_income()) && (*p).borrow().amount().is_positive()
                })
                .collect(),
        );
        disps
    }

    pub fn fees(&self, acquisition: bool) -> Vec<&T>
    where
        T: Borrow<Posting>,
    {
        let mut fees = vec![];
        fees.append(
            &mut self
                .postings()
                .into_iter()
                .filter(|p| {
                    (acquisition && (*p).borrow().is_tagged(TAG_ACQ_FEE))
                        || (!acquisition && (*p).borrow().is_tagged(TAG_DIS_FEE))
                })
                .collect(),
        );
        fees
    }*/

    pub fn into_inner(self) -> (PostingGroup<T>, PostingGroup<T>) {
        (self.source, self.target)
    }

    /// All source postings chained with target postings
    pub fn postings(&self) -> impl Iterator<Item = &T>
    where
        T: Borrow<Posting>,
    {
        self.source.postings().into_iter().chain(self.target.postings().into_iter())
    }

    /// Gets whether all postings are valued
    pub fn is_valued(&self, in_curr: &'static Unit) -> bool
    where
        T: Borrow<Posting>,
    {
        self.postings().all(|p| p.borrow().amount_in(in_curr).is_some())
    }

    /// All valuation currencies used in this transaction
    pub fn valuation_currencies(&self) -> impl Iterator<Item = &'static Unit> + '_
    where
        T: Borrow<Posting>,
    {
        self.postings().flat_map(|p| p.borrow().value_currencies())
    }

    /// Attempt to derive the valuation which only succeeds if at least one posting has
    /// been valued. Returns a list of modified postings.
    pub fn derive_valuation(&mut self, in_curr: &'static Unit)
    where
        T: BorrowMut<Posting>,
        T: Borrow<Posting>,
    {
        // Working with multiple currencies is not supported for now.
        if self.source.currencies().len() > 1 || self.target.currencies().len() > 1 {
            return;
        }

        let option_add = |acc: Option<Amount<'static>>, amnt: Amount<'static>| -> Option<Amount> {
            match acc {
                Some(acc) => Some(acc + amnt),
                None => Some(amnt),
            }
        };

        let source_total_val: Option<Amount> = self
            .source
            .postings()
            .filter_map(|p| p.borrow().amount_in(in_curr))
            .fold(None, |acc, a| option_add(acc, a));
        let source_total_amount: Option<Amount> = self
            .source
            .postings()
            .filter(|p| (*p).borrow().amount_in(in_curr).is_some())
            .map(|p| (*p).borrow().amount())
            .fold(None, option_add);
        let target_total_val: Option<Amount> = self
            .target
            .postings()
            .filter_map(|p| (*p).borrow().amount_in(in_curr))
            .fold(None, |acc, a| option_add(acc, a));
        let target_total_amount: Option<Amount> = self
            .target
            .postings()
            .filter(|p| (*p).borrow().amount_in(in_curr).is_some())
            .map(|p| (*p).borrow().amount())
            .fold(None, option_add);

        // There are no postings on either side in the valuation unit.
        if source_total_val.is_none() && target_total_val.is_none() {
            return;
        }

        // The larger side will be the valuation basis.
        let group_to_value;
        let group_to_complete;
        let group_to_complete_total_amount;
        let group_to_complete_total_val;
        if target_total_val.is_none()
            || source_total_val.is_some()
                && source_total_val.as_ref().unwrap().abs()
                    > target_total_val.as_ref().unwrap().abs()
        {
            group_to_complete = &mut self.source;
            group_to_complete_total_amount = source_total_amount.unwrap();
            group_to_complete_total_val = source_total_val.unwrap();
            group_to_value = &mut self.target;
        } else {
            group_to_complete = &mut self.target;
            group_to_complete_total_amount = target_total_amount.unwrap();
            group_to_complete_total_val = target_total_val.unwrap();
            group_to_value = &mut self.source;
        }
        let mut value_remaining = group_to_complete_total_val.clone();

        // Complete the first group (if necessary).
        for pst in group_to_complete.postings_mut() {
            if (*pst).borrow().amount_in(in_curr).is_none() {
                let pst_amount = (*pst).borrow().amount();
                pst.borrow_mut()
                    .set_valuation(Valuation::Total(
                        (group_to_complete_total_val
                            * (pst_amount / group_to_complete_total_amount).amount())
                        .rounded()
                        .abs(),
                        true,
                    ))
                    .unwrap();
                value_remaining += (*pst).borrow().valuation(in_curr).unwrap().value();
            }
        }

        // Value the entirety of the second group
        // The value remaining of the group we're valuing is the additive inverse of the other group's value.
        value_remaining = value_remaining.negate();
        let total_amount = group_to_value.total_amount().unwrap().abs();
        let mut amount_remaining = group_to_value.total_amount().unwrap().clone();
        for pst in group_to_value.postings_mut() {
            // Already has a value, let's use that
            if (*pst).borrow().amount_in(in_curr).is_some() {
                amount_remaining -= (*pst).borrow().amount();
                value_remaining -= (*pst).borrow().amount_in(in_curr).unwrap();
                continue;
            }

            // Last posting
            if amount_remaining - (*pst).borrow().amount() == 0 {
                // Use remainder to avoid creating a problem where the two valuations don't balance due to rounding.
                pst.borrow_mut()
                    .set_valuation(Valuation::Total(value_remaining.abs(), true))
                    .unwrap();
                break;
            } else {
                if total_amount == 0 {
                    pst.borrow_mut()
                        .set_valuation(Valuation::Total(in_curr.amount(0), true))
                        .unwrap()
                } else {
                    let pst_amount = (*pst).borrow().amount();
                    pst.borrow_mut()
                        .set_valuation(Valuation::Total(
                            (group_to_complete_total_val
                                * (pst_amount.abs() / total_amount).amount())
                            .rounded()
                            .abs(),
                            true,
                        ))
                        .unwrap();
                }
                amount_remaining -= (*pst).borrow().amount();
                value_remaining -= (*pst).borrow().amount_in(in_curr).unwrap();
            }
        }
    }

    /// Gets whether this transaction is balanced
    pub fn is_balanced(&self) -> bool
    where
        T: Borrow<Posting>,
    {
        let source_amount = self.source.total_amount();
        let target_amount = self.target.total_amount();
        if let (Some(sa), Some(ta)) = (source_amount, target_amount) {
            sa + ta == dec!(0)
        } else {
            false
        }
    }

    /// A transfer is considered to be a movement across asset holdings and so isn't considered an
    /// acquisition or disposal.
    /// This check is strict in that if there are postings other than assets involved, this will
    /// return false.
    pub fn is_transfer(&self) -> bool
    where
        T: Borrow<Posting>,
    {
        let source_currs = self.source.currencies();
        let target_currs = self.target.currencies();
        if source_currs == target_currs {
            self.postings()
                .all(|p| p.borrow().account().is_asset() || p.borrow().account().is_equity())
        } else {
            false
        }
    }

    pub fn is_trade(&self) -> bool
    where
        T: Borrow<Posting>,
    {
        let source_currs = self.source.currencies();
        let target_currs = self.target.currencies();
        source_currs != target_currs
    }

    /// An income transaction is strictly between a +income or -expense amount and an asset
    pub fn is_income(&self) -> bool
    where
        T: Borrow<Posting>,
    {
        self.source.is_income() && self.target().is_asset()
    }

    /// The transaction is not foreign when it has been fully conducted in to_curr.
    pub fn is_foreign(&self, to_curr: &Unit) -> bool
    where
        T: Borrow<Posting>,
    {
        self.source.is_foreign(to_curr) || self.target.is_foreign(to_curr)
    }
}

/// Any subset from the set of postings in a single journal entry.
/// Groups are never allowed to be empty and so must contain at least one posting.
/// A posting group will usually, in reality be a single unit though this is not necessarily the case. For example,
/// consider a transaction where 3 cows are traded for 5 sheep and 10 chickens.
#[derive(Debug, Clone)]
pub struct PostingGroup<T> {
    postings: SmallVec<[T; 8]>,
}
impl<T> PostingGroup<T>
where
    T: Borrow<Posting>,
{
    pub fn many<'a>(postings: Vec<T>) -> Result<PostingGroup<T>, String> {
        if postings.is_empty() {
            return Err("Cannot make an empty posting group".into());
        }
        let mut group = PostingGroup { postings: SmallVec::with_capacity(postings.len()) };
        for pst in postings.into_iter() {
            group.postings.push(pst);
        }
        Ok(group)
    }

    pub fn single(posting: T) -> PostingGroup<T> {
        PostingGroup { postings: smallvec![posting] }
    }

    pub fn postings(&self) -> impl Iterator<Item = &T>
    where
        T: Borrow<Posting>,
    {
        self.postings.iter()
    }

    pub fn postings_mut(&mut self) -> Vec<&mut T>
    where
        T: BorrowMut<Posting>,
    {
        self.postings.iter_mut().collect()
    }

    pub fn drain(&mut self) -> impl Iterator<Item = T> + '_ {
        self.postings.drain(..)
    }

    pub fn push(&mut self, posting: T) {
        self.postings.push(posting)
    }

    pub fn currencies(&self) -> HashSet<&'static Unit>
    where
        T: Borrow<Posting>,
    {
        let mut currs = HashSet::new();
        for pst in self.postings() {
            currs.insert(pst.borrow().unit());
        }
        currs
    }

    pub fn has_currency(&self, curr: &Unit) -> bool
    where
        T: AsRef<Posting>,
    {
        for pst in self.postings() {
            if pst.as_ref().unit() == curr {
                return true;
            }
        }
        false
    }

    pub fn is_positive(&self) -> bool
    where
        T: Borrow<Posting>,
    {
        // Same unit, then check the total is positive
        if let Some(amount) = self.total_amount() {
            return amount > dec!(0);
        }
        // Different currencies, then all amount must be positive
        self.postings().all(|p| p.borrow().amount() >= dec!(0))
    }

    /// Sums up the amounts on the postings and returns the amount which may be positive
    /// or negative.
    pub fn total_amount(&self) -> Option<Amount<'static>>
    where
        T: Borrow<Posting>,
    {
        let currs = self.currencies();
        if currs.len() > 1 {
            return None;
        }
        let total = Amount::new(currs.iter().next().unwrap(), 0);
        Some(self.postings().map(|p| p.borrow().amount()).fold(total, |acc, a| acc + a))
    }

    /// Gets the total amount of the group on the positive or negative side.
    /// Every member on either the positive or negative side must have an amount present
    /// in the specified unit. Otherwise this will return None.
    pub fn total_amount_in_currency(&self, in_curr: &'static Unit) -> Option<Amount<'static>>
    where
        T: Borrow<Posting>,
    {
        let mut total: Option<Amount> = None;
        for pst in self.postings() {
            match pst.borrow().amount_in(in_curr) {
                Some(tc) => match &mut total {
                    Some(total) => *total += tc,
                    None => total = Some(tc),
                },
                None => return None,
            }
        }
        total
    }

    pub fn split_positive_negative(&mut self) -> (Self, &mut Self)
    where
        T: Borrow<Posting>,
    {
        let pos_first = self.is_positive();
        let mut pos = {
            let mut positive_postings: SmallVec<[T; 8]> = SmallVec::new();
            let mut i = 0;
            while i < self.postings.len() {
                if self.postings[i].borrow().amount().amount().is_sign_negative() {
                    positive_postings.push(self.postings.remove(i));
                } else {
                    i += 1;
                }
            }
            PostingGroup { postings: positive_postings }
        };
        // Preserve some sense of order from the original entry
        if !pos_first {
            mem::swap(self, &mut pos);
        }
        (pos, self)
    }

    /// The transaction is not foreign when it has been fully conducted in to_curr.
    pub fn is_foreign(&self, to_curr: &Unit) -> bool {
        let currs = self.currencies();
        return currs.len() > 1 || currs.into_iter().next().unwrap() != to_curr;
    }

    /// Returns whether this group is strictly a set of income (or negative expense) postings
    pub fn is_income(&self) -> bool
    where
        T: Borrow<Posting>,
    {
        for pst in self.postings() {
            if !pst.borrow().account().is_income() || !pst.borrow().account().is_expense() {
                return false;
            }
            if !pst.borrow().amount().amount().is_sign_negative() {
                return false;
            }
        }
        true
    }

    /// Returns whether this group is strictly a set of expense (or positive income) postings
    pub fn is_expense(&self) -> bool
    where
        T: Borrow<Posting>,
    {
        for pst in self.postings() {
            if !pst.borrow().account().is_income() || !pst.borrow().account().is_expense() {
                return false;
            }
            if !pst.borrow().amount().amount().is_sign_positive() {
                return false;
            }
        }
        true
    }

    /// Returns whether this group is strictly a set of asset postings
    pub fn is_asset(&self) -> bool
    where
        T: Borrow<Posting>,
    {
        self.postings().all(|p| p.borrow().account().is_asset())
    }

    pub fn contains_equity(&self) -> bool
    where
        T: Borrow<Posting>,
    {
        self.postings().any(|p| p.borrow().account().is_equity())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_derive_valuation() {
        /* FIXME: Broken

        let (journal, entry) = parse_postings(
            " Assets:A1  $100.00 @@ £50.00\n \
             Assets:A2  $200.00\n \
             Assets:A3  -$300.00",
        );
        let sterling = journal.get_currency("£").unwrap();
        let mut trans_list: Vec<Transaction> = entry.transactions().unwrap();
        assert_eq!(trans_list.len(), 1);
        let trans = trans_list.get_mut(0).unwrap();
        assert_eq!(trans.derive_valuation(&sterling), true);
        assert_eq!(entry.get_posting(0).amount_in_currency(&sterling).unwrap(), Money::from_str("£50").unwrap());
        assert_eq!(entry.get_posting(1).amount_in_currency(&sterling).unwrap(), Money::from_str("£100").unwrap());
        assert_eq!(entry.get_posting(2).amount_in_currency(&sterling).unwrap(), Money::from_str("-£150").unwrap());

        let (_, entry) = parse_postings(
            " Assets:A1  $100.00\n \
             Assets:A2  $200.00\n \
             Assets:A3  -$300.00 @@ £150.00",
        );
        let mut trans_list = entry.transactions().unwrap();
        let trans = trans_list.get_mut(0).unwrap();
        assert_eq!(trans.derive_valuation(&sterling), true);
        assert_eq!(entry.get_posting(0).amount_in_currency(&sterling).unwrap(), Money::from_str("£50").unwrap());
        assert_eq!(entry.get_posting(1).amount_in_currency(&sterling).unwrap(), Money::from_str("£100").unwrap());
        assert_eq!(entry.get_posting(2).amount_in_currency(&sterling).unwrap(), Money::from_str("-£150").unwrap());

        let (_, entry) = parse_postings(
            " Assets:A1  -$100 @@ £100.00\n \
             Assets:A2  $0.05 @@ £0.05\n \
             Assets:A3  £99.95",
        );
        let mut trans_list = entry.transactions().unwrap();
        let trans = trans_list.get_mut(0).unwrap();
        assert_eq!(trans.derive_valuation(&sterling), true);
        assert_eq!(entry.get_posting(0).amount_in_currency(&sterling).unwrap(), Money::from_str("-£100").unwrap());
        assert_eq!(entry.get_posting(1).amount_in_currency(&sterling).unwrap(), Money::from_str("£0.05").unwrap());
        assert_eq!(entry.get_posting(2).amount_in_currency(&sterling).unwrap(), Money::from_str("£99.95").unwrap());

        let (_, entry) = parse_postings(
            " Assets:A1  $100.00 @@ £100.00\n \
             Assets:A2  -£101.00\n \
             Assets:A3  £1.00",
        );
        let mut trans_list = entry.transactions().unwrap();
        let trans = trans_list.get_mut(0).unwrap();
        assert_eq!(trans.derive_valuation(&sterling), true);
        assert_eq!(entry.get_posting(0).amount_in_currency(&sterling).unwrap(), Money::from_str("£100").unwrap());
        assert_eq!(entry.get_posting(1).amount_in_currency(&sterling).unwrap(), Money::from_str("-£101").unwrap());
        assert_eq!(entry.get_posting(2).amount_in_currency(&sterling).unwrap(), Money::from_str("£1").unwrap());*/
    }

    /*
    #[test]
    fn test_elements() {
        // Simple Transfer is untouched
        let entry_str = "2010-12-31 Desc \n \
                         Assets:A1  £10\n \
                         Assets:A2  -£10";
        let entry: JournalEntry = entry_str.parse().unwrap();
        let trans: Vec<Transaction> = entry.transactions().into_iter().flat_map(|t| t.elements()).collect();
        assert_eq!(trans.len(), 1);
        assert_eq!(trans[0].source().total_amount(), "-£10".parse::<Money>().unwrap());
        assert_eq!(trans[0].target().total_amount(), "£10".parse::<Money>().unwrap());

        // Trade with expense from source
        let entry_str = "2010-12-31 Desc \n \
                         Assets:A1  $10\n \
                         Assets:A2  -£11\n \
                         Expenses:E1  £1";
        let entry: JournalEntry = entry_str.parse().unwrap();
        let trans: Vec<Transaction> = entry.transactions().into_iter().flat_map(|t| t.elements()).collect();
        assert_eq!(trans.len(), 2);
        assert_eq!(trans[0].source().total_amount(), "-£10".parse::<Money>().unwrap());
        assert_eq!(trans[0].target().total_amount(), "$10".parse::<Money>().unwrap());
        assert_eq!(trans[1].source().total_amount(), "-£1".parse::<Money>().unwrap());
        assert_eq!(trans[1].target().total_amount(), "£1".parse::<Money>().unwrap());

        // Trade with expense from target
        let entry_str = "2010-12-31 Desc \n \
                         Assets:A1  $9\n \
                         Assets:A2  -£10\n \
                         Expenses:E1  $1";
        let entry: JournalEntry = entry_str.parse().unwrap();
        let trans: Vec<Transaction> = entry.transactions().into_iter().flat_map(|t| t.elements()).collect();
        assert_eq!(trans.len(), 2);
        assert_eq!(trans[0].source().total_amount(), "-£10".parse::<Money>().unwrap());
        assert_eq!(trans[0].target().total_amount(), "$10".parse::<Money>().unwrap());
        assert_eq!(trans[1].source().total_amount(), "-$1".parse::<Money>().unwrap());
        assert_eq!(trans[1].target().total_amount(), "$1".parse::<Money>().unwrap());

        // Trade with liability
        let entry_str = "2010-12-31 Desc \n \
                         Liability:A1  $9\n \
                         Assets:A2  -£10\n \
                         Expenses:E1  $1";
        let entry: JournalEntry = entry_str.parse().unwrap();
        let trans: Vec<Transaction> = entry.transactions().into_iter().flat_map(|t| t.elements()).collect();
        assert_eq!(trans.len(), 2);
        assert_eq!(trans[0].source().total_amount(), "-£10".parse::<Money>().unwrap());
        assert_eq!(trans[0].target().total_amount(), "$10".parse::<Money>().unwrap());
        assert_eq!(trans[1].source().total_amount(), "-$1".parse::<Money>().unwrap());
        assert_eq!(trans[1].target().total_amount(), "$1".parse::<Money>().unwrap());
    }*/
}
