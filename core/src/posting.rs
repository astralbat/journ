/*
 * Copyright (c) 2019-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::account::Account;
use crate::alloc::HerdAllocator;
use crate::amount::Amount;
use crate::configuration::Configuration;
use crate::parsing::text_block::{BlockObject, TextBlock, TextBlockBuf};
use crate::tree_id::TreeId;
use crate::unit::Unit;
use crate::valued_amount::{PostingValuation, ValuedAmount};
use crate::valuer::Valuation;
use std::cell::Cell;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::Write;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

pub type PostingId = TreeId;

#[derive(Debug, Clone)]
pub struct Posting<'h> {
    posting_id: TreeId,
    /// The block from which this posting was parsed, if it was parsed.
    block: Option<&'h TextBlock<'h>>,
    account: Arc<Account<'h>>,
    valued_amount: ValuedAmount<'h>,
    balance_assertion: Option<Amount<'h>>,
    comment: Option<&'h str>,
    amount_elided: bool, // If the amount was elided
}

impl<'h> Posting<'h> {
    pub fn new(
        block: Option<&'h TextBlock<'h>>,
        account: Arc<Account<'h>>,
        valued_amount: ValuedAmount<'h>,
        balance_assertion: Option<Amount<'h>>,
        comment: Option<&'h str>,
    ) -> Self {
        Self {
            posting_id: TreeId::new_root(),
            block,
            amount_elided: valued_amount.is_nil(),
            account,
            valued_amount,
            balance_assertion,
            comment,
        }
    }

    /// Attaches this posting to the entry specified by `entry_id`.
    pub(crate) fn attach(&mut self, entry_id: &TreeId) {
        thread_local! {
            static POSTING_COUNTER: Cell<usize> = Cell::new(1);
        }
        let new_id = POSTING_COUNTER.with(|k| {
            let prev_id = k.get();
            k.set(prev_id + 1);
            prev_id + 1
        });
        self.posting_id = entry_id.branch(new_id);
    }

    /*
    pub(super) fn detach(&mut self) {
        self.posting_id = TreeId::new_root();
    }*/

    pub fn id(&self) -> &TreeId {
        &self.posting_id
    }

    /// The block from which this posting was parsed.
    pub fn block(&self) -> Option<&'h TextBlock<'h>> {
        self.block
    }

    pub fn account(&self) -> &Arc<Account<'h>> {
        &self.account
    }

    pub fn set_account(&mut self, acc: Arc<Account<'h>>) {
        self.account = acc;
    }

    #[allow(non_fmt_panics)]
    pub fn valued_amount(&self) -> &ValuedAmount<'h> {
        debug_assert!(
            !self.valued_amount.is_nil(),
            "Posting amount has not yet been derived: {self}"
        );

        &self.valued_amount
    }

    #[allow(non_fmt_panics)]
    pub fn amount(&self) -> Amount<'h> {
        debug_assert!(
            !self.valued_amount.is_nil(),
            "Posting amount has not yet been derived: {self}"
        );

        self.valued_amount.amount()
    }

    pub fn amounts(&self) -> impl Iterator<Item = Amount<'h>> + '_ {
        debug_assert!(!self.valued_amount.is_nil(), "Amount not set; set amount first");

        self.valued_amount.amounts()
    }

    /// The amount's unit
    pub fn unit(&self) -> &'h Unit<'h> {
        self.amount().unit()
    }

    pub fn has_elided_amount(&self) -> bool {
        self.amount_elided
    }

    pub(crate) fn is_amount_set(&self) -> bool {
        !self.valued_amount.is_nil()
    }

    /// Sets the amount for the posting which may/may not be elided when later syncing to
    /// the file. Usually, only one posting in an entry may be elided.
    pub fn set_amount(
        &mut self,
        amount: Amount<'h>,
        elided: bool,
        allocator: &'h HerdAllocator<'h>,
    ) {
        if self.valued_amount.is_nil() {
            // Todo: Set a good pretext here in case the elision has changed and this gets written out.
            self.valued_amount = ValuedAmount::new_in(amount, allocator);
        }
        self.valued_amount.set_amount(amount);
        self.amount_elided = elided;
    }

    /// Gets the balance assertion if one was set. (<Account> <Amount> = <assertion>)
    pub fn balance_assertion(&self) -> Option<Amount<'h>> {
        self.balance_assertion.as_ref().map(|a| *a)
    }

    pub fn valuations(&self) -> impl Iterator<Item = Valuation<'h>> {
        self.valued_amount.valuations()
    }

    pub fn posting_valuations(&self) -> impl Iterator<Item = &PostingValuation<'h>> + Clone {
        debug_assert!(!self.valued_amount.is_nil(), "Amount not set; set amount first");

        self.valued_amount.posting_valuations()
    }

    /// Adds or replaces an existing valuation on this posting with the same unit. The unit of the valuation specified must not
    /// be the same as the amount's unit.
    pub fn set_valuation(&mut self, val: PostingValuation<'h>) {
        debug_assert!(!self.valued_amount.is_nil(), "Amount not set; set amount first");
        self.valued_amount.set_valuation(val)
    }

    pub fn remove_valuation(&mut self, unit: &Unit<'h>) -> bool {
        self.valued_amount.remove_valuation(unit)
    }

    pub fn value_units(&self) -> impl Iterator<Item = &'h Unit<'h>> + '_ {
        debug_assert!(!self.valued_amount.is_nil(), "Amount not set; set amount first");

        self.valued_amount.posting_valuations().map(|v| v.unit())
    }

    /// Tries to get the amount in the specified unit. This is either going to be the amount
    /// itself, or the unit/total price.
    /// The returned value will be signed according to the amount's sign.
    pub fn amount_in(&self, in_unit: &'h Unit<'h>) -> Option<Amount<'h>> {
        debug_assert!(!self.valued_amount.is_nil(), "Amount not set; set amount first");

        self.valued_amount.value_in(in_unit)
    }

    pub fn comment(&self) -> Option<&'h str> {
        self.comment
    }

    pub fn is_debit(&self) -> bool {
        debug_assert!(!self.valued_amount.is_nil(), "Amount not set; set amount first");

        self.amount() > 0
    }

    pub fn is_credit(&self) -> bool {
        debug_assert!(!self.valued_amount.is_nil(), "Amount not set; set amount first");

        self.amount() < 0
    }

    /// Gets whether this posting contains another. This makes senses when it has the same account, unit
    /// and its amount is >= other's amount. We can think of `other` being 'inside' `self`.
    pub fn contains(&self, other: &'_ Posting<'h>) -> bool {
        self.account == other.account
            && self.unit() == other.unit()
            && self.amount() >= other.amount()
    }
}

impl Hash for Posting<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.account.hash(state);
        self.valued_amount.hash(state);
        self.comment.hash(state);
    }
}

impl PartialEq for Posting<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.account == other.account
            && self.valued_amount == other.valued_amount
            && self.comment == other.comment
    }
}

impl Eq for Posting<'_> {}

impl PartialOrd for Posting<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Posting<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.account
            .cmp(&other.account)
            .then_with(|| self.valued_amount.cmp(&other.valued_amount))
            .then_with(|| self.comment.cmp(&other.comment))
    }
}

impl fmt::Display for Posting<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut buf = TextBlockBuf::new();
        buf.write(self, None);
        write!(f, "{}", buf)
    }
}

impl BlockObject for Posting<'_> {
    fn write(&self, buf: &mut TextBlockBuf, _config: Option<&Configuration<'_>>) {
        write!(buf, "{}", self.account).unwrap();
        if !self.valued_amount.is_nil() && (buf.include_elided() || !self.amount_elided) {
            write!(buf, "  ").unwrap();
            self.valued_amount.write(buf).unwrap();
        }
        if let Some(ba) = &self.balance_assertion {
            write!(buf, "{ba}").unwrap();
        }
        if let Some(comment) = &self.comment {
            write!(buf, "{comment}").unwrap();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_derive_amount() {
        let entry = entry!("2000-01-01 desc\n Acc1  £3.00\n Acc2  £3.00\n  Acc3");
        for pst in entry.postings() {
            if pst.account().name() == "Acc3" {
                assert_eq!(pst.amount(), amount!("-£6.00"));
            }
        }
    }
}
