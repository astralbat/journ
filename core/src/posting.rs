/*
 * Copyright (c) 2019-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::account::Account;
use crate::alloc::HerdAllocator;
use crate::amount::{Amount, AmountExpr};
use crate::journal_entry::EntryId;
use crate::unit::Unit;
use crate::valued_amount::{Valuation, ValuedAmount};
use std::cell::Cell;
use std::fmt;
use std::sync::{Arc, OnceLock};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PostingId<'h> {
    entry_id: EntryId<'h>,
    id: u32,
}

impl<'h> PostingId<'h> {
    pub fn allocate(entry_id: EntryId<'h>) -> PostingId<'h> {
        thread_local! {
            static POSTING_COUNTER: Cell<u32> = Cell::new(0);
        }
        let new_id = POSTING_COUNTER.with(|k| {
            let prev_id = k.get();
            k.set(prev_id + 1);
            prev_id + 1
        });
        PostingId { entry_id, id: new_id }
    }
    pub fn entry_id(&self) -> EntryId<'h> {
        self.entry_id
    }
}

#[derive(Debug, Clone)]
pub struct Posting<'h> {
    posting_id: OnceLock<PostingId<'h>>,
    // Account spacing prefix.
    account_spacing: &'h str,
    account: Arc<Account<'h>>,
    valued_amount: ValuedAmount<'h>,
    balance_assertion: Option<AmountExpr<'h>>,
    comment: Option<&'h str>,
    amount_elided: bool, // If the amount was elided
}

impl<'h> Posting<'h> {
    pub fn new(
        account_spacing: &'h str,
        account: Arc<Account<'h>>,
        valued_amount: ValuedAmount<'h>,
        balance_assertion: Option<AmountExpr<'h>>,
        comment: Option<&'h str>,
    ) -> Self {
        Self {
            posting_id: OnceLock::new(),
            amount_elided: valued_amount.is_nil(),
            account_spacing,
            account,
            valued_amount,
            balance_assertion,
            comment,
        }
    }

    pub(crate) fn set_entry_id(&mut self, entry_id: EntryId<'h>) {
        self.posting_id.set(PostingId::allocate(entry_id)).expect("Entry ID has already been set");
    }

    pub fn id(&self) -> PostingId<'h> {
        *self.posting_id.get().expect("Entry ID has not been set")
    }

    pub fn leading_whitespace(&self) -> &'h str {
        self.account_spacing
    }

    pub fn set_leading_whitespace(&mut self, spacing: &'h str) {
        self.account_spacing = spacing;
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
            self.valued_amount =
                ValuedAmount::new_in(AmountExpr::new(amount, " ", None), allocator);
        }
        self.valued_amount.set_amount(amount);
        self.amount_elided = elided;
    }

    /// Gets the balance assertion if one was set. (<Account> <Amount> = <assertion>)
    pub fn balance_assertion(&self) -> Option<Amount<'h>> {
        self.balance_assertion.as_ref().map(|a| **a)
    }

    pub fn valuations(&self) -> impl Iterator<Item = &Valuation<'h>> + Clone {
        debug_assert!(!self.valued_amount.is_nil(), "Amount not set; set amount first");

        self.valued_amount.valuations()
    }

    /// Adds or replaces an existing valuation on this posting with the same unit. The unit of the valuation specified must not
    /// be the same as the amount's unit.
    pub fn set_valuation(&mut self, val: Valuation<'h>) {
        debug_assert!(!self.valued_amount.is_nil(), "Amount not set; set amount first");

        self.valued_amount.set_valuation(val)
    }

    pub fn value_units(&self) -> impl Iterator<Item = &'h Unit<'h>> + '_ {
        debug_assert!(!self.valued_amount.is_nil(), "Amount not set; set amount first");

        self.valued_amount.valuations().map(|v| v.unit())
    }

    /// Tries to get the amount in the specified unit. This is either going to be the amount
    /// itself, or the unit/total price.
    /// The returned value will be signed according to the amount's sign.
    pub fn amount_in(&self, in_curr: &'h Unit<'h>) -> Option<Amount<'h>> {
        debug_assert!(!self.valued_amount.is_nil(), "Amount not set; set amount first");

        self.valued_amount.value_in(in_curr)
    }

    pub fn comment(&self) -> Option<&'h str> {
        self.comment
    }

    pub fn matches_account_filter(&self, filter: &str) -> bool {
        self.account.name().contains(filter)
    }

    pub fn is_debit(&self) -> bool {
        debug_assert!(!self.valued_amount.is_nil(), "Amount not set; set amount first");

        self.amount() > 0
    }

    pub fn is_credit(&self) -> bool {
        debug_assert!(!self.valued_amount.is_nil(), "Amount not set; set amount first");

        self.amount() < 0
    }

    /*
    pub fn clone_in<A2: Allocator>(&self, allocator: A2) -> Posting<'h, A2> {
        Posting {
            posting_id: OnceLock::new(),
            account_spacing: self.account_spacing,
            account: self.account.clone(),
            valued_amount: self.valued_amount.clone(),
            balance_assertion: self.balance_assertion.clone(),
            comment: self.comment,
            amount_elided: self.amount_elided,
        }
    }*/

    pub fn write<W: fmt::Write>(&self, w: &mut W, include_elided: bool) -> fmt::Result {
        write!(w, "{}{}", self.account_spacing, self.account)?;
        if !self.valued_amount.is_nil() && (include_elided || !self.amount_elided) {
            self.valued_amount.write(w, include_elided)?;
        }
        if let Some(ba) = &self.balance_assertion {
            write!(w, "{ba}")?;
        }
        if let Some(comment) = &self.comment {
            write!(w, "{comment}")?;
        }

        Ok(())
    }
}

/// Two postings are equal when their accounts and currencies are equal
impl PartialEq for Posting<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.account == other.account && self.amount().unit() == other.amount().unit()
    }
}

impl Eq for Posting<'_> {}

impl fmt::Display for Posting<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.write(f, false)
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
