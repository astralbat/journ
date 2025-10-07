/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::expr::aggregation::AggState;
use crate::expr::context::{EvalContext, IdentifierContext, PostingContext};
use crate::expr::{ColumnValue, Expr};
use journ_core::amount::Amount;
use journ_core::configuration::{AccountFilter, Filter};
use journ_core::err;
use journ_core::error::{JournError, JournResult};
use journ_core::journal_entry::EntryId;
use smallvec::SmallVec;

#[derive(Debug, PartialEq)]
pub struct CoSum<'h> {
    args: Vec<Expr<'h>>,
    account_filter: Option<AccountFilter>,
    totals: SmallVec<[Amount<'h>; 2]>,
    completed_entries: Vec<EntryId<'h>>,
}
impl<'h> CoSum<'h> {
    pub fn new(args: Vec<Expr<'h>>) -> JournResult<Self> {
        Ok(Self {
            args,
            account_filter: None,
            totals: SmallVec::new(),
            completed_entries: Vec::new(),
        })
    }
}

impl<'h, 'a> AggState<'h> for CoSum<'h> {
    fn add(&mut self, context: &mut dyn IdentifierContext<'h>) -> JournResult<()> {
        // Initialize the account filter on the first call to add()
        if self.account_filter.is_none() {
            let accounts = self.args.iter().map(|arg| arg.eval(context)?.into_string().ok_or(err!("cosum() requires arguments of type `String`, representing account patterns to match"))).collect::<Result<Vec<_>, _>>()?;
            self.account_filter = Some(AccountFilter::new(accounts.iter()));
        }
        let account_filter = self.account_filter.as_ref().unwrap();

        match context.as_posting_context() {
            Some(context) => {
                if self.completed_entries.contains(&context.entry().id()) {
                    return Ok(());
                }
                for pst in context.entry().postings() {
                    if account_filter.is_included(pst.account()) {
                        self.totals += pst.amount();
                    }
                }
                self.completed_entries.push(context.entry().id());
                Ok(())
            }
            None => Err(err!("Cosum() is not supported in this context")),
        }
    }

    fn merge(&mut self, other: &dyn AggState<'h>) -> JournResult<()> {
        let b = other.finalize();

        b.as_list().iter().try_fold(&mut self.totals, |mut acc, v| {
            let amount =
                v.as_amount().ok_or_else(|| err!("CoSum() can only sum `Amount` types"))?;
            if !amount.is_nil() {
                *acc += amount;
            }
            Ok::<_, JournError>(acc)
        })?;
        Ok(())
    }

    fn finalize(&self) -> ColumnValue<'h> {
        match self.totals.len() {
            0 => ColumnValue::Amount(Amount::nil()),
            1 => ColumnValue::Amount(self.totals[0]),
            _ => ColumnValue::List(self.totals.iter().map(|a| ColumnValue::Amount(*a)).collect()),
        }
    }
}
