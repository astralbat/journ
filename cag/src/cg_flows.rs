/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::deal::Deal;
use journ_core::account::{Account, AccountType};
use journ_core::amount::Amount;
use journ_core::error::JournResult;
use journ_core::journal_entry::JournalEntry;
use journ_core::journal_entry_flow::{Flow, FlowVec, Flows};
use journ_core::metadata::Metadata;
use journ_core::unit::Unit;
use smallvec::{SmallVec, smallvec};
use std::marker::PhantomData;

pub struct CgFlows<'h, F: Flows<'h>> {
    flows: F,
    phantom: PhantomData<&'h ()>,
}

impl<'h, F: Flows<'h>> CgFlows<'h, F> {
    pub fn new(flows: F) -> Self {
        Self { flows, phantom: PhantomData }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Flow<'h>> {
        self.flows.as_slice().iter()
    }

    /// Creates a series of sub flows for each unit within, where each sub flow
    /// is homogenous.
    pub fn by_unit(&self, unit: &Unit<'h>) -> CgFlows<'h, FlowVec<'h>> {
        let mut unit_group = smallvec![];

        for flow in self.flows.as_slice().iter() {
            if flow.unit() == unit {
                unit_group.push(flow.clone());
            }
        }
        CgFlows::new(unit_group)
    }

    pub fn equity_flows(&self) -> CgFlows<'h, SmallVec<[Flow<'h>; 4]>> {
        CgFlows::new(
            self.flows
                .as_slice()
                .iter()
                .filter(|f| {
                    matches!(
                        f.account_type(),
                        Some(AccountType::Asset)
                            | Some(AccountType::Liability)
                            | Some(AccountType::Equity)
                    )
                })
                .cloned()
                .collect(),
        )
    }

    /// Divides the flows in to those which are transferred, and those which have been exchanged.
    fn transfer_exchange_flows(&self) -> (CgFlows<'h, FlowVec<'h>>, CgFlows<'h, FlowVec<'h>>) {
        if !self.flows.is_homogenous() {
            panic!("Deals can only be created from homogenous flows");
        }

        let credit_flows = self.flows.credits();
        let debit_flows = self.flows.debits();

        // The amount transferred is the common (min) amount between credits and debits
        // It is an abs() value, showing the side transferred from the debit side.
        let transferred_amount = credit_flows
            .sum()
            .map(|s| s.amount().abs())
            .unwrap_or(Amount::nil())
            .min(debit_flows.sum().map(|s| s.amount()).unwrap_or(Amount::nil()));

        let (mut taken, remainder) = self.flows.take_amount(transferred_amount);
        let (mut credits_taken, remainder) = remainder.take_amount(-transferred_amount);
        taken.append(&mut credits_taken);

        (CgFlows::new(taken), CgFlows::new(remainder))
    }

    /// Creates deals for changes in equity. They won't have any expenses at this point - they need to be added
    /// later.
    ///
    /// # Panics
    /// If the flows are not homogenous.
    pub fn create_deals(
        &self,
        entry: &'h JournalEntry<'h>,
        unit_of_account: &'h Unit<'h>,
    ) -> JournResult<SmallVec<[Deal<'h>; 2]>> {
        if !self.flows.is_homogenous() {
            panic!("Deals can only be created from homogenous flows");
        }
        if self.flows.is_empty() {
            return Ok(smallvec![]);
        }

        let md_lines = |account: &Account<'h>, key: &str| {
            let mut all_md = smallvec![];
            all_md.extend(entry.metadata().cloned());
            all_md.extend(
                account
                    .metadata_by_key(key)
                    .last()
                    .copied()
                    .map(|md| md.value_as_metadata_lines())
                    .unwrap_or_default(),
            );
            all_md.append(
                &mut entry
                    .metadata_by_key(key)
                    .last()
                    .copied()
                    .map(|md| md.value_as_metadata_lines())
                    .unwrap_or_default(),
            );
            all_md.extend(account.metadata().filter(|md| md.key().is_match("CAG-..")).cloned());
            all_md
        };

        let equity_flows = self.equity_flows();
        let (transfer_flows, exchange_flows) = equity_flows.transfer_exchange_flows();

        let mut deals = smallvec![];

        // Ordinarily, we do not create deals from transfers. However, they can be forced.
        for flow in transfer_flows.flows {
            let md = if flow.is_debit() {
                md_lines(flow.account_root().unwrap(), "CAG-OnTransferTo")
            } else {
                md_lines(flow.account_root().unwrap(), "CAG-OnTransferFrom")
            };
            if md.iter().any(|l: &Metadata| l.key() == "CAG-ForceDeal") {
                let valued_amount = flow.valued_amount().clone();
                deals.push(Deal::new(
                    None,
                    entry,
                    md,
                    valued_amount,
                    None,
                    false,
                    unit_of_account,
                )?);
            }
        }

        // We do create deals automatically for exchanges unless the account mentions not to
        for flow in exchange_flows.flows {
            let md = if flow.is_debit() {
                md_lines(flow.account_root().unwrap(), "CAG-OnAcquire")
            } else {
                md_lines(flow.account_root().unwrap(), "CAG-OnDispose")
            };
            if !md.iter().any(|l| l.key() == "CAG-NoDeal") {
                let valued_amount = flow.valued_amount().clone();
                if valued_amount.unit() != unit_of_account {
                    deals.push(Deal::new(
                        None,
                        entry,
                        md,
                        valued_amount,
                        None,
                        false,
                        unit_of_account,
                    )?);
                }
            }
        }
        Ok(deals)
    }
}
