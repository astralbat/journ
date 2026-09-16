/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use journ_core::account::AccountType;
use journ_core::amount::Amount;
use journ_core::journal_entry_flow::{Flow, FlowVec, Flows};
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
}
