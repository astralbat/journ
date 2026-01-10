/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjustment;
use crate::adjustment::{Adjustment, AmountAdjustment};
use crate::deal_group::DealGroup;
use crate::deal_holding::DealHolding;
use crate::pool::PoolBalance;
use crate::pool_event::{MatchDetails, PoolEvent, PoolEventKind};
use itertools::Itertools;
use journ_core::configuration::Configuration;
use journ_core::datetime::JDateTime;
use journ_core::error::JournResult;
use journ_core::eval_identifier;
use journ_core::journal::Journal;
use journ_core::report::expr::{ColumnValue, EvalContext, IdentifierContext, ValuerContext};
use journ_core::valued_amount::ValuedAmount;
use journ_core::valuer::{SystemValuer, Valuer};
use smartstring::alias::String as SS;
use std::collections::HashMap;

pub struct CagContext<'h, 'e> {
    journal: &'e Journal<'h>,
    event: &'e PoolEvent<'h>,
    variables: HashMap<SS, ColumnValue<'h>>,
}

impl<'h, 'e> CagContext<'h, 'e> {
    pub fn new(journal: &'e Journal<'h>, event: &'e PoolEvent<'h>) -> Self {
        CagContext { journal, event, variables: HashMap::new() }
    }
}

impl<'h, 'e> EvalContext<'h> for CagContext<'h, 'e> {
    fn config(&self) -> &Configuration<'h> {
        self.journal.config()
    }

    fn as_valuer_context(&self) -> Option<&dyn ValuerContext<'h>> {
        Some(self)
    }

    fn as_valuer_context_mut(&mut self) -> Option<&mut dyn ValuerContext<'h>> {
        Some(self)
    }
}

impl<'h> ValuerContext<'h> for CagContext<'h, '_> {
    fn valuer<'a>(&'a self, datetime: Option<JDateTime>) -> JournResult<Box<dyn Valuer<'h> + 'a>> {
        let sys_valuer = match datetime {
            Some(datetime) => SystemValuer::on_date(self.journal.config().clone(), datetime),
            None => SystemValuer::on_date(
                self.journal.config().clone(),
                self.event.event_datetime().start(),
            ),
        };
        Ok(Box::new(sys_valuer))
    }
}

impl<'h, 'e> IdentifierContext<'h> for CagContext<'h, 'e>
where
    'h: 'e,
{
    fn variables(&self) -> &HashMap<SS, ColumnValue<'h>> {
        &self.variables
    }

    fn variables_mut(&mut self) -> &mut HashMap<SS, ColumnValue<'h>> {
        &mut self.variables
    }

    fn eval_identifier(&self, identifier: &str) -> Option<ColumnValue<'h>> {
        use EventObj::*;
        let res = eval_identifier!(identifier, EventObj<'h, 'e>,
            "eventDate" => Value(ColumnValue::DatetimeRange(self.event.event_datetime())),
            "dealDate" => Value(ColumnValue::DatetimeRange(self.event.deal_datetime())),
            "description" => Value(ColumnValue::String(self.event.description().iter().join(",").into())),
            "unit" => Value(ColumnValue::Unit(self.event.unit())),
            "pool" => Pool(self.event),
            "acquired" => {
                match self.event.acquired() {
                    Some(acquired) => CagValuedAmount(acquired),
                    None => Value(ColumnValue::Undefined)
                }
            },
            "disposed" => {
                match self.event.disposed() {
                    Some(disposed) => CagValuedAmount(disposed.abs()),
                    None => Value(ColumnValue::Undefined)
                }
            },
            "expenses" => {
                match self.event.expenses() {
                    Some(expenses) => Value(ColumnValue::Amount(expenses)),
                    None => Value(ColumnValue::Undefined)
                }
            },
            "netProceeds" => {
                match self.event.net_proceeds() {
                    Some(net_proceeds) => Value(ColumnValue::Amount(net_proceeds)),
                    None => Value(ColumnValue::Undefined)
                }
            },
            "actualCost" => match self.event.actual_cost() {
                Some(ac) => Value(ColumnValue::Amount(ac)),
                None => Value(ColumnValue::Undefined)
            },
            "type" => {
                match self.event.kind() {
                    PoolEventKind::PooledDeal(_) => Value(ColumnValue::String("Pooled".into())),
                    PoolEventKind::MovedDeal(_, _) => Value(ColumnValue::String("Moved".into())),
                    PoolEventKind::Match(_) => Value(ColumnValue::String("Matched".into())),
                    PoolEventKind::Adjustment(_) => Value(ColumnValue::String("Adjusted".into()))
                }
            },
            "pooled" => {
                match self.event.kind() {
                    PoolEventKind::PooledDeal(dh) | PoolEventKind::MovedDeal(dh, _) => DealHolding(dh),
                    _ => Value(ColumnValue::Undefined)
                }
            },
            "match" => {
                match self.event.kind() {
                    PoolEventKind::Match(details) => Match(details),
                    _ => Value(ColumnValue::Undefined)
                }
            },
            "adjustment" => {
                match self.event.kind() {
                    PoolEventKind::Adjustment(adj) => Adjustment(adj),
                    _ => Value(ColumnValue::Undefined)
                }
            }
        );

        if res.is_none() {
            if identifier.starts_with('+') {
                let md_values = self.event.metadata_by_key(&identifier[1..]);
                if !md_values.is_empty() {
                    Some(ColumnValue::List(
                        self.event
                            .metadata_by_key(&identifier[1..])
                            .iter()
                            .filter_map(|m| m.value().map(|v| ColumnValue::String(v.trim().into())))
                            .collect(),
                    ))
                } else {
                    Some(ColumnValue::Undefined)
                }
            } else {
                self.variables().get(identifier.to_lowercase().as_str()).cloned()
            }
        } else {
            res
        }
    }
}

pub enum EventObj<'h, 'e> {
    Pool(&'e PoolEvent<'h>),
    PoolBalance(&'e PoolBalance<'h>),
    DealHolding(&'e DealHolding<'h>),
    DealGroup(&'e DealGroup<'h>),
    Match(&'e MatchDetails<'h>),
    Adjustment(&'e Adjustment<'h>),
    AmountAdjustment(&'e AmountAdjustment<'h>),
    CagValuedAmount(ValuedAmount<'h>),
    Value(ColumnValue<'h>),
}
impl<'h, 'e> EventObj<'h, 'e> {
    pub fn eval_identifier(self, identifier: &str) -> Option<ColumnValue<'h>> {
        use ColumnValue::*;
        use EventObj::*;

        if let Value(cv) = self {
            return cv.eval_identifier(identifier);
        }

        eval_identifier!(identifier, EventObj<'h, 'e>, self,
            PoolBalance(bal) if "amount" => Value(Amount(bal.amount())),
            PoolBalance(bal) if "cost" => Value(Amount(bal.cost())),
            PoolBalance(bal) => Value(ValuedAmount(bal.valued_amount().clone())),
            CagValuedAmount(bal) if "amount" => Value(Amount(bal.amount())),
            CagValuedAmount(bal) if "cost" => Value(Amount(bal.valuations().next().unwrap().value())),
            Pool(event) if "" => Value(String(event.pool_name().into())),
            Pool(event) if "name" => Value(String(event.pool_name().into())),
            Pool(event) if "balanceBefore" => PoolBalance(event.balance_before()),
            Pool(event) if "balanceAfter" => PoolBalance(event.balance_after()),
            Match(details) if "buySide" => if details.target().total().amount().is_positive() {
                Value(String("target".into()))
            } else {
                Value(String("source".into()))
            },
            Match(details) if "sellSide" => if details.target().total().amount().is_positive() {
                Value(String("source".into()))
            } else {
                Value(String("target".into()))
            },
            Match(details) if "source" => DealHolding(details.originator()),
            Match(details) if "target" => DealHolding(details.target()),
            Match(details) if "gain" => Value(Amount(details.gain())),
            DealGroup(group) if "parent" => {
                    match group.split_parent() {
                        Some(parent) => DealGroup(parent),
                        None => return Some(Undefined),
                    }
            },
            DealGroup(group) if "total" => PoolBalance(group.total()),
            DealGroup(group) if "expenses" => Value(Amount(group.expenses().amount())),
            DealGroup(group) if "totalBeforeExpenses" => CagValuedAmount(group.total_before_expenses().clone()),
            DealGroup(group) if "date" => Value(DatetimeRange(group.datetime())),
            DealGroup(group) if "remainder" => Value(Boolean({
                match group.split_parent() {
                    Some(parent) => parent.total().amount() != group.total().amount(),
                    None => false
                }
            })),
            DealGroup(group) if "" => PoolBalance(group.total()),
            DealHolding(holding) if "" => PoolBalance(holding.total()),
            DealHolding(holding) if "parent" => {
                match holding.split_parent() {
                    Some(parent) => DealGroup(parent),
                    None => return Some(Undefined),
                }
            },
            DealHolding(holding) if "total" => PoolBalance(holding.total()),
            DealHolding(holding) if "expenses" => Value(Amount(holding.expenses().amount())),
            DealHolding(holding) if "totalBeforeExpenses" => CagValuedAmount(holding.total_before_expenses()),
            DealHolding(holding) if "date" => Value(DatetimeRange(holding.datetime())),
            DealHolding(holding) if "remainder" => Value(Boolean({
                match holding.split_parent() {
                    Some(parent) => parent.total().amount() != holding.total().amount(),
                    None => false
                }
            })),
            Adjustment(adj) if "unit" => AmountAdjustment(&adj.amount_adjustments()[0]),
            Adjustment(adj) if "cost" => match adj.amount_adjustments().get(1) {
                Some(amnt_adj) => AmountAdjustment(amnt_adj),
                None => return Some(Undefined)
            },
            AmountAdjustment(amnt_adj) if "amount" => Value(Amount(amnt_adj.amount().abs())),
            AmountAdjustment(amnt_adj) if "op" => match amnt_adj {
                adjustment::AmountAdjustment::Add(a) if a.is_positive() => Value(String("+".into())),
                adjustment::AmountAdjustment::Add(_) => Value(String("-".into())),
                adjustment::AmountAdjustment::Scale(_) => Value(String("*".into())),
                adjustment::AmountAdjustment::Set(_) => Value(String("=".into())),
            }
        )
    }
}
