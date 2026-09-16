/*
 * Copyright (c) 2025-2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjusted_value::AdjustedValue;
use crate::adjustment;
use crate::adjustment::{Adjustment, AmountAdjustment};
use crate::holding::{DealHolding, DealHoldingSummary};
use crate::pool_event::{MatchDetails, PoolEvent, PoolEventKind};
use journ_core::datetime::JDateTime;
use journ_core::error::JournResult;
use journ_core::eval_identifier;
use journ_core::journal_context::JContext;
use journ_core::report::expr::{ColumnValue, EvalContext, IdentifierContext, ValuerContext};
use journ_core::valuer::{SystemValuer, Valuer};
use smartstring::alias::String as SS;
use std::collections::HashMap;

pub struct CagContext<'h, 'e> {
    event: &'e PoolEvent<'h>,
    variables: HashMap<SS, ColumnValue<'h>>,
}

impl<'h, 'e> CagContext<'h, 'e> {
    pub fn new(event: &'e PoolEvent<'h>) -> Self {
        CagContext { event, variables: HashMap::new() }
    }
}

impl<'h, 'a, 'e> EvalContext<'h, 'a> for CagContext<'h, 'e> {
    fn as_valuer_context(&self) -> Option<&dyn ValuerContext<'h, 'a>> {
        Some(self)
    }

    fn as_valuer_context_mut(&mut self) -> Option<&mut dyn ValuerContext<'h, 'a>> {
        Some(self)
    }
}

impl<'h, 'p> ValuerContext<'h, 'p> for CagContext<'h, '_> {
    fn valuer<'a>(&'a self, datetime: Option<JDateTime>) -> JournResult<Box<dyn Valuer<'h> + 'a>>
    where
        'h: 'a,
    {
        let sys_valuer = match datetime {
            Some(datetime) => {
                SystemValuer::on_date(JContext::get().journal().config().clone(), datetime)
            }
            None => SystemValuer::on_date(
                JContext::get().journal().config().clone(),
                self.event.event_datetime().start(),
            ),
        };
        Ok(Box::new(sys_valuer))
    }
}

impl<'h, 'a, 'e> IdentifierContext<'h, 'a> for CagContext<'h, 'e>
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
            "description" => Value(ColumnValue::Description(self.event.description())),
            "unit" => Value(ColumnValue::Unit(self.event.unit())),
            "amount" => {
                Value(ColumnValue::Amount(self.event.balance_after().amount() - self.event.balance_before().amount(), true))
            },
            "pooledAmount" => Value(self.event.original_amount().map(|a| ColumnValue::Amount(a, true)).unwrap_or(ColumnValue::Undefined)),
            "cost" => {
                Value(ColumnValue::Amount(self.event.balance_after().value() - self.event.balance_before().value(), false))
            },
            "pool" => Pool(self.event),
            "acquired" => {
                match self.event.acquired() {
                    Some(acquired) => AdjustedValue(acquired),
                    None => Value(ColumnValue::Undefined)
                }
            },
            "disposed" => {
                self.event.disposed().map(AdjustedValue).unwrap_or(Value(ColumnValue::Undefined))
            },
            "consideration" => {
                match self.event.consideration() {
                    Some(consideration) => Value(ColumnValue::Amount(consideration, false)),
                    None => Value(ColumnValue::Undefined)
                }
            },
            "expenses" => {
                match self.event.expenses() {
                    Some(expenses) => Value(ColumnValue::Amount(expenses, false)),
                    None => Value(ColumnValue::Undefined)
                }
            },
            "netProceeds" => {
                match self.event.net_proceeds() {
                    Some(net_proceeds) => Value(ColumnValue::Amount(net_proceeds, false)),
                    None => Value(ColumnValue::Undefined)
                }
            },
            "actualCost" => match self.event.actual_cost() {
                Some(ac) => Value(ColumnValue::Amount(ac, false)),
                None => Value(ColumnValue::Undefined)
            },
            "type" => {
                match self.event.kind() {
                    PoolEventKind::PooledDeal(_) => Value(ColumnValue::String("Pooled".into())),
                    PoolEventKind::MovedFrom(_, _) => Value(ColumnValue::String("MovedFrom".into())),
                    //PoolEventKind::MatchedFrom(..) => Value(ColumnValue::String("MatchedFrom".into())),
                    PoolEventKind::MovedTo(_, _) => Value(ColumnValue::String("MovedTo".into())),
                    PoolEventKind::Match(_) => Value(ColumnValue::String("Matched".into())),
                    PoolEventKind::Adjustment(_) => Value(ColumnValue::String("Adjusted".into()))
                }
            },
            "pooled" => {
                match self.event.kind() {
                    PoolEventKind::PooledDeal(dh) | PoolEventKind::MovedTo(dh, _) => DealHoldingSummary(dh),
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

        res.or_else(|| {
            identifier.strip_prefix('+').map(|key| {
                let md_values = self.event.entry_metadata_by_key(key);
                if !md_values.is_empty() {
                    self.event
                        .entry_metadata_by_key(key)
                        .iter()
                        .map(|m| {
                            m.value()
                                .map(|v| ColumnValue::String(v.into()))
                                .unwrap_or_else(|| ColumnValue::String(SS::new()))
                        })
                        .collect()
                } else {
                    ColumnValue::Undefined
                }
            })
        })
        .or_else(|| self.variables().get(identifier.to_lowercase().as_str()).cloned())
    }
}

pub enum EventObj<'h, 'e> {
    Pool(&'e PoolEvent<'h>),
    DealHolding(&'e DealHolding<'h>),
    DealHoldingSummary(&'e DealHoldingSummary<'h>),
    Match(&'e MatchDetails<'h>),
    Adjustment(&'e Adjustment<'h>),
    AmountAdjustment(&'e AmountAdjustment<'h>),
    AdjustedValue(AdjustedValue<'h>),
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
            AdjustedValue(ref bal) if "" => Value(ValuedAmount(bal.as_valued_amount())),
            AdjustedValue(ref bal) if "amount" => Value(Amount(bal.amount(), true)),
            AdjustedValue(ref bal) if "adjCost" => Value(Amount(bal.value(), false)),
            AdjustedValue(ref bal) if "expenses" => Value(Amount(bal.expenses(), false)),
            AdjustedValue(ref bal) if "consideration" => Value(Amount(bal.consideration(), false)),
            Pool(event) if "" => Value(String(event.pool_name().into())),
            Pool(event) if "name" => Value(String(event.pool_name().into())),
            Pool(event) if "balanceBefore" => AdjustedValue(event.balance_before()),
            Pool(event) if "balanceAfter" => AdjustedValue(event.balance_after()),
            Match(details) if "buy" => {
                if details.target().amount().is_positive() {
                    DealHolding(details.target())
                } else {
                    DealHolding(details.originator())
                }
            },
            Match(details) if "buySide" => if details.target().amount().is_positive() {
                Value(String("target".into()))
            } else {
                Value(String("source".into()))
            },
            Match(details) if "sell" => {
                if details.target().amount().is_positive() {
                    DealHolding(details.originator())
                } else {
                    DealHolding(details.target())
                }
            },
            Match(details) if "sellSide" => if details.target().amount().is_positive() {
                Value(String("source".into()))
            } else {
                Value(String("target".into()))
            },
            Match(details) if "source" => DealHolding(details.originator()),
            Match(details) if "target" => DealHolding(details.target()),
            Match(details) if "gain" => Value(Amount(details.gain(), false)),
            DealHoldingSummary(summary) if "parent" => {
                    match summary.parent() {
                        Some(parent) => DealHoldingSummary(parent),
                        None => return Some(Undefined),
                    }
            },
            DealHoldingSummary(group) if "total" => AdjustedValue(group.adjusted_value()),
            DealHoldingSummary(group) if "expenses" => Value(Amount(group.expenses(), false)),
            DealHoldingSummary(group) if "date" => Value(DatetimeRange(group.datetime())),
            DealHoldingSummary(group) if "remainder" => Value(Boolean({
                match group.parent() {
                    Some(parent) => parent.amount() != group.amount(),
                    None => false
                }
            })),
            DealHoldingSummary(group) if "" => AdjustedValue(group.adjusted_value()),
            DealHolding(holding) if "" => AdjustedValue(holding.adjusted_value()),
            DealHolding(holding) if "parent" => {
                match holding.split_parent() {
                    Some(parent) => DealHoldingSummary(parent),
                    None => return Some(Undefined),
                }
            },
            DealHolding(holding) if "total" => AdjustedValue(holding.adjusted_value()),
            DealHolding(holding) if "expenses" => Value(Amount(holding.expenses(), false)),
            DealHolding(holding) if "date" => Value(DatetimeRange(holding.datetime())),
            DealHolding(holding) if "remainder" => Value(Boolean({
                match holding.split_parent() {
                    Some(parent) => parent.amount() != holding.amount(),
                    None => false
                }
            })),
            DealHolding(holding) if "file" => {
                let root_node = JContext::get()
                    .journal()
                    .root();
                Value(holding.entries().map(|e| {
                    root_node.find_by_node_id(&e.id().parent().unwrap())
                    .unwrap()
                    .nearest_filename()
                    .map(|p| String(p.to_str().unwrap().into()))
                    .unwrap_or(Undefined)
                }).collect())
            },
            DealHolding(holding) => |ident: &str| {
                ident.strip_prefix('+').map(|key| {
                    let md_values = holding.entry_metadata_by_key(key);
                    if !md_values.is_empty() {
                        Value(
                            md_values
                            .iter()
                            .map(|m| m.value().map(|v| String(v.into())).unwrap_or(String(SS::new())))
                            .collect::<ColumnValue>(),
                        )
                    } else {
                        Value(Undefined)
                    }
                })
            },
            Adjustment(adj) if "unit" => AmountAdjustment(&adj.amount_adjustments()[0]),
            Adjustment(adj) if "cost" => match adj.amount_adjustments().get(1) {
                Some(amnt_adj) => AmountAdjustment(amnt_adj),
                None => return Some(Undefined)
            },
            AmountAdjustment(amnt_adj) if "amount" => Value(Amount(amnt_adj.amount().abs(), true)),
            AmountAdjustment(amnt_adj) if "op" => match amnt_adj {
                adjustment::AmountAdjustment::Add(a) if a.is_positive() => Value(String("+".into())),
                adjustment::AmountAdjustment::Add(_) => Value(String("-".into())),
                adjustment::AmountAdjustment::Scale(_) => Value(String("*".into())),
                adjustment::AmountAdjustment::Set(_) => Value(String("=".into())),
            },
            _ => |_| None
        )
    }
}
