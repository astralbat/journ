/*
 * Copyright (c) 2021-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjustment::Adjustment;
use crate::deal::Deal;
use crate::deal_group::DealGroup;
use journ_core::date_and_time::JDateTimeRange;
use journ_core::unit::Unit;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, PartialEq, Eq)]
pub enum DealingEvent<'h> {
    Deal(Deal<'h>),
    Group(DealGroup<'h>),
    PoolAdjustment(Adjustment<'h>),
}
impl<'h> DealingEvent<'h> {
    pub fn unit(&self) -> &'h Unit<'h> {
        match self {
            DealingEvent::Deal(deal) => deal.unit(),
            DealingEvent::Group(group) => group.unit(),
            DealingEvent::PoolAdjustment(reorg) => reorg.unit(),
        }
    }

    pub fn datetime(&self) -> JDateTimeRange<'h> {
        match self {
            DealingEvent::Deal(deal) => deal.datetime(),
            DealingEvent::Group(group) => group.datetime(),
            DealingEvent::PoolAdjustment(reorg) => reorg.datetime(),
        }
    }

    /*
    pub fn with_config<R, F: FnOnce(&Configuration<'h>) -> R>(&self, f: F) -> R {
        match self {
            DealingEvent::Deal(deal) => f(deal.entry().config()),
            DealingEvent::Group(group) => f(group.first().entry().config()),
            DealingEvent::PoolAdjustment(pa) => f(pa.entry().config()),
        }
    }*/
}

impl Ord for DealingEvent<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        let self_dt = self.datetime();
        let other_dt = other.datetime();
        (self_dt.start(), self_dt.end()).cmp(&(other_dt.start(), other_dt.end()))
    }
}

impl PartialOrd for DealingEvent<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for DealingEvent<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DealingEvent::Deal(deal) => {
                write!(f, "{}..{}: {}", deal.datetime().start(), deal.datetime().end(), deal)
            }
            DealingEvent::Group(group) => {
                write!(f, "{}..{}: {}", group.datetime().start(), group.datetime().end(), group)
            }
            DealingEvent::PoolAdjustment(reorg) => {
                write!(f, "{}..{}: {}", reorg.datetime().start(), reorg.datetime().end(), reorg)
            }
        }
    }
}
