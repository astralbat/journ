/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::Amount;
use crate::datetime::JDateTime;
use crate::err;
use crate::unit::Unit;
use crate::valuer::{Valuation, ValuationError, ValuationResult, Valuer};

/// A valuer that looks up valuations from price databases for both `base_unit` and `quote_unit`
pub struct PriceDatabaseValuer {
    datetime: JDateTime,
    within_seconds: usize,
}

impl PriceDatabaseValuer {
    pub fn new(datetime: JDateTime, within_seconds: usize) -> Self {
        PriceDatabaseValuer { datetime, within_seconds }
    }
}

impl<'h> Valuer<'h> for PriceDatabaseValuer {
    fn value(&mut self, quote_unit: &'h Unit<'h>, amount: Amount<'h>) -> ValuationResult<'h> {
        let do_lookup = |_unit| {
            if let Some(price_db) = quote_unit.prices() {
                if let Some(price) = price_db.get_closest(
                    self.datetime,
                    self.within_seconds,
                    amount.unit(),
                    quote_unit,
                ) {
                    return Ok(Valuation::new(price.price() * amount.quantity()));
                }
            }
            Err(ValuationError::Undetermined(err!("No price found in database")))
        };

        do_lookup(amount.unit()).or_else(|_| do_lookup(quote_unit))
    }
}
