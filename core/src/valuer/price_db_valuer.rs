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
use crate::journal_context::JContext;
use crate::unit::{DEFAULT_PRICE_LOOKUP_WITHIN_SECS, Unit};
use crate::valuer::{Valuation, ValuationError, ValuationResult, Valuer};

/// A valuer that looks up valuations from price databases for both `base_unit` and `quote_unit`
pub struct PriceDatabaseValuer {
    datetime: JDateTime,
}

impl PriceDatabaseValuer {
    pub fn new(datetime: JDateTime) -> Self {
        PriceDatabaseValuer { datetime }
    }
}

impl<'h> Valuer<'h> for PriceDatabaseValuer {
    fn value(&mut self, quote_unit: &'h Unit<'h>, amount: Amount<'h>) -> ValuationResult<'h> {
        /*
        let do_lookup = |unit: &'h Unit<'h>| {
            if let Some(price_db) = unit.prices() {
                if let Some(price) = price_db.get_closest(
                    self.datetime,
                    amount.unit().pricedb_lookup_within_secs().unwrap_or(DEFAULT_PRICE_LOOKUP_WITHIN_SECS),
                    amount.unit(),
                    quote_unit,
                ) {
                    return Ok(Valuation::binary(price.price() * amount.quantity(), amount));
                }
            }
            Err(ValuationError::Undetermined(err!("No price found in database")))
        };*/

        let price_db = amount.unit().prices().unwrap_or_else(|| JContext::get().price_database());
        if let Some(price) = price_db.get_closest(
            self.datetime,
            amount.unit().pricedb_lookup_within_secs().unwrap_or(DEFAULT_PRICE_LOOKUP_WITHIN_SECS),
            amount.unit(),
            quote_unit,
        ) {
            Ok((price, false))
        // Try the reverse lookup if the first one fails
        } else if let Some(price) = price_db.get_closest(
            self.datetime,
            quote_unit.pricedb_lookup_within_secs().unwrap_or(DEFAULT_PRICE_LOOKUP_WITHIN_SECS),
            quote_unit,
            amount.unit(),
        ) {
            Ok((price, true))
        } else {
            Err(ValuationError::Undetermined(err!("No price found in database")))
        }
        .map(|(price, reverse)| {
            // Put in terms of the quote unit passed in (as per contract). The quote unit in the price db
            // might have different rounding precision.
            let price_amount = if reverse { price.inverse().price() } else { price.price() };
            let mut v = Valuation::binary(
                quote_unit.with_quantity(price_amount.quantity() * amount.quantity()),
                amount,
            );
            price.sources().for_each(|s| {
                v.add_source(s);
            });
            v
        })

        /*
        do_lookup(amount.unit()).or_else(|_| {
            let mut v = do_lookup(quote_unit)?;
            Valuer::value(&mut v, quote_unit, amount)
        })*/
    }
}
