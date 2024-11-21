/*
 * Copyright (c) 2020-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::Amount;
use crate::date_and_time::{DateFormat, JDateTime, TimeFormat};
use crate::parsing;
use crate::unit::Unit;
use chrono_tz::Tz;
use std::{cmp, fmt};

#[derive(Debug, Clone)]
pub struct Price<'h> {
    datetime: JDateTime<'h>,
    base_unit: &'h Unit<'h>,
    price: Amount<'h>,
    /// Comma separated list of sources.
    sources: Option<&'h str>,
}

impl<'h> Price<'h> {
    pub fn new(
        datetime: JDateTime<'h>,
        base_unit: &'h Unit<'h>,
        price: Amount<'h>,
        sources: Option<&'h str>,
    ) -> Self {
        Price { datetime, base_unit, price, sources }
    }

    pub fn datetime(&self) -> JDateTime<'h> {
        self.datetime
    }

    pub fn set_date_format(&mut self, date_format: &'h DateFormat<'h>) {
        self.datetime = self.datetime.with_date_format(date_format);
    }

    pub fn set_time_format(&mut self, time_format: &'h TimeFormat<'h>) {
        self.datetime = self.datetime.with_time_format(time_format);
    }

    pub fn set_timezone(&mut self, timezone: Tz) {
        self.datetime = self.datetime.with_timezone(timezone);
    }

    pub fn base_unit(&self) -> &'h Unit<'h> {
        self.base_unit
    }

    pub fn set_base_unit(&mut self, curr: &'h Unit<'h>) {
        self.base_unit = curr
    }

    pub fn quote_unit(&self) -> &'h Unit<'h> {
        self.price.unit()
    }

    /// The price is the quantity of quote units per base unit.
    /// The returned amount will always be >= 0.
    pub fn price(&self) -> Amount<'h> {
        self.price
    }

    pub fn set_price(&mut self, price: Amount<'h>) {
        self.price = price
    }

    pub fn sources(&self) -> impl Iterator<Item = &'h str> + '_ {
        parsing::util::separated_fields(',', self.sources.unwrap_or(""))
    }

    /// Sets the sources as a comma separated list, each of which
    /// may optionally be enclosed in double quotes.
    pub fn set_sources(&mut self, sources: Option<&'h str>) {
        self.sources = sources
    }

    pub fn base_valuation(&self, amount: Amount<'h>) -> Amount<'h> {
        assert_eq!(amount.unit(), self.base_unit, "Base currencies must be the same");

        (self.price() * amount.quantity()).rounded()
    }

    /// Creates a new price that's the inverse of this one.
    pub fn inverse(&self) -> Price<'h> {
        Price::new(
            self.datetime,
            self.quote_unit(),
            Amount::new(self.base_unit, dec!(1) / self.price.quantity()),
            self.sources,
        )
    }
}

impl fmt::Display for Price<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "P {} \"{}\" {}",
            self.datetime,
            self.base_unit.code(),
            self.price.format_precise()
        )?;
        if let Some(sources) = self.sources {
            write!(f, " {}", sources)?;
        }
        Ok(())
    }
}

impl PartialEq for Price<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.datetime == other.datetime
            && self.base_unit == other.base_unit
            && self.price.unit() == other.price.unit()
    }
}

impl Eq for Price<'_> {}

impl Ord for Price<'_> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.datetime
            .datetime()
            .cmp(&other.datetime.datetime())
            .then(self.base_unit.cmp(other.base_unit()))
            .then(self.price.unit().cmp(other.price.unit()))
    }
}

impl PartialOrd for Price<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}
