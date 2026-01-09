/*
 * Copyright (c) 2020-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::amount::Amount;
use crate::configuration::Configuration;
use crate::datetime::JDateTime;
use crate::parsing;
use crate::unit::Unit;
use std::{cmp, io};

#[derive(Debug, Clone)]
pub struct Price<'h> {
    datetime: JDateTime,
    base_unit: &'h Unit<'h>,
    price: Amount<'h>,
    /// Comma separated list of sources.
    sources: Option<&'h str>,
}

impl<'h> Price<'h> {
    pub fn new(
        datetime: JDateTime,
        base_unit: &'h Unit<'h>,
        price: Amount<'h>,
        sources: Option<&'h str>,
    ) -> Self {
        Price { datetime, base_unit, price, sources }
    }

    pub fn datetime(&self) -> JDateTime {
        self.datetime
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

    pub fn write<W: io::Write>(
        &self,
        writer: &mut W,
        config: &Configuration<'h>,
    ) -> io::Result<()> {
        let dtf = config.datetime_format();
        let tz = config.timezone();
        write!(
            writer,
            "P {} \"{}\" {}",
            self.datetime.with_timezone(tz).format(dtf),
            self.base_unit.code(),
            self.price.format_precise()
        )?;
        if let Some(sources) = self.sources {
            write!(writer, " {}", sources)?;
        }
        Ok(())
    }
}

/*
impl fmt::Display for Price<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        //let dtf = Cmd::get().datetime_fmt_cmd().datetime_format_or_default();
        write!(
            f,
            "P {} \"{}\" {}",
            self.datetime.format(dtf),
            self.base_unit.code(),
            self.price.format_precise()
        )?;
        if let Some(sources) = self.sources {
            write!(f, " {}", sources)?;
        }
        Ok(())
    }
}*/

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
