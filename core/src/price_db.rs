/*
 * Copyright (c) 2019-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::directive::DirectiveKind;
use crate::error::JournResult;
use crate::journal_node::JournalNode;
use crate::price::Price;
use crate::unit::Unit;
use chrono::DateTime;
use chrono_tz::Tz;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, MutexGuard};

#[derive(Debug, Default)]
pub struct PriceDatabase<'h> {
    node: Option<&'h JournalNode<'h>>,
    // The prices and an initialised flag.
    prices: Mutex<(Vec<Arc<Price<'h>>>, bool)>,
    // True if a price has been added/updated at all since the last save.
    modified: AtomicBool,
}

impl<'h> PriceDatabase<'h> {
    pub fn new(node: &'h JournalNode<'h>) -> Self {
        Self { node: Some(node), ..Default::default() }
    }

    fn prices_init(&self) -> MutexGuard<(Vec<Arc<Price<'h>>>, bool)> {
        let mut prices_lock = self.prices.lock().unwrap();
        let (prices, initialised) = &mut *prices_lock;
        if !*initialised {
            if let Some(node) = self.node {
                for dir in node.all_directives_iter() {
                    if let DirectiveKind::Price(p) = dir.kind() {
                        prices.push(Arc::clone(p))
                    }
                }
                prices.sort_unstable();
                *initialised = true;
            }
        }
        prices_lock
    }

    #[allow(clippy::option_as_ref_deref)]
    pub fn node(&self) -> Option<&'h JournalNode<'h>> {
        self.node.as_ref().map(|f| &**f)
    }

    #[cfg(test)]
    pub fn prices(&self) -> MutexGuard<(Vec<Arc<Price<'h>>>, bool)> {
        self.prices_init()
    }

    /// Gets the price closest to the time provided and within plus or minus the number of hours_tolerance.
    pub fn get_closest(
        &self,
        time: DateTime<Tz>,
        within_seconds: i64,
        base_curr: &Unit<'h>,
        quote_curr: &Unit<'h>,
    ) -> Option<Arc<Price<'h>>> {
        debug!(
            "{} Looking in price_db within_seconds: {}, base_curr: {}, quote_curr: {}",
            time, within_seconds, base_curr, quote_curr
        );

        let closest = self.closest_by_key(time, |price| {
            price.base_unit() == base_curr && price.quote_unit() == quote_curr
        });
        if let Some(closest) = closest.as_ref() {
            if within_seconds >= 0
                && (closest.datetime().datetime() - time).num_seconds().abs() > within_seconds
            {
                return None;
            }
        }
        closest
    }

    pub fn get_closest_by_date(
        &self,
        time: DateTime<Tz>,
        within_seconds: i64,
    ) -> Option<Arc<Price<'h>>> {
        let closest = self.closest_by_key(time, |_p| true);
        if let Some(closest) = closest.as_ref() {
            if within_seconds >= 0
                && (closest.datetime().datetime() - time).num_seconds().abs() > within_seconds
            {
                return None;
            }
        }
        closest
    }

    /// Adds a price to the database. If a price for the same date already exists
    /// in the database, it is overwritten.
    pub fn put(&self, price: Arc<Price<'h>>) {
        let mut prices_lock = self.prices_init();
        let (prices, _) = &mut *prices_lock;

        let pos = match prices.binary_search(&price) {
            Ok(pos) => {
                // The price we're putting is the same as the price that's already there
                if prices[pos] == price && prices[pos].price() == price.price() {
                    return;
                }
                prices.remove(pos);
                pos
            }
            Err(pos) => pos,
        };

        prices.insert(pos, price);
        self.modified.store(true, Ordering::Relaxed);
    }

    /// Writes to the backing file if the database has been modified since the last save,
    /// and if this database is file backed, otherwise this is a no-op.
    pub fn write_file(&self) -> JournResult<()> {
        if self.modified.load(Ordering::Relaxed)
            && let Some(node) = self.node()
        {
            // Initialise memory _before_ clearing directives.
            let prices_lock = self.prices_init();
            Self::write_file_internal(node, prices_lock)?;
            self.modified.store(false, Ordering::Relaxed);
        }
        Ok(())
    }

    fn write_file_internal(
        node: &'h JournalNode<'h>,
        prices: MutexGuard<(Vec<Arc<Price<'h>>>, bool)>,
    ) -> JournResult<()> {
        if let Some(file) = node.nearest_filename() {
            trace!("Writing prices to file: {}", file.file_name().unwrap().to_str().unwrap());
            node.clear_directives_filter(|d| matches!(d.kind(), DirectiveKind::Price(_)));
            let config = node.segments().last().unwrap().config();
            for price in prices.0.iter() {
                let mut p = (**price).clone();
                p.set_date_format(config.date_format());
                p.set_time_format(config.time_format());
                p.set_timezone(config.timezone());
                node.append_directive(DirectiveKind::Price(Arc::new(p)))
            }
            node.write_nearest_file()?;
        }
        Ok(())
    }

    /// Find's the closest price to key that the supplied matches() function returns true for.
    /// This works by performing an initial binary search to find a central position from which the
    /// search moves out to the left and right.
    fn closest_by_key<F>(&self, key: DateTime<Tz>, matches: F) -> Option<Arc<Price<'h>>>
    where
        F: Fn(&Price<'h>) -> bool,
    {
        let prices_lock = self.prices_init();
        let prices = &prices_lock.0;
        let i = match prices.binary_search_by_key(&key, |p| *p.datetime()) {
            Ok(pos) => pos,
            Err(pos) => pos,
        };
        let mut left_offset = 0;
        let mut right_offset = 0;
        loop {
            let left = if left_offset < i { prices.get(i - left_offset - 1) } else { None };
            let right =
                if i + right_offset < prices.len() { prices.get(i + right_offset) } else { None };

            let left_matches = left.map(|l| matches(l));
            let right_matches = right.map(|r| matches(r));

            match (left_matches, right_matches) {
                (Some(true), Some(true)) => {
                    // Both match - find which is closer by key
                    let left_key = left.unwrap().datetime();
                    let right_key = right.unwrap().datetime();
                    if key.naive_utc() - left_key.naive_utc()
                        <= right_key.naive_utc() - key.naive_utc()
                    {
                        break left.cloned();
                    } else {
                        break right.cloned();
                    }
                }
                (None, Some(true)) | (Some(false), Some(true)) => {
                    // If the right's key is closer to the key than the left's, it is the closest, otherwise
                    // we still need to check further left.
                    match left {
                        Some(left) => {
                            let left_key = left.datetime();
                            let right_key = right.unwrap().datetime();
                            if right_key.naive_utc() - key.naive_utc()
                                <= key.naive_utc() - left_key.naive_utc()
                            {
                                break right.cloned();
                            } else {
                                left_offset += 1;
                            }
                        }
                        None => break right.cloned(),
                    }
                }
                (Some(true), None) | (Some(true), Some(false)) => {
                    // If the left's key is closer to the key than the right's, it is the closest, otherwise
                    // we still need to check further right.
                    match right {
                        Some(right) => {
                            let left_key = left.unwrap().datetime();
                            let right_key = right.datetime();
                            if key.naive_utc() - left_key.naive_utc()
                                <= right_key.naive_utc() - key.naive_utc()
                            {
                                break left.cloned();
                            } else {
                                right_offset += 1;
                            }
                        }
                        None => break left.cloned(),
                    }
                }
                (Some(false), None) | (None, Some(false)) | (Some(false), Some(false)) => {
                    left_offset += 1;
                    right_offset += 1;
                }
                (None, None) => break None,
            };
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::date_and_time::JDateTime;
    use crate::price::Price;
    use crate::price_db::PriceDatabase;
    use crate::unit::Unit;
    use crate::*;
    use chrono::*;
    use chrono_tz::Tz;
    use smallvec::smallvec;
    use std::ops::Add;
    use std::sync::Arc;

    #[test]
    fn test_price_db() {
        let price_db = PriceDatabase::default();

        let config = config!();
        let time = Tz::UTC.from_utc_datetime(&NaiveDateTime::new(
            NaiveDate::from_ymd(2000, 1, 1),
            NaiveTime::from_hms(0, 0, 0),
        ));
        let base_curr = &*Box::leak(Box::new(Unit::new("£")));
        let quote_curr = &*Box::leak(Box::new(Unit::new("$")));

        // Exact Time
        price_db.put(Arc::new(Price::new(
            JDateTime::from_datetime(time, Some(config.date_format()), Some(config.time_format())),
            base_curr,
            amount!("$2"),
            None,
        )));
        assert_eq!(
            price_db.get_closest(time, 0, base_curr, quote_curr).unwrap().map(|p| p.price()),
            Some(amount!("$2"))
        );

        // Another entry 30 mins later. Check the first entry is still there and that the closest 16 minutes is the new one.
        price_db.put(Arc::new(Price::new(
            JDateTime::from_datetime(
                time.add(Duration::minutes(30)),
                Some(config.date_format()),
                Some(config.time_format()),
            ),
            base_curr,
            amount!("$3"),
            None,
        )));
        assert_eq!(
            price_db.get_closest(time, 1, base_curr, quote_curr).unwrap().map(|p| p.price()),
            Some(amount!("$2"))
        );
        assert_eq!(
            price_db
                .get_closest(time.add(Duration::minutes(16)), 30 * 60, base_curr, quote_curr)
                .unwrap()
                .map(|p| p.price()),
            Some(amount!("$3"))
        );
    }
}
