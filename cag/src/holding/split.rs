/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use journ_core::amount::Amount;
use smallvec::SmallVec;
use std::iter;

pub struct Split<'h> {
    of: SmallVec<[Amount<'h>; 4]>,
    from: SmallVec<[Amount<'h>; 4]>,
    round: SmallVec<[bool; 4]>,
}

impl<'h> Split<'h> {
    pub fn new<S: IntoIterator<Item = Amount<'h>>>(of: S, from: S) -> Self {
        let of: SmallVec<_> = of.into_iter().collect();
        let from: SmallVec<_> = from.into_iter().collect();
        let round = iter::repeat_n(false, from.len()).collect();
        assert_eq!(of.len(), from.len(), "Split amounts must have the same length");
        Self { of, from, round }
    }

    pub fn of(&self) -> &[Amount<'h>] {
        &self.of
    }

    pub fn from(&self) -> &[Amount<'h>] {
        &self.from
    }

    pub fn set_round(&mut self, round: &[bool]) {
        assert_eq!(
            self.from.len(),
            round.len(),
            "Round amounts must have the same length as from amounts"
        );
        self.round.clear();
        self.round.extend_from_slice(round);
    }

    pub fn split_off(&mut self, amounts: &[Amount<'h>]) -> Self {
        assert_eq!(self.of.len(), amounts.len());

        let mut new_of: SmallVec<[Amount; 4]> = SmallVec::with_capacity(self.of.len());
        let mut new_from: SmallVec<[Amount; 4]> = SmallVec::with_capacity(self.from.len());
        for (i, amt) in amounts.iter().enumerate() {
            if self.from[i].is_zero() {
                assert_eq!(
                    self.of[i], 0,
                    "Cannot split off from a zero amount unless the corresponding 'of' amount is also zero"
                );
                new_of.push(amt.with_quantity(0));
                new_from.push(amt.with_quantity(0));
                continue;
            }
            let ratio = (self.of[i] / self.from[i]).quantity();
            let (taken, taken_rem) = amt.split_percent(
                ratio,
                if self.round[i] {
                    Some(
                        amt.scale()
                            .max(self.of[i].scale())
                            .max(self.from[i].scale())
                            .max(amt.max_scale()),
                    )
                } else {
                    None
                },
            );
            self.of[i] -= taken;
            self.from[i] -= amt;

            new_of.push(taken);
            new_from.push(taken_rem);
        }

        Self::new(new_of, new_from)
    }
}
