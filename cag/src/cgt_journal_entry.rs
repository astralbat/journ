/*
 * Copyright (c) 2021-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjustment::{Adjustment, AmountAdjustment};
use crate::deal::Deal;
use journ_core::error::parsing::{promote, tag_err, IParseError};
use journ_core::error::JournError;
use journ_core::journal_entry::JournalEntry;
use journ_core::metadata::Metadata;
use journ_core::parse_block;
use journ_core::parsing::entry::valued_amount;
use journ_core::parsing::text_input::{BlockInput, ConfigInput, TextInput};
use journ_core::parsing::util::double_space;
use journ_core::parsing::{amount, entry, IParseResult};
use journ_core::unit::Unit;
use journ_core::valued_amount::ValuedAmount;
use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case, take, take_while1};
use nom::character::complete::space0;
use nom::combinator::{all_consuming, map, map_parser, opt};
use nom::sequence::{preceded, terminated, tuple};
use nom::Err as NomErr;
use std::fmt::Debug;

pub trait CapitalGainsMetadataAccess<'h> {
    fn cg_metadata(&self) -> Result<CapitalGainsEntryMetadata<'h>, JournError>;
}

impl<'h> CapitalGainsMetadataAccess<'h> for &'h JournalEntry<'h> {
    fn cg_metadata(&self) -> Result<CapitalGainsEntryMetadata<'h>, JournError> {
        let mut deal_metadata = vec![];
        let mut adjustment_metadata = vec![];

        for (position, metadata) in self.metadata().enumerate() {
            if metadata.key() == "CAG-Deal" || metadata.key() == "CAG-Deal!" {
                deal_metadata.push(CapitalGainsEntryMetadata::parse_deal(
                    metadata,
                    self,
                    position as u32,
                )?)
            } else if metadata.key() == "CAG-Adjust" {
                adjustment_metadata.push(CapitalGainsEntryMetadata::parse_adjustment(
                    metadata,
                    self,
                    position as u32,
                )?)
            } else if metadata.key() == "CAG-AdjustPool" {
                adjustment_metadata.push(CapitalGainsEntryMetadata::parse_pool_adjustment(
                    metadata,
                    self,
                    position as u32,
                )?)
            }
        }

        Ok(CapitalGainsEntryMetadata { entry: self, deal_metadata, adjustment_metadata })
    }
}

#[derive(Debug, Clone)]
pub struct CapitalGainsEntryMetadata<'h> {
    entry: &'h JournalEntry<'h>,
    deal_metadata: Vec<Deal<'h>>,
    adjustment_metadata: Vec<Adjustment<'h>>,
}

impl<'h> CapitalGainsEntryMetadata<'h> {
    pub fn deals(&self, unit: &'h Unit<'h>) -> Vec<Deal<'h>> {
        self.deal_metadata.iter().filter(|a| a.unit() == unit).cloned().collect()
    }

    pub fn all_deals(&self) -> impl Iterator<Item = &Deal<'h>> {
        self.deal_metadata.iter()
    }

    pub fn adjustments(&self) -> impl Iterator<Item = &Adjustment<'h>> {
        self.adjustment_metadata.iter()
    }

    pub fn entry(&self) -> &'h JournalEntry<'h> {
        self.entry
    }

    pub fn add_deal(&mut self, deal: Deal<'h>) {
        self.deal_metadata.push(deal);
    }

    fn parse_metadata<I, O, F>(
        key: &'static str,
        mut value_parser: F,
    ) -> impl FnMut(I) -> IParseResult<'h, I, O>
    where
        I: TextInput<'h> + ConfigInput<'h> + BlockInput<'h>,
        F: FnMut(I) -> IParseResult<'h, I, O>,
    {
        move |input| {
            let (input, _) = Metadata::parse_pretext(input)?;
            let (input, _) =
                terminated(terminated(tag_no_case(key), opt(tag("!"))), double_space)(input)?;
            let (input, value) = value_parser(input)?;

            // Ensure the block is now empty
            let (input, _) = tag_err("Unexpected value", all_consuming(space0))(input)?;

            Ok((input, value))
        }
    }

    /// Parses all `CAG-Deal` metadata values from the entry.
    /// # Example:
    /// ## Valid:
    /// * `+CAG-Deal  $400 @@ £300 ++ £10  ; An acquisition of $400 costing a total of £310 including £10 in expenses`
    /// * `+CAG-Deal  $400 @@ £310 -- £10  ; Same as above`
    /// * `+CAG-Deal  -$400 @@ £310 -- £10 ; A disposal of $400 for a gross total of £310, £300 net after expenses`
    /// * `+CAG-Deal  -$400 @@ £300 ++ £10 ; Same as above`
    /// * `+CAG-Deal  $400 @@ £300 @@ €400 ++ £10 @@ €14`
    fn parse_deal(
        metadata: &Metadata<'h>,
        entry: &'h JournalEntry<'h>,
        position: u32,
    ) -> Result<Deal<'h>, JournError> {
        let err = "Unable to parse CAG-Deal. Deals should be in the format:\n\
                CAG-Deal  <deal_amount> [++ deal_expenses] [== taxable_gain] -OR- CAG-Deal  -<deal_amount> [-- deal_expenses] [== taxable_gain]";
        let mut config = entry.config().clone();
        let (_, res) = parse_block!(
            metadata.block(),
            promote::<_, JournError, _, _>(
                err,
                Self::parse_metadata("CAG-Deal", Self::deal_parser(entry, position))
            ),
            &mut config
        )
        .0?;

        Ok(res)
    }

    fn deal_parser<I>(
        entry: &'h JournalEntry<'h>,
        position: u32,
    ) -> impl FnMut(I) -> IParseResult<'h, I, Deal<'h>>
    where
        I: TextInput<'h> + ConfigInput<'h> + BlockInput<'h>,
    {
        move |input| {
            let orig_input = input.clone();

            // Read the required valued amount
            let (input, valued_amount) = Self::take_valued_amount(input)?;

            // Read optional expenses
            let (input, mut expenses) = if let Ok((rem, _)) =
                preceded::<_, _, _, (), _, _>(space0, tag("--"))(input.clone())
            {
                let (rem, mut expenses) = entry::valued_amount(rem)?;
                // Base amount is inferred. The amount we have is the total cost, so subtract the expenses to get the base cost.
                if valued_amount.amount().is_positive() {
                    return Err(NomErr::Error(IParseError::new(
                        "Expenses should be appended with '++' when the deal represents an acquisition", orig_input)));
                }

                expenses.set_pretext("");

                // The expenses are negative when using --.
                //expenses.negate();
                (rem, expenses)
            } else if let Ok((rem, _)) =
                preceded::<_, _, _, (), _, _>(space0, tag("++"))(input.clone())
            {
                let (rem, mut expenses) = entry::valued_amount(rem)?;
                expenses.set_pretext("");

                if valued_amount.amount().is_negative() {
                    return Err(NomErr::Error(IParseError::new(
                        "Expenses should be appended with '--' when the deal represents a disposal",
                        orig_input,
                    )));
                }
                (rem, expenses)
            } else {
                (input, ValuedAmount::nil())
            };

            if expenses.units().count() == 1 && expenses.unit() == valued_amount.unit() {
                return Err(NomErr::Error(IParseError::new(
                    "Expenses should be valued in a different unit to the base amount",
                    orig_input,
                )));
            }
            expenses = expenses.without_unit(valued_amount.unit());

            // Read optional taxable gain
            let (rem, taxable_gain) =
                match preceded::<_, _, _, (), _, _>(space0, tag("=="))(input.clone()) {
                    Ok((tg, _)) => map(entry::valued_amount, Some)(tg)?,
                    Err(_) => (input, None),
                };

            let deal =
                Deal::new(Some(position), entry, valued_amount, expenses, taxable_gain, true);
            Ok((rem, deal))
        }
    }

    /// Gets the valuedAmount from the input. This is like just reading an ordinary valued amount,
    /// except that it is more sensitive to strings of double '--', '++' or '==' which are used to indicate
    /// the separation of the valued amount with its expenses/gain.
    fn take_valued_amount<I>(input: I) -> IParseResult<'h, I, ValuedAmount<'h>>
    where
        I: TextInput<'h> + ConfigInput<'h>,
    {
        let mut neg_last = false;
        let mut pos_last = false;
        let mut eq_last = false;
        let mut to_take: usize = 0;
        for c in input.text().chars() {
            match c {
                '-' if neg_last => break,
                '-' => neg_last = true,
                '+' if pos_last => break,
                '+' => pos_last = true,
                '=' if eq_last => break,
                '=' => eq_last = true,
                _ => {
                    neg_last = false;
                    pos_last = false;
                    eq_last = false;
                }
            }
            to_take += 1;
        }
        if pos_last || neg_last || eq_last {
            to_take -= 1;
        }
        map_parser(
            take(to_take),
            tag_err("Invalid valued amount", all_consuming(terminated(valued_amount, space0))),
        )(input)
    }

    /// Parses CAG-Adjust: metadata values from the entry. This type of adjustment will
    /// adjust all of the pools. Additive adjustments are not allowed here since they are
    /// difficult to distribute in a consistent way.
    ///
    /// Examples:
    /// `CAG-Adjust: -$400, -£300`
    /// `CAG-Adjust: +€100`
    /// `CAG-Adjust: 0 ACME, $300`
    /// `CAG-Adjust: *3 ACME`
    pub fn parse_adjustment(
        metadata: &Metadata<'h>,
        entry: &'h JournalEntry<'h>,
        position: u32,
    ) -> Result<Adjustment<'h>, JournError> {
        let err = "Adjustments should be in the format:\nCAG-Adjust: *<amount_adjustment>, [consideration_adjustment]";
        let mut config = entry.config().clone();
        let (_rem, amount_adjustments) = parse_block!(
            metadata.block(),
            promote::<_, JournError, _, _>(
                err,
                Self::parse_metadata("CAG-Adjust", Self::adj_parser())
            ),
            &mut config
        )
        .0?;

        if amount_adjustments.iter().any(|a| a.is_additive()) {
            return Err(metadata.err(
                    "Additive adjustments are not allowed in CAG-Adjust metadata. Use format \"CAG-AdjustPool <pool_name>, <amount_adjustment>, [consideration_adjustment]\" instead.".to_string(), None),
            );
        }

        Ok(Adjustment::new(position, entry, None, amount_adjustments))
    }

    fn adj_parser<I>() -> impl FnMut(I) -> IParseResult<'h, I, Vec<AmountAdjustment<'h>>>
    where
        I: TextInput<'h> + ConfigInput<'h>,
    {
        move |mut input| {
            let mut adjustments = vec![];

            // Read the required amount
            let (rem, amount_adjustment) = Self::amount_adj_parser()(input.clone())?;
            adjustments.push(amount_adjustment);
            input = rem;

            // Read further optional amount adjustments
            while let Ok((rem, _)) = preceded::<_, _, _, (), _, _>(space0, tag(","))(input.clone())
            {
                input = rem;
                let (rem, amount_adjustment) = Self::amount_adj_parser()(input.clone())?;
                adjustments.push(amount_adjustment);
                input = rem;
            }

            Ok((input, adjustments))
        }
    }

    fn amount_adj_parser<I>() -> impl FnMut(I) -> IParseResult<'h, I, AmountAdjustment<'h>>
    where
        I: TextInput<'h> + ConfigInput<'h>,
    {
        let add_parser = |input| {
            let (rem, amount) = amount::amount_expr(input)?;
            Ok((rem, AmountAdjustment::Add(amount.amount())))
        };
        let scalar_parser = |input| {
            let (rem, amount) =
                preceded(tuple((space0, tag("*"), space0)), amount::amount_expr)(input)?;
            Ok((rem, AmountAdjustment::Scale(*amount)))
        };
        let set_quantity_parser = |input| {
            let (rem, amount) =
                preceded(tuple((space0, tag("="), space0)), amount::amount_expr)(input)?;
            Ok((rem, AmountAdjustment::Set(*amount)))
        };
        alt((add_parser, scalar_parser, set_quantity_parser))
    }

    pub fn parse_pool_adjustment(
        metadata: &Metadata<'h>,
        entry: &'h JournalEntry<'h>,
        position: u32,
    ) -> Result<Adjustment<'h>, JournError> {
        let err = "Adjustments should be in the format:\nCAG-AdjustPool: <pool_name>, <amount_adjustment>, [consideration_adjustment]";
        let mut config = entry.config().clone();
        let (_rem, parsed) = parse_block!(
            metadata.block(),
            promote::<_, JournError, _, _>(
                err,
                Self::parse_metadata("CAG-AdjustPool", Self::pool_adj_parser(entry, position))
            ),
            &mut config
        )
        .0?;
        Ok(parsed)
    }

    fn pool_adj_parser<'p, 'c, I>(
        entry: &'h JournalEntry<'h>,
        position: u32,
    ) -> impl FnMut(I) -> IParseResult<'h, I, Adjustment<'h>> + 'p
    where
        I: TextInput<'h> + ConfigInput<'h>,
        'h: 'c,
        'c: 'p,
    {
        move |input| {
            let (rem, pool_name) = take_while1(|c| c != ',' && c != '\n')(input.clone())?;
            let rem = tuple((space0, tag(","), space0))(rem)?.0;

            let (rem, amount_adjustments) = Self::adj_parser()(rem)?;

            Ok((rem, Adjustment::new(position, entry, Some(pool_name.text()), amount_adjustments)))
        }
    }

    /// Removes all Deal metadata except keys ending with a '!' whose values are determined by the user.
    pub fn clear_deals(entry: &mut JournalEntry) {
        entry.remove_metadata_tags_by_key("CAG-Deal");
    }
}
