/*
 * Copyright (c) 2021-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::adjustment::{Adjustment, AmountAdjustment};
use journ_core::error::parsing::{IErrorMsg, IParseError, tag_err};
use journ_core::error::{JournError, JournResult};
use journ_core::journal_entry::JournalEntry;
use journ_core::metadata::Metadata;
use journ_core::parsing::entry::valued_amount;
use journ_core::parsing::input::{BlockInput, ConfigInput, TextInput};
use journ_core::parsing::text_block::block_leading_whitespace;
use journ_core::parsing::{IParseResult, amount, entry};
use journ_core::valued_amount::ValuedAmount;
use nom::Err as NomErr;
use nom::branch::alt;
use nom::bytes::complete::{tag, take, take_while1};
use nom::character::complete::space0;
use nom::combinator::{all_consuming, map, map_parser};
use nom::sequence::{preceded, terminated, tuple};
use smallvec::{SmallVec, smallvec};
use std::fmt::Debug;

/// Manual inclusion of deals on entries that overrides the implied deal detection for a particular unit.
pub static CAG_DEAL: &str = "CAG-Deal";
/// An adjustment to all holdings of a particular unit.
pub static CAG_ADJUST: &str = "CAG-Adjust";
/// An adjustment to a particular holding of a unit.
pub static CAG_ADJUST_POOL: &str = "CAG-AdjustPool";
/// Account tag which is recognised by all linked flows on the credit or debit side
/// and applied to credit-side deals to force the capital gain to be such that the effective proceeds are zero.
pub static CAG_ZERO_PROCEEDS: &str = "CAG-ZeroProceeds";
/// Account tag which overrides the default capital gains include flow expression for the entry.
pub static CAG_INCLUDE: &str = "CAG-Include";
/// Notes about the deals to be included in reports.
pub static CAG_NOTE: &str = "CAG-Note";

pub trait CapitalGainsMetadataAccess<'h> {
    fn cg_metadata(&self) -> JournResult<CapitalGainsEntryMetadata<'h>>;
}

impl<'h> CapitalGainsMetadataAccess<'h> for &'h JournalEntry<'h> {
    fn cg_metadata(&self) -> JournResult<CapitalGainsEntryMetadata<'h>> {
        let mut deal_metadata = vec![];
        let mut adjustment_metadata = vec![];
        let mut position = 0;

        for metadata in self.metadata() {
            if metadata.key() == CAG_DEAL {
                let (valued_amount, expenses, taxable_gain) =
                    CapitalGainsEntryMetadata::parse_deal(metadata)?;
                deal_metadata.push((valued_amount, expenses, taxable_gain, position));
            } else if metadata.key() == CAG_ADJUST {
                adjustment_metadata
                    .push(CapitalGainsEntryMetadata::parse_adjustment(metadata, self, position)?)
            } else if metadata.key() == CAG_ADJUST_POOL {
                adjustment_metadata.push(CapitalGainsEntryMetadata::parse_pool_adjustment(
                    metadata, self, position,
                )?)
            }
            position += 1;
        }

        Ok(CapitalGainsEntryMetadata {
            entry: self,
            deal_metadata,
            adjustment_metadata,
            md_count: position + 1,
        })
    }
}

#[derive(Debug, Clone)]
pub struct CapitalGainsEntryMetadata<'h> {
    entry: &'h JournalEntry<'h>,
    deal_metadata: Vec<(ValuedAmount<'h>, ValuedAmount<'h>, Option<ValuedAmount<'h>>, usize)>,
    adjustment_metadata: Vec<Adjustment<'h>>,
    md_count: usize,
}

impl<'h> CapitalGainsEntryMetadata<'h> {
    pub fn into_deal_metadata(
        self,
    ) -> Vec<(ValuedAmount<'h>, ValuedAmount<'h>, Option<ValuedAmount<'h>>, usize)> {
        self.deal_metadata
    }

    pub fn deal_metadata(
        &self,
    ) -> &Vec<(ValuedAmount<'h>, ValuedAmount<'h>, Option<ValuedAmount<'h>>, usize)> {
        &self.deal_metadata
    }

    pub fn adjustments(&self) -> impl Iterator<Item = &Adjustment<'h>> {
        self.adjustment_metadata.iter()
    }

    pub fn entry(&self) -> &'h JournalEntry<'h> {
        self.entry
    }

    pub fn md_count(&self) -> usize {
        self.md_count
    }

    /// Parses all `CAG-Deal` metadata values from the entry.
    /// # Example:
    /// ## Valid:
    /// * `+CAG-Deal  $400 @@ £300 ++ £10  ; An acquisition of $400 costing a total of £310 including £10 in expenses`
    /// * `+CAG-Deal  $400 @@ £310 -- £10  ; Not allowed currently, but represents a total cost of £310 including £10 in expenses (same as above)`
    /// * `+CAG-Deal  -$400 @@ £310 -- £10 ; A disposal of $400 for a gross total of £310, £300 net after expenses`
    /// * `+CAG-Deal  -$400 @@ £300 ++ £10 ; Not allowed currently, but represents net proceeds of £300 after expenses of £10 (same as above)`
    /// * `+CAG-Deal  $400 @@ £300 @@ €400 ++ £10 @@ €14`
    fn parse_deal(
        metadata: &Metadata<'h>,
    ) -> Result<(ValuedAmount<'h>, ValuedAmount<'h>, Option<ValuedAmount<'h>>), JournError> {
        let err = "Unable to parse CAG-Deal. Deals should be in the format:\n\
                CAG-Deal  <deal_amount> [++ deal_expenses] [== taxable_gain] -OR- CAG-Deal  -<deal_amount> [-- deal_expenses] [== taxable_gain]";
        let res = metadata.parse_value(Self::deal_parser(), err)?;
        Ok(res)
    }

    fn deal_parser<I>() -> impl FnMut(
        I,
    ) -> IParseResult<
        'h,
        I,
        (ValuedAmount<'h>, ValuedAmount<'h>, Option<ValuedAmount<'h>>),
    >
    where
        I: TextInput<'h> + ConfigInput<'h> + BlockInput<'h>,
    {
        move |input| {
            let orig_input = input.clone();

            // Read the required valued amount
            let (input, valued_amount) =
                preceded(block_leading_whitespace, Self::take_valued_amount)(input)?;

            // Read optional expenses
            let (input, mut expenses) = if let Ok((rem, _)) =
                preceded::<_, _, _, (), _, _>(space0, tag("--"))(input.clone())
            {
                let (rem, expenses) = entry::valued_amount(rem)?;
                // Base amount is inferred. The amount we have is the total cost, so subtract the expenses to get the base cost.
                if valued_amount.amount().is_positive() {
                    return Err(NomErr::Error(IParseError::new(
                        "Expenses should be appended with '++' when the deal represents an acquisition",
                        orig_input,
                    )));
                }

                (rem, expenses)
            } else if let Ok((rem, _)) =
                preceded::<_, _, _, (), _, _>(space0, tag("++"))(input.clone())
            {
                let (rem, expenses) = entry::valued_amount(rem)?;
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

            Ok((rem, (valued_amount, expenses, taxable_gain)))
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
                // Nested metadata may follow
                '\r' | '\n' => break,
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
        position: usize,
    ) -> Result<Adjustment<'h>, JournError> {
        let err = "Adjustments should be in the format:\nCAG-Adjust: *<amount_adjustment>, [consideration_adjustment]";
        let amount_adjustments = metadata.parse_value(Self::adj_parser(), err)?;

        if amount_adjustments.iter().any(|a| a.is_additive()) {
            return Err(metadata.err(
                    "Additive adjustments are not allowed in CAG-Adjust metadata. Use format \"CAG-AdjustPool <pool_name>, <amount_adjustment>, [consideration_adjustment]\" instead.".to_string(), None),
            );
        }

        let mut all_md = smallvec![metadata.clone()];
        // Add entry metadata
        all_md.append(
            &mut entry.metadata_by_key(CAG_NOTE).into_iter().cloned().collect::<SmallVec<[_; 2]>>(),
        );
        // Append nested metadata under this CAG-Deal
        all_md.append(&mut metadata.value_as_metadata_lines());

        Ok(Adjustment::new(position, entry, all_md, None, amount_adjustments))
    }

    fn adj_parser<I>() -> impl FnMut(I) -> IParseResult<'h, I, Vec<AmountAdjustment<'h>>>
    where
        I: TextInput<'h> + ConfigInput<'h>,
    {
        move |mut input| {
            let mut adjustments = vec![];

            // Read the required amount
            let (rem, amount_adjustment) =
                preceded(block_leading_whitespace, Self::amount_adj_parser())(input.clone())?;
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
            Ok((rem, AmountAdjustment::Add(amount)))
        };
        let scalar_parser = |input| {
            let (rem, amount) =
                preceded(tuple((space0, tag("*"), space0)), amount::amount_expr)(input)?;
            Ok((rem, AmountAdjustment::Scale(amount)))
        };
        let set_quantity_parser = |input| {
            let (rem, amount) =
                preceded(tuple((space0, tag("="), space0)), amount::amount_expr)(input)?;
            Ok((rem, AmountAdjustment::Set(amount)))
        };
        tag_err(IErrorMsg::AMOUNT, alt((add_parser, scalar_parser, set_quantity_parser)))
    }

    pub fn parse_pool_adjustment(
        metadata: &'h Metadata<'h>,
        entry: &'h JournalEntry<'h>,
        position: usize,
    ) -> Result<Adjustment<'h>, JournError> {
        let err = "Adjustments should be in the format:\nCAG-AdjustPool: <pool_name>, <amount_adjustment>, [consideration_adjustment]";
        let parsed = metadata.parse_value(Self::pool_adj_parser(entry, metadata, position), err)?;
        Ok(parsed)
    }

    fn pool_adj_parser<'p, 'c, I>(
        entry: &'h JournalEntry<'h>,
        metadata: &'h Metadata<'h>,
        position: usize,
    ) -> impl FnMut(I) -> IParseResult<'h, I, Adjustment<'h>> + 'p
    where
        I: TextInput<'h> + ConfigInput<'h>,
        'h: 'c,
        'c: 'p,
    {
        move |input| {
            let (rem, pool_name) = preceded(
                block_leading_whitespace,
                take_while1(|c| c != ',' && c != '\n'),
            )(input.clone())?;
            let rem = tuple((space0, tag(","), space0))(rem)?.0;

            let (rem, amount_adjustments) = Self::adj_parser()(rem)?;

            let mut all_md = smallvec![metadata.clone()];
            // Add entry metadata
            all_md.append(
                &mut entry
                    .metadata_by_key(CAG_NOTE)
                    .into_iter()
                    .cloned()
                    .collect::<SmallVec<[_; 2]>>(),
            );
            // Append nested metadata under this CAG-Deal
            all_md.append(&mut metadata.value_as_metadata_lines());

            Ok((
                rem,
                Adjustment::new(
                    position,
                    entry,
                    all_md,
                    Some(pool_name.text()),
                    amount_adjustments,
                ),
            ))
        }
    }

    /// Removes all Deal metadata except keys ending with a '!' whose values are determined by the user.
    pub fn clear_deals(entry: &mut JournalEntry) {
        entry.remove_metadata_tags_by_key(CAG_DEAL);
    }
}
