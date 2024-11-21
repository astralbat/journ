/*
 * Copyright (c) 2023-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::cgt_configuration::DEFAULT_POOL;
use journ_core::error::parsing::{tag_err, IParseError};
use journ_core::parsing::text_block::block;
use journ_core::parsing::text_input::{BlockInput, LocatedInput, TextInput};
use journ_core::parsing::util::{blank_line0, separated_field, word};
use journ_core::parsing::IParseResult;
use nom::branch::alt;
use nom::bytes::complete::tag_no_case;
use nom::character::complete::{digit1, space0};
use nom::combinator::{map, opt, rest};
use nom::multi::fold_many1;
use nom::sequence::{delimited, pair, preceded, terminated};
use std::fmt;
use std::ops::Deref;

/// A list of rules to be applied to incoming deals.
///
/// Importantly, the ruleset is immutable since rule processing can be suspended for an indefinite
/// period of time and resumed. A new ruleset must be created to change the rules, and
/// only new deals after that point will be affected. Old deals will continue to be processed using
/// the old ruleset.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuleSet {
    rules: Vec<Rule>,
}
impl RuleSet {
    pub fn into_rules(self) -> Vec<Rule> {
        self.rules
    }
}

/*
impl<'h> TryFrom<&'h TextBlock<'h>> for RuleSet {
    type Error = NomErr<TextBlockErr<'h, ()>>;

    fn try_from(block: &'h TextBlock<'h>) -> Result<Self, Self::Error> {
        //let mut rules = vec![];
        let mut reader = TextBlockReader::new(block);
        reader.read(ruleset)

        /*
        for child in reader.read_children().filter(TextBlock::is_ordinary) {
            let leading_word = preceded(space0, word)(child.skip_leading_blank_lines()).unwrap().1;
            if tagged_ignore_case(leading_word, "if") {
                rules.push(Rule::Decision(DealDecision::try_from(child)?));
            }
        }*/
        //Ok(RuleSet { rules })
    }
}*/
impl Default for RuleSet {
    fn default() -> Self {
        // Default ruleset works with the default pool, matching and pooling all deals.
        RuleSet {
            rules: vec![
                Rule::Action(ActionRule::Match(DEFAULT_POOL.to_string())),
                Rule::Action(ActionRule::Pool(DEFAULT_POOL.to_string(), Condition::False)),
            ],
        }
    }
}

impl Deref for RuleSet {
    type Target = Vec<Rule>;

    fn deref(&self) -> &Self::Target {
        &self.rules
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Rule {
    Action(ActionRule),
    Decision(DealDecision),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ActionRule {
    /// Match against the pool with the specified name.
    Match(String),
    /// Add to the pool with the specified name until the optional condition is reached.
    Pool(String, Condition),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DealDecision {
    condition: Condition,
    /// The action to take if all conditions are met.
    rules: RuleSet,
}
impl DealDecision {
    pub fn condition(&self) -> &Condition {
        &self.condition
    }

    pub fn rules(&self) -> &RuleSet {
        &self.rules
    }

    pub fn into_rules(self) -> RuleSet {
        self.rules
    }
}

/*
impl<'h> TryFrom<&'h TextBlock<'h>> for DealDecision {
    type Error = NomErr<TextBlockErr<'h, ()>>;

    fn try_from(block: &'h TextBlock<'h>) -> Result<Self, Self::Error> {
        static E_NOT_DECISION: &str = "Not a decision rule. Decision rules start with 'if'";
        static E_EXPECTED_ACTION: &str = "Expected an action rule to follow a decision";

        let mut reader = TextBlockReader::new(block);

        // Parse the word 'if'.
        reader.read(context(E_NOT_DECISION, preceded(space0, tag_no_case("if"))))?;

        // Parse conditions.

        let mut condition = Condition::True;
        while let Ok(sep_field) = reader.read(separated_field(',')) {
            condition = Condition::And(Box::new(condition),
                Box::new(Condition::try_from(*sep_field.fragment())
                    .map_err(|e| NomErr::Failure(block.raise(Some(sep_field), e, None)))?));
        }

        // Parse the ruleset.
        let rule_block =
            reader.read_block().ok_or_else(|| NomErr::Failure(block.raise(None, E_EXPECTED_ACTION, None)))?;
        let rules = RuleSet::try_from(rule_block)?;
        Ok(DealDecision { condition, rules })
    }
}*/

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DealKind {
    Buy,
    Sell,
}
impl fmt::Display for DealKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DealKind::Buy => write!(f, "buy"),
            DealKind::Sell => write!(f, "sell"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AgeUnit {
    Days,
    /// Calendar Days
    CalDays,
}
impl fmt::Display for AgeUnit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AgeUnit::Days => write!(f, "d"),
            AgeUnit::CalDays => write!(f, "cd"),
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum Condition {
    /// Always
    #[default]
    True,
    /// Never
    False,
    /// The kind of deal.
    Kind(DealKind),
    /// The age of the deal.
    Age(u32, AgeUnit),
    /// Combine two conditions with a logical AND.
    And(Box<Condition>, Box<Condition>),
}
impl Condition {
    pub fn and(self, other: Condition) -> Self {
        if self == Condition::True {
            return other;
        }
        if other == Condition::True {
            return self;
        }
        Condition::And(Box::new(self), Box::new(other))
    }
}
impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Condition::True => write!(f, "true"),
            Condition::False => write!(f, "false"),
            Condition::Kind(kind) => write!(f, "kind={}", kind),
            Condition::Age(age, unit) => write!(f, "age={}{}", age, unit),
            Condition::And(lhs, rhs) => write!(f, "{} and {}", lhs, rhs),
        }
    }
}

/*
impl<'h> TryFrom<&'h str> for Condition {
    type Error = &'static str;

    fn try_from(condition: &'h str) -> Result<Self, Self::Error> {
        static E_BAD_CONDITION: &str = "Bad condition. Valid conditions are 'kind' or 'age'.";
        static E_BAD_KIND: &str = "Bad kind. Kind must be 'buy' or 'sell'.";
        static E_BAD_MAX_AGE: &str = "Bad age. Age must be a valid duration. e.g. '30D'.";
        static E_MISSING_EQUALS: &str =
            "Missing equals sign. Conditions must be of the form 'if kind=buy' or 'if age=30D'.";

        let (rem, cond_type) =
            alt::<_, _, (), _>((tag_no_case("kind"), tag_no_case("age")))(condition).map_err(|_| E_BAD_CONDITION)?;
        let cond_value = preceded::<_, _, _, (), _, _>(space0, tag_no_case("="))(rem).map_err(|_| E_MISSING_EQUALS)?.0;

        if cond_type.eq_ignore_ascii_case("kind") {
            match cond_value.to_ascii_lowercase().as_str() {
                "buy" => Ok(Condition::Kind(DealKind::Buy)),
                "sell" => Ok(Condition::Kind(DealKind::Sell)),
                _ => Err(E_BAD_KIND),
            }
        } else if cond_type.eq_ignore_ascii_case("age") {
            let (duration_type, num_days) = digit1::<_, ()>(cond_value).map_err(|_| E_BAD_MAX_AGE)?;
            tag_no_case::<_, _, ()>("d")(duration_type).map_err(|_| E_BAD_MAX_AGE)?;
            Ok(Condition::Age(Duration::days(num_days.parse().map_err(|_| E_BAD_MAX_AGE)?)))
        } else {
            Err(E_BAD_CONDITION)
        }
    }
}*/

pub fn ruleset<'h, I: TextInput<'h> + BlockInput<'h> + LocatedInput<'h>>(
    mut input: I,
) -> IParseResult<'h, I, RuleSet> {
    let mut rules = vec![];

    while input.input_len() > 0 {
        let (rem, rule) =
            alt((map(action_rule, Rule::Action), map(deal_decision, Rule::Decision)))(input)?;
        rules.push(rule);
        input = rem;
    }
    if rules.is_empty() {
        return Err(nom::Err::Error(IParseError::new("There must be at least one rule", input)));
    }
    Ok((input, RuleSet { rules }))
}

static E_NOT_DECISION: &str = "Not a decision rule. Decision rules start with 'if'";
fn deal_decision<'h, I: TextInput<'h> + BlockInput<'h> + LocatedInput<'h>>(
    input: I,
) -> IParseResult<'h, I, DealDecision> {
    let (rem, dec_block) = terminated(block, blank_line0)(input)?;
    // Parse the word 'if'.
    let (input, _) = tag_err(E_NOT_DECISION, preceded(space0, tag_no_case("if")))(dec_block)?;

    // Parse conditions.
    let (input, condition) = terminated(conditions, blank_line0)(input)?;

    // Parse the ruleset.
    let (_, ruleset) = ruleset(input)?;
    Ok((rem, DealDecision { condition, rules: ruleset }))
}

static E_BAD_CONDITION: &str = "Bad condition. Valid conditions are 'kind' or 'age'";
static E_BAD_KIND: &str = "Bad kind. Kind must be 'buy' or 'sell'";
static E_BAD_AGE: &str = "Bad age. Age must be a valid duration. e.g. '30D' or '1 CD'";
static E_MISSING_EQUALS: &str =
    "Missing equals sign. Conditions must be of the form 'if kind=buy' or 'if age=30D'";
fn condition<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, Condition> {
    let read_cond_name = tag_err(E_BAD_CONDITION, alt((tag_no_case("kind"), tag_no_case("age"))));
    let (input, (cond_name, cond_value)) = pair(
        read_cond_name,
        preceded(delimited(space0, tag_err(E_MISSING_EQUALS, tag_no_case("=")), space0), rest),
    )(input)?;

    if cond_name.text().eq_ignore_ascii_case("kind") {
        let val =
            tag_err(E_BAD_KIND, alt((tag_no_case("buy"), tag_no_case("sell"))))(cond_value)?.1;
        match val.text().to_ascii_lowercase().as_str() {
            "buy" => Ok((input, Condition::Kind(DealKind::Buy))),
            "sell" => Ok((input, Condition::Kind(DealKind::Sell))),
            _ => unreachable!(),
        }
    } else if cond_name.text().eq_ignore_ascii_case("age") {
        let (duration_type, num_days) = tag_err(E_BAD_AGE, digit1)(cond_value)?;
        let duration_type = preceded(
            space0,
            alt((
                map(tag_no_case("cd"), |_| AgeUnit::CalDays),
                map(tag_no_case("d"), |_| AgeUnit::Days),
            )),
        )(duration_type)?
        .1;
        Ok((input, Condition::Age(num_days.text().parse().unwrap(), duration_type)))
    } else {
        unreachable!()
    }
}

fn conditions<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, Condition> {
    let (input, condition) = fold_many1(
        |input| {
            let (rem, cond_value) = separated_field(',')(input)?;
            Ok((rem, condition(cond_value)?.1))
        },
        || Condition::True,
        |acc, cond| acc.and(cond),
    )(input)?;
    Ok((input, condition))
}

fn action_rule<'h, I: TextInput<'h>>(input: I) -> IParseResult<'h, I, ActionRule> {
    let pool_action = |mut input: I| {
        input = preceded(space0, tag_no_case("pool"))(input)?.0;
        let (input, pool_name) = delimited(space0, word, space0)(input)?;
        let (input, condition) = terminated(
            opt(preceded(pair(tag_no_case("until"), space0), conditions)),
            blank_line0,
        )(input)?;
        Ok((
            input,
            ActionRule::Pool(pool_name.text().to_string(), condition.unwrap_or(Condition::False)),
        ))
    };
    let match_action = |mut input: I| {
        input = preceded(space0, tag_no_case("match"))(input)?.0;
        let (input, pool_name) = terminated(delimited(space0, word, space0), blank_line0)(input)?;
        Ok((input, ActionRule::Match(pool_name.text().to_string())))
    };
    alt((pool_action, match_action))(input)
}
