/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::account::Account;
use crate::configuration::Expression;
use crate::directive::{Directive, DirectiveKind};
use crate::error::parsing::promote;
use crate::match_blocks;
use crate::parsing::directive::{E_ACCOUNT_NAME, E_METADATA_OR_UNIT};
use crate::parsing::input::{BlockInput, ConfigInput, LocatedInput, NodeInput, TextInput};
use crate::parsing::unit_directive::unit_definition_body;
use crate::parsing::util::{double_quoted, param_value, repeat1, separated_field, word};
use crate::parsing::{JParseResult, entry, util};
use nom::Parser;
use nom::branch::alt;
use nom::combinator::{map, rest};
use std::collections::HashSet;
use std::sync::Arc;

pub fn account_directive<'h, 's, 'p, I>(input: I) -> JParseResult<I, Directive<'h>>
where
    I: TextInput<'h> + ConfigInput<'h> + BlockInput<'h> + LocatedInput<'h> + NodeInput<'h, 's, 'p>,
    'h: 's,
    's: 'p,
{
    let mut all_names = vec![];
    let account_name = || map(separated_field(','), |s: I| s.text());
    let input = promote(
        E_ACCOUNT_NAME,
        repeat1(account_name().and_then(|name: &'h str| {
            all_names.push(name.to_string());
            Ok(("", ()))
        })),
    )(input)?
    .0;

    let (rem, acc) = read_account_body(input.clone(), all_names)?;
    let merged = input.config_mut().merge_account(acc);
    Ok((rem, Directive::new(Some(input.block()), DirectiveKind::Account(merged))))
}

pub fn accounts_directive<'h, 's, 'p, I>(input: I) -> JParseResult<I, Directive<'h>>
where
    I: TextInput<'h> + NodeInput<'h, 's, 'p> + BlockInput<'h> + ConfigInput<'h> + LocatedInput<'h>,
    'h: 's,
    's: 'p,
{
    let (input, account_pattern) =
        promote("Account pattern expected", alt((double_quoted, word)))(input)?;

    let (rem, acc) = read_account_body(input.clone(), Vec::new())?;
    let acc = Arc::new(acc);
    input.config_mut().merge_account_pattern(
        Expression::from_glob_pattern(account_pattern.text(), None),
        Arc::clone(&acc),
    );

    Ok((rem, Directive::new(Some(input.block()), DirectiveKind::Accounts(acc))))
}

fn read_account_body<'h, 's, 'p, I>(
    input: I,
    all_names: Vec<String>,
) -> JParseResult<I, Account<'h>>
where
    I: TextInput<'h> + NodeInput<'h, 's, 'p> + BlockInput<'h> + ConfigInput<'h> + LocatedInput<'h>,
    'h: 's,
    's: 'p,
{
    let mut metadata = vec![];
    let mut units = HashSet::new();

    let rem = match_blocks!(input.clone(),
        param_value("unit") => |input: I| {
            let config = input.config().clone();
            let alloc = input.parse_node().allocator();
            let (_, (unit, aliases, new_code)) = unit_definition_body(input.clone())?;
            let merged = config.merged_unit(unit, &aliases, new_code);
            units.insert(&*alloc.alloc(merged));
            Ok(())
        },
        entry::metadata => |m| Ok(metadata.push(m)),
        util::comment => |_| Ok(()),
        rest => |i: I| Err(NomErr::Error(err!(i.into_err(E_METADATA_OR_UNIT))))
    )?
    .0;
    let mut config_mut = input.config_mut();
    let parent = Account::parent_str(all_names.first().unwrap())
        .map(|s| config_mut.get_or_create_account(&s));
    let mut acc =
        Account::new(all_names.first().cloned().unwrap_or_default(), parent, metadata, units);
    acc.set_aliases(all_names);
    Ok((rem, acc))
}
