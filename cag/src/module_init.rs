/*
 * Copyright (c) 2023-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::cgt_configuration::UnitOfAccountChange;
use crate::cgt_configuration::{AssignExpenses, MatchMethod};
use crate::cgt_configuration::{CgtConfiguration, PoolConfiguration};
use crate::ruleset;
use chrono_tz::Tz;
use env_logger::DEFAULT_WRITE_STYLE_ENV;
use journ_core::alloc::HerdAllocator;
use journ_core::directive::{Directive, DirectiveKind};
use journ_core::error::parsing::IErrorMsg;
use journ_core::error::parsing::promote;
use journ_core::module::{Module, ModuleDirective, ModuleDirectiveInput};
use journ_core::parsing::JParseResult;
use journ_core::parsing::text_input::{BlockInput, ConfigInput, LocatedInput, TextInput};
use journ_core::parsing::util::rest_line1;
use journ_core::parsing::util::{line_value, param_value};
use journ_core::parsing::{amount, entry};
use journ_core::unit::Unit;
use journ_core::{err, match_blocks};
use nom::combinator::{map, rest};
use std::str::FromStr;
use std::sync::LazyLock;

pub static MODULE_NAME: &str = "capital_gains";
pub static DEFAULT_CGT_CONFIG: LazyLock<CgtConfiguration> =
    LazyLock::new(|| CgtConfiguration::default());

pub fn initialize() -> Module {
    let mut module = Module::new(MODULE_NAME);
    module.add_directive(Box::new(CgtDirective));
    module.set_default_config(&*DEFAULT_CGT_CONFIG);
    module
}

pub const E_BAD_ALLOCEXPENSES: &str = "allocExpenses must be 'split' or 'unit'";
pub const E_BAD_ROUNDDEALS: &str = "roundDeals must be 'true' or 'false'";
pub const E_BAD_MATCHWHEN: &str = "match condition must be a lambda expression of two parameters";
pub const E_BAD_MATCH_METHOD: &str =
    "match method must be 'fifo', 'lifo' or 'average', or a combination of these separated by '/'";
pub const E_BAD_MAX_AGE: &str = "maxAge must be a number of at least 1";
pub const E_MAX_AGE_FINAL_POOL: &str = "maxAge is not allowed in the final pool configuration";
pub const E_MAX_AGE_WITH_AVERAGE_METHOD: &str =
    "maxAge is not allowed on a pool that uses the 'average' match method";
pub const E_UNKNOWN_PREMATCH_KEY: &str = "Unknown prematch parameter";
pub const E_DATE_IN_FUTURE: &str = "Date must not be in the future";

struct CgtDirective;
impl ModuleDirective for CgtDirective {
    fn name(&self) -> &'static str {
        "capitalgains"
    }
    fn parse<'h, 's, 'e, 'p>(
        &self,
        input: ModuleDirectiveInput<'h, 's, 'e, 'p>,
    ) -> JParseResult<ModuleDirectiveInput<'h, 's, 'e, 'p>, Directive<'h>>
    where
        'h: 'e,
        'e: 's,
    {
        let mut config = CgtConfiguration::empty();
        let (rem, input) = rest(input)?;

        match_blocks!(input,
            param_value("roundDealValues") => |input: ModuleDirectiveInput<'h, 's, 'e, 'p>| {
                config.set_round_deal_values(
                    bool::from_str(input.text()).map_err(|e| NomErr::Error(input.into_err(E_BAD_ROUNDDEALS).with_source(e)))?,
                );
                Ok(())
            },
            param_value("assignExpenses") => |input: ModuleDirectiveInput<'h, 's, 'e, 'p>| {
                config.set_assign_expenses(AssignExpenses::from_str(input.text()).map_err(NomErr::Error)?);
                Ok(())
            },
            param_value("timezone") => |input: ModuleDirectiveInput<'h, 's, 'e, 'p>| {
                config.set_timezone(Tz::from_str(input.text()).map_err(|e| NomErr::Error(input.into_err("Invalid timezone").with_source(e)))?);
                Ok(())
            },
            param_value("unitOfAccount") => |input: ModuleDirectiveInput<'h, 's, 'e, 'p>| {
                let (input, unit_code) = promote(IErrorMsg::UNIT, amount::unit)(input)?;
                let allocator = input.config().allocator();
                let unit = input.config_mut().merge_unit(&Unit::new(unit_code), allocator);
                let mut value_date = None;
                match_blocks!(input,
                    param_value("revalueDate") => |input: ModuleDirectiveInput<'h, 's, 'e, 'p>| {
                        let tz = input.config().timezone();
                        value_date = Some(promote(IErrorMsg::DATE, entry::datetime(tz))(input)?.1.datetime());
                        Ok(())
                    }
                )?;
                config.set_unit_of_account_change(Some(UnitOfAccountChange::new(unit.code().to_string(), value_date)));
                Ok(())
            },
            param_value("pool") => |input: ModuleDirectiveInput<'h, 's, 'e, 'p>| {
                config.set_pool(read_pool(input)?.1);
                Ok(())
            },
            param_value("rules") => |input: ModuleDirectiveInput<'h, 's, 'e, 'p>| {
                config.set_ruleset(promote("Invalid rules", ruleset::ruleset)(input)?.1);
                Ok(())
            },
            rest => |input: ModuleDirectiveInput<'h, 's, 'e, 'p>| {
                Err(NomErr::Failure(input.into_err(
                    "Invalid capitalgains directive"
                )))
            }
        )?;

        let existing: CgtConfiguration =
            input.config().module_config(MODULE_NAME).cloned().unwrap();
        match existing.merge(&config) {
            Ok(new_config) => {
                let allocated = input.allocator().alloc(new_config);
                input.config_mut().set_module_config(MODULE_NAME, allocated);
                Ok((rem, Directive::new(input.block(), DirectiveKind::Module(allocated))))
            }
            Err(_e) => unreachable!("Merge infallible right now"),
        }
    }
}

fn read_pool<'h, 'p, 's, 'e, I>(input: I) -> JParseResult<I, PoolConfiguration>
where
    I: TextInput<'h> + ConfigInput<'h> + BlockInput<'h> + LocatedInput<'h>,
    'h: 'e,
    'e: 's,
{
    let (rem, input) = rest(input)?;
    let (input, pool_name) = promote("Pool name expected", rest_line1)(input)?;
    let mut pool_config = PoolConfiguration::new(pool_name.text().trim().to_string());

    match_blocks!(input,
        param_value("unitOfAccount") => |input: I| {
            let (input, unit_code) = promote(IErrorMsg::UNIT, amount::unit)(input)?;
            let allocator = input.config().allocator();
            let unit = input.config_mut().merge_unit(&Unit::new(unit_code), allocator);
            let mut value_date = None;
            match_blocks!(input,
                param_value("revalueDate") => |input: I| {
                    let tz = input.config().timezone();
                    value_date = Some(promote(IErrorMsg::DATE, entry::datetime(tz))(input)?.1.datetime());
                    Ok(())
                }
            )?;
            pool_config.set_unit_of_account_change(Some(UnitOfAccountChange::new(unit.code().to_string(), value_date)));
            Ok(())
        },
        param_value("name") => |input: I| {
            pool_config.set_new_name(promote(IErrorMsg::VALUE, line_value)(input)?.1.text().to_string());
            Ok(())
        },
        param_value("method") => |orig_input: I| {
            let (_, value) = map(promote(IErrorMsg::VALUE, line_value), |l: I| l.text())(orig_input.clone())?;
            match value.find('/').map(|pos| value.split_at(pos)) {
                Some((pos_method, neg_method)) => {
                    let pos = MatchMethod::from_str(pos_method.trim_end())
                        .map_err(|_e| NomErr::Error(err!(orig_input.clone().into_err(E_BAD_MATCH_METHOD))))?;
                    let neg = MatchMethod::from_str(neg_method[1..].trim_start())
                        .map_err(|_e| NomErr::Error(err!(orig_input.clone().into_err(E_BAD_MATCH_METHOD))))?;
                    pool_config.set_methods((pos, neg));
                },
                None => {
                    let method = MatchMethod::from_str(orig_input.text())
                        .map_err(|_e| NomErr::Error(err!(orig_input.into_err(E_BAD_MATCH_METHOD))))?;
                    pool_config.set_methods((method, method));
                }
            }
            Ok(())
        }
    )?;
    Ok((rem, pool_config))
}
