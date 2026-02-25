/*
 * Copyright (c) 2023. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::directive::{Directive, DirectiveKind};
use crate::error::JournResult;
use crate::journal::Journal;
use crate::parsing::JParseResult;
use crate::parsing::input::TextBlockInput;
use crate::parsing::parser::JournalParseNode;
use crate::report::command::ExecCommand;
use crate::report::command::arguments::Arguments;
use std::any::Any;
use std::fmt::Debug;
use std::sync::Mutex;

pub struct Module {
    name: &'static str,
    directives: Vec<Box<dyn ModuleDirective>>,
    default_config: Option<&'static dyn ModuleConfiguration>,
    command: Option<Box<dyn ModuleCommand>>,
}

pub static MODULES: Mutex<Vec<Module>> = Mutex::new(vec![]);

impl Module {
    pub fn register(module: Module) {
        MODULES.lock().unwrap().push(module);
    }

    pub fn new(name: &'static str) -> Self {
        Self { name, directives: vec![], default_config: None, command: None }
    }

    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn directives(&self) -> impl Iterator<Item = &dyn ModuleDirective> {
        self.directives.iter().map(|d| d.as_ref())
    }

    pub fn default_config(&self) -> Option<&'static dyn ModuleConfiguration> {
        self.default_config
    }

    pub fn set_default_config(&mut self, config: &'static dyn ModuleConfiguration) {
        self.default_config = Some(config);
    }

    pub fn add_directive(&mut self, dir: Box<dyn ModuleDirective>) {
        self.directives.push(dir);
    }

    pub fn command(&self) -> Option<&dyn ModuleCommand> {
        self.command.as_deref()
    }

    pub fn set_command(&mut self, cmd: Box<dyn ModuleCommand>) {
        self.command = Some(cmd);
    }
}

/// Implementing types must be static in order to be able to implement `Any`. This is necessary
/// when retrieving the configuration and downcasting it to the concrete type.
pub trait ModuleConfiguration:
    Any + Debug + Send + Sync + ModuleConfigurationEq + ModuleConfigurationClone
{
    fn as_any(&self) -> &dyn Any;

    fn merge(&mut self, other: Box<dyn ModuleConfiguration>);
}
pub trait ModuleConfigurationEq {
    fn eq(&self, other: &dyn ModuleConfiguration) -> bool;
}
pub trait ModuleConfigurationClone {
    fn clone_box(&self) -> Box<dyn ModuleConfiguration>;
}

/// Implement `PartialEq` on `DirectiveKind` rather than `Self` or `ModuleDirectiveObj` as
/// a workaround to make object-safe.
pub trait ModuleDirectiveObj:
    Any + Debug + for<'h> PartialEq<DirectiveKind<'h>> + Send + Sync
{
    fn as_any(&self) -> &dyn Any;
}

pub type ModuleDirectiveInput<'h, 's, 'p> = TextBlockInput<'h, &'p JournalParseNode<'h, 's>>;

pub trait ModuleDirective: Send {
    fn name(&self) -> &'static str;
    fn parse<'h, 's, 'e, 'p>(
        &self,
        input: ModuleDirectiveInput<'h, 's, 'p>,
    ) -> JParseResult<ModuleDirectiveInput<'h, 's, 'p>, Directive<'h>>
    where
        'h: 'e,
        'e: 's;
}

pub trait ModuleCommand: Send {
    fn name(&self) -> &'static str;

    fn create(
        &self,
        journal: &Journal,
        args: &Arguments,
        command_args: &[String],
    ) -> JournResult<Box<dyn ExecCommand>>;
}
