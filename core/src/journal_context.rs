/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::alloc::HerdAllocator;
use crate::configuration::Configuration;
use crate::journal::Journal;
use crate::price_db::PriceDatabase;
use crate::report::command::ExecCommand;
use std::any::Any;
use std::cell::Cell;
use std::ptr;
use std::sync::atomic::{AtomicU16, AtomicUsize, Ordering};
use std::sync::{
    Arc, MappedRwLockReadGuard, MappedRwLockWriteGuard, Mutex, OnceLock, RwLock, RwLockReadGuard,
    RwLockWriteGuard,
};

thread_local! {
    static JOURNAL_CONTEXT: Cell<*const ()> = const { Cell::new(ptr::null()) };
}

/// The JournalContext is a scoped context set on a temporary `thread_local` high up on the call stack,
/// and _should_ be available to all low-level functions.
/// The context enables global variables of sorts that are specific to each journal, a design that
/// enables the program to support multiple journals at once.
pub struct JContext<'h> {
    /// Unique journal id
    jid: u16,
    journal: RwLock<Option<Journal<'h>>>,
    /// The current configuration in scope. This is useful when parsing the journal or processing entries in an execution context.
    curr_config: RwLock<Option<Configuration<'h>>>,
    allocator: &'h HerdAllocator<'h>,
    root_cmd: OnceLock<&'h dyn ExecCommand>,
    curr_cmd: OnceLock<Mutex<&'h dyn ExecCommand>>,
    chain_position: AtomicUsize,
    price_database: Arc<PriceDatabase<'h>>,
}

impl<'h> JContext<'h> {
    pub fn new(allocator: &'h HerdAllocator<'h>) -> Self {
        static JID_SEQ: AtomicU16 = AtomicU16::new(1);
        let jid = JID_SEQ.fetch_add(1, Ordering::SeqCst);

        Self {
            journal: RwLock::new(None),
            curr_config: RwLock::new(None),
            allocator,
            jid,
            root_cmd: OnceLock::new(),
            curr_cmd: OnceLock::new(),
            chain_position: AtomicUsize::new(0),
            price_database: Arc::new(PriceDatabase::default()),
        }
    }

    /// Sets the context on the call stack for lower level functions to use.
    /// The previous context, if any, is saved and restored after the inner
    /// function has completed.
    pub fn with<R>(&self, f: impl FnOnce() -> R) -> R {
        struct Guard(*const ());
        impl Drop for Guard {
            fn drop(&mut self) {
                let _ = JOURNAL_CONTEXT.try_with(|c| c.set(self.0));
            }
        }

        let prev = JOURNAL_CONTEXT
            .try_with(|c| {
                let prev = c.get();
                c.set(self as *const _ as *const ());
                prev
            })
            .unwrap_or(ptr::null());

        let _guard = Guard(prev);
        f()
    }

    /// Gets the context set higher up on the call stack.
    pub fn get() -> &'h JContext<'h> {
        JOURNAL_CONTEXT.with(|c| {
            let ptr = c.get();
            assert!(
                !ptr.is_null(),
                "No journal context in scope. Call JournalContextScope::with() first"
            );

            // SAFETY: The pointer is only valid when called inside with() and the guard
            // ensures it is never read after being dropped.
            unsafe { &*(ptr as *const JContext<'h>) }
        })
    }

    pub fn cmd(&self) -> &'h dyn ExecCommand {
        *self.curr_cmd.get().expect("Command not yet initialised").lock().unwrap()
    }

    /// Gets and converts the `Command` being run
    ///
    /// # Panics
    /// If the command hasn't been set yet, or the type of the command is not correct.
    pub fn cast_cmd<Cmd: ExecCommand>(&self) -> &Cmd {
        (self.cmd() as &dyn Any).downcast_ref::<Cmd>().expect("Command not of expected type")
    }

    pub fn set_cmd(&self, cmd: &'h dyn ExecCommand) -> &dyn ExecCommand {
        self.root_cmd.set(cmd).expect("Command already initialized");
        self.curr_cmd
            .set(Mutex::new(*self.root_cmd.get().unwrap()))
            .expect("Command already initialized");
        self.cmd()
    }

    pub fn advance_chain(&self) -> Option<&dyn ExecCommand> {
        {
            let mut curr_cmd =
                self.curr_cmd.get().expect("Command not yet initialised").lock().unwrap();
            let as_chainable = curr_cmd.as_chainable()?;
            let next = as_chainable.next_chain()?;
            *curr_cmd = next;
            self.chain_position.fetch_add(1, Ordering::SeqCst);
        }
        Some(self.cmd())
    }

    /// Gets the position of the current command in the entire command chain
    /// where `0` is the first command in the chain.
    pub fn chain_position(&self) -> usize {
        self.chain_position.load(Ordering::SeqCst)
    }

    /// Gets the current journal in scope.
    pub fn journal(&self) -> MappedRwLockReadGuard<'_, Journal<'h>> {
        let guard = self.journal.read().unwrap();
        RwLockReadGuard::map(guard, |j| j.as_ref().expect("Journal not yet initialized"))
    }

    pub fn journal_mut(&self) -> MappedRwLockWriteGuard<'_, Journal<'h>> {
        let guard = self.journal.write().unwrap();
        RwLockWriteGuard::map(guard, |j| j.as_mut().expect("Journal not yet initialized"))
    }

    pub fn set_journal(&self, journal: Journal<'h>) {
        let mut guard = self.journal.write().unwrap();
        *guard = Some(journal);
    }

    pub fn into_journal(self) -> Journal<'h> {
        self.journal.into_inner().unwrap().expect("Journal not yet initialized")
    }

    pub fn config(&self) -> MappedRwLockReadGuard<'_, Configuration<'h>> {
        let guard = self.curr_config.read().unwrap();
        RwLockReadGuard::map(guard, |c| c.as_ref().expect("Configuration not yet initialized"))
    }

    /// Gets a mutable reference to the current configuration that was set in scope.
    ///
    /// Note that any changes made to the configuration affect only the configuration held in the context.
    pub fn config_mut(&self) -> MappedRwLockWriteGuard<'_, Configuration<'h>> {
        let guard = self.curr_config.write().unwrap();
        RwLockWriteGuard::map(guard, |c| c.as_mut().expect("Configuration not yet initialized"))
    }

    pub fn set_config(&self, config: Configuration<'h>) {
        let mut guard = self.curr_config.write().unwrap();
        *guard = Some(config);
    }

    /// Gets the allocator in scope.
    pub fn allocator(&self) -> &'h HerdAllocator<'h> {
        self.allocator
    }

    pub fn jid(&self) -> u16 {
        self.jid
    }

    /// Gets the transient price database. The context may use this for caching price lookups when there
    /// is no permanent alternative.
    pub fn price_database(&self) -> &Arc<PriceDatabase<'h>> {
        &self.price_database
    }
}
