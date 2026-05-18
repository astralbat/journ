/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::alloc::HerdAllocator;
use crate::journal::Journal;
use std::cell::Cell;
use std::ptr;
use std::sync::atomic::{AtomicU16, Ordering};
use std::sync::{
    MappedRwLockReadGuard, MappedRwLockWriteGuard, RwLock, RwLockReadGuard, RwLockWriteGuard,
};

thread_local! {
    static JOURNAL_CONTEXT: Cell<*const ()> = Cell::new(ptr::null());
}

/// The JournalContext is a scoped context set on a temporary `thread_local` high up on the call stack,
/// and _should_ be available to all low-level functions.
/// The context enables global variables of sorts that are specific to each journal, a design that
/// enables the program to support multiple journals at once.
pub struct JournalContext<'h> {
    /// Unique journal id
    jid: u16,
    journal: RwLock<Option<Journal<'h>>>,
    allocator: &'h HerdAllocator<'h>,
}

impl<'h> JournalContext<'h> {
    pub fn new(allocator: &'h HerdAllocator<'h>) -> Self {
        static JID_SEQ: AtomicU16 = AtomicU16::new(1);
        let jid = JID_SEQ.fetch_add(1, Ordering::SeqCst);

        Self { journal: RwLock::new(None), allocator, jid }
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
    pub fn current() -> &'h JournalContext<'h> {
        JOURNAL_CONTEXT.with(|c| {
            let ptr = c.get();
            assert!(
                !ptr.is_null(),
                "No journal context in scope. Call JournalContextScope::with() first"
            );

            // SAFETY: The pointer is only valid when called inside with() and the guard
            // ensures it is never read after being dropped.
            unsafe { &*(ptr as *const JournalContext<'h>) }
        })
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

    /// Gets the allocator in scope.
    pub fn allocator(&self) -> &'h HerdAllocator<'h> {
        self.allocator
    }

    pub fn jid(&self) -> u16 {
        self.jid
    }
}
