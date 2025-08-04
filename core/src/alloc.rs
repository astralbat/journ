/*
 * Copyright (c) 2023. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use bumpalo_herd::{Herd, Member};
use std::alloc::{AllocError, Layout};
use std::ptr::NonNull;
use thread_local::ThreadLocal;

/// This should only be used for testing. Unfortunately, there's no way to do conditional compilation
/// for doctests until https://github.com/rust-lang/rust/issues/67295 is resolved.
pub static HERD: std::sync::LazyLock<Herd> = std::sync::LazyLock::new(Default::default);

/*
pub trait Allocator<'h> {
    fn alloc<T>(&self, obj: T) -> &'h mut T;
    fn herd(&self) -> &'h Herd;
}

/// The allocator implements `Send` but not `Sync` due to `Member` not being `Sync`.
pub struct ThreadAllocator<'h> {
    herd: &'h Herd,
    member: OnceCell<Member<'h>>,
}
impl<'h> ThreadAllocator<'h> {
    pub fn new(herd: &'h Herd) -> Self {
        Self { herd, member: OnceCell::new() }
    }

    pub fn herd(&self) -> &'h Herd {
        self.herd
    }

    pub fn as_bump(&self) -> &Bump {
        let member = self.member.get_or_init(|| self.herd.get());
        member.as_bump()
    }

    pub fn alloc<T>(&self, obj: T) -> &'h mut T {
        let member = self.member.get_or_init(|| self.herd.get());
        member.alloc(obj)
    }
}

impl<'h, A> Allocator<'h> for &A
where
    A: Allocator<'h>,
{
    fn alloc<T>(&self, obj: T) -> &'h mut T {
        (*self).alloc(obj)
    }
    fn herd(&self) -> &'h Herd {
        (*self).herd()
    }
}

impl<'h> Allocator<'h> for ThreadAllocator<'h> {
    fn alloc<T>(&self, obj: T) -> &'h mut T {
        ThreadAllocator::alloc(self, obj)
    }
    fn herd(&self) -> &'h Herd {
        ThreadAllocator::herd(self)
    }
}

/*
impl<'h, 'a> Allocator<'h> for &'a ThreadAllocator<'h> {
    fn alloc<T>(&self, obj: T) -> &'h mut T {
        ThreadAllocator::alloc(self, obj)
    }
    fn herd(&self) -> &'h Herd {
        ThreadAllocator::herd(self)
    }
}*/

impl Clone for ThreadAllocator<'_> {
    fn clone(&self) -> Self {
        Self { herd: self.herd, member: OnceCell::new() }
    }
}*/

/// An allocator for `Herd` that implements the `std::alloc::Allocator` trait.
#[derive(Debug)]
pub struct HerdAllocator<'h> {
    herd: &'h Herd,
    member: ThreadLocal<Member<'h>>,
}
impl<'h> HerdAllocator<'h> {
    pub fn new(herd: &'h Herd) -> Self {
        Self { herd, member: ThreadLocal::new() }
    }

    pub fn member(&self) -> &Member<'h> {
        self.member.get_or(|| self.herd.get())
    }

    pub fn alloc<T>(&self, obj: T) -> &'h mut T {
        self.member.get_or(|| self.herd.get()).alloc(obj)
    }
}
unsafe impl<'h> std::alloc::Allocator for HerdAllocator<'h> {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        self.member.get_or(|| self.herd.get()).as_bump().allocate(layout)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        unsafe { self.member.get_or(|| self.herd.get()).as_bump().deallocate(ptr, layout) }
    }
}
