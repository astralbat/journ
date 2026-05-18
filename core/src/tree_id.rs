/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use std::alloc::{Layout, alloc, dealloc};
use std::fmt::Formatter;
use std::hash::Hash;
use std::ops::Deref;
use std::sync::atomic;
use std::sync::atomic::AtomicUsize;
use std::{cmp, fmt, ptr};

/// A hierarchy identifier. It encapsulates an incrementing identifier, along with its parent
/// identifiers in a very memory-efficient manor. Virtually always using 8 bytes of memory on 64bit
/// platforms - spilling on to the heap only when the number of levels exceeds what can be encoded in a single word.
pub union TreeId {
    inline: [u8; size_of::<usize>()],
    heap: *mut u8,
}

// SAFETY: This is ok since we guarantee that there's no interior mutability through shared refs
unsafe impl Sync for TreeId {}
unsafe impl Send for TreeId {}

impl TreeId {
    const WORD: usize = size_of::<usize>();
    pub const MIN: TreeId = TreeId::min();
    pub const MAX_INLINE: TreeId = TreeId::max_inline();
    pub const MAX_INLINE_VALUE: usize = (1 << ((Self::WORD - 1) * 7)) - 1;

    const fn min() -> Self {
        let mut inline = [0x0u8; Self::WORD];
        // The smallest legal id is 1.
        inline[0] = 0x01;
        // Mark the final byte to be the inline flag.
        inline[Self::WORD - 1] = 0x01;
        TreeId { inline }
    }

    /// While the max `TreeId` is usize::MAX, we can for the sake of argument derive
    /// a maximum `TreeId` that is the largest possible inline id. This should suffice for any
    /// practical need.
    ///
    /// This creates the number [Self::MAX_INLINE_VALUE] which represents that nth child under the root.
    const fn max_inline() -> Self {
        let mut inline = [0xffu8; Self::WORD];
        inline[Self::WORD - 2] = 0x7f;
        // Flag
        inline[Self::WORD - 1] = 0x01;
        TreeId { inline }
    }

    /// Creates a new Id using the encoded `bytes`.
    fn new(bytes: &[u8]) -> Self {
        debug_assert!(!bytes.contains(&0), "TreeId bytes must not contain null");
        debug_assert!(
            bytes.is_empty() || bytes[bytes.len() - 1] & 0x80 == 0,
            "Final byte cannot be a continuation byte"
        );
        // 255 bytes of hierarchy ought to be enough for anyone.
        debug_assert!(bytes.len() <= 255, "Maximum number of levels exceeded");

        // We can use inline if the number of bytes fits in a WORD
        if bytes.len() < Self::WORD - 1 {
            let mut inline = [0u8; Self::WORD];
            inline[..bytes.len()].copy_from_slice(bytes);

            // Mark the final byte as the inline flag.
            inline[Self::WORD - 1] = 0x01;

            TreeId { inline }
        } else {
            unsafe {
                let layout = Layout::from_size_align(Self::WORD + bytes.len(), Self::WORD).unwrap();

                let ptr = alloc(layout);
                if ptr.is_null() {
                    std::alloc::handle_alloc_error(layout);
                }

                // store total length
                *ptr = bytes.len() as u8;

                ptr::copy_nonoverlapping(bytes.as_ptr(), ptr.add(1), bytes.len());

                TreeId { heap: ptr }
            }
        }
    }

    pub fn new_root() -> Self {
        Self::new(&[])
    }

    pub fn parent(&self) -> Option<Self> {
        let slice = self.as_slice();
        if slice.len() > 0 {
            let mut try_parent = &slice[0..slice.len() - 1];
            while try_parent.len() > 0 && try_parent[try_parent.len() - 1] >= 0x80 {
                try_parent = &try_parent[..try_parent.len() - 1]
            }
            Some(Self::new(&try_parent))
        } else {
            None
        }
    }

    /// Gets whether this id starts with `other` id.
    pub fn starts_with(&self, other: &Self) -> bool {
        // Can't start with something longer than itself
        if other.iter().count() > self.iter().count() {
            return false;
        }
        for (self_val, other_val) in self.iter().zip(other.iter()) {
            if self_val != other_val {
                return false;
            }
        }
        true
    }

    /// The id or 0 if this is the root.
    pub fn id(&self) -> usize {
        self.iter().next_back().unwrap_or(0)
    }

    /// Branches from this parent, returning an id that points
    /// to the child_num'th child of this node. child_num is 1-indexed.
    pub fn branch(&self, child_num: usize) -> Self {
        debug_assert!(child_num > 0, "child_num must be > 0");

        let slice = self.as_slice();
        let mut buf = Vec::with_capacity(slice.len() + 2);
        buf.extend_from_slice(slice);
        encode_next_leb128(child_num, &mut buf);
        Self::new(&buf)
    }

    /// Adds 1 to the last component of the id.
    ///
    /// # Panics
    /// If this is the root node.
    pub fn increment(&mut self) {
        *self = self.incremented();
    }

    /// Adds 1 to the last component of the id, returning the result.
    ///
    /// # Panics
    /// If this is the root node.
    pub fn incremented(&self) -> Self {
        let mut buf = Vec::new();
        let mut iter = self.iter().peekable();
        if iter.peek().is_none() {
            panic!("Root node cannot be incremented")
        }

        while let Some(val) = iter.next() {
            if iter.peek().is_none() {
                encode_next_leb128(val.checked_add(1).expect("TreeId overflow"), &mut buf);
            } else {
                encode_next_leb128(val, &mut buf);
            }
        }
        drop(iter);
        Self::new(&buf)
    }

    #[inline]
    fn is_heap(&self) -> bool {
        unsafe { (self.inline[Self::WORD - 1] & 0x01) == 0x00 }
    }

    #[inline]
    fn heap_ptr(&self) -> *mut u8 {
        unsafe { (self.heap as usize) as *mut u8 }
    }

    /// Gets the encoded slice of bytes representing the tree of ids.
    /// In the inline case, the final byte will include the inline flag, and thus appear to be a continuation byte. This
    /// needs to be ignored during decode.
    fn as_slice(&self) -> &[u8] {
        unsafe {
            if self.is_heap() {
                let ptr = self.heap_ptr();
                let len = *(ptr as *const u8);
                std::slice::from_raw_parts(ptr.add(1), len as usize)
            } else {
                // Ignore final byte which carries the flag.
                &self.inline
                    [..self.inline.iter().position(|&b| b == 0).unwrap_or(self.inline.len() - 1)]
            }
        }
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = usize> + '_ {
        struct ParentIterator<'a> {
            slice: &'a [u8],
        }
        impl Iterator for ParentIterator<'_> {
            type Item = usize;
            fn next(&mut self) -> Option<usize> {
                if self.slice.len() > 0 {
                    let val = decode_next_leb128(&mut self.slice).unwrap();
                    Some(val)
                } else {
                    None
                }
            }
        }
        impl DoubleEndedIterator for ParentIterator<'_> {
            fn next_back(&mut self) -> Option<usize> {
                if self.slice.is_empty() {
                    return None;
                }
                if self.slice.len() == 1 {
                    let val = decode_next_leb128(&mut self.slice).unwrap();
                    return Some(val);
                }
                let mut i = self.slice.len() - 1;
                while i > 0 && self.slice[i - 1] & 0x80 != 0 {
                    i -= 1;
                }
                let val = decode_next_leb128(&mut &self.slice[i..]).unwrap();
                self.slice = &self.slice[..i];
                Some(val)
            }
        }
        ParentIterator { slice: self.as_slice() }
    }
}

impl Drop for TreeId {
    fn drop(&mut self) {
        if self.is_heap() {
            unsafe {
                let ptr = self.heap_ptr();
                let len = *(ptr as *const u8);

                let layout =
                    Layout::from_size_align(TreeId::WORD + len as usize, TreeId::WORD).unwrap();

                dealloc(ptr, layout);
            }
        }
    }
}

impl Clone for TreeId {
    fn clone(&self) -> Self {
        Self::new(self.as_slice())
    }
}

impl Hash for TreeId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_slice().hash(state);
    }
}

impl PartialEq for TreeId {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl Eq for TreeId {}

impl PartialOrd for TreeId {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TreeId {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.iter().cmp(other.iter())
    }
}

impl fmt::Display for TreeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut peekable = self.iter().peekable();
        while let Some(item) = peekable.next() {
            write!(f, "{}", item)?;
            if peekable.peek().is_some() {
                write!(f, ".")?;
            }
        }
        Ok(())
    }
}

impl fmt::Debug for TreeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(self, f)
    }
}

pub struct BranchCountingTreeId {
    inner: TreeId,
    branch_count: AtomicUsize,
}

impl BranchCountingTreeId {
    pub fn branch(&self) -> TreeId {
        self.inner.branch(self.branch_count.fetch_add(1, atomic::Ordering::SeqCst))
    }
}

impl From<TreeId> for BranchCountingTreeId {
    fn from(tree_id: TreeId) -> Self {
        BranchCountingTreeId { inner: tree_id, branch_count: AtomicUsize::new(1) }
    }
}
impl PartialEq for BranchCountingTreeId {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl PartialEq<TreeId> for BranchCountingTreeId {
    fn eq(&self, other: &TreeId) -> bool {
        self.inner == *other
    }
}

impl Deref for BranchCountingTreeId {
    type Target = TreeId;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl AsRef<TreeId> for BranchCountingTreeId {
    fn as_ref(&self) -> &TreeId {
        &self.inner
    }
}

impl fmt::Display for BranchCountingTreeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

impl fmt::Debug for BranchCountingTreeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(self, f)
    }
}

/// Appends to the `buf` the encoding of `value`.
/// This will never push a null byte unless value is 0.
fn encode_next_leb128(value: usize, buf: &mut Vec<u8>) -> &[u8] {
    let mut val = value;
    let start = buf.len();
    while val >= 0x80 {
        buf.push((val as u8 & 0x7F) | 0x80);
        val >>= 7;
    }
    buf.push(val as u8);
    &buf[start..]
}

/// Reads the next usize value, truncating the front of the slice the number
/// of read bytes.
///
/// Returns `None` iff the slice is empty.
fn decode_next_leb128(slice: &mut &[u8]) -> Option<usize> {
    let mut value = 0usize;
    let mut shift = 0;
    let mut read = 0;

    if slice.len() == 0 {
        return None;
    }

    for b in slice.iter().copied() {
        read += 1;

        value |= ((b & 0x7F) as usize) << shift;

        // If this is the final byte it cannot be used as a continuation byte. So
        // assume that if it is set, it is the inline flag bit that needs ignoring.
        if b & 0x80 == 0 {
            *slice = &slice[read..];
            return Some(value);
        }

        shift += 7;
        if shift >= usize::BITS as usize {
            panic!("leb128 out of range");
        }
    }
    panic!("leb128 next byte not available")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cmp() {
        assert_eq!(TreeId::new(&[1, 2, 3]).cmp(&TreeId::new(&[1, 2, 3])), cmp::Ordering::Equal);
        assert_eq!(TreeId::new(&[1, 2]).cmp(&TreeId::new(&[1, 2, 3])), cmp::Ordering::Less);
        assert_eq!(TreeId::new(&[1, 1, 4]).cmp(&TreeId::new(&[1, 1, 3])), cmp::Ordering::Greater);
    }

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", TreeId::new(&[1, 2, 3])), "1.2.3");
        assert_eq!(format!("{}", TreeId::new(&[0x80, 0x1, 0x80, 0x2])), "128.256");
    }

    #[test]
    fn test_encode() {
        // Encode 1
        let mut buf = Vec::<u8>::new();
        encode_next_leb128(1, &mut buf);
        assert_eq!(buf, [0x01]);

        // Encode 127
        let mut buf = Vec::<u8>::new();
        encode_next_leb128(127, &mut buf);
        assert_eq!(buf, [0x7F]);

        // Encode 128
        let mut buf = Vec::<u8>::new();
        encode_next_leb128(128, &mut buf);
        assert_eq!(buf, [0x80, 0x01]);

        // Encode 129
        let mut buf = Vec::<u8>::new();
        encode_next_leb128(129, &mut buf);
        assert_eq!(buf, [0x81, 0x01]);

        // Encode 256
        let mut buf = Vec::<u8>::new();
        encode_next_leb128(256, &mut buf);
        assert_eq!(buf, [0x80, 0x02]);
    }

    #[test]
    fn test_iter() {
        let id = TreeId::new(&[1, 2, 3, 0x80, 0x01]);
        let mut iter = id.iter();
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next_back(), Some(128));
        assert_eq!(iter.next_back(), Some(3));
        assert_eq!(iter.next_back(), Some(2));
        assert_eq!(iter.next_back(), None);
    }

    #[test]
    fn test_min_max() {
        assert_eq!(TreeId::min().iter().next(), Some(1));
        assert_eq!(TreeId::max_inline().iter().next(), Some(TreeId::MAX_INLINE_VALUE));
        assert_eq!(
            encode_next_leb128(TreeId::MAX_INLINE_VALUE, &mut vec![]),
            [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f]
        );
    }

    #[test]
    fn test_heap() {
        // Incrementing the max inline id, spills it on to the heap
        let mut id = TreeId::max_inline();
        assert!(!id.is_heap());
        id.increment();
        assert!(id.is_heap());
        assert_eq!(id.iter().next(), Some(TreeId::MAX_INLINE_VALUE + 1))
    }
}
