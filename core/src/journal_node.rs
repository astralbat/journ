/*
 * Copyright (c) 2020-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::alloc::HerdAllocator;
use crate::directive::{Directive, DirectiveKind};
use crate::err;
use crate::error::JournResult;
use crate::journal_entry::{EntryId, JournalEntry};
use crate::journal_node_segment::JournalNodeSegment;
use crate::parsing::text_block::TextBlock;
use crate::parsing::text_input::TextBlockInput;
use std::fs::OpenOptions;
use std::hash::Hash;
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{MutexGuard, OnceLock};
use std::{cmp, fs};

/// An iterator over the directives from a journal node's tree.
pub struct DirectiveTreeIter<'h, 'a, 'b> {
    inner: &'b MutexGuard<'a, Vec<Directive<'h>>>,
    position: usize,
    current_segment: Option<&'h JournalNodeSegment<'h>>,
}

impl<'h, 'a, 'b> DirectiveTreeIter<'h, 'a, 'b> {
    pub fn new(root: &'a JournalNode<'h>) -> Self {
        let current_segment = root.segments().first().copied();

        // Leak the guard and reclaim it later. We bound the lifetime of the guard to the external lifetime 'b.
        // We need to do this because iterator in Rust cannot return a reference to a variable in the iterator struct.
        let dti = Self {
            inner: Box::leak(Box::new(current_segment.unwrap().directives())),
            position: 0,
            current_segment,
        };
        dti
    }
}

impl<'h, 'a, 'b> Iterator for DirectiveTreeIter<'h, 'a, 'b> {
    type Item = &'b Directive<'h>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(dir) = self.inner.get(self.position) {
                self.position += 1;
                return Some(dir);
            }
            let current_segment = self.current_segment.and_then(|seg| seg.next_segment());
            match current_segment {
                Some(seg) => {
                    self.current_segment = Some(seg);
                    unsafe {
                        let _ = Box::from_raw(
                            self.inner as *const MutexGuard<'a, Vec<Directive<'h>>>
                                as *mut MutexGuard<'a, Vec<Directive<'h>>>,
                        );
                    }
                    self.inner = Box::leak(Box::new(seg.directives()));
                    self.position = 0;
                }
                None => return None,
            }
            continue;
        }
    }
}

impl<'t, 'a, 'b> Drop for DirectiveTreeIter<'t, 'a, 'b> {
    fn drop(&mut self) {
        // Free guard that were leaked during the iteration.
        #[allow(unused_must_use)]
        unsafe {
            Box::from_raw(
                self.inner as *const MutexGuard<'a, Vec<Directive>>
                    as *mut MutexGuard<'a, Vec<Directive>>,
            );
        }
    }
}

/// A unique identifier for nodes in journal trees. Each node id is allocated uniquely.
///
/// NodeIds can be compared as nodes are created with _some_ guarantee of order (assumed behaviour of parser):
/// - Siblings are always created in order.
/// - Children Ids are always created after parent Ids.
// Implementation notes:
// Different branches may create nodes at different rates with different ordering of `id`.
#[derive(Debug, Clone, Copy)]
pub struct NodeId<'h> {
    /// Parent, or `Err(i)` if at the root node. Where `i` represents the `ith` journal incarnation.
    parent: Result<&'h NodeId<'h>, u32>,
    /// A unique identifier.
    id: u32,
}

impl<'h> NodeId<'h> {
    pub fn new_root() -> Self {
        static ROOT_COUNTER: AtomicU32 = AtomicU32::new(1);
        Self { parent: Err(ROOT_COUNTER.fetch_add(1, Ordering::Relaxed)), id: Self::generate_id() }
    }

    fn generate_id() -> u32 {
        static ID_COUNTER: AtomicU32 = AtomicU32::new(1);
        ID_COUNTER.fetch_add(1, Ordering::Relaxed)
    }

    /// Gets the depth of the node in the journal tree where 0 is the root.
    fn depth(&self) -> u32 {
        match self.parent {
            Ok(p) => p.depth() + 1,
            Err(_) => 0,
        }
    }

    /// Gets the journal incarnation number. That is, how many journals have been created
    /// since the start of the program. Each journal is assigned a unique sequential incarnation
    /// number.
    /// The first journal has an incarnation of 1.
    pub fn journal_incarnation(&self) -> u32 {
        match self.parent {
            Ok(p) => p.journal_incarnation(),
            Err(i) => i,
        }
    }

    pub fn branch(&'h self) -> Self {
        Self { parent: Ok(self), id: Self::generate_id() }
    }

    /// A numeric identifier for this node, guaranteed to be unique. However, it should
    /// not be used for ordering purposes.
    pub fn id(&self) -> u32 {
        self.id
    }
}

impl PartialEq for NodeId<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for NodeId<'_> {}

impl Hash for NodeId<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialOrd for NodeId<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for NodeId<'_> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        let self_depth = self.depth();
        let other_depth = other.depth();

        // Bring the lower node up to the same depth as the upper one.
        let mut self_base = self;
        let mut other_base = other;
        while self_depth > other_depth {
            self_base = self_base.parent.unwrap();
        }
        while other_depth > self_depth {
            other_base = other_base.parent.unwrap();
        }

        // Compare the nodes starting from the same depth.
        self_base.parent.cmp(&other_base.parent).then_with(|| self_base.id.cmp(&other_base.id))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum JournalNodeKind {
    Entry,
    Prices,
    Python,
}

/// A node usually representing a file on the filesystem.
#[derive(Debug)]
pub struct JournalNode<'h> {
    node_id: &'h NodeId<'h>,
    kind: JournalNodeKind,
    filename: Option<&'h Path>,
    input: TextBlockInput<'h, ()>,
    allocator: &'h HerdAllocator<'h>,
    segments: OnceLock<Vec<&'h JournalNodeSegment<'h>>>,
    //directives: Mutex<Vec<Directive<'h>>>,
    parent: Option<&'h JournalNode<'h>>,
    children: OnceLock<Vec<&'h JournalNode<'h>>>,
}

impl<'h> JournalNode<'h> {
    /// Creates a new JournalFile
    pub fn new(
        parent: Option<&'h JournalNode<'h>>,
        node_id: &'h NodeId,
        filename: Option<&'h Path>,
        kind: JournalNodeKind,
        input: TextBlockInput<'h, ()>,
        //directives: Vec<Directive<'h>>,
        allocator: &'h HerdAllocator<'h>,
    ) -> &'h JournalNode<'h> {
        allocator.alloc(Self {
            node_id,
            filename,
            kind,
            input,
            allocator,
            segments: OnceLock::new(),
            parent,
            children: OnceLock::new(),
        })
    }

    pub fn parent(&self) -> Option<&'h JournalNode<'h>> {
        self.parent
    }

    pub fn children(&self) -> impl Iterator<Item = &JournalNode<'h>> + '_ {
        self.children.get().into_iter().flat_map(|c| c.iter().map(|a| &**a))
    }

    pub(crate) fn set_children(&self, children: Vec<&'h JournalNode<'h>>) {
        self.children.set(children).expect("Children already set");
    }

    /// Gets a list of files in depth-first order.
    pub fn children_recursive(&self) -> Vec<&JournalNode<'h>> {
        let mut files = vec![];
        files.push(self);
        for child in self.children.get().into_iter().flatten() {
            files.append(&mut child.children_recursive());
        }
        files
    }

    pub fn find_by_node_id(&self, node_id: &'h NodeId<'h>) -> Option<&Self> {
        if self.node_id == node_id {
            return Some(self);
        }
        for child in self.children.get().into_iter().flatten() {
            if let Some(node) = child.find_by_node_id(node_id) {
                return Some(node);
            }
        }
        None
    }

    pub fn filename(&self) -> Option<&'h Path> {
        self.filename
    }

    /// Gets the path of the file this node belongs to, if any.
    pub fn nearest_filename(&self) -> Option<&'h Path> {
        if let Some(file) = self.filename {
            return Some(file);
        }

        let mut parent = self.parent();
        while let Some(p) = parent {
            if let Some(file) = p.filename {
                return Some(file);
            }
            parent = p.parent();
        }
        None
    }

    /// Gets the canonical path of the file this node belongs to, if any.
    pub fn canonical_filename(&self) -> Option<PathBuf> {
        Some(fs::canonicalize(self.nearest_filename()?).expect("Failed to canonicalize filename"))
    }

    pub fn allocator(&self) -> &'h HerdAllocator<'h> {
        self.allocator
    }

    pub fn id(&self) -> &'h NodeId<'h> {
        self.node_id
    }

    pub fn file_kind(&self) -> JournalNodeKind {
        self.kind
    }

    /// Gets the segments of this node. The node will always have at least one segment.
    pub fn segments(&self) -> &Vec<&'h JournalNodeSegment<'h>> {
        &self.segments.get().expect("Segments not initialised")
    }

    pub fn set_segments(&self, segments: Vec<&'h JournalNodeSegment<'h>>) {
        self.segments.set(segments).expect("Segments already set");
    }

    pub fn input(&self) -> TextBlockInput<'h, ()> {
        self.input
    }

    pub fn block(&self) -> &'h TextBlock<'h> {
        self.input.block()
    }

    pub fn clear_directives_filter<F>(&self, filter: F)
    where
        F: Fn(&Directive) -> bool,
    {
        for seg in self.segments() {
            seg.directives().retain(|dir| !filter(dir));
        }
    }

    /// Appends a new entry directive to the end of the node.
    pub(crate) fn append_entry(
        &self,
        entry: JournalEntry<'h>,
    ) -> (&'h TextBlock<'h>, &'h JournalEntry<'h>) {
        let alloc_entry: &'h JournalEntry<'h> = self.allocator.alloc(entry);
        let mut tb: Option<&'h TextBlock<'h>> = None;
        self.append_directives(|_len, leading_sp| match tb {
            None => {
                let entry_string = format!("{}{}", leading_sp, alloc_entry);
                let entry_str = self.allocator.alloc(entry_string);
                tb = Some(self.allocator.alloc(TextBlock::from(entry_str.as_str())));
                Some(Directive::new(tb.unwrap(), DirectiveKind::Entry(alloc_entry)))
            }
            _ => None,
        });
        (tb.unwrap(), alloc_entry)
    }

    pub(crate) fn append_directives<F>(&self, mut dir_fn: F)
    where
        F: FnMut(usize, &'h str) -> Option<Directive<'h>>,
    {
        let mut dir_lock = self.segments().last().unwrap().directives();
        loop {
            match dir_lock.len() {
                0 => match dir_fn(0, "") {
                    Some(dir) => dir_lock.push(dir),
                    None => break,
                },
                n => {
                    let leading_space = dir_lock[n - 1].raw().leading_blank_lines();
                    match dir_fn(n, leading_space) {
                        Some(dir) => dir_lock.push(dir),
                        None => break,
                    }
                }
            }
        }
    }

    /// Performs a binary search, looking for an insert position.
    /// Directives aren't expected to be ordered within a node, but this will make a reasonable attempt
    /// to find a good insert position.
    pub(crate) fn insert_directive<F>(&self, dir_kind: &DirectiveKind<'h>, dir_fn: F)
    where
        F: FnOnce(usize, &'h str) -> Directive<'h>,
    {
        // First, find the segment we need to be in by searching each in turn.
        for seg in self.segments() {
            if let Some(last_dir) = seg.directives().last() {
                match last_dir.kind().partial_cmp(dir_kind) {
                    None | Some(cmp::Ordering::Less) => {
                        // This segment is not the right one, so continue to the next.
                        continue;
                    }
                    _ => {
                        self.insert_directive_in_segment(seg, dir_kind, dir_fn);
                        break;
                    }
                }
            }
        }
    }

    pub(crate) fn insert_directive_in_segment<F>(
        &self,
        segment: &JournalNodeSegment<'h>,
        dir_kind: &DirectiveKind<'h>,
        dir_fn: F,
    ) where
        F: FnOnce(usize, &'h str) -> Directive<'h>,
    {
        // This is the segment we need to insert into.
        // Perform a binary search to find the right position.

        let mut dir_lock = segment.directives();
        let mut min = 0usize;
        let mut max = dir_lock.len();

        'outer: while max > min {
            let mid = min + ((max as f32 - min as f32) / 2_f32).ceil() as usize;

            // Walk back until same kind of directive found.
            // Walking back as opposed to forwards is preferred as new kinds of directives will be
            // inserted at the end of the file.
            for i in (min..mid).rev() {
                match dir_lock[i].kind().partial_cmp(dir_kind) {
                    Some(cmp::Ordering::Greater) => max = i,
                    Some(cmp::Ordering::Less) => min = i + 1,
                    Some(cmp::Ordering::Equal) => {
                        max = i;
                        min = i;
                    }
                    // Not comparable, keep looking
                    None => continue,
                }
                continue 'outer;
            }
            // No directive found that's comparable, so look later
            min = mid;
        }

        match max {
            0 => dir_lock.insert(0, dir_fn(0, "")),
            n => {
                let leading_space = dir_lock[n - 1].raw().leading_blank_lines();
                dir_lock.insert(n, dir_fn(n, leading_space));
            }
        }
    }

    /// Inserts a JournalEntry within the specified file and in the correct date position.
    pub(crate) fn insert_entry(
        &self,
        entry: JournalEntry<'h>,
    ) -> (&'h TextBlock<'h>, &'h JournalEntry<'h>) {
        let entry: &'h JournalEntry<'h> = self.allocator.alloc(entry);
        let kind = &DirectiveKind::Entry(entry);
        let mut tb: Option<&'h TextBlock<'h>> = None;
        self.insert_directive(kind, |_pos, leading_sp| {
            let entry_string = format!("{}{}", leading_sp, entry);
            let entry_str = self.allocator.alloc(entry_string);
            tb = Some(self.allocator.alloc(TextBlock::from(entry_str.as_str())));
            Directive::new(tb.as_ref().unwrap(), DirectiveKind::Entry(entry))
        });
        (tb.unwrap(), entry)
    }

    pub fn replace_entry(
        &self,
        entry: JournalEntry<'h>,
        allocator: &'h HerdAllocator<'h>,
    ) -> (&'h JournalEntry<'h>, (&'h TextBlock<'h>, &'h JournalEntry<'h>)) {
        let mut entry_string = entry.to_string();
        let entry = allocator.alloc(entry);
        for seg in self.segments() {
            for dir in seg.directives().iter_mut() {
                if let DirectiveKind::Entry(curr_entry) = dir.kind() {
                    if curr_entry.id() == entry.id() {
                        let curr_entry = *curr_entry;

                        entry_string.insert_str(0, dir.raw().leading_blank_lines());
                        let entry_str = allocator.alloc(entry_string);
                        let tb = allocator.alloc(TextBlock::from(entry_str.as_str()));

                        *dir = Directive::new(tb, DirectiveKind::Entry(entry));
                        return (curr_entry, (tb, entry));
                    }
                }
            }
        }
        panic!("Entry no longer exists")
    }

    pub fn remove_entry(&self, date_id: u64) {
        for seg in self.segments() {
            let mut dir_lock = seg.directives();
            dir_lock.retain(|dir| {
                if let DirectiveKind::Entry(parsed_entry) = dir.kind() {
                    parsed_entry.date_id() != date_id
                } else {
                    true
                }
            })
        }
    }

    /// Gets all directives in their included order.
    pub fn all_directives_iter<'a, 'b>(&'a self) -> DirectiveTreeIter<'h, 'a, 'b> {
        DirectiveTreeIter::new(self)
    }

    pub fn entry(&self, entry_id: EntryId) -> &'h JournalEntry<'h> {
        assert_eq!(entry_id.node_id(), self.node_id, "Entries fileId does not match");

        for seg in self.segments() {
            for dir in seg.directives().iter() {
                if let DirectiveKind::Entry(next_entry) = dir.kind() {
                    if next_entry.id() == entry_id {
                        return next_entry;
                    }
                }
            }
        }
        panic!("Entry no longer exists")
    }

    pub fn write_file(&self) -> JournResult<()> {
        match self.filename {
            Some(file) => {
                debug!("Writing {}", file.to_str().unwrap());
                let mut writer = BufWriter::new(
                    OpenOptions::new()
                        .write(true)
                        .create(true)
                        .truncate(true)
                        .open(file)
                        .map_err(|e| err!(e; "IO Error"))?,
                );
                self.write(&mut writer, None)?;
                Ok(())
            }
            None => Err(err!("Cannot write journal file: file represents a text stream")),
        }
    }

    /// Writes the nearest file to this node which is the node's file or the nearest file in the parent chain.
    pub fn write_nearest_file(&self) -> JournResult<()> {
        if self.filename.is_some() {
            self.write_file()
        } else {
            match &self.parent {
                Some(parent) => parent.write_nearest_file(),
                None => Err(err!(
                    "Cannot write journal node: node and its parents have no backing file"
                )),
            }
        }
    }

    /// Writes the file backing this node if it has one, and recursively, all child nodes.
    pub fn write_file_recursive(&self) -> JournResult<()> {
        if self.filename.is_some() {
            self.write_file()?;
        }

        for seg in self.segments() {
            for dir in seg.directives().iter() {
                match dir.kind() {
                    DirectiveKind::Branch(node) | DirectiveKind::Include(node) => {
                        node.write_file_recursive()?;
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    }

    pub fn write<W: Write>(&self, writer: &mut W, account_filter: Option<&str>) -> JournResult<()> {
        for seg in self.segments() {
            self.write_from_dir_iter(Box::new(seg.directives().iter()), writer, account_filter)?;
        }
        Ok(())
    }

    pub fn flat_write_all<W: Write>(
        &self,
        writer: &mut W,
        account_filter: Option<&str>,
    ) -> JournResult<()> {
        self.write_from_dir_iter(Box::new(self.all_directives_iter()), writer, account_filter)
    }

    fn write_from_dir_iter<'a, W: Write>(
        &self,
        dir_iter: Box<dyn Iterator<Item = &'a Directive> + 'a>,
        writer: &mut W,
        account_filter: Option<&str>,
    ) -> JournResult<()> {
        let dir_iter: Box<dyn Iterator<Item = &'a Directive>> = Box::new(dir_iter.filter(|d| {
            if let Some(filter) = &account_filter {
                if let DirectiveKind::Entry(entry) = d.kind() {
                    return entry.matches_account_filter(filter);
                }
            }
            true
        }));
        for dir in dir_iter {
            write!(writer, "{dir}").map_err(|e| err!(e; "IO Error"))?;
        }
        // Ensure a newline at the end of the file
        writeln!(writer).map_err(|e| err!(e; "IO Error"))?;
        Ok(())
    }

    /// Prints the journal to stdout
    pub fn print(&self, account_filter: Option<&str>) -> JournResult<()> {
        let stdout = std::io::stdout();
        let mut writer = BufWriter::new(stdout);
        self.write(&mut writer, account_filter)
    }

    pub fn print_all(&self, account_filter: Option<&str>) -> JournResult<()> {
        let stdout = std::io::stdout();
        let mut writer = BufWriter::new(stdout);
        self.flat_write_all(&mut writer, account_filter)
    }
}

impl PartialEq for JournalNode<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.node_id == other.node_id
    }
}

impl Eq for JournalNode<'_> {}

#[cfg(test)]
mod tests {
    use crate::directive::Directive;
    use crate::*;

    #[test]
    fn insert_directive() {
        let journ = journ!("");
        let e1: Directive = entry_dir!("2020-01-01");
        let e1_clone: Directive = entry_dir!("2020-01-01");
        let e2: Directive = entry_dir!("2020-01-02");
        let e2_clone: Directive = entry_dir!("2020-01-02");
        let e3: Directive = entry_dir!("2020-01-03");
        let e3_clone: Directive = entry_dir!("2020-01-03");
        let e4: Directive = entry_dir!("2020-01-04");
        let e4_clone: Directive = entry_dir!("2020-01-04");

        // Check they are inserted in the correct order
        let jn = journ.root();
        jn.insert_directive(e2);
        jn.insert_directive(e1);
        jn.insert_directive(e4);
        jn.insert_directive(e3);
        let directives = jn.directives();
        assert_eq!(directives.len(), 4);
        assert_eq!(directives[0], e1_clone);
        assert_eq!(directives[1], e2_clone);
        assert_eq!(directives[2], e3_clone);
        assert_eq!(directives[3], e4_clone);
    }
}
