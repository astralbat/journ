/*
 * Copyright (c) 2020-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::alloc::HerdAllocator;
use crate::configuration::{AlwaysIncluded, Filter};
use crate::directive::{Directive, DirectiveKind};
use crate::err;
use crate::error::JournResult;
use crate::journal_context::JContext;
use crate::journal_entry::{EntryId, JournalEntry};
use crate::journal_node_segment::JournalNodeSegment;
use crate::parsing::text_block::{PaddingPolicy, TextBlockWriter};
use crate::tree_id::{BranchCountingTreeId, TreeId};
use std::fmt::Debug;
use std::fs::OpenOptions;
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};
use std::sync::{MutexGuard, OnceLock};
use std::{cmp, fmt, fs, io};

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
        Self {
            inner: Box::leak(Box::new(current_segment.unwrap().directives())),
            position: 0,
            current_segment,
        }
    }
}

impl<'h, 'a, 'b> Iterator for DirectiveTreeIter<'h, 'a, 'b> {
    type Item = (&'b JournalNodeSegment<'h>, &'b Directive<'h>);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(dir) = self.inner.get(self.position) {
                self.position += 1;
                return Some((self.current_segment.as_ref().unwrap(), dir));
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

pub type NodeId = BranchCountingTreeId;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum JournalNodeKind {
    Entry,
    Prices,
    Python,
}

/// A node usually representing a file on the filesystem.
pub struct JournalNode<'h> {
    node_id: NodeId,
    kind: JournalNodeKind,
    filename: Option<&'h Path>,
    allocator: &'h HerdAllocator<'h>,
    segments: OnceLock<Vec<&'h JournalNodeSegment<'h>>>,
    parent: Option<&'h JournalNode<'h>>,
    children: OnceLock<Vec<&'h JournalNode<'h>>>,
}

impl<'h> JournalNode<'h> {
    /// Creates a new JournalFile
    pub fn new(
        parent: Option<&'h JournalNode<'h>>,
        node_id: NodeId,
        filename: Option<&'h Path>,
        kind: JournalNodeKind,
        allocator: &'h HerdAllocator<'h>,
    ) -> &'h JournalNode<'h> {
        allocator.alloc(Self {
            node_id,
            filename,
            kind,
            //input,
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

    /// Gets the leaf child at the right-most section of the subtree.
    pub fn last_child_recursive(&self) -> Option<&JournalNode<'h>> {
        self.children().last().and_then(|c| c.last_child_recursive())
    }

    pub fn find_by_node_id(&self, node_id: &TreeId) -> Option<&Self> {
        if &self.node_id == node_id {
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

    /// Gets the path of the file this node belongs to, if any. This will
    /// always be `Some` if the journal was loaded from a file.
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

    pub fn id(&self) -> &NodeId {
        &self.node_id
    }

    pub fn file_kind(&self) -> JournalNodeKind {
        self.kind
    }

    /// Gets the segments of this node. The node will always have at least one segment.
    pub fn segments(&self) -> &Vec<&'h JournalNodeSegment<'h>> {
        self.segments.get().expect("Segments not initialised")
    }

    pub fn set_segments(&self, segments: Vec<&'h JournalNodeSegment<'h>>) {
        // expect() call calls debug() which can panic, so keep it simple.
        self.segments.set(segments).unwrap_or_else(|_| panic!("Segments already set"));
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
    pub(crate) fn append_entry(&self, mut entry: JournalEntry<'h>) -> &'h JournalEntry<'h> {
        entry.attach(self);
        let alloc_entry: &'h JournalEntry<'h> = self.allocator.alloc(entry);
        self.append_directive(DirectiveKind::Entry(alloc_entry));
        alloc_entry
    }

    pub(crate) fn append_directive(&self, dir_kind: DirectiveKind<'h>) {
        let mut dir_lock = self.segments().last().unwrap().directives();
        dir_lock.push(Directive::new(None, dir_kind));
    }

    /// Performs a binary search, looking for an insert position.
    /// Directives aren't expected to be ordered within a node, but this will make a reasonable attempt
    /// to find a good insert position.
    pub(crate) fn insert_directive(&self, dir_kind: DirectiveKind<'h>) {
        // First, find the segment we need to be in by searching each in turn.
        let mut insert_seg = None;
        for seg in self.segments() {
            if let Some(last_dir) = seg.directives().last() {
                match last_dir.kind().partial_cmp(&dir_kind) {
                    None | Some(cmp::Ordering::Less) => {
                        // This segment is not the right one, so continue to the next.
                        continue;
                    }
                    _ => {
                        insert_seg = Some(seg);
                        break;
                    }
                }
            }
        }
        match insert_seg {
            Some(seg) => self.insert_directive_in_segment(seg, dir_kind),
            None => {
                // Insert in the last segment.
                let seg = self.segments().last().unwrap();
                self.insert_directive_in_segment(seg, dir_kind)
            }
        }
    }

    /// Inserts the directive within the segment by using a binary search to find the right position.
    /// This is done by comparing `DirectiveKind` until a suitable position is found.
    ///
    /// The insert position is guaranteed to be following any other equal `DirectiveKinds` within the segment.
    pub(crate) fn insert_directive_in_segment(
        &self,
        segment: &JournalNodeSegment<'h>,
        dir_kind: DirectiveKind<'h>,
    ) {
        let mut dir_lock = segment.directives();
        let mut min = 0usize;
        let mut max = dir_lock.len();

        'outer: while max > min {
            let mid = min + ((max as f32 - min as f32) / 2_f32).ceil() as usize;

            // Walk back until same kind of directive found.
            // Walking back as opposed to forwards is preferred as new kinds of directives will be
            // inserted at the end of the file.
            for i in (min..mid).rev() {
                match dir_lock[i].kind().partial_cmp(&dir_kind) {
                    Some(cmp::Ordering::Greater) => max = i,
                    Some(cmp::Ordering::Less | cmp::Ordering::Equal) => min = i + 1,
                    // Not comparable, keep looking
                    None => continue,
                }
                continue 'outer;
            }
            // No directive found that's comparable, so look later
            min = mid;
        }

        dir_lock.insert(max, Directive::new(None, dir_kind))
    }

    /// Inserts a JournalEntry within the specified file and in the correct date position.
    pub(crate) fn insert_entry(&self, mut entry: JournalEntry<'h>) -> &'h JournalEntry<'h> {
        entry.attach(self);
        let entry: &'h JournalEntry<'h> = self.allocator.alloc(entry);
        self.insert_directive(DirectiveKind::Entry(entry));
        entry
    }

    /// Returns `(old_entry, new_entry)`.
    pub fn replace_entry(
        &self,
        entry: JournalEntry<'h>,
    ) -> Option<(&'h JournalEntry<'h>, &'h JournalEntry<'h>)> {
        let entry = JContext::get().allocator().alloc(entry);
        for seg in self.segments() {
            for dir in seg.directives().iter_mut() {
                if let DirectiveKind::Entry(curr_entry) = dir.kind()
                    && curr_entry.id() == entry.id()
                {
                    let curr_entry = *curr_entry;

                    *dir = Directive::new(None, DirectiveKind::Entry(entry));
                    return Some((curr_entry, entry));
                }
            }
        }
        None
    }

    pub fn remove_entry(&self, entry: &JournalEntry<'h>) -> Option<Directive<'h>> {
        for seg in self.segments() {
            let mut dirs = seg.directives();
            dirs.retain(|dir| {
                if let DirectiveKind::Entry(curr_entry) = dir.kind()
                    && curr_entry.id() == entry.id()
                {
                    return false;
                }
                true
            });
        }
        None
    }

    /// Gets all directives in their included order.
    pub fn all_directives_iter<'a, 'b>(&'a self) -> DirectiveTreeIter<'h, 'a, 'b> {
        DirectiveTreeIter::new(self)
    }

    pub fn entry(&self, entry_id: &EntryId) -> &'h JournalEntry<'h> {
        assert_eq!(
            entry_id.parent().as_ref(),
            Some(&*self.node_id),
            "Entries fileId does not match"
        );

        for seg in self.segments() {
            for dir in seg.directives().iter() {
                if let DirectiveKind::Entry(next_entry) = dir.kind()
                    && next_entry.id() == entry_id
                {
                    return next_entry;
                }
            }
        }
        panic!("Entry no longer exists")
    }

    fn write_file(&self, padding_policy: PaddingPolicy) -> JournResult<()> {
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
                let mut block_writer = TextBlockWriter::new(&mut writer);
                block_writer.set_padding_policy(padding_policy);
                self.write(&mut block_writer, &AlwaysIncluded)?;
                // Ensure a newline at the end of the file as is best practice.
                writeln!(writer).map_err(|e| err!(e; "IO Error"))?;
                Ok(())
            }
            None => Err(err!("Cannot write journal file: file represents a text stream")),
        }
    }

    /// Writes the nearest file to this node which is the node's file or the nearest file in the parent chain.
    pub fn write_nearest_file(&self, padding_policy: PaddingPolicy) -> JournResult<()> {
        if self.filename.is_some() {
            self.write_file(padding_policy)
        } else {
            match &self.parent {
                Some(parent) => parent.write_nearest_file(padding_policy),
                None => Err(err!(
                    "Cannot write journal node: node and its parents have no backing file"
                )),
            }
        }
    }

    /// Writes the file backing this node if it has one, and recursively, all child nodes.
    pub fn write_file_recursive(&self, padding_policy: PaddingPolicy) -> JournResult<()> {
        if self.filename.is_some() {
            self.write_file(padding_policy)?;
        }

        for seg in self.segments() {
            for dir in seg.directives().iter() {
                match dir.kind() {
                    DirectiveKind::Branch(node) | DirectiveKind::Include(node) => {
                        node.write_file_recursive(padding_policy)?;
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    }

    pub fn write<W: Write, F: Filter<Directive<'h>>>(
        &self,
        writer: &mut TextBlockWriter<W>,
        directive_filter: &F,
    ) -> JournResult<()> {
        for seg in self.segments().iter() {
            self.write_from_dir_iter(
                Box::new(seg.directives().iter().map(|dir| (*seg, dir))),
                writer,
                directive_filter,
            )?;
        }
        Ok(())
    }

    pub fn flat_write_all<W: Write, F: Filter<Directive<'h>>>(
        &self,
        writer: &mut TextBlockWriter<W>,
        directive_filter: &F,
    ) -> JournResult<()> {
        self.write_from_dir_iter(Box::new(self.all_directives_iter()), writer, directive_filter)
    }

    fn write_from_dir_iter<'a, W: Write, F: Filter<Directive<'h>>>(
        &self,
        dir_iter: Box<dyn Iterator<Item = (&'a JournalNodeSegment<'h>, &'a Directive<'h>)> + 'a>,
        writer: &mut TextBlockWriter<W>,
        directive_filter: &F,
    ) -> JournResult<()> {
        let map_err = |e| err!(e; "IO Error");
        let dir_iter: Box<dyn Iterator<Item = (&'a JournalNodeSegment, &'a Directive)>> =
            Box::new(dir_iter.filter(|(_, d)| directive_filter.is_included(d)));
        for (seg, dir) in dir_iter {
            writer.write(dir, Some(seg.config())).map_err(map_err)?;
        }
        Ok(())
    }

    /// Prints the journal to stdout
    pub fn print<F: Filter<Directive<'h>>>(&self, directive_filter: &F) -> JournResult<()> {
        let stdout = io::stdout();
        let mut writer = BufWriter::new(stdout);
        let mut block_writer = TextBlockWriter::new(&mut writer);
        self.write(&mut block_writer, directive_filter)
    }

    pub fn print_all<F: Filter<Directive<'h>>>(&self, directive_filter: &F) -> JournResult<()> {
        let stdout = io::stdout();
        let mut writer = BufWriter::new(stdout);
        let mut block_writer = TextBlockWriter::new(&mut writer);
        self.flat_write_all(&mut block_writer, directive_filter)
    }
}

impl PartialEq for JournalNode<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.node_id == other.node_id
    }
}

impl Eq for JournalNode<'_> {}

impl Debug for JournalNode<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "JournalNode(id={:?}, filename={:?})", self.node_id, self.filename)
    }
}

#[cfg(test)]
mod tests {
    use crate::directive::Directive;
    use crate::*;
    use indoc::indoc;

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
        jn.insert_directive(e2.into_inner().1);
        jn.insert_directive(e1.into_inner().1);
        jn.insert_directive(e4.into_inner().1);
        jn.insert_directive(e3.into_inner().1);
        let mut directives = jn.all_directives_iter().map(|(_seg, dir)| dir);
        assert_eq!(directives.next(), Some(&e1_clone));
        assert_eq!(directives.next(), Some(&e2_clone));
        assert_eq!(directives.next(), Some(&e3_clone));
        assert_eq!(directives.next(), Some(&e4_clone));
    }

    #[test]
    fn test_segment_config() {
        let journ = journ!(indoc! {"
             unit $
               format $0.00

             branch
               2000-01-01
                 Account  $12.00
                 Account2
        "});

        let branch_node = journ.root().children().next();
        assert!(branch_node.is_some());
        let branch_node = branch_node.unwrap();
        let branch_config = branch_node.segments().last().unwrap().config();
        println!(
            "{}",
            branch_node.segments().last().unwrap().directives().last().unwrap().parsed().unwrap()
        );
        let unit = branch_config.get_unit("$");
        assert!(unit.is_some());
        let unit = unit.unwrap();
        assert_eq!(unit.with_quantity(dec!(12)).to_string(), "$12.00");
    }
}
