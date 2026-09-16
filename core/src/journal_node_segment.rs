/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::configuration::Configuration;
use crate::directive::Directive;
use crate::journal_node::JournalNode;
use crate::tree_id::BranchCountingTreeId;
use std::fmt::{Debug, Formatter};
use std::sync::{Mutex, MutexGuard, OnceLock};

pub type SegmentId = BranchCountingTreeId;

/// A segment of a journal node.
/// The complete list of directives of a node can be grouped into segments, where each segment is the directive runs up to and including 'include' and 'branch' statements.
/// The configuration of each segment also forms a configuration tree, where each segment can have its own configuration that can override the parent segment's configuration.
pub struct JournalNodeSegment<'h> {
    id: SegmentId,
    directives: Mutex<Vec<Directive<'h>>>,
    node: &'h JournalNode<'h>,
    configuration: OnceLock<Configuration<'h>>,
    /// The next logical segment in a depth-first traversal of the journal.
    /// When `None`, this indicates this is the last segment of the journal.
    next_segment: Mutex<Option<&'h JournalNodeSegment<'h>>>,
}

impl<'h> JournalNodeSegment<'h> {
    pub fn new(node: &'h JournalNode<'h>) -> JournalNodeSegment<'h> {
        JournalNodeSegment {
            // The segment is a branch from the node
            id: node.id().next_id().into(),
            directives: Mutex::new(Vec::new()),
            node,
            next_segment: Mutex::new(None),
            configuration: OnceLock::new(),
        }
    }

    pub fn id(&self) -> &SegmentId {
        &self.id
    }

    pub fn directives(&self) -> MutexGuard<'_, Vec<Directive<'h>>> {
        self.directives.lock().unwrap()
    }

    pub fn remove_directive(&self, index: usize) -> Option<Directive<'h>> {
        let mut directives = self.directives.lock().unwrap();
        if directives.len() <= index {
            return None;
        }
        Some(directives.remove(index))
    }

    pub(crate) fn set_directives(&self, directives: Vec<Directive<'h>>) {
        let mut guard = self.directives.lock().unwrap();
        *guard = directives;
    }

    /// Gets the configuration state as it is at the end of the segment.
    pub fn config(&self) -> &Configuration<'h> {
        self.configuration.get().expect("Journal node segment has no configuration")
    }

    /// Sets the configuration state as it is at the end of the segment.
    pub(crate) fn set_config(&self, configuration: Configuration<'h>) {
        self.configuration.set(configuration).expect("Configuration should only be set once");
    }

    pub fn node(&self) -> &'h JournalNode<'h> {
        self.node
    }

    pub fn next_segment(&self) -> Option<&'h JournalNodeSegment<'h>> {
        *self.next_segment.lock().unwrap()
    }

    pub(crate) fn set_next_segment(&self, next_segment: Option<&'h JournalNodeSegment<'h>>) {
        let mut guard = self.next_segment.lock().unwrap();
        *guard = next_segment;
    }
}

impl<'h> PartialEq for JournalNodeSegment<'h> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<'h> Eq for JournalNodeSegment<'h> {}

impl<'h> PartialOrd for JournalNodeSegment<'h> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'h> Ord for JournalNodeSegment<'h> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl Debug for JournalNodeSegment<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "JournalNodeSegment({:?})", self.id)
    }
}
