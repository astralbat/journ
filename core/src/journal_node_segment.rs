/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::configuration::Configuration;
use crate::directive::Directive;
use crate::journal_node::{JournalNode, NodeId};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Mutex, MutexGuard, OnceLock};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct SegmentId<'h> {
    node_id: &'h NodeId<'h>,
    segment_index: u32,
}
impl<'h> SegmentId<'h> {
    pub fn new(node_id: &'h NodeId<'h>) -> Self {
        SegmentId { node_id, segment_index: SegmentId::generate_id() }
    }

    fn generate_id() -> u32 {
        static ID_COUNTER: AtomicU32 = AtomicU32::new(1);
        ID_COUNTER.fetch_add(1, Ordering::Relaxed)
    }
}

/// A segment of a journal node.
/// The complete list of directives of a node can be grouped into segments, where each segment is the directive runs up to and including 'include' and 'branch' statements.
/// The configuration of each segment also forms a configuration tree, where each segment can have its own configuration that can override the parent segment's configuration.
#[derive(Debug)]
pub struct JournalNodeSegment<'h> {
    id: SegmentId<'h>,
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
            id: SegmentId::new(node.id()),
            directives: Mutex::new(Vec::new()),
            node,
            next_segment: Mutex::new(None),
            configuration: OnceLock::new(),
        }
    }

    pub fn directives(&self) -> MutexGuard<Vec<Directive<'h>>> {
        self.directives.lock().unwrap()
    }

    pub(crate) fn set_directives(&self, directives: Vec<Directive<'h>>) {
        let mut guard = self.directives.lock().unwrap();
        *guard = directives;
    }

    pub fn config(&self) -> &Configuration<'h> {
        self.configuration.get().expect("Journal node segment has no configuration")
    }

    pub(crate) fn set_config(&self, configuration: Configuration<'h>) {
        self.configuration.set(configuration).expect("Configuration should only be set once");
    }

    pub fn node(&self) -> &'h JournalNode<'h> {
        self.node
    }

    pub fn next_segment(&self) -> Option<&'h JournalNodeSegment<'h>> {
        self.next_segment.lock().unwrap().clone()
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
