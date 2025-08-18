/*
 * Copyright (c) 2022-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::alloc::HerdAllocator;
use crate::configuration::Configuration;
use crate::error::{BlockContextError, JournErrors, JournResult};
use crate::journal_node::{JournalNode, JournalNodeKind, NodeId};
use crate::journal_node_segment::JournalNodeSegment;
use crate::parsing::text_block::TextBlock;
use crate::parsing::text_input::{BlockInput, LocatedInput, TextBlockInput, TextInput};
use crate::parsing::util::interim_space;
use crate::python::environment::PythonEnvironment;
use crate::{err, parsing};
use nom_locate::LocatedSpan;
use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;
use std::sync::Mutex;
use std::thread::Scope;
use std::{fmt, panic, thread};

trait ParseFunc<'h>: FnOnce() -> JournResult<&'h JournalNode<'h>> {}
impl<'h, F> ParseFunc<'h> for F where F: FnOnce() -> JournResult<&'h JournalNode<'h>> {}
impl<'h, 's> fmt::Debug for dyn ParseFunc<'h> + 's {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ParseFunc")
    }
}
type ParseOperation<'h, 's> = Box<dyn ParseFunc<'h> + 's>;

/// A JournalNode view used during parsing.
/// The main idea here is that this node stays on a single thread and does not require
/// synchronisation.
#[derive(Debug)]
pub struct JournalParseNode<'h, 's, 'e> {
    /// The ongoing segments being parsed for the current node. The last segment is the current one.
    /// The list is never empty.
    segments: RefCell<Vec<&'h JournalNodeSegment<'h>>>,
    node: &'h JournalNode<'h>,
    input: TextBlockInput<'h, ()>,
    configuration: Rc<RefCell<Configuration<'h>>>,
    children: Mutex<Vec<ParseOperation<'h, 's>>>,
    parent_scope: &'s Scope<'s, 'e>,
    allocator: &'h HerdAllocator<'h>,
}
impl<'h, 's, 'e> JournalParseNode<'h, 's, 'e>
where
    'h: 'e,
    'e: 's,
{
    pub fn new_root(
        node: &'h JournalNode<'h>,
        configuration: Configuration<'h>,
        parent_scope: &'s Scope<'s, 'e>,
    ) -> Self {
        let allocator = configuration.allocator();
        JournalParseNode {
            segments: RefCell::new(vec![allocator.alloc(JournalNodeSegment::new(node))]),
            input: TextBlockInput::new(
                LocatedSpan::new(node.block().text()),
                node.block(),
                allocator,
            ),
            node,
            configuration: Rc::new(RefCell::new(configuration)),
            children: Mutex::new(vec![]),
            parent_scope,
            allocator,
        }
    }

    pub fn node(&self) -> &'h JournalNode<'h> {
        self.node
    }

    pub fn config(&self) -> &Rc<RefCell<Configuration<'h>>> {
        &self.configuration
    }

    pub fn allocator(&self) -> &'h HerdAllocator<'h> {
        self.allocator
    }

    pub fn parse(self) -> JournResult<&'h JournalNode<'h>> {
        let mut parse_errors = vec![];
        let time = std::time::Instant::now();

        match self.node.file_kind() {
            JournalNodeKind::Python => {
                let block = self.input.block();
                let outdented = block.text_outdented(3);
                let outdented_sans_python = outdented.strip_prefix("python").unwrap_or(&outdented);
                let python_code = outdented_sans_python.trim_start_matches(interim_space);

                let filename =
                    self.node.canonical_filename().map(|p| p.to_str().unwrap().to_string());
                if let Err(mut e) = PythonEnvironment::run(
                    python_code,
                    Some(self.node.id().journal_incarnation()),
                    None,
                    filename.as_deref(),
                ) {
                    if let Some(bce) = e.msg_mut::<BlockContextError>() {
                        bce.context_mut().set_file(
                            self.node.nearest_filename().map(|p| p.to_str().unwrap().to_string()),
                        );
                    }
                    parse_errors = vec![e];
                }
                self.node.set_segments(self.segments.take());
            }
            _ => match parsing::directive::directives(self.input.map_extra(|_| &self)) {
                Ok(segment_dirs) => {
                    let segments = self.segments.take();
                    for (segment, dirs) in segments.iter().copied().zip(segment_dirs) {
                        segment.set_directives(dirs);
                    }
                    self.node.set_segments(segments);
                }
                Err(err) => {
                    parse_errors = vec![err];
                }
            },
        }

        if let Some(filename) = self.node.filename() {
            info!(
                "{} '{}' in {}ms",
                if self.node.file_kind() == JournalNodeKind::Python {
                    "Executed"
                } else {
                    "Parsed"
                },
                filename.to_str().unwrap(),
                time.elapsed().as_millis()
            );
        }

        let mut children = vec![];
        for child in self.children.lock().unwrap().drain(..) {
            match child() {
                Ok(node) => children.push(node),
                Err(err) => parse_errors.push(err),
            }
        }

        match parse_errors.len() {
            0 => {
                self.node.set_children(children);
                trace!(
                    "Setting segment #{} configuration in file: {:?}",
                    self.node.segments().len(),
                    self.node.nearest_filename()
                );
                trace!("{:?}", self.configuration.borrow());
                self.node
                    .segments()
                    .last()
                    .unwrap()
                    .set_config(self.configuration.borrow().clone());
                Ok(self.node)
            }
            _ => {
                let journ_errs = JournErrors::new(String::new(), parse_errors).flatten();
                let num_errs = journ_errs.len();
                match self.node.nearest_filename() {
                    Some(filename) => Err(err!(journ_errs.with_message(format!(
                        "Found {} errors in {}",
                        num_errs,
                        filename.to_str().unwrap()
                    ),))),
                    None => Err(err!(journ_errs.with_message(format!("Found {num_errs} errors"),))),
                }
            }
        }
    }

    pub fn include_kind<I>(
        &self,
        input: I,
        path: Option<&'h Path>,
        kind: JournalNodeKind,
    ) -> JournResult<&'h JournalNode<'h>>
    where
        I: TextInput<'h> + BlockInput<'h> + LocatedInput<'h>,
    {
        match Self::create_child_node(self, input, path, kind) {
            Err(file) => Ok(file),
            Ok(child) => {
                // Parse the child in the same thread.
                // Note that the underlying Configuration is not cloned.
                let child_clone = child;
                let branched_to_segment = self.branch_to_new_segment(child);

                thread::scope(move |scope| {
                    let jfp_node = JournalParseNode {
                        segments: RefCell::new(vec![branched_to_segment]),
                        input: child.input(),
                        node: child,
                        configuration: Rc::clone(&self.configuration),
                        children: Mutex::new(vec![]),
                        parent_scope: scope,
                        allocator: self.allocator(),
                    };
                    jfp_node.parse()?;

                    //*self.inner.children_wait.0.lock().unwrap() += 1;
                    self.children.lock().unwrap().push(Box::new(move || Ok(child)));
                    Ok(child_clone)
                })
            }
        }
    }

    pub fn branch_kind<I>(
        &self,
        input: I,
        path: Option<&'h Path>,
        kind: JournalNodeKind,
    ) -> &'h JournalNode<'h>
    where
        I: TextInput<'h> + BlockInput<'h> + LocatedInput<'h>,
        'h: 's,
    {
        match Self::create_child_node(self, input.clone(), path, kind) {
            Err(file) => file,
            Ok(child) => {
                let child_config = self.configuration.borrow().branch();

                let inner_node = child;
                let allocator = self.allocator();
                let branched_to_segment = self.branch_to_new_segment(child);

                let mut t_builder = thread::Builder::new();
                if let Some(filename) = child.nearest_filename() {
                    t_builder = t_builder.name(filename.to_str().unwrap().to_string());
                }
                let join_handle = t_builder
                    .spawn_scoped(self.parent_scope, move || {
                        thread::scope(|scope| {
                            let jfp_node = JournalParseNode {
                                segments: RefCell::new(vec![branched_to_segment]),
                                input: child.input(),
                                node: child,
                                configuration: Rc::new(RefCell::new(child_config)),
                                children: Mutex::new(vec![]),
                                parent_scope: scope,
                                allocator,
                            };
                            jfp_node.parse()
                        })
                    })
                    .unwrap();

                self.children.lock().unwrap().push(Box::new(|| match join_handle.join() {
                    Ok(node) => node,
                    Err(e) => panic::resume_unwind(e),
                }));
                inner_node
            }
        }
    }

    /// Start new segments for the branched node and for the current node, and link them together
    /// in depth-first fashion.
    /// Return the segment that the child node branches to.
    fn branch_to_new_segment(&self, child: &'h JournalNode<'h>) -> &'h JournalNodeSegment<'h> {
        let branched_config = self.configuration.borrow().branch();
        // Set self.configuration to be a branched version, ready for parsing a new segment.
        // The old configuration can then be set on the current segment in the process of finalising it.
        let old_config = self.configuration.replace(branched_config);
        self.segments.borrow_mut().last_mut().unwrap().set_config(old_config);

        let branched_to_segment: &_ = self.allocator.alloc(JournalNodeSegment::new(child));
        let continuation_segment = self.allocator.alloc(JournalNodeSegment::new(self.node));
        continuation_segment
            .set_next_segment(self.segments.borrow().last().unwrap().next_segment());
        self.segments.borrow().last().unwrap().set_next_segment(Some(branched_to_segment));
        branched_to_segment.set_next_segment(Some(continuation_segment));
        self.segments.borrow_mut().push(continuation_segment);

        branched_to_segment
    }

    /// Compares the `stream` to the current node's stream and its parents to see if the stream is already being/been parsed.
    /// If found, the found node will be returned, otherwise we would eventually overflow the stack in an infinite loop.
    /// This is in fact a configuration error on the part of the user, but we can alleviate it.
    ///
    /// This prevents circular parsing, but does not prevent two sub-trees of a parent from parsing the same file.
    /// This latter scenario may be desired, for example, including common configuration.
    fn find_stream_amongst_parents(
        node: &'h JournalNode<'h>,
        block: &TextBlock<'h>,
    ) -> Option<&'h JournalNode<'h>> {
        // There isn't a need to be concerned with raw text streams since although two may be identical, they
        // won't ever be circular indefinitely.
        if let Some(filename) = block.location().unwrap().file() {
            if block == node.block() {
                warn!("Circular loop detected whilst parsing file: {}", filename);
                return Some(node);
            }
            match node.parent() {
                Some(parent) => Self::find_stream_amongst_parents(parent, block),
                None => None,
            }
        } else {
            None
        }
    }

    /// Creates a new child node with the specified `stream` and `kind`. If the specified `stream` has already been
    /// created by the parent node, or one of its parents, then circular parsing will be avoided and an `Err` result
    /// will be returned.
    fn create_child_node<I>(
        &self,
        input: I,
        path: Option<&'h Path>,
        kind: JournalNodeKind,
    ) -> Result<&'h JournalNode<'h>, &'h JournalNode<'h>>
    where
        I: TextInput<'h> + BlockInput<'h> + LocatedInput<'h>,
    {
        match Self::find_stream_amongst_parents(self.node, input.block()) {
            Some(file) => Err(file),
            None => {
                // Create a new file representing the branch.
                let node_id = self.allocator.alloc(NodeId::branch(self.node.id()));
                let loc_span = input.clone().into_located_span(());
                let child_node = self.allocator.alloc(JournalNode::new(
                    Some(self.node),
                    node_id,
                    path,
                    kind,
                    TextBlockInput::new(loc_span, input.block(), self.allocator),
                    self.allocator,
                ));
                Ok(child_node)
            }
        }
    }
}

impl PartialEq for JournalParseNode<'_, '_, '_> {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}
