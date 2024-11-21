/*
 * Copyright (c) 2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::reporting::table::cell::column::CellColumnInfo;
use crate::reporting::table::cell::content::Alignment;
use crate::reporting::table::cell::iter::{CellIterator, CellIteratorMut};
use crate::reporting::table::cell::sequence::CellSequence;
use crate::reporting::term_style::{Colour, Style};
use std::borrow::Cow;
use std::cell::RefCell;
use std::fmt;
use std::iter::FromIterator;
use std::rc::Rc;

#[derive(Clone)]
pub enum CellNodeKind<'cell> {
    Leaf(Cow<'cell, str>),
    /// Each branch holds its children and the separator string in a `CellSequence`.
    Branch(CellSequence<'cell>),
    /// The root cell holds the lines of the cell
    ColRoot(CellSequence<'cell>),
}

impl<'a> CellNodeKind<'a> {
    pub fn into_owned<'b>(self) -> CellNodeKind<'b> {
        match self {
            CellNodeKind::Leaf(content) => CellNodeKind::Leaf(Cow::Owned(content.into_owned())),
            CellNodeKind::Branch(seq) => CellNodeKind::Branch(seq.into_owned()),
            CellNodeKind::ColRoot(seq) => CellNodeKind::ColRoot(seq.into_owned()),
        }
    }

    pub fn set_separator(&mut self, separator: &'static str) {
        match self {
            CellNodeKind::Leaf(_) => {
                panic!("Cannot set separator on single cell")
            }
            CellNodeKind::Branch(seq) | CellNodeKind::ColRoot(seq) => seq.set_separator(separator),
        }
    }
}

#[derive(Default, Copy, Clone, PartialEq, Eq)]
pub enum WrapPolicy {
    #[default]
    Any,
    /// Indicates wrapping should not be performed on this cell or its descendants.
    Never,
    Position(usize),
    With(fn(&Cell) -> WrapPolicy),
}

#[derive(Clone)]
pub struct Cell<'cell> {
    node_kind: CellNodeKind<'cell>,
    alignment: Alignment,
    /// How many columns this cell spans across
    span: u16,
    style: Style,
    /// The width, if set, is always >= content width
    pad_to_width: Option<usize>,
    padding_char: char,
    wrap_policy: WrapPolicy,
}

impl<'cell> Cell<'cell> {
    /// Creates a new cell with the given content.
    /// If the content spans multiple lines, then it is split into multiple cells, each
    /// separated by a newline character.
    pub fn new(content: impl Into<Cow<'cell, str>>) -> Self {
        let content = content.into();

        Self { node_kind: CellNodeKind::Leaf(content), ..Default::default() }
    }

    pub fn new_branch(seq: CellSequence<'cell>) -> Self {
        Self { node_kind: CellNodeKind::Branch(seq), ..Default::default() }
    }

    pub(crate) fn new_root(seq: CellSequence<'cell>) -> Self {
        Self { node_kind: CellNodeKind::ColRoot(seq), ..Default::default() }
    }

    /// Converts the cell into an owned value by rendering its content to a `String`.
    pub fn into_owned<'b>(self) -> Cell<'b> {
        Cell { node_kind: self.node_kind.into_owned(), ..self }
    }

    /*
    pub(crate) fn make_row_root(self) -> Cell<'_> {
        match self.node_kind {
            CellNodeKind::Leaf(content) => Cell {
                node_kind: CellNodeKind::ColRoot(CellSequence::new(
                    [Rc::new(RefCell::new(self))],
                    "",
                )),
                ..Default::default()
            },
        }

        self.node_kind = match &mut self.node_kind {
            CellNodeKind::Leaf(content) => CellNodeKind::ColRoot(CellSequence::new(
                [Rc::new(RefCell::new(Cell::new(content.clone())))],
                "",
            )),
            CellNodeKind::Branch(seq) => {
                let mut new_branch = Cell::new_branch(mem::take(seq));
                new_branch.wrap_policy = self.wrap_policy;
                self.wrap_policy = WrapPolicy::Any;
                CellNodeKind::ColRoot(CellSequence::new([Rc::new(RefCell::new(new_branch))], ""))
            }
            CellNodeKind::ColRoot(seq) => CellNodeKind::ColRoot(mem::take(seq)),
        }
    }*/

    pub fn kind<'a>(&'a self) -> &'a CellNodeKind<'cell> {
        &self.node_kind
    }

    pub fn kind_mut(&mut self) -> &mut CellNodeKind<'cell> {
        &mut self.node_kind
    }

    pub fn as_leaf(&self) -> Option<&Cow<'cell, str>> {
        match &self.node_kind {
            CellNodeKind::Leaf(content) => Some(content),
            _ => None,
        }
    }

    pub fn as_leaf_mut(&mut self) -> Option<&mut Cow<'cell, str>> {
        match &mut self.node_kind {
            CellNodeKind::Leaf(content) => Some(content),
            _ => None,
        }
    }

    pub fn as_branch(&self) -> Option<&CellSequence<'cell>> {
        match &self.node_kind {
            CellNodeKind::Branch(seq) => Some(seq),
            _ => None,
        }
    }

    pub fn as_branch_mut(&mut self) -> Option<&mut CellSequence<'cell>> {
        match &mut self.node_kind {
            CellNodeKind::Branch(seq) => Some(seq),
            _ => None,
        }
    }

    pub fn as_root(&self) -> Option<&CellSequence<'cell>> {
        match &self.node_kind {
            CellNodeKind::ColRoot(seq) => Some(seq),
            _ => None,
        }
    }

    /// Gets the number of children cells within this cell.
    pub fn len(&self) -> usize {
        match &self.node_kind {
            CellNodeKind::Leaf(_) => 0,
            CellNodeKind::Branch(seq) => seq.len(),
            CellNodeKind::ColRoot(seq) => seq.len(),
        }
    }

    pub fn iter<'a>(&'a self) -> CellIterator<'a, 'cell> {
        CellIterator::new(self)
    }

    pub fn iter_mut<'a>(&'a mut self) -> CellIteratorMut<'a, 'cell> {
        CellIteratorMut::new(self)
    }

    pub fn span(&self) -> u16 {
        self.span
    }

    pub fn set_span(&mut self, span: u16) {
        assert!(span > 0);
        self.span = span;
    }

    pub fn with_span(mut self, span: u16) -> Self {
        self.set_span(span);
        self
    }

    pub fn style(&self) -> Style {
        self.style
    }

    pub fn padding_char(&self) -> char {
        self.padding_char
    }

    pub fn set_padding_char(&mut self, padding_char: char) {
        self.padding_char = padding_char;
    }

    pub fn with_padding_char(mut self, padding_char: char) -> Self {
        self.set_padding_char(padding_char);
        self
    }

    pub fn set_foreground(&mut self, foreground: Option<Colour>) {
        self.style = match foreground {
            Some(fg) => self.style.with_fg(fg),
            None => self.style.reset_fg(),
        };
    }

    pub fn with_foreground(mut self, foreground: Option<Colour>) -> Self {
        self.set_foreground(foreground);
        self
    }

    pub fn set_bold(&mut self, bold: bool) {
        self.style = self.style.with_bold(bold)
    }

    pub fn set_faint(&mut self, faint: bool) {
        self.style = self.style.with_faint(faint)
    }

    pub fn set_alignment(&mut self, alignment: Alignment) {
        self.alignment = alignment;
    }

    pub fn with_alignment(mut self, alignment: Alignment) -> Self {
        self.set_alignment(alignment);
        self
    }

    pub fn set_separator(&mut self, separator: &'static str) {
        self.node_kind.set_separator(separator);
    }

    /// Gets all cells in the tree, starting from leaf nodes and including the branch nodes,
    /// ending with this cell itself.
    pub(crate) fn all_cell_info<'a>(cell: &'a Rc<RefCell<Self>>) -> Vec<CellColumnInfo<'cell>> {
        let mut cell_info = vec![];
        cell_info.push(CellColumnInfo::new(Rc::clone(cell), 0, None));
        Self::all_cell_info_internal(cell, &mut cell_info);
        cell_info
    }

    pub(crate) fn all_cell_info_internal<'a>(
        cell: &'a RefCell<Cell<'cell>>,
        info: &mut Vec<CellColumnInfo<'cell>>,
    ) {
        let parent = info.last().cloned();
        let cell_borrow = cell.borrow();

        match &cell_borrow.node_kind {
            CellNodeKind::Leaf(_) => {}
            CellNodeKind::Branch(seq) => {
                for (col, cell) in seq.cells().iter().enumerate() {
                    info.push(CellColumnInfo::new(
                        Rc::clone(cell),
                        col as u16,
                        parent.as_ref().map(|p| Box::new(p.clone())),
                    ));
                    Self::all_cell_info_internal(cell, info);
                }
            }
            CellNodeKind::ColRoot(lines) => {
                for line in lines.cells().iter() {
                    info.push(CellColumnInfo::new(
                        Rc::clone(line),
                        0,
                        parent.as_ref().map(|p| Box::new(p.clone())),
                    ));
                    Self::all_cell_info_internal(line, info);
                }
            }
        }
    }

    /// Gets the width of the cell's content, the number of characters its content
    /// would naturally take up. This includes the padding of its children.
    pub fn content_width(&self) -> usize {
        match &self.node_kind {
            CellNodeKind::Leaf(content) => content.chars().count(),
            CellNodeKind::Branch(seq) => seq.padded_width(),
            CellNodeKind::ColRoot(seq) => seq.iter().map(|c| c.padded_width()).max().unwrap_or(0),
        }
    }

    pub fn actual_content_width(&self) -> usize {
        match &self.node_kind {
            CellNodeKind::Leaf(content) => content.chars().count(),
            CellNodeKind::Branch(seq) => seq.actual_content_width(),
            CellNodeKind::ColRoot(seq) => {
                seq.iter().map(|c| c.actual_content_width()).max().unwrap_or(0)
            }
        }
    }

    pub fn padded_width(&self) -> usize {
        match self.pad_to_width {
            Some(width) => width,
            None => self.content_width(),
        }
    }

    pub fn left_padding(&self) -> usize {
        match self.pad_to_width {
            Some(width) => {
                let extra_padding = width - self.content_width();
                match self.alignment {
                    Alignment::Left => 0,
                    Alignment::Right => extra_padding,
                    Alignment::Center => extra_padding / 2,
                }
            }
            None => 0,
        }
    }

    pub fn right_padding(&self) -> usize {
        match self.pad_to_width {
            Some(width) => {
                let extra_padding = width - self.content_width();
                match self.alignment {
                    Alignment::Left => extra_padding,
                    Alignment::Right => 0,
                    Alignment::Center => extra_padding - self.left_padding(),
                }
            }
            None => 0,
        }
    }

    pub fn pad_to_width(&mut self, width: usize) {
        self.pad_to_width = Some(width);
    }

    pub fn set_wrap_policy(&mut self, policy: WrapPolicy) {
        self.wrap_policy = policy;
    }

    /// Wraps '\n' characters that appear in the content.
    pub(crate) fn wrap_text_eager(&mut self) -> (bool, Option<CellSequence<'cell>>) {
        match &mut self.node_kind {
            CellNodeKind::Leaf(content) => {
                let mut line_iter = content.lines().rev();
                let last_line = line_iter.next();
                let first_lines: String = line_iter.collect();
                if !first_lines.is_empty() {
                    let last_line = last_line.unwrap().to_string();
                    *content = Cow::Owned(first_lines);
                    (
                        true,
                        Some(CellSequence::new([Rc::new(RefCell::new(Cell::from(last_line)))], "")),
                    )
                } else {
                    (false, None)
                }
            }
            CellNodeKind::Branch(seq) => match seq.wrap_text_eager() {
                Some(seq) => (true, Some(seq)),
                None => (false, None),
            },
            CellNodeKind::ColRoot(seq) => {
                let mut wrapped = false;
                let wrapped_part = seq.wrap_text_eager();
                if let Some(wrapped_part) = wrapped_part {
                    wrapped = true;
                    seq.push(Rc::new(RefCell::new(Cell::new_branch(wrapped_part))));
                }
                (wrapped, None)
            }
        }
    }

    /// Causes the cell to wrap its text. This is done in order to reduce its width to
    /// get the row to fit in to the row's desired width.
    /// This can be called repeatedly, and each time the cell will attempt to wrap more aggressively.
    ///
    /// Returns `true` if the cell was able to wrap its text further in such a way
    /// that reduced its width, `false` otherwise.
    pub(super) fn wrap_text(&mut self) -> (bool, Option<CellSequence<'cell>>) {
        let wrap_policy = if let WrapPolicy::With(func) = self.wrap_policy {
            func(self)
        } else {
            self.wrap_policy
        };
        let wrap_pos = match wrap_policy {
            WrapPolicy::Any => None,
            WrapPolicy::Never => return (false, None),
            WrapPolicy::Position(pos) => Some(pos),
            WrapPolicy::With(_) => unreachable!("WrapAdvice::With should have been handled"),
        };

        let wrap = match &mut self.node_kind {
            CellNodeKind::Leaf(text) => {
                // Use the policy position, or find one at a whitespace position with the best one being the one closest to the middle.
                let wrap_pos = wrap_pos.or_else(|| {
                    let num_chars = text.chars().count();
                    let mut best_i = None;
                    for (i, c) in text.char_indices() {
                        if c.is_whitespace() {
                            match best_i {
                                Some(prev_best) => {
                                    let prev_best_dist =
                                        ((num_chars / 2) as isize - prev_best as isize).abs();
                                    if ((num_chars / 2) as isize - i as isize).abs()
                                        < prev_best_dist
                                    {
                                        best_i = Some(i);
                                    }
                                }
                                None => best_i = Some(i),
                            }
                        }
                    }
                    best_i
                });
                let mut wrapped = false;
                let mut wrapped_part = None;
                if let Some(wrap_pos) = wrap_pos {
                    if wrap_pos > 0 && wrap_pos < text.len() {
                        let (first, second) = text.split_at(wrap_pos);
                        if !first.is_empty() {
                            wrapped = true;
                            wrapped_part = Some(second.trim_start().to_string());
                            *text = Cow::Owned(first.to_string());
                        }
                    }
                }
                (
                    wrapped,
                    wrapped_part
                        .map(|s| CellSequence::new([Rc::new(RefCell::new(Cell::new(s)))], "")),
                )
            }
            CellNodeKind::Branch(seq) => match seq.wrap_text(wrap_pos) {
                Some(seq) => (true, Some(seq)),
                None => (false, None),
            },
            CellNodeKind::ColRoot(seq) => {
                let mut wrapped = false;
                let (longest_line, _l) = seq
                    .iter()
                    .enumerate()
                    .map(|(i, l)| (i, l.content_width()))
                    .max_by_key(|(_, l)| *l)
                    .unwrap();
                let wrapped_part = seq[longest_line].borrow_mut().wrap_text().1;
                if let Some(wrapped_part) = wrapped_part {
                    let as_cell = match wrapped_part.try_into_cell() {
                        Ok(cell) => cell,
                        Err(wrapped_part) => {
                            Rc::new(RefCell::new(Cell::new_branch(wrapped_part.clone())))
                        }
                    };
                    wrapped = true;
                    seq.insert(longest_line + 1, as_cell);
                }
                (wrapped, None)
            }
        };
        wrap
    }

    pub fn print<W: fmt::Write>(&self, w: &mut W, style: Style) -> fmt::Result {
        for _ in 0..self.left_padding() {
            write!(w, "{}", self.padding_char)?;
        }

        match &self.node_kind {
            CellNodeKind::Leaf(content) => style.fmt(w, content),
            CellNodeKind::Branch(seq) => seq.print(w, style.overlay(self.style)),
            CellNodeKind::ColRoot(seq) => {
                for (i, cell) in seq.iter().enumerate() {
                    cell.print(w, style.overlay(self.style))?;
                    if i < seq.len() - 1 {
                        writeln!(w)?;
                    }
                }
                Ok(())
            }
        }?;

        for _ in 0..self.right_padding() {
            write!(w, "{}", self.padding_char)?;
        }

        Ok(())
    }
}

impl fmt::Debug for Cell<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        for _ in 0..self.left_padding() {
            write!(f, "_")?;
        }

        match &self.node_kind {
            CellNodeKind::Leaf(content) => write!(f, "\"{}\"", content),
            CellNodeKind::Branch(seq) => write!(f, "B({:?})", seq),
            CellNodeKind::ColRoot(seq) => write!(f, "C({:?})", seq),
        }?;

        for _ in 0..self.right_padding() {
            write!(f, "_")?;
        }
        write!(f, "]")?;
        Ok(())
    }
}

impl Default for Cell<'_> {
    fn default() -> Self {
        Self {
            node_kind: CellNodeKind::Leaf(Cow::Borrowed("")),
            pad_to_width: None,
            style: Style::default(),
            alignment: Alignment::default(),
            span: 1,
            padding_char: ' ',
            wrap_policy: WrapPolicy::default(),
        }
    }
}

impl PartialEq<&str> for Cell<'_> {
    fn eq(&self, other: &&str) -> bool {
        match &self.node_kind {
            CellNodeKind::Leaf(content) => content == other,
            _ => false,
        }
    }
}

impl<'a> From<&'a &str> for Cell<'a> {
    fn from(s: &'a &str) -> Self {
        Self { node_kind: CellNodeKind::Leaf(Cow::Borrowed(s)), ..Default::default() }
    }
}

impl From<String> for Cell<'_> {
    fn from(s: String) -> Self {
        Self { node_kind: CellNodeKind::Leaf(Cow::Owned(s)), ..Default::default() }
    }
}

impl<'a, T: Into<Cell<'a>>> From<Option<T>> for Cell<'a> {
    fn from(opt: Option<T>) -> Self {
        match opt {
            Some(val) => val.into(),
            None => Cell::default(),
        }
    }
}

impl<'a, C> FromIterator<C> for Cell<'a>
where
    C: Into<Cell<'a>>,
{
    fn from_iter<T: IntoIterator<Item = C>>(iter: T) -> Self {
        let mut iter = iter.into_iter();
        match iter.next() {
            Some(first) => {
                let mut seq = CellSequence::new([Rc::new(RefCell::new(first.into()))], "");
                seq.extend(iter.map(C::into).map(RefCell::new).map(Rc::new));
                Cell::new_branch(seq)
            }
            /*
            Some(first) => match iter.next() {
                Some(second) => {
                    let mut seq = CellSequence::new(
                        [Rc::new(RefCell::new(first.into())), Rc::new(RefCell::new(second.into()))],
                        "",
                    );
                    seq.extend(iter.map(C::into).map(RefCell::new).map(Rc::new));
                    Cell::new_branch(seq)
                }
                None => first.into(),
            },*/
            None => Cell::default(),
        }
    }
}

impl<'a, C: Into<Cell<'a>>> From<Vec<C>> for Cell<'a> {
    fn from(value: Vec<C>) -> Self {
        // This should always be a multi cell, even if it has only 0 or
        // 1 children. This is important for rows that use this constructor,
        // and it is probably what the user expects.
        Cell::new_branch(CellSequence::new(
            IntoIterator::into_iter(value).map(|c| c.into()).map(RefCell::new).map(Rc::new),
            "",
        ))
    }
}

macro_rules! from_array {
    ($n:literal) => {
        impl<'a, C: Into<Cell<'a>>> From<[C; $n]> for Cell<'a> {
            fn from(value: [C; $n]) -> Self {
                match $n {
                    0 => unreachable!(),
                    _ => Cell::new_branch(CellSequence::new(
                        IntoIterator::into_iter(value)
                            .map(|c| c.into())
                            .map(RefCell::new)
                            .map(Rc::new),
                        "",
                    )),
                }
            }
        }
    };
}
from_array!(1);
from_array!(2);
from_array!(3);
from_array!(4);
from_array!(5);
from_array!(6);
from_array!(7);
from_array!(8);
from_array!(9);
from_array!(10);
from_array!(11);
from_array!(12);
from_array!(13);
from_array!(14);
from_array!(15);
from_array!(16);
