/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
mod cell;
mod cell_width;
mod column;
pub mod fmt;
mod row;
mod table;

pub use cell::aligned::{AlignedCell, Alignment};
pub use cell::binary::BinaryCell;
pub use cell::blank::BlankCell;
pub use cell::ellipsis::EllipsisCell;
pub use cell::multi::MultiLineCell;
pub use cell::separator::SeparatorCell;
pub use cell::styled::StyledCell;
pub use cell::wrapped::{PolicyWrappingCell, WrapEase, WrapPolicy};
pub use cell::{Cell, CellRef, ShrinkableCell};
pub use cell_width::{CellWidth, SpaceDistribution};
pub use column::TableColumn;
pub use row::Row;
pub use table::Table;
