/*
 * Copyright (c) 2026. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
#[derive(clap::Args, Debug, Default)]
pub struct TableFormatArguments {
    #[arg(long = "table-title-sep", help = "The padding character for section titles")]
    title_separator: Option<char>,
    #[arg(long = "table-total-sep", help = "The padding character for separating totals")]
    total_separator: Option<char>,
    #[arg(
        long = "table-grand-total-sep",
        help = "The padding character for separating the grand total"
    )]
    grand_total_separator: Option<char>,
    #[arg(
        long = "table-chain-sep",
        help = "The padding character for separating chained commands"
    )]
    chain_separator: Option<char>,
}

impl TableFormatArguments {
    pub fn into_cmd(self) -> TableFormatCommand {
        TableFormatCommand {
            total_separator: self.total_separator,
            grand_total_separator: self.grand_total_separator,
            chain_separator: self.chain_separator,
            title_separator: self.title_separator,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct TableFormatCommand {
    title_separator: Option<char>,
    total_separator: Option<char>,
    grand_total_separator: Option<char>,
    chain_separator: Option<char>,
}

impl TableFormatCommand {
    const DEFAULT_TITLE_SEPARATOR: char = '\u{2501}';
    const DEFAULT_CHAIN_SEPARATOR: char = ' ';
    const DEFAULT_GRAND_TOTAL_SEPARATOR: char = '\u{2550}';
    const DEFAULT_TOTAL_SEPARATOR: char = '\u{2500}';

    pub fn title_separator(&self) -> char {
        self.title_separator.unwrap_or(Self::DEFAULT_TITLE_SEPARATOR)
    }

    pub fn chain_separator(&self) -> char {
        self.chain_separator.unwrap_or(Self::DEFAULT_CHAIN_SEPARATOR)
    }

    pub fn total_separator(&self) -> char {
        self.total_separator.unwrap_or(Self::DEFAULT_TOTAL_SEPARATOR)
    }

    pub fn grand_total_separator(&self) -> char {
        self.grand_total_separator.unwrap_or(Self::DEFAULT_GRAND_TOTAL_SEPARATOR)
    }

    pub fn merge_from(&self, other: &Self) -> Self {
        Self {
            total_separator: self.total_separator.or(other.total_separator),
            title_separator: self.title_separator.or(other.title_separator),
            chain_separator: self.chain_separator.or(other.chain_separator),
            grand_total_separator: self.grand_total_separator.or(other.grand_total_separator),
        }
    }
}
