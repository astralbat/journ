/*
 * Copyright (c) 2021-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::cgt_configuration::{
    CagCommand, CapitalGainsColumn, CapitalGainsGroupBy, CapitalGainsOrderBy,
};
use crate::pool_event::{AggregatedPoolEvent, PoolEvent};
use chrono_tz::OffsetName;
use itertools::Itertools;
use journ_core::arguments::Arguments;
use journ_core::reporting::table::Cell;
use journ_core::reporting::table::{Row, Table};
use std::collections::BTreeSet;
use std::fmt;
use yaml_rust::yaml::Hash;
use yaml_rust::{Yaml, YamlEmitter};

#[derive(Default)]
pub struct CapitalGains<'h> {
    events: Vec<PoolEvent<'h>>,
}

impl<'h> CapitalGains<'h> {
    pub fn new(events: Vec<PoolEvent<'h>>) -> Self {
        Self { events }
    }

    /*
    pub fn update_journal(&self, journal: &Journal<'h>) {
        // Clear existing metadata completely, first before writing any new metadata.
        // This avoids any potential issue with metadata being cleared after writing on some entries.
        let all_entries: HashMap<EntryId, _> = self
            .events
            .iter()
            .flat_map(|e| e.deal_iter().map(|deal| (deal.id().entry_id(), deal.entry())))
            .collect();
        for (_, entry) in all_entries.into_iter() {
            CapitalGainsEntryMetadata::clear_deals(&mut entry.lock().unwrap());
        }

        let allocator = journal.root().config().allocator();
        for deal in self.events.iter().flat_map(|e| e.deal_iter()) {
            if !deal.is_required() {
                CapitalGainsEntryMetadata::write_deal(deal, allocator);
            }
        }
    }*/

    pub fn write_table<W: fmt::Write>(&self, writer: &mut W) -> fmt::Result {
        let cag = Arguments::get().cast_cmd::<CagCommand>().unwrap();
        let mut aggregated_events = self.events.iter().map(AggregatedPoolEvent::from).collect();
        combine_events(&mut aggregated_events, &cag.group_by);
        let filtered_and_grouped_events =
            ordered_events(filtered_events(aggregated_events.clone()));

        // Get Event abbreviations
        let mut event_tzs = BTreeSet::new();
        for e in filtered_and_grouped_events.iter() {
            if !event_tzs.contains(e.event_datetime().start().datetime().offset().abbreviation()) {
                event_tzs.insert(
                    e.event_datetime().start().datetime().offset().abbreviation().to_string(),
                );
            }
        }

        // Get Deal Timezone abbreviations
        let mut deal_tzs = BTreeSet::new();
        for e in filtered_and_grouped_events.iter() {
            if !deal_tzs.contains(e.deal_datetime().start().datetime().offset().abbreviation()) {
                deal_tzs.insert(
                    e.deal_datetime().start().datetime().offset().abbreviation().to_string(),
                );
            }
        }

        let mut table = Table::default();
        let mut heading_col_cells = vec![];
        for col in cag.columns.iter() {
            match col {
                CapitalGainsColumn::EventDate => {
                    heading_col_cells.push(format!("Event Date ({})", event_tzs.iter().join("/")))
                }
                CapitalGainsColumn::DealDate => {
                    heading_col_cells.push(format!("Deal Date ({})", deal_tzs.iter().join("/")))
                }
                CapitalGainsColumn::Unit => heading_col_cells.push("Unit".to_string()),
                CapitalGainsColumn::Pool => heading_col_cells.push("Pool".to_string()),
                CapitalGainsColumn::Event => heading_col_cells.push("Event".to_string()),
                CapitalGainsColumn::Acquired => heading_col_cells.push("Acquired".to_string()),
                CapitalGainsColumn::ActualCost => heading_col_cells.push("Actual Cost".to_string()),
                CapitalGainsColumn::TotalCost => heading_col_cells.push("Total Cost".to_string()),
                CapitalGainsColumn::Disposed => heading_col_cells.push("Disposed".to_string()),
                CapitalGainsColumn::NetProceeds => {
                    heading_col_cells.push("Net Proceeds".to_string())
                }
                CapitalGainsColumn::Expenses => heading_col_cells.push("Expenses".to_string()),
                CapitalGainsColumn::Gain => heading_col_cells.push("Gain".to_string()),
                CapitalGainsColumn::Loss => heading_col_cells.push("Loss".to_string()),
                CapitalGainsColumn::PoolBalBefore => {
                    heading_col_cells.push("Pool Bal Before".to_string())
                }
                CapitalGainsColumn::PoolBalAfter => {
                    heading_col_cells.push("Pool Bal After".to_string())
                }
                CapitalGainsColumn::Description => {
                    heading_col_cells.push("Description".to_string())
                }
                CapitalGainsColumn::Metadata(tag) => heading_col_cells.push("+".to_string() + tag),
            }
        }
        table.set_heading_row(heading_col_cells);
        table.set_column_separator(" | ");

        for event in filtered_and_grouped_events.iter() {
            table.add_rows(event.as_table_rows(cag.show_group, false));
        }

        // Total rows.
        // Show totals for `grouped_events` i.e. events before filtering by event type.
        // This seems to make more sense when viewing.
        if aggregated_events.len() > 1 {
            let mut sep_dashes = vec![];
            for _ in 0..cag.columns.len() {
                let mut cell: Cell = Cell::from(&"");
                cell.set_padding_char('-');
                sep_dashes.push(cell);
            }
            table.add_row(sep_dashes);

            let units = aggregated_events.iter().filter_map(|e| e.unit()).collect::<BTreeSet<_>>();
            for unit in units.iter() {
                let unit_filtered = AggregatedPoolEvent::Many(
                    aggregated_events.iter().filter(|e| e.unit() == Some(*unit)).cloned().collect(),
                );
                table.add_rows(
                    unit_filtered.as_table_rows(false, true).into_iter().map(Row::into_owned),
                );
            }

            if units.len() > 1 {
                let total = AggregatedPoolEvent::Many(aggregated_events.clone());
                table.add_rows(total.as_table_rows(false, true).into_iter().map(Row::into_owned));
            }
        }

        table.print(writer)?;
        Ok(())
    }
}

impl<'h> fmt::Display for CapitalGains<'h> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let cmd = Arguments::get().cast_cmd::<CagCommand>().unwrap();
        if cmd.output_yaml() {
            let mut emitter = YamlEmitter::new(f);
            emitter.dump(&Yaml::from(self)).map_err(|_| fmt::Error)
        } else {
            self.write_table(f)
        }
    }
}

impl<'h> From<&CapitalGains<'h>> for Yaml {
    fn from(value: &CapitalGains<'h>) -> Self {
        let cag = Arguments::get().cast_cmd::<CagCommand>().unwrap();
        let mut map = Hash::new();
        let mut units = BTreeSet::new();
        //let mut events_yaml = vec![];
        let mut aggregated_events = value.events.iter().map(AggregatedPoolEvent::from).collect();
        combine_events(&mut aggregated_events, &cag.group_by);
        let filtered_and_grouped_events =
            ordered_events(filtered_events(aggregated_events.clone()));
        for event in filtered_and_grouped_events.iter() {
            if let Some(unit) = event.unit() {
                units.insert(unit);
            }
            if let Some(bal_after) = event.balance_after() {
                for val in bal_after.valuations() {
                    units.insert(val.unit());
                }
            }
        }
        // Add a lookup table, detailing the units used in the gains
        let mut unit_map = Hash::new();
        for unit in units.iter() {
            unit_map.insert(Yaml::String(unit.code().to_string()), (*unit).into());
        }
        map.insert(Yaml::String("units".to_string()), Yaml::Hash(unit_map));
        map.insert(
            Yaml::String("events".to_string()),
            Yaml::Array(filtered_and_grouped_events.into_iter().map(|e| e.as_yaml(true)).collect()),
        );

        let mut units_total_map = Hash::new();
        for unit in units {
            let iter = aggregated_events.iter().filter(|e| e.unit() == Some(unit));
            if iter.clone().next().is_none() {
                continue;
            }
            let unit_events = AggregatedPoolEvent::Many(iter.cloned().collect());
            units_total_map
                .insert(Yaml::String(unit.code().to_string()), unit_events.as_yaml(false));
        }
        map.insert(Yaml::String("totals_by_unit".to_string()), Yaml::Hash(units_total_map));

        if !aggregated_events.is_empty() {
            map.insert(Yaml::String("total".to_string()), {
                let total = AggregatedPoolEvent::Many(aggregated_events);
                total.as_yaml(false)
            });
        }

        // Have a single root node, as is conventional for JSON/YAML.
        let mut gains_map = Hash::new();
        gains_map.insert(Yaml::String("capital_gains".to_string()), Yaml::Hash(map));
        Yaml::Hash(gains_map)
    }
}

fn filtered_events<'h, 'e>(
    events: Vec<AggregatedPoolEvent<'h, 'e>>,
) -> Vec<AggregatedPoolEvent<'h, 'e>> {
    let cmd = Arguments::get().cast_cmd::<CagCommand>().unwrap();
    let event_filter = cmd.event_filter();

    // Filter the events based on what the user has asked for
    events.into_iter().filter_map(|e| e.filter_events(&event_filter)).collect()
}

fn ordered_events<'h, 'e>(
    mut events: Vec<AggregatedPoolEvent<'h, 'e>>,
) -> Vec<AggregatedPoolEvent<'h, 'e>> {
    let cmd = Arguments::get().cast_cmd::<CagCommand>().unwrap();
    for order_by in cmd.order_by.iter().rev() {
        match order_by {
            CapitalGainsOrderBy::EventDate => {
                events.sort();
            }
            CapitalGainsOrderBy::DealDate => {
                events.sort();
                events.sort_by(|a, b| a.deal_datetime().start().cmp(&b.deal_datetime().start()));
            }
            CapitalGainsOrderBy::Unit => {
                events.sort();
                events.sort_by(|a, b| a.unit().cmp(&b.unit()));
            }
        }
    }
    events
}

fn combine_events(events: &mut Vec<AggregatedPoolEvent>, group_by: &[CapitalGainsGroupBy]) {
    // If there are no groupings, then there's nothing to do.
    if group_by.is_empty() {
        return;
    }

    let mut i = 0;
    while i < events.len() - 1 {
        let mut j = i + 1;
        while j < events.len() {
            if group_by.iter().all(|g| g.can_group(&events[i], &events[j])) {
                let combined = events.remove(i) + events.remove(j - 1);
                events.insert(i, combined);
            } else {
                j += 1;
            }
        }
        i += 1;
    }
}
