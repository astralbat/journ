/*
 * Copyright (c) 2022-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use journ_core::amount;
use journ_core::currency_ref;
use journ_core::error::JournResult;
use journ_core::journ;
use journ_core::unit::Unit;
use journ_tax::acquire::{Acquire, AcquireGroup};
use journ_tax::dealing_event::DealingEvent;
use journ_tax::pool::{AveragePool, DealHolding};

macro_rules! cgt {
    ($journ:expr, $args:expr) => {{
        let cgt_config = journ_tax::cgt_configuration::CgtConfiguration::default();
        journ_tax::computer::compute_gains(&cgt_config, &mut $journ).unwrap()
    }};
}

fn test_runner<T>(test: T)
where
    T: FnOnce() -> JournResult<()>,
{
    if let Err(e) = test() {
        eprintln!("{}", e)
    }
}

#[test]
fn test_push() {
    let mut journ = journ!(
        "unit £\n\
         unit C\n\

        2010-01-01\n \
          ; CGT-Acquire: 5C @@ £30"
    )
    .unwrap();
    let cg = cgt!(journ, "--year-end 2010");
    let acq = cg.acquisitions_by_currency(currency_ref!("C")).unwrap();
    println!("{}", acq[0].amount());
    let mut acq_avg_pool =
        AveragePool::new(DealHolding::Acquisition, money!("CURR"), money!("$"), None);
    //acq_avg_pool.push(UnmatchedEvent::Acquisition(AcquireGroup::new(Acquire::new())))
}
