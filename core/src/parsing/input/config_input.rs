/*
 * Copyright (c) 2025. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::configuration::Configuration;
use crate::parsing::DerefMutAndDebug;
use crate::parsing::input::TextBlockInput;
use crate::parsing::parser::JournalParseNode;
use nom_locate::LocatedSpan;
use std::cell::{Ref, RefCell, RefMut};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

pub trait ConfigInput<'h> {
    fn config(&self) -> impl Deref<Target = Configuration<'h>>;
    fn config_mut(&self) -> impl DerefMut<Target = Configuration<'h>>;
}

macro_rules! refcell_impl {
    ($ty:ty) => {
        impl<'s, 'h> ConfigInput<'h> for $ty {
            fn config(&self) -> impl Deref<Target = Configuration<'h>> {
                Ref::map(self.extra.borrow(), |c| &**c)
            }

            fn config_mut(&self) -> impl DerefMut<Target = Configuration<'h>> {
                RefMut::map(self.extra.borrow_mut(), |c| &mut **c)
            }
        }
    };
}
refcell_impl!(LocatedSpan<&'h str, &RefCell<dyn DerefMutAndDebug<'h, 's, Configuration<'h>>>>);
refcell_impl!(TextBlockInput<'h, &RefCell<dyn DerefMutAndDebug<'h, 's, Configuration<'h>>>>);

macro_rules! refcell_impl2 {
    ($ty:ty) => {
        impl<'h> ConfigInput<'h> for $ty {
            fn config(&self) -> impl Deref<Target = Configuration<'h>> {
                self.extra.borrow()
            }

            fn config_mut(&self) -> impl DerefMut<Target = Configuration<'h>> {
                self.extra.borrow_mut()
            }
        }
    };
}
refcell_impl2!(LocatedSpan<&'h str, Rc<RefCell<Configuration<'h>>>>);
impl<'h, 's, 'e, 'p> ConfigInput<'h> for TextBlockInput<'h, &'p JournalParseNode<'h, 's, 'e>>
where
    'h: 'e,
    'e: 's,
    's: 'p,
{
    fn config(&self) -> impl Deref<Target = Configuration<'h>> {
        self.extra.config().borrow()
    }

    fn config_mut(&self) -> impl DerefMut<Target = Configuration<'h>> {
        self.extra.config().borrow_mut()
    }
}

macro_rules! refcell_impl3 {
    ($ty:ty) => {
        impl<'h> ConfigInput<'h> for $ty {
            fn config(&self) -> impl Deref<Target = Configuration<'h>> {
                self.extra.borrow()
            }
            fn config_mut(&self) -> impl DerefMut<Target = Configuration<'h>> {
                self.extra.borrow_mut()
            }
        }
    };
}
refcell_impl3!(LocatedSpan<&'h str, RefCell<Configuration<'h>>>);
refcell_impl3!(TextBlockInput<'h, RefCell<Configuration<'h>>>);

/*
macro_rules! refcell_impl4 {
    ($ty:ty) => {
        impl<'s, 'h> ConfigInput<'h> for $ty {
            fn config(&self) -> impl Deref<Target = Configuration<'h>> {
                Ref::map(self.extra.borrow(), |c| &***c)
            }

            fn config_mut(&self) -> impl DerefMut<Target = Configuration<'h>> {
                RefMut::map(self.extra.borrow_mut(), |c| Arc::make_mut(c))
            }
        }
    };
}
refcell_impl4!(LocatedSpan<&'h str, RefCell<&'s mut Arc<Configuration<'h>>>>);
refcell_impl4!(TextBlockInput<'h, RefCell<&'s mut Arc<Configuration<'h>>>>);

 */
