#![crate_type = "lib"]
#![no_std]
#![feature(phase, lang_items, unsafe_destructor)]

#[phase(plugin, link)] extern crate core;
use core::prelude::{Copy, Drop, Deref, DerefMut};
use core::prelude::{Option, Some, None};
use core::kinds::marker;



/*
pub struct UnsafeCell<T> {
    value: T,
}

impl<T> UnsafeCell<T> {
    pub fn new(value: T) -> UnsafeCell<T> {
        UnsafeCell { value: value }
    }

    pub unsafe fn get(&self) -> *mut T { &self.value as *const T as *mut T }
}
*/

use core::cell::UnsafeCell;



pub struct Cell<T> {
    value: UnsafeCell<T>,
    noshare: marker::NoSync,
}

impl<T:Copy> Cell<T> {
    pub fn new(value: T) -> Cell<T> {
        Cell {
            value: UnsafeCell::new(value),
            noshare: marker::NoSync,
        }
    }

    pub fn get(&self) -> T {
        unsafe{ *self.value.get() }
    }

    pub fn set(&self, value: T) {
        unsafe {
            *self.value.get() = value;
        }
    }
}



pub struct RefCell<T> {
    value: UnsafeCell<T>,
    borrow: Cell<BorrowFlag>,
    nocopy: marker::NoCopy,
    noshare: marker::NoSync,
}

// Values [1, MAX-1] represent the number of `Ref` active
// (will not outgrow its range since `uint` is the size of the address space)
type BorrowFlag = uint;
const UNUSED: BorrowFlag = 0;
const WRITING: BorrowFlag = -1;

impl<T> RefCell<T> {
    pub fn new(value: T) -> RefCell<T> {
        RefCell {
            value: UnsafeCell::new(value),
            borrow: Cell::new(UNUSED),
            nocopy: marker::NoCopy,
            noshare: marker::NoSync,
        }
    }

    pub fn try_borrow<'a>(&'a self) -> Option<Ref<'a, T>> {
        match self.borrow.get() {
            WRITING => None,
            borrow => {
                self.borrow.set(borrow + 1);
                Some(Ref { _parent: self })
            }
        }
    }

    pub fn borrow<'a>(&'a self) -> Ref<'a, T> {
        match self.try_borrow() {
            Some(ptr) => ptr,
            None => panic!("RefCell<T> already mutably borrowed")
        }
    }

    pub fn try_borrow_mut<'a>(&'a self) -> Option<RefMut<'a, T>> {
        match self.borrow.get() {
            UNUSED => {
                self.borrow.set(WRITING);
                Some(RefMut { _parent: self })
            },
            _ => None
        }
    }

    pub fn borrow_mut<'a>(&'a self) -> RefMut<'a, T> {
        match self.try_borrow_mut() {
            Some(ptr) => ptr,
            None => panic!("RefCell<T> already borrowed")
        }
    }
}


pub struct Ref<'b, T:'b> {
    _parent: &'b RefCell<T>
}

#[unsafe_destructor]
impl<'b, T> Drop for Ref<'b, T> {
    fn drop(&mut self) {
        let borrow = self._parent.borrow.get();
        debug_assert!(borrow != WRITING && borrow != UNUSED);
        self._parent.borrow.set(borrow - 1);
    }
}

impl<'b, T> Deref<T> for Ref<'b, T> {
    fn deref<'a>(&'a self) -> &'a T {
        unsafe { &*self._parent.value.get() }
    }
}


pub struct RefMut<'b, T:'b> {
    _parent: &'b RefCell<T>
}

#[unsafe_destructor]
impl<'b, T> Drop for RefMut<'b, T> {
    fn drop(&mut self) {
        let borrow = self._parent.borrow.get();
        debug_assert!(borrow == WRITING);
        self._parent.borrow.set(UNUSED);
    }
}

impl<'b, T> Deref<T> for RefMut<'b, T> {
    fn deref<'a>(&'a self) -> &'a T {
        unsafe { &*self._parent.value.get() }
    }
}

impl<'b, T> DerefMut<T> for RefMut<'b, T> {
    fn deref_mut<'a>(&'a mut self) -> &'a mut T {
        unsafe { &mut *self._parent.value.get() }
    }
}
