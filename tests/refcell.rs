// This code is mostly copied from rust/src/libcore/cell.rs, which is distributed under the
// following license:
//
// Copyright 2012-2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.


#![crate_type = "lib"]
#![no_std]
#![feature(phase, lang_items, unsafe_destructor)]

#[phase(plugin, link)] extern crate core;
use core::prelude::{Copy, Drop, Deref, DerefMut};
//use core::prelude::{Option, Some, None};
//use core::kinds::marker;
//use core::cell::UnsafeCell;


use Option::{None, Some};

pub enum Option<T> {
    None,
    Some(T),
}


pub mod marker {
    pub struct NoCopy;
    pub struct NoSync;
}


pub struct UnsafeCell<T> {
    value: T,
}

impl<T> UnsafeCell<T> {
    pub fn new(value: T) -> UnsafeCell<T> {
        UnsafeCell { value: value }
    }

    pub unsafe fn get(&self) -> *mut T { &self.value as *const T as *mut T }
}



pub struct Abstract {
    x: uint,
}

pub fn crust_abort() -> ! {
    unsafe { core::intrinsics::abort() }
}



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
            //None => panic!("RefCell<T> already mutably borrowed")
            None => crust_abort(),
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
            //None => panic!("RefCell<T> already borrowed")
            None => crust_abort(),
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
        //debug_assert!(borrow != WRITING && borrow != UNUSED);
        //debug_assert!(borrow != -1 && borrow != 0);
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
        //debug_assert!(borrow == WRITING);
        //debug_assert!(borrow == -1);
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


pub fn crust_init() -> (RefCell<Abstract>,) { (RefCell::new(Abstract { x: 0, }),) }
