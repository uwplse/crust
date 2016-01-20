#![feature(no_std)]
#![feature(core)]

#![crate_type = "lib"]
#![no_std]
#![feature(lang_items, unsafe_destructor)]

extern crate core;

use core::prelude::Copy;
use core::cell::UnsafeCell;

pub struct Cell<T> {
    value: UnsafeCell<T>,
}

impl<T: Copy> Cell<T> {
    pub fn new(value: T) -> Cell<T> {
        Cell {
            value: UnsafeCell::new(value),
        }
    }

    pub fn get(&self) -> T {
        unsafe { *self.value.get() }
    }

    pub fn set(&self, value: T) {
        unsafe {
            *self.value.get() = value;
        }
    }
}
