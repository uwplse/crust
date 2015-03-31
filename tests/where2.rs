#![crate_type = "lib"]
#![no_std]
extern crate core;
use core::ops::Add;
use core::prelude::*;

trait Tr<T> {
    fn f(&self, x: T);
}

impl<T> Tr<T> for ()
        where T: Add<T> + Copy + Sized,
              <T as Add<T>>::Output: Sized {
    fn f(&self, x: T) {
        let _ = x + x;
    }
}
