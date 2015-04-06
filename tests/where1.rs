#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;
use core::ops::Add;
use core::prelude::*;

fn f<T: Add<T> + Copy>(x: T) -> <T as Add<T>>::Output
        where <T as Add<T>>::Output: Sized {
    x + x
}
