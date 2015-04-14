#![crate_type = "lib"]
#![crate_name = "__crust"]
#![feature(no_std)]
#![feature(core)]
#![no_std]
extern crate core;

pub fn nondet<T>() -> T {
    unsafe { core::intrinsics::abort() }
}

pub fn assume(cond: bool) { }
pub fn assert(cond: bool) { }

pub fn unreachable() -> ! {
    unsafe { core::intrinsics::abort() }
}
