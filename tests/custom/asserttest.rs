//! An example of a test based on user assertions

#![crate_type = "lib"]
#![no_std]
#![feature(unsafe_destructor, no_std)]
#![feature(core, alloc, collections)]
#[macro_use] extern crate core;
extern crate alloc;
extern crate collections;
extern crate __crust;
extern crate __crust2;


fn main() {
    let i = __crust::nondet::<i32>();
    __crust::assert((i + 1) as i32 > i);
}

fn crust_init() -> (u8,) {
    main();
    (0,)
}
