#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

fn crust_abort() -> ! {
    unsafe {
        core::intrinsics::abort()
    }
}

fn do_thing(x : u32) -> u32 {
    x
}

fn whatever() {
    let x : isize = crust_abort();
    do_thing(crust_abort());
}
