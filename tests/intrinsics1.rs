#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]

extern crate core;

fn foo() {
    let x = 4u;
    let mut y = 0u;
    unsafe {
        core::intrinsics::copy_memory(&mut y as *mut usize, &x as *const usize, 1);
    }
    let z : isize = unsafe { core::intrinsics::init() };
}

fn crust_init() -> (isize,) {
    (4,)
}
