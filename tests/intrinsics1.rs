#![crate_type = "lib"]
#![no_std]

extern crate core;

fn foo() {
    let x = 4u;
    let mut y = 0u;
    unsafe {
        core::intrinsics::copy_memory(&mut y as *mut uint, &x as *const uint, 1);
    }
    let z : int = unsafe { core::intrinsics::init() };
}

fn crust_init() -> (int,) {
    (4,)
}
