#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]

extern crate core;

fn foo() -> bool {
    let z = 
        unsafe {
            let x : i32 = 44;
            let y : i32 = 55;
            core::intrinsics::i32_add_with_overflow(x, y)
        }
    ;
    match z {
        (_,flag) => flag
    }
}

fn crust_init() -> (isize,) {
    (0,)
}
