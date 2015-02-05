#![crate_type = "lib"]
#![no_std]
extern crate core;

fn f(mut x : uint) -> uint {
    let mut y;
    y = 4;
    while x > 0 {
        y += 4;
        x -= 1;
    }
    y
}
