#![crate_type = "lib"]
#![no_std]
#![feature(unsafe_destructor)]
#![feature(int_uint)]

extern crate core;

fn foo(x: [i32; 3]) -> i32 {
    4
}

/*fn bar() -> [i32;4] {
    [1,2,3,4]
}*/

fn bar() {
    let x = [1,2,3];
    let y = x;
    foo([1,2,3]);
}
