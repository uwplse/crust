#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

fn f() -> i32 {
    let x = 1_i32;
    let x = 2_i32;
    let x = 3_i32;
    x
}

fn g() -> i32 {
    {
        let x = 1_i32;
        x;
    }
    {
        let x = 1_i32;
        x
    }
}

fn h() -> i32 {
    let x = 1_i32;
    {
        let x = 2_i32;
    }
    x
}

fn i(x: i32) -> i32 {
    let x = 1_i32;
    x
}

fn crust_init() -> (usize, ) { (0, ) }
