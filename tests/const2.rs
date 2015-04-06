#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]

const A: u32 = 0;
const B: u32 = A;
const C: u32 = B;
const D: u32 = C;
const E: u32 = D;

fn f() {}
