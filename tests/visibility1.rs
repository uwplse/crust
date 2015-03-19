#![crate_type = "lib"]
#![no_std]
extern crate core;

struct S;

trait T {
    fn f(&self) -> u32;
    fn g(&self) -> u32 { self.f() }
    fn h(&self) -> u32 { self.f() }
}

impl T for S {
    fn f(&self) -> u32 { 0 }
    fn g(&self) -> u32 { 1 }
}

impl S {
    pub fn a(&self) -> u32 { 2 }
    fn b(&self) -> u32 { 3 }
}
