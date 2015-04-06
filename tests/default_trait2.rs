#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]

trait T {
    fn f(&self) -> u32;

    fn g(&self) -> u32 { self.f() }
}

struct S;

impl T for S {
    fn f(&self) -> u32 { 777 }
}

fn h(s: S) -> u32 {
    s.f() + s.g()
}
