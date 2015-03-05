#![crate_type = "lib"]

trait T {
    fn f<T>(&self, x: T) -> u32;

    fn g<T>(&self, x: T) -> u32 { 999 }
}

struct S;

impl T for S {
    fn f<U>(&self, x: U) -> u32 { 777 }
}

fn h(s: S) -> u32 {
    s.f(17u32) + s.g(17u32)
}
