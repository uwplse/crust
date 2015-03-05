#![crate_type = "lib"]

trait T<I> {
    fn f(&self) -> I;

    fn g(&self) -> I { self.f() }
}

struct S;

impl T<u32> for S {
    fn f(&self) -> u32 { 777 }
}

fn h(s: S) -> u32 {
    s.f() + s.g()
}
