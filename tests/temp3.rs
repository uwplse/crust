#![crate_type = "lib"]

fn f(x: &u32) { }

unsafe fn g() {
    f(&0)
}


