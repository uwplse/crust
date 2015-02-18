#![crate_type = "lib"]
#![no_std]
extern crate core;

fn match_complex<'a>(x: &'a int) -> int {
    match x {
        &y => y
    }
}

fn match_complex2<'a>(x: &'a (int,int)) -> int {
    match *x {
        (ref a,ref b) => *a + *b
    }
}

fn crust_init() -> (int,(int,int)) {
    (4,(5,5))
}
