#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

fn match_complex<'a>(x: &'a isize) -> isize {
    match x {
        &y => y
    }
}

fn match_complex2<'a>(x: &'a (isize,isize)) -> isize {
    match *x {
        (ref a,ref b) => *a + *b
    }
}

fn crust_init() -> (isize,(isize,isize)) {
    (4,(5,5))
}
