#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;   // for lang items

enum Option<T> {
    Some(T),
    None,
}

impl<T> Option<T> {
    fn new(x: T) -> Option<T> {
        Option::Some(x)
    }

    fn whatever(self) -> Option<Option<T>> {
        Option::Some(self)
    }
}

fn crust_init() -> (Option<isize>,) {
    (Option::new(3), )
}
