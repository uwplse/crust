#![crate_type = "lib"]
#![no_std]
extern crate std;   // for lang items

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

fn crust_init() -> (Option<int>,) {
    (Option::new(3), )
}
