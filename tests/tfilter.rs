#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]

extern crate core;

struct Exclude {
    contents: isize
}

impl Exclude {
    fn do_thing(&self) -> isize {
        self.contents + 1
    }
    fn do_other_thing(&self) -> isize {
        self.contents
    }
}

fn global_fn(foo : &Exclude, b: isize) -> isize {
    foo.contents  + b
}

struct Include {
    i_contents : isize
}

impl Include {
    fn to_exclude(&self) -> Exclude {
        Exclude { contents: self.i_contents }
    }
}

fn crust_init() -> (Include,) {
    (Include { i_contents: 5 },)
}
