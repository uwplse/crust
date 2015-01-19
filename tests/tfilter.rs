#![crate_type = "lib"]
#![no_std]

extern crate core;

struct Exclude {
    contents: int
}

impl Exclude {
    fn do_thing(&self) -> int {
        self.contents + 1
    }
    fn do_other_thing(&self) -> int {
        self.contents
    }
}

fn global_fn(foo : &Exclude, b: int) -> int {
    foo.contents  + b
}

struct Include {
    i_contents : int
}

impl Include {
    fn to_exclude(&self) -> Exclude {
        Exclude { contents: self.i_contents }
    }
}

fn crust_init() -> (Include,) {
    (Include { i_contents: 5 },)
}
