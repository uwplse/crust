#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

struct S {
    x: isize
}

impl S {
    fn do_thing(x: &isize) -> isize{
        *x + 1
    }
    
}

fn whatever(y: isize, x : (isize,(usize,&isize))) -> &isize {
    match x {
        (_,(_,p)) => p
    }
}

struct Cont<T> {
    contents: T
}

impl<T> Cont<T> {
    fn get_mut_ref<'a>(&'a mut self) -> &'a mut T {
        &mut self.contents
    }
    fn get_ref(&self) -> &T {
        &self.contents
    }
}

fn crust_init () -> (S,Cont<isize>) {
    (S { x : 4 },Cont { contents: 4 })
}
