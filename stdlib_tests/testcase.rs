#![crate_type = "bin"]
#![no_std]

#[macro_use] extern crate core;
extern crate simplert;
extern crate collections;
use core::prelude::*;
use collections::Vec;


#[no_mangle]
pub fn main() -> bool {
    let mut v = Vec::new();
    v.push(());

    Some(&v[0]) == None
    //unsafe { Some(core::mem::transmute::<usize, &usize>(0)) == None }
}
