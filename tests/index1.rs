#![crate_type = "lib"]
#![no_std]
#![feature(unsafe_destructor)]
extern crate core;

fn test(a: &[u8]) -> u8 {
    a[1]
}

fn test_mut(a: &mut [u8]) -> u8 {
    a[1]
}

fn test_set(a: &mut [u8]) {
    a[1] = 1;
}


fn test_addr(a: &mut [u8]) {
    let p = &a[1];
}

fn test_addr_mut(a: &mut [u8]) {
    let p = &mut a[1];
}
