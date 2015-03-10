#![crate_type = "lib"]
#![no_std]
#![feature(unsafe_destructor)]
extern crate core;
extern crate alloc;

use core::prelude::*;
use core::mem;
use core::ptr;
use alloc::heap;


unsafe fn allocate<T>() -> *mut T {
    let size = mem::size_of::<T>();
    let align = mem::align_of::<T>();
    heap::allocate(size, align) as *mut T
}

unsafe fn free<T>(ptr: *mut T) {
    let size = mem::size_of::<T>();
    let align = mem::align_of::<T>();
    heap::deallocate(ptr as *mut u8, size, align)
}


pub struct Box<T> {
    ptr: *mut T,
}

impl<T> Box<T> {
    pub fn new(x: T) -> Box<T> {
        let p = unsafe { allocate() };
        unsafe { ptr::write(p, x) };
        Box { ptr: p }
    }

    pub fn get(&self) -> &T {
        unsafe { mem::transmute(self.ptr) }
    }

    pub fn get_mut(&mut self) -> &mut T {
        unsafe { mem::transmute(self.ptr) }
    }

    pub fn clone(&self) -> Box<T> {
        Box { ptr: self.ptr }
    }
}

#[unsafe_destructor]
impl<T> Drop for Box<T> {
    fn drop(&mut self) {
        unsafe { ptr::read(self.ptr) };
        unsafe { free(self.ptr) };
        self.ptr = ptr::null_mut();
    }
}


pub fn crust_init(x: u8) -> (Box<u8>,) {
    (Box::new(x),)
}
