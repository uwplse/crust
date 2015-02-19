#![crate_type = "lib"]
#![no_std]
#![feature(unsafe_destructor)]
extern crate core;
extern crate alloc;

use core::option::Option::{self, Some, None};
use core::prelude::{Copy, Drop};
use core::ptr::{PtrExt, MutPtrExt};
use core::ptr;


pub struct Vec<T> {
    ptr: *mut T,
    len: usize,
    cap: usize,
}

impl<T: Copy> Vec<T> {
    pub fn with_capacity(cap: usize) -> Vec<T> {
        // BUG: forgetting to multiply by size_of::<T> allows out-of-bounds access
        let size = cap * core::mem::size_of::<T>();
        let align = core::mem::align_of::<T>();
        let ptr = unsafe { alloc::heap::allocate(size, align) as *mut T };
        Vec {
            ptr: ptr,
            len: 0,
            cap: cap,
        }
    }

    pub fn grow(&mut self) {
        let new_cap = self.cap * 2;
        let new_size = new_cap * core::mem::size_of::<T>();
        let align = core::mem::align_of::<T>();
        let new_ptr = unsafe { alloc::heap::allocate(new_size, align) as *mut T };

        let old_size = self.cap * core::mem::size_of::<T>();
        unsafe { core::intrinsics::copy_nonoverlapping_memory(new_ptr, self.ptr, old_size) };
        self.cap = new_cap;
        self.ptr = new_ptr;
    }

    pub fn push(&mut self, val: T) {
        if (self.len == self.cap) {
            self.grow();
            // BUG: without this assert, staring with capacity 0 will lead to memory errors
            if (self.len == self.cap) {
                panic!();
            }
        }
        unsafe {
            let ptr = self.ptr.offset(self.len as isize);
            ptr::write(ptr, val);
            self.len += 1;
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        if (self.len == 0) {
            return None;
        }

        unsafe {
            let ptr = self.ptr.offset(self.len as isize - 1);
            let val = ptr::read(ptr);
            self.len -= 1;
            Some(val)
        }
    }

    pub fn get(&self, i : u32) -> T {
        if((i as usize)  >= self.len) {
            panic!("out of bounds!");
        }
        unsafe {
            let ptr = self.ptr.offset(i as isize);
            let val = ptr::read(ptr);
            val
        }
    }

    /*
    // BUG: removing 'a from self allows inappropriate aliasing (with get_mut)
    fn get<'a>(&'a self, index: usize) -> &'a T {
        // BUG: forgetting check allows out-of-bounds access
        // BUG: casting index to int before checking against len allows out-of-bounds access
        if index >= self.len {
            crust_abort();
        }

        unsafe { &*self.ptr.offset(index as size) }
    }

    // BUG: using &'a self instead of &'a mut self allows inappropriate aliasing
    fn get_mut<'a>(&'a mut self, index: usize) -> &'a mut T {
        if index >= self.len {
            crust_abort();
        }

        unsafe { &mut *self.ptr.offset(index as size) }
    }
    */
}

#[unsafe_destructor]
impl<T> Drop for Vec<T> {
    fn drop(&mut self) {
        let size = self.cap * core::mem::size_of::<T>();
        let align = core::mem::align_of::<T>();
        unsafe { alloc::heap::deallocate(self.ptr as *mut u8, size, align) };
    }
}

pub fn crust_init() -> (Vec<u8>,Vec<u8>) { (Vec::with_capacity(2),Vec::with_capacity(2)) }
