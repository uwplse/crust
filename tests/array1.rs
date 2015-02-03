#![crate_type = "lib"]
#![no_std]
#![feature(unsafe_destructor)]
extern crate core;
extern crate alloc;

use core::option::Option::{self, Some, None};
use core::prelude::{Copy, Drop};
use core::ptr::{PtrExt, MutPtrExt};

// Hacky workaround to avoid needing compiled versions of liblibc and liballoc
//extern crate alloc;

/*
mod alloc {
    pub mod heap {
        pub unsafe fn allocate(size: uint, align: uint) -> *mut u8 {
            // Die horribly if the translation fails to replace this function.
            *(0 as *mut *mut u8)
        }

        pub unsafe fn deallocate(ptr: *mut u8, size: uint, align: uint) {
            *(0 as *mut ())
        }
    }
}
*/

fn panic() -> ! {
    unsafe { core::intrinsics::abort() };
}


struct Array<T> {
    ptr: *mut T,
    len: uint,
}

impl<T: Copy> Array<T> {
    fn new(len: uint) -> Array<T> {
        // BUG: forgetting to multiply by size_of::<T> allows out-of-bounds access
        let size = len * core::mem::size_of::<T>();
        let align = core::mem::align_of::<T>();
        let ptr = unsafe { alloc::heap::allocate(size, align) as *mut T };
        // BUG?: forgetting to zero memory allows access to uninitialized memory
        unsafe { core::ptr::zero_memory(ptr, len) };
        Array {
            ptr: ptr,
            len: len,
        }
    }

    // BUG: removing 'a from self allows inappropriate aliasing (with get_mut)
    fn get<'a>(&'a self, index: uint) -> &'a T {
        // BUG: forgetting check allows out-of-bounds access
        // BUG: casting index to int before checking against len allows out-of-bounds access
        if index >= self.len {
            panic();
        }

        unsafe { &*self.ptr.offset(index as int) }
    }

    // BUG: using &'a self instead of &'a mut self allows inappropriate aliasing
    fn get_mut<'a>(&'a mut self, index: uint) -> &'a mut T {
        if index >= self.len {
            panic();
        }

        unsafe { &mut *self.ptr.offset(index as int) }
    }
}

#[unsafe_destructor]
impl<T> Drop for Array<T> {
    fn drop(&mut self) {
        let size = self.len * core::mem::size_of::<T>();
        let align = core::mem::align_of::<T>();
        unsafe { alloc::heap::deallocate(self.ptr as *mut u8, size, align) };
    }
}

fn crust_init() -> (Array<u32>,) { (Array::new(10),) }
