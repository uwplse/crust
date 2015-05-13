#![crate_type = "lib"]
#![crate_name = "__crust2"]
#![feature(no_std)]
#![feature(core)]
#![no_std]
extern crate core;
extern crate __crust;

use core::mem;
use core::prelude::Sized;
use core::ptr;
use core::raw::Repr;

pub trait AsPtr {
    fn as_ptr_len(&self) -> (usize, usize);
    fn try_deref(&self);
}

pub fn assert_not_null<T: AsPtr>(p: &T) {
    __crust::assert(p.as_ptr_len().0 != 0);
    if mem::size_of::<T>() > 0 {
        p.try_deref();
    }
}

pub fn assert_not_aliased<T: AsPtr, U: AsPtr>(p1: &T, p2: &U) {
    let (p1, l1) = p1.as_ptr_len();
    let (p2, l2) = p2.as_ptr_len();
    // Check that p1..p1+l1 is either completely before or completely after p2..p2+l2.
    __crust::assert(p2 + l2 <= p1 || p1 + l1 <= p2);
}


impl<'a, T: Sized> AsPtr for &'a T {
    fn as_ptr_len(&self) -> (usize, usize) {
        (*self as *const T as usize, mem::size_of::<T>())
    }

    fn try_deref(&self) {
        unsafe {
            mem::forget(ptr::read(*self as *const T));
        }
    }
}

impl<'a, T: Sized> AsPtr for &'a mut T {
    fn as_ptr_len(&self) -> (usize, usize) {
        (*self as *mut T as *const T as usize, mem::size_of::<T>())
    }

    fn try_deref(&self) {
        unsafe {
            mem::forget(ptr::read(*self as *mut T as *const T));
        }
    }
}


impl<'a, T: Sized> AsPtr for &'a [T] {
    fn as_ptr_len(&self) -> (usize, usize) {
        let r = self.repr();
        (r.data as usize, r.len * mem::size_of::<T>())
    }

    fn try_deref(&self) {
        let r = self.repr();
        unsafe {
            if r.len > 0 {
                // Deref the first and last to make sure that both are within bounds.
                mem::forget(ptr::read(r.data));
                mem::forget(ptr::read(r.data.offset(r.len as isize - 1)));
            }
        }
    }
}

impl<'a, T: Sized> AsPtr for &'a mut [T] {
    fn as_ptr_len(&self) -> (usize, usize) {
        let r = self.repr();
        (r.data as usize, r.len * mem::size_of::<T>())
    }

    fn try_deref(&self) {
        let r = self.repr();
        unsafe {
            if r.len > 0 {
                mem::forget(ptr::read(r.data));
                mem::forget(ptr::read(r.data.offset(r.len as isize - 1)));
            }
        }
    }
}
