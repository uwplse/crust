use core::prelude::*;
use core::{ptr, mem};
use alloc::heap::{allocate, deallocate};

pub struct DumbList<T> {
    length: u32,
    head: *mut Node<T>,
    tail: *mut Node<T>
}

struct Node<T> {
    value: T,
    prev: *mut Node<T>,
    next: *mut Node<T>
}


impl<T> DumbList<T> {
    pub fn new() -> DumbList<T> {
        DumbList {
            length: 0,
            head: ptr::null_mut(),
            tail: ptr::null_mut()
        }
    }

    pub fn push_front(&mut self, x: T) {
        let node : *mut Node<T> = unsafe { allocate(mem::size_of::<Node<T>>(), mem::min_align_of::<Node<T>>()) as *mut Node<T> };
        unsafe {
            (*node).prev = ptr::null_mut();
            (*node).next = ptr::null_mut();
            (*node).value = x;
        }
        if self.head == ptr::null_mut() {
            self.head = node;
            self.tail = node;
        } else {
            let head = self.head;
            unsafe {
                (*node).next = head;
                (*head).prev = node;
                self.head = node;
            }
        }
        self.length += 1;
    }

    pub fn pop_back(&mut self) -> T {
        if self.tail == ptr::null_mut() {
            panic!()
        }
        let p_node = self.tail;
        let to_ret = unsafe { ptr::read(p_node).value };
        unsafe {
            self.tail = (*p_node).prev;
            deallocate(p_node as *mut u8, mem::size_of::<Node<T>>(), mem::min_align_of::<Node<T>>());
        }
        to_ret
    }
}

///
pub fn crust_init() -> (DumbList<u8>,) {
    let mut x = DumbList::new();
    x.push_front(1);
    (x,)
}
