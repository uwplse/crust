#![stable]


use core::prelude::*;
use core::cmp::Ordering;
use core::fmt::{self, Debug};
use core::hash::{Hash, Hasher};
use core::iter;
use core::marker::{NoCopy};
use core::{ptr, mem};
use alloc::heap::{allocate, deallocate};
use core::ops::{Deref, DerefMut};

// FIXME(Gankro): Although the internal interface we have here is *safer* than std's DList,
// it's still by no means safe. Any claims we make here about safety in the internal APIs
// are complete hand-waving. For now I'm leaving it like this while we work on better solutions.

/// A DList node.
struct Node<T> {
    prev: Raw<T>,
    next: Link<T>,
    elem: T,
}

fn Box_new<T>(x: T) -> *mut T {
    let ptr = unsafe { allocate(mem::size_of::<T>(), mem::min_align_of::<T>()) as *mut T };
    unsafe { ptr::write(ptr, x); }
    ptr
}

impl<T> Node<T> {
    /// Makes a node with the given element.
    #[inline]
    fn new(elem: T) -> Node<T> {
        Node {
            prev: Raw::none(),
            next: None,
            elem: elem,
        }
    }

    /// Joins two lists.
    #[inline]
    fn link(&mut self, mut next: *mut Node<T>) {
        unsafe { (*next).prev = Raw::some(self) };
        self.next = Some(next);
    }

    /// Makes the given node come after this one, appropriately setting all other links.
    /// Assuming that self has a `next`.
    #[inline]
    fn splice_next(&mut self, mut next: *mut Node<T>) {
        let mut old_next = self.next.take();
        match old_next.as_mut() {
            None => (),
            Some(node) => unsafe { (**node).prev = Raw::some(unsafe { &mut *next }) }
        };
            //.map(|node| node.prev = Raw::some(&mut *next));
        unsafe { (*next).prev = Raw::some(self);
        (*next).next = old_next };
        self.next = Some(next);
    }

    /// Takes the next node from this one, breaking the list into two correctly linked lists.
    #[inline]
    fn take_next(&mut self) -> Option<*mut Node<T>> {
        let mut next = self.next.take();
        match next.as_mut() {
            None => (),
            Some(node) => unsafe { (**node).prev = Raw::none() }
        };
            /*.map(|node| {
            ;
        });*/
        next
    }
}

/// An owning link.
type Link<T> = Option<*mut Node<T>>;

/// A non-owning link, based on a raw ptr.
struct Raw<T> {
    ptr: *mut Node<T>,
    marker: NoCopy,
}

impl<T> Raw<T> {
    /// Makes a null reference.
    #[inline]
    fn none() -> Raw<T> {
        Raw { ptr: ptr::null_mut(), marker: NoCopy }
    }

    /// Makes a reference to the given node.
    #[inline]
    fn some(ptr: &mut Node<T>) -> Raw<T> {
        Raw { ptr: ptr, marker: NoCopy }
    }

    /// Converts the ref to an Option containing a reference.
    #[inline]
    fn as_ref(&self) -> Option<& Node<T>> {
        unsafe { self.ptr.as_ref() }
    }

    /// Converts the ref to an Option containing a mutable reference.
    #[inline]
    fn as_mut(&mut self) -> Option<&mut Node<T>> {
        unsafe { self.ptr.as_mut() }
    }

    /// Takes the reference out and nulls out this one.
    #[inline]
    fn take(&mut self) -> Raw<T> {
        mem::replace(self, Raw::none())
    }

    /// Clones this reference. Note that mutability differs from standard clone.
    /// We don't want these to be cloned in the immutable case.
    #[inline]
    fn clone(&mut self) -> Raw<T> {
        Raw { ptr: self.ptr, marker: NoCopy }
    }
}

/// An experimental rewrite of DList to provide a more cursor-oriented API.
pub struct DList<T> {
    len: usize,
    head: Link<T>,
    tail: Raw<T>,
}

impl<T> DList<T> {
    /// Makes a new DList.
    #[inline]
    pub fn new() -> DList<T> {
        DList { head: None, tail: Raw::none(), len: 0 }
    }

    /// Appends an element to the back of the list.
    pub fn push_back(&mut self, elem: T) {
        self.len += 1;
        let mut node = Box_new(Node::new(elem));
        // unconditionally make the new node the new tail
        let mut old_tail = unsafe { mem::replace(&mut self.tail, Raw::some(&mut *node)) };
        match old_tail.as_mut() {
            // List was empty, so the new node is the new head
            None => self.head = Some(node),
            // List wasn't empty, just need to append this to the old tail
            Some(tail) => tail.link(node),
        }

    }

    /// Appends an element to the front of the list.
    pub fn push_front(&mut self, elem: T) {
        self.len += 1;
        let mut node = Box_new(Node::new(elem));
        match self.head.take() {
            // List was empty, so the new node is the new tail
            None => unsafe { self.tail = Raw::some(&mut *node) },
            // List wasn't empty, append the old head to the new node
            Some(head) => unsafe { (*node).link(head) },
        }
        // unconditionally make the new node the new head
        self.head = Some(node);
    }

    /// Removes the element at back of the list. Returns None if the list is empty.
    pub fn pop_back(&mut self) -> Option<T> {
        // null out the list's tail pointer unconditionally
        match self.tail.take().as_mut() /*.and_then(|tail|*/ {
            None => None,
            Some(tail) => {
            // tail pointer wasn't null, so decrease the len
            self.len -= 1;
            match tail.prev.take().as_mut() {
                // tail had no previous value, so the list only contained this node.
                // So we have to take this node out by removing the head itself
                None => match self.head.take() {
                    None => None,
                    Some(x) => unsafe { Some((ptr::read(x)).elem) }
                },
                    //.map(|box node| node.elem),
                // tail had a previous value, so we need to make that the new tail
                // and take the node out of its next field
                Some(prev) => {
                    self.tail = Raw::some(prev);
                    match prev.next.take() {
                        None => None,
                        Some(x) => unsafe  { Some((ptr::read(x)).elem) }
                    }
                        //.map(|box node| node.elem)
                }
            }
            }
        }
    }

    /// Removes the element at front of the list. Returns None if the list is empty.
    pub fn pop_front(&mut self) -> Option<T> {
        // null out the list's head pointer unconditionally
        match self.head.take() {
            None => None,
            Some(mut head) => { 
                self.len -= 1;
                unsafe {
                match (*head).take_next() {
                    // head had no next value, so just null out the tail
                    None => self.tail = Raw::none(),
                    // head had a next value, which should be the new head
                    Some(next) => self.head = Some(next),
                }
                Some((ptr::read(head)).elem)
                }
            }
        }
//        .map(|mut head| {
            // head wasn't null, so decrease the len

        //})
    }

    /// Gets the element at the front of the list, or None if empty.
    #[inline]
    pub fn front(&self) -> Option<&T> {
        match self.head.as_ref() {
            None => None,
            Some(node) => unsafe { Some(&((**node).elem)) }
        }

            //.map(|node| &node.elem)
    }

    /// Gets the element at the back of the list, or None if empty.
    #[inline]
    pub fn back(&self) -> Option<&T> {
        match self.tail.as_ref() {
            None => None,
            Some(node) => Some(&node.elem)
        }// .map(|node| &node.elem)
    }

    /// Gets the element at the front of the list mutably, or None if empty.
    #[inline]
    pub fn front_mut(&mut self) -> Option<&mut T> {
        match self.head.as_mut() {
            None => None,
            Some(node) => unsafe { Some(&mut (**node).elem) }
        }
            //.map(|node| &mut node.elem)
    }

    /// Gets the element at the back of the list mutably, or None if empty.
    #[inline]
    pub fn back_mut(&mut self) -> Option<&mut T> {
        match self.tail.as_mut() {
            None => None,
            Some(node) => Some(&mut node.elem)
        }

            //.map(|node| &mut node.elem)
    }

    /// Inserts an element at the given index.
    ///
    /// # Panics
    ///
    /// Panics if the index is greater than the length of the list.
    #[inline]
    pub fn insert(&mut self, index: usize, elem: T) {
        if !(index <= self.len()) {
            panic!()
        }
        let mut cursor = self.cursor();
        cursor.seek_forward(index);
        cursor.insert(elem);
    }

    /// Removes the element at the given index. Returns None if the index is out of bounds.
    #[inline]
    pub fn remove(&mut self, index: usize) -> Option<T> {
        if index >= self.len() {
            None
        } else {
            let mut cursor = self.cursor();
            cursor.seek_forward(index);
            cursor.remove()
        }
    }

    /// Splits the list into two lists at the given index. Returns the right side of the split.
    /// Returns an empty list if index is out of bounds.
    pub fn split_at(&mut self, index: usize) -> DList<T> {
        if index >= self.len() {
            DList::new()
        } else {
            let mut cursor = self.cursor();
            cursor.seek_forward(index);
            cursor.split()
        }
    }

    /// Appends the given list to the end of this one. The old list will be empty afterwards.
    pub fn append(&mut self, other: &mut DList<T>) {
        let mut cursor = self.cursor();
        cursor.prev();
        cursor.splice(other);
    }

    /// Inserts the given list at the given index. The old list will be empty afterwards.
    pub fn splice(&mut self, index: usize, other: &mut DList<T>) {
        let mut cursor = self.cursor();
        cursor.seek_forward(index);
        cursor.splice(other);
    }

    /// Gets the number of elements in the list.
    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    /// Whether the list is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Removes all elements from the list.
    #[inline]
    pub fn clear(&mut self) {
        while !self.is_empty() {
            self.pop_front();
        }
    }

    /// Gets a cursor over the list.
    #[inline]
    pub fn cursor(&mut self) -> Cursor<T> {
        Cursor {
            list: self,
            prev: Raw::none(),
            index: 0,
        }
    }

    /// Provides a forward iterator.
    #[inline]
    pub fn iter<'a>(&'a self) -> Iter<'a, T> {
        Iter{nelem: self.len(), head: &self.head, tail: &self.tail}
    }

    /// Provides a forward iterator with mutable references.
    #[inline]
    pub fn iter_mut<'a>(&'a mut self) -> IterMut<'a, T> {
        let head_raw = match self.head.as_mut() {
            Some(head) => unsafe { Raw::some(&mut **head) },
            None => Raw::none(),
        };
        IterMut{
            nelem: self.len(),
            head: head_raw,
            tail: self.tail.clone(),
        }
    }

    /// Consumes the list into an iterator yielding elements by value.
    #[inline]
    pub fn into_iter(self) -> IntoIter<T> {
        IntoIter{list: self}
    }

}

/// A Cursor is like an iterator, except that it can freely seek back-and-forth, and can
/// safely mutate the list during iteration. This is because the lifetime of its yielded
/// references are tied to its own lifetime, instead of just the underlying list. This means
/// cursors cannot yield multiple elements at once.
///
/// Cursors always rest between two elements in the list, and index in a logically circular way.
/// To accomadate this, there is a "ghost" non-element that yields None between the head and tail
/// of the List.
///
/// When created, cursors start between the ghost and the front of the list. That is, `next` will
/// yield the front of the list, and `prev` will yield None. Calling `prev` again will yield
/// the tail.
pub struct Cursor<'a, T: 'a> {
    list: &'a mut DList<T>,
    prev: Raw<T>,
    // index of `next`, where the ghost is at `len`.
    index: usize,
}

// Note, the Cursor's ops and repr are specifically designed so that the cursor's reference
// into the list never needs to update after an operation. It always mutates in front of
// itself. Also to gain ownership of a node, we generally need a ref to the previous Node.
// This is why we hold `prev` rather than `next`.

impl<'a, T> Cursor<'a, T> {
    /// Resets the cursor to lie between the first and last element in the list.
    #[inline]
    pub fn reset(&mut self) {
        self.prev = Raw::none();
        self.index = 0;
    }

    /// Gets the next element in the list.
    pub fn next(&mut self) -> Option<&mut T> {
        self.index += 1;
        match self.prev.take().as_mut() {
            // We had no previous element; the cursor was sitting at the start position
            // Next element should be the head of the list
            None => match self.list.head.as_mut() {
                // No head. No elements.
                None => {
                    self.index = 0;
                    None
                }
                // Got the head. Set it as prev and yield its element
                Some(head) => {
                    unsafe { 
                    self.prev = Raw::some(&mut **head);
                    Some(&mut (**head).elem)
                    }
                }
            },
            // We had a previous element, so let's go to its next
            Some(prev) => match prev.next.as_mut() {
                // No next. We're back at the start point, null the prev and yield None
                None => {
                    self.index = 0;
                    self.prev = Raw::none();
                    None
                }
                // Got a next. Set it as prev and yield its element
                Some(next) => {
                    unsafe {
                    self.prev = Raw::some(&mut **next);
                        Some(mem::copy_mut_lifetime(self, &mut (**next).elem))
                    }
                }
            }
        }
    }

    /// Gets the previous element in the list.
    pub fn prev(&mut self) -> Option<&mut T> {
        match self.prev.take().as_mut() {
            // No prev. We're at the start of the list. Yield None and jump to the end.
            None => {
                self.prev = self.list.tail.clone();
                self.index = self.list.len();
                None
            },
            // Have a prev. Yield it and go to the previous element.
            Some(prev) => {
                self.index -= 1;
                self.prev = prev.prev.clone();
                 unsafe {
                    Some(mem::copy_mut_lifetime(self, &mut prev.elem))
                }
            }
        }
    }

    /// Gets the next element in the list, without moving the cursor head.
    pub fn peek_next(&mut self) -> Option<&mut T> {
        let list = &mut self.list;
        let prev = &mut self.prev;
        //let Cursor{ref mut list, ref mut prev, ..} = *self;
        match prev.as_mut() {
            None => list.front_mut(),
            Some(prev) => {
                match prev.next.as_mut() {
                    None => None,
                    Some(next) => unsafe { Some(&mut (**next).elem) }
                }
                    //.map(|next| &mut next.elem),
            }
        }
    }

    /// Gets the previous element in the list, without moving the cursor head.
    pub fn peek_prev(&mut self) -> Option<&mut T> {
        match self.prev.as_mut() {
            None => None,
            Some(prev) => Some(&mut prev.elem)
        }
            //.map(|prev| &mut prev.elem)
    }

    /// Inserts an element at the cursor's location in the list, and moves the cursor head to
    /// lie before it. Therefore, the new element will be yielded by the next call to `next`.
    pub fn insert(&mut self, elem: T) {
        // destructure so that we can mutate list while working with preve
        let list = &mut self.list;
        let prev = &mut self.prev;
        //let Cursor{ref mut list, ref mut prev, ..} = *self;
        match prev.as_mut() {
            // No prev, we're at the start of the list
            // Also covers empty list
            None =>  list.push_front(elem),
            Some(node) => {
                if node.next.as_mut().is_none() {
                    // No prev.next, we're at the end of the list
                    list.push_back(elem);
                } else {
                    // We're somewhere in the middle, splice in the new node
                    list.len += 1;
                    node.splice_next(Box_new(Node::new(elem)));
                }
            }
        }
    }

    /// Removes the next element in the list, without moving the cursor. Returns None if the list
    /// is empty, or if `next` is the ghost element
    pub fn remove(&mut self) -> Option<T> {
        //let Cursor{ref mut list, ref mut prev, ..} = *self;
        let list = &mut self.list;
        let prev = &mut self.prev;
        match prev.as_mut() {
            // No prev, we're at the start of the list
            // Also covers empty list
            None => list.pop_front(),
            Some(prev) => match prev.take_next() {
                // No prev.next, we're at the ghost, yield None
                None => None,
                // We're somewhere in the middle, rip out prev.next
                Some(x) => {
                    let mut next = unsafe { ptr::read(x) };
                    list.len -= 1;
                    match next.next.take() {
                        // We were actually at the end of the list, so fix the list's tail
                        None => list.tail = Raw::some(prev),
                        // Really in the middle, link the results of removing next
                        Some(next_next) => prev.link(next_next),
                    }
                    Some(next.elem)
                }
            }
        }
    }

    // Splits the list into two at the cursor's current position. This will return a new list
    // consisting of everything after the cursor, with the original list retaining everything
    // before. The cursor will then lie between the tail and the ghost.
    pub fn split(&mut self) -> DList<T> {
        //let Cursor{ref mut list, ref mut prev, index} = *self;
        let list = &mut self.list;
        let prev = &mut self.prev;
        let index = self.index;
        let new_tail = prev.clone();
        let len = list.len();
        match prev.as_mut() {
            // We're at index 0. The new list is the whole list, so just swap
            None => mem::replace(*list, DList::new()),
            // We're not at index 0. This means we don't have to worry about fixing
            // the old list's head.
            Some(prev) => {
                let next_tail = list.tail.clone();
                list.len = index;
                list.tail = new_tail; // == prev
                let next_head = prev.take_next();

                DList {
                    head: next_head,
                    tail: next_tail,
                    len: len - index
                }
            }
        }
    }

    /// Inserts the entire list's contents right after the cursor.
    pub fn splice(&mut self, other: &mut DList<T>) {
        if other.is_empty() { return; }
        let len = other.len;
        other.len = 0;
        let mut head = other.head.take();
        let mut tail = other.tail.take();
        //let Cursor{ref mut list, ref mut prev, ..} = *self;
        let list = &mut self.list;
        let prev = &mut self.prev;

        list.len += len;
        match prev.as_mut() {
            // We're at the head of the list
            None => match list.head.take() {
                // self list is empty, should just be the other list
                None => {
                    list.head = head;
                    list.tail = tail;
                },
                // self list isn't empty
                Some(self_head) => {
                    list.head = head;
                    tail.as_mut().unwrap().link(self_head);
                }
            },
            // Middle or end
            Some(prev) => match prev.take_next() {
                // We're at the end of the list
                None => {
                    prev.link(head.take().unwrap());
                    list.tail = tail;
                }
                // We're strictly in the middle. Self's head and tail won't change
                Some(next) => {
                    prev.link(head.take().unwrap());
                    tail.as_mut().unwrap().link(next);
                }
            }
        }
    }

    /// Calls `next` the specified number of times.
    pub fn seek_forward(&mut self, by: usize) {
        for _ in range(0, by) { self.next(); }
    }

    /// Calls `prev` the specified number of times.
    pub fn seek_backward(&mut self, by: usize) {
        for _ in range(0, by) { self.prev(); }
    }
}


/// An iterator over references to the items of a `DList`.
#[derive(Clone)]
pub struct Iter<'a, T:'a> {
    head: &'a Link<T>,
    tail: &'a Raw<T>,
    nelem: usize,
}

/// An iterator over mutable references to the items of a `DList`.
pub struct IterMut<'a, T:'a> {
    head: Raw<T>,
    tail: Raw<T>,
    nelem: usize
}

/// An iterator over mutable references to the items of a `DList`.
//#[derive(Clone)]
pub struct IntoIter<T> {
    list: DList<T>
}

impl<'a, A> Iterator for Iter<'a, A> {
    type Item = &'a A;
    #[inline]
    fn next(&mut self) -> Option<&'a A> {
        if self.nelem == 0 {
            return None;
        }
        match self.head.as_ref() {
            None => None,
            Some(head) => {
                self.nelem -= 1;
                unsafe {
                self.head = &((**head).next);
                Some(&(**head).elem)
                }
            }
        }/*.map(|head| {
            
        })*/
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.nelem, Some(self.nelem))
    }
}

impl<'a, A> DoubleEndedIterator for Iter<'a, A> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a A> {
        if self.nelem == 0 {
            return None;
        }
        match self.tail.as_ref() {
            None => None,
            Some(tail) => {
                self.nelem -= 1;
                self.tail = &tail.prev;
                Some(&tail.elem)
            }
        }
                /*.map(|tail| {
        })*/
    }
}

impl<'a, A> ExactSizeIterator for Iter<'a, A> {}

impl<'a, A> Iterator for IterMut<'a, A> {
    type Item = &'a mut A;
    #[inline]
    fn next(&mut self) -> Option<&'a mut A> {
        if self.nelem == 0 {
            return None;
        }
        match self.head.take().as_mut() {
            None => None,
            Some(next) => {
                self.nelem -= 1;
                self.head = match next.next {
                    Some(ref mut node) => unsafe { Raw::some(&mut **node) },
                    None => Raw::none(),
                };
                unsafe {
                    //upgrade ref to the necessary lifetime
                    Some(&mut *((&mut next.elem) as *mut _))
                }
            }
        }
        
        /*.map(|next| {
            
        })*/
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.nelem, Some(self.nelem))
    }
}

impl<'a, A> DoubleEndedIterator for IterMut<'a, A> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a mut A> {
        if self.nelem == 0 {
            return None;
        }
        match self.tail.take().as_mut() {
            None => None,
            Some(prev) => {
            self.nelem -= 1;
            self.tail = prev.prev.clone();
            unsafe {
                //upgrade ref to the necessary lifetime
                Some(&mut *((&mut prev.elem) as *mut _))
            }
            }
        }
        /*
            .map(|prev| {
        })*/
    }
}

impl<'a, A> ExactSizeIterator for IterMut<'a, A> {}

impl<A> Iterator for IntoIter<A> {
    type Item = A;
    #[inline]
    fn next(&mut self) -> Option<A> { self.list.pop_front() }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.list.len(), Some(self.list.len()))
    }
}

impl<A> DoubleEndedIterator for IntoIter<A> {
    #[inline]
    fn next_back(&mut self) -> Option<A> { self.list.pop_back() }
}

#[unsafe_destructor]
impl<T> Drop for DList<T> {
    fn drop(&mut self) {
        self.clear()
    }
}

impl<A> iter::FromIterator<A> for DList<A> {
    fn from_iter<T: Iterator<Item=A>>(iterator: T) -> DList<A> {
        let mut ret = DList::new();
        ret.extend(iterator);
        ret
    }
}

impl<A> Extend<A> for DList<A> {
    fn extend<T: Iterator<Item=A>>(&mut self, mut iterator: T) {
        for elt in iterator { self.push_back(elt); }
    }
}


impl<A: PartialEq> PartialEq for DList<A> {
    fn eq(&self, other: &DList<A>) -> bool {
        self.len() == other.len() &&
            iter::order::eq(self.iter(), other.iter())
    }

    fn ne(&self, other: &DList<A>) -> bool {
        self.len() != other.len() ||
            iter::order::ne(self.iter(), other.iter())
    }
}

impl<A: Eq> Eq for DList<A> {}

impl<A: PartialOrd> PartialOrd for DList<A> {
    fn partial_cmp(&self, other: &DList<A>) -> Option<Ordering> {
        iter::order::partial_cmp(self.iter(), other.iter())
    }
}

impl<A: Ord> Ord for DList<A> {
    #[inline]
    fn cmp(&self, other: &DList<A>) -> Ordering {
        iter::order::cmp(self.iter(), other.iter())
    }
}
/*
impl<T: Clone> Clone for DList<T> {
    fn clone(&self) -> DList<T> {
        self.iter().cloned().collect()
    }
}
*/


pub fn crust_init() -> (DList<u8>,) {
    let mut ret = DList::new();
    ret.push_back(8);
    (ret,)
}
