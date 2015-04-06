#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
#![feature(unsafe_destructor)]
#![feature(int_uint)]
extern crate core;
extern crate alloc;
use core::prelude::*;


use alloc::heap::{EMPTY, allocate, reallocate, deallocate};
use core::borrow::{Cow, IntoCow};
use core::cmp::max;
use core::cmp::{Ordering};
use core::default::Default;
use core::fmt;
use core::hash::{self, Hash};
use core::iter::{repeat, FromIterator};
use core::marker::{ContravariantLifetime, InvariantType};
use core::mem;
use core::nonzero::NonZero;
use core::num::{Int, UnsignedInt};
use core::ops::{Index, IndexMut, Deref, Add};
use core::ops;
use core::ptr;
use core::raw::Slice as RawSlice;
use core::usize;

#[unsafe_no_drop_flag]
#[stable]
pub struct Vec<T> {
    ptr: *mut T,
    len: usize,
    cap: usize,
}

const MAX_VEC_SIZE : u32 = 4;

////////////////////////////////////////////////////////////////////////////////
// Inherent methods
////////////////////////////////////////////////////////////////////////////////

impl<T> Vec<T> {
/*
    #[inline]
    #[stable]
    pub fn new() -> Vec<T> {
        // We want ptr to never be NULL so instead we set it to some arbitrary
        // non-null value which is fine since we never call deallocate on the ptr
        // if cap is 0. The reason for this is because the pointer of a slice
        // being NULL would break the null pointer optimization for enums.
        Vec { ptr: EMPTY as *mut T, len: 0, cap: 0 }
    }
*/
    #[inline]
    #[stable]
    pub fn with_capacity(capacity: usize) -> Vec<T> {
        if mem::size_of::<T>() == 0 {
            Vec { ptr: EMPTY as *mut T , len: 0, cap: usize::MAX }
        } else if capacity == 0 {
            Vec { ptr: EMPTY as *mut T, len: 0, cap: 0 }
        } else {
            let size = match capacity.checked_mul(mem::size_of::<T>()) {
                Some(i) => i,
                None => panic!()
            };
            /*
             * don't support static strings
            .expect("capacity overflow");*
            */
            let ptr = unsafe { allocate(size, mem::min_align_of::<T>()) };
            if ptr.is_null() { ::alloc::oom() }
            Vec { ptr: ptr as *mut T, len: 0, cap: capacity }
        }
    }
/* TODO(jtoman): these trigger infinite loops in the api discovery :(
    #[stable]
    pub unsafe fn from_raw_parts(ptr: *mut T, length: usize,
                                 capacity: usize) -> Vec<T> {
        Vec { ptr: ptr, len: length, cap: capacity }
    }

    #[inline]
    #[unstable = "may be better expressed via composition"]
    pub unsafe fn from_raw_buf(ptr: *const T, elts: usize) -> Vec<T> {
        let mut dst = Vec::with_capacity(elts);
        dst.set_len(elts);
        ptr::copy_nonoverlapping_memory(dst.as_mut_ptr(), ptr, elts);
        dst
    }
*/
    #[stable]
    pub fn reserve(&mut self, additional: usize) {
        if self.cap - self.len < additional {
            //let err_msg = "Vec::reserve: `usize` overflow";
            /*let new_cap = match self.len.checked_add(additional).expect(err_msg)
                .checked_next_power_of_two().expect(err_msg);*/
            let new_cap = match self.len.checked_add(additional) {
                Some(i) => i,
                None => panic!()
            };
            //let new_cap = self.len + additional;
            self.grow_capacity(new_cap);
        }
    }

    #[stable]
    pub fn reserve_exact(&mut self, additional: usize) {
        if self.cap - self.len < additional {
            match self.len.checked_add(additional) {
                Some(new_cap) => self.grow_capacity(new_cap),
                None => panic!("Vec::reserve: `usize` overflow")
            }
            let new_cap = self.len + additional;
            
            self.grow_capacity(new_cap)
        }
    }

    #[stable]
    pub fn shrink_to_fit(&mut self) {
        if mem::size_of::<T>() == 0 { return }

        if self.len == 0 {
            if self.cap != 0 {
                unsafe {
                    dealloc(self.ptr, self.cap)
                }
                self.cap = 0;
            }
        } else {
            unsafe {
                // Overflow check is unnecessary as the vector is already at
                // least this large.
                let ptr = reallocate(self.ptr as *mut u8,
                                     self.cap * mem::size_of::<T>(),
                                     self.len * mem::size_of::<T>(),
                                     mem::min_align_of::<T>()) as *mut T;
                if ptr.is_null() { ::alloc::oom() }
                self.ptr = ptr;
            }
            self.cap = self.len;
        }
    }

    // XXX(jtoman): don't support boxes or slices
    // #[unstable]
    // pub fn into_boxed_slice(mut self) -> Box<[T]> {
    //     self.shrink_to_fit();
    //     unsafe {
    //         let xs: Box<[T]> = mem::transmute(self.as_mut_slice());
    //         mem::forget(self);
    //         xs
    //     }
    // }

    #[stable]
    pub fn truncate(&mut self, len: usize) {
        unsafe {
            // drop any extra elements
            while len < self.len {
                // decrement len before the read(), so a panic on Drop doesn't
                // re-drop the just-failed value.
                self.len -= 1;
                ptr::read(self.get_unchecked(self.len));
            }
        }
    }

    // XXX(jtoman): don't support iterators
    // #[inline]
    // #[stable]
    // pub fn into_iter(self) -> IntoIter<T> {
    //     unsafe {
    //         let ptr = *self.ptr;
    //         let cap = self.cap;
    //         let begin = ptr as *const T;
    //         let end = if mem::size_of::<T>() == 0 {
    //             (ptr as usize + self.len()) as *const T
    //         } else {
    //             ptr.offset(self.len() as isize) as *const T
    //         };
    //         mem::forget(self);
    //         IntoIter { allocation: ptr, cap: cap, ptr: begin, end: end }
    //     }
    // }

    #[inline]
    #[stable]
    pub unsafe fn set_len(&mut self, len: usize) {
        self.len = len;
    }

    #[stable]
    pub fn insert(&mut self, index: usize, element: T) {
        let len = self.len();
        assert!(index <= len);
        // space for the new element
        self.reserve(1);

        unsafe { // infallible
            // The spot to put the new value
            {
                let p = self.as_mut_ptr().offset(index as isize);
                // Shift everything over to make space. (Duplicating the
                // `index`th element into two consecutive places.)
                ptr::copy_memory(p.offset(1), &*p, len - index);
                // Write it in, overwriting the first copy of the `index`th
                // element.
                ptr::write(&mut *p, element);
            }
            self.set_len(len + 1);
        }
    }

    #[stable]
    pub fn remove(&mut self, index: usize) -> T {
        let len = self.len();
        assert!(index < len); // transform to panic
        unsafe { // infallible
            let ret;
            {
                // the place we are taking from.
                let ptr = self.as_mut_ptr().offset(index as isize);
                // copy it out, unsafely having a copy of the value on
                // the stack and in the vector at the same time.
                ret = ptr::read(ptr);

                // Shift everything down to fill in that spot.
                ptr::copy_memory(ptr, &*ptr.offset(1), len - index - 1);
            }
            self.set_len(len - 1);
            ret
        }
    }

    #[inline]
    #[stable]
    pub fn push(&mut self, value: T) {
        if mem::size_of::<T>() == 0 {
            // zero-size types consume no memory, so we can't rely on the
            // address space running out
            self.len = match self.len.checked_add(1) {
                Some(i) => i,
                None => panic!()
            };
             //   .expect("length overflow");
            unsafe { mem::forget(value); }
            return
        }
        if self.len == self.cap {
            let old_size = self.cap * mem::size_of::<T>();
            let size = max(old_size, 2 * mem::size_of::<T>()) * 2;
            if old_size > size { panic!("capacity overflow") }
            unsafe {
                let ptr = alloc_or_realloc(self.ptr, old_size, size);
                if ptr.is_null() { ::alloc::oom() }
                self.ptr = ptr;
            }
            self.cap = max(self.cap, 2) * 2;
        }

        unsafe {
            let end = (self.ptr).offset(self.len as isize);
            ptr::write(&mut *end, value);
            self.len += 1;
        }
    }

    #[inline]
    #[stable]
    pub fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            None
        } else {
            unsafe {
                self.len -= 1;
                Some(ptr::read(self.get_unchecked(self.len())))
            }
        }
    }

    #[inline]
    #[unstable = "new API, waiting for dust to settle"]
    pub fn append(&mut self, other: &mut Self) {
        if mem::size_of::<T>() == 0 {
            // zero-size types consume no memory, so we can't rely on the
            // address space running out
            self.len = match self.len.checked_add(other.len()) {
                Some(i) => i,
                None => panic!()
            };
            //    .expect("length overflow");
            unsafe { other.set_len(0) }
            return;
        }
        self.reserve(other.len());
        let len = self.len();
        unsafe {
            ptr::copy_nonoverlapping_memory(
                self.get_unchecked_mut(len),
                other.as_ptr(),
                other.len());
        }

        self.len += other.len();
        unsafe { other.set_len(0); }
    }

    // XXX(jtoman): don't support drain yet?
    // #[inline]
    // #[unstable = "matches collection reform specification, waiting for dust to settle"]
    // pub fn drain<'a>(&'a mut self) -> Drain<'a, T> {
    //     unsafe {
    //         let begin = *self.ptr as *const T;
    //         let end = if mem::size_of::<T>() == 0 {
    //             (*self.ptr as usize + self.len()) as *const T
    //         } else {
    //             (*self.ptr).offset(self.len() as isize) as *const T
    //         };
    //         self.set_len(0);
    //         Drain {
    //             ptr: begin,
    //             end: end,
    //             marker: ContravariantLifetime,
    //         }
    //     }
    // }

    #[inline]
    #[stable]
    pub fn clear(&mut self) {
        self.truncate(0)
    }

    #[inline]
    #[stable]
    pub fn len(&self) -> usize { self.len }

    #[stable]
    pub fn is_empty(&self) -> bool { self.len() == 0 }

    
    /*
    XXX(jtoman): don't support higher functions
    #[unstable = "API may change to provide stronger guarantees"]
    pub fn map_in_place<U, F>(self, mut f: F) -> Vec<U> where F: FnMut(T) -> U {
        // FIXME: Assert statically that the types `T` and `U` have the same
        // size.
        assert!(mem::size_of::<T>() == mem::size_of::<U>());

        let mut vec = self;

        if mem::size_of::<T>() != 0 {
            // FIXME: Assert statically that the types `T` and `U` have the
            // same minimal alignment in case they are not zero-sized.

            // These asserts are necessary because the `min_align_of` of the
            // types are passed to the allocator by `Vec`.
            assert!(mem::min_align_of::<T>() == mem::min_align_of::<U>());

            // This `as isize` cast is safe, because the size of the elements of the
            // vector is not 0, and:
            //
            // 1) If the size of the elements in the vector is 1, the `isize` may
            //    overflow, but it has the correct bit pattern so that the
            //    `.offset()` function will work.
            //
            //    Example:
            //        Address space 0x0-0xF.
            //        `u8` array at: 0x1.
            //        Size of `u8` array: 0x8.
            //        Calculated `offset`: -0x8.
            //        After `array.offset(offset)`: 0x9.
            //        (0x1 + 0x8 = 0x1 - 0x8)
            //
            // 2) If the size of the elements in the vector is >1, the `usize` ->
            //    `isize` conversion can't overflow.
            let offset = vec.len() as isize;
            let start = vec.as_mut_ptr();

            let mut pv = PartialVecNonZeroSized {
                vec: vec,

                start_t: start,
                // This points inside the vector, as the vector has length
                // `offset`.
                end_t: unsafe { start.offset(offset) },
                start_u: start as *mut U,
                end_u: start as *mut U,
            };
            //  start_t
            //  start_u
            //  |
            // +-+-+-+-+-+-+
            // |T|T|T|...|T|
            // +-+-+-+-+-+-+
            //  |           |
            //  end_u       end_t

            while pv.end_u as *mut T != pv.end_t {
                unsafe {
                    //  start_u start_t
                    //  |       |
                    // +-+-+-+-+-+-+-+-+-+
                    // |U|...|U|T|T|...|T|
                    // +-+-+-+-+-+-+-+-+-+
                    //          |         |
                    //          end_u     end_t

                    let t = ptr::read(pv.start_t);
                    //  start_u start_t
                    //  |       |
                    // +-+-+-+-+-+-+-+-+-+
                    // |U|...|U|X|T|...|T|
                    // +-+-+-+-+-+-+-+-+-+
                    //          |         |
                    //          end_u     end_t
                    // We must not panic here, one cell is marked as `T`
                    // although it is not `T`.

                    pv.start_t = pv.start_t.offset(1);
                    //  start_u   start_t
                    //  |         |
                    // +-+-+-+-+-+-+-+-+-+
                    // |U|...|U|X|T|...|T|
                    // +-+-+-+-+-+-+-+-+-+
                    //          |         |
                    //          end_u     end_t
                    // We may panic again.

                    // The function given by the user might panic.
                    let u = f(t);

                    ptr::write(pv.end_u, u);
                    //  start_u   start_t
                    //  |         |
                    // +-+-+-+-+-+-+-+-+-+
                    // |U|...|U|U|T|...|T|
                    // +-+-+-+-+-+-+-+-+-+
                    //          |         |
                    //          end_u     end_t
                    // We should not panic here, because that would leak the `U`
                    // pointed to by `end_u`.

                    pv.end_u = pv.end_u.offset(1);
                    //  start_u   start_t
                    //  |         |
                    // +-+-+-+-+-+-+-+-+-+
                    // |U|...|U|U|T|...|T|
                    // +-+-+-+-+-+-+-+-+-+
                    //            |       |
                    //            end_u   end_t
                    // We may panic again.
                }
            }

            //  start_u     start_t
            //  |           |
            // +-+-+-+-+-+-+
            // |U|...|U|U|U|
            // +-+-+-+-+-+-+
            //              |
            //              end_t
            //              end_u
            // Extract `vec` and prevent the destructor of
            // `PartialVecNonZeroSized` from running. Note that none of the
            // function calls can panic, thus no resources can be leaked (as the
            // `vec` member of `PartialVec` is the only one which holds
            // allocations -- and it is returned from this function. None of
            // this can panic.
            unsafe {
                let vec_len = pv.vec.len();
                let vec_cap = pv.vec.capacity();
                let vec_ptr = pv.vec.as_mut_ptr() as *mut U;
                mem::forget(pv);
                Vec::from_raw_parts(vec_ptr, vec_len, vec_cap)
            }
        } else {
            // Put the `Vec` into the `PartialVecZeroSized` structure and
            // prevent the destructor of the `Vec` from running. Since the
            // `Vec` contained zero-sized objects, it did not allocate, so we
            // are not leaking memory here.
            let mut pv = PartialVecZeroSized::<T,U> {
                num_t: vec.len(),
                num_u: 0,
                marker_t: InvariantType,
                marker_u: InvariantType,
            };
            unsafe { mem::forget(vec); }

            while pv.num_t != 0 {
                unsafe {
                    // Create a `T` out of thin air and decrement `num_t`. This
                    // must not panic between these steps, as otherwise a
                    // destructor of `T` which doesn't exist runs.
                    let t = mem::uninitialized();
                    pv.num_t -= 1;

                    // The function given by the user might panic.
                    let u = f(t);

                    // Forget the `U` and increment `num_u`. This increment
                    // cannot overflow the `usize` as we only do this for a
                    // number of times that fits into a `usize` (and start with
                    // `0`). Again, we should not panic between these steps.
                    mem::forget(u);
                    pv.num_u += 1;
                }
            }
            // Create a `Vec` from our `PartialVecZeroSized` and make sure the
            // destructor of the latter will not run. None of this can panic.
            let mut result = Vec::new();
            unsafe {
                result.set_len(pv.num_u);
                mem::forget(pv);
            }
            result
        }
    }
    */

    // work around for not implementing slice
    unsafe fn as_ptr(&self) -> *const T {
        self.ptr as *const T
    }
    unsafe fn as_mut_ptr(&mut self) -> *mut T {
        self.ptr
    }
    unsafe fn get_unchecked_mut(&mut self, offs: usize) -> *mut T {
        self.as_mut_ptr().offset(offs as isize)
    }
    unsafe fn get_unchecked(&self, offs : usize) -> *const T {
        self.as_ptr().offset(offs as isize)
    }
}
/*
 XXX(jtoman): so much functionality
impl<T: Clone> Vec<T> {
    /// Resizes the `Vec` in-place so that `len()` is equal to `new_len`.
    ///
    /// Calls either `extend()` or `truncate()` depending on whether `new_len`
    /// is larger than the current value of `len()` or not.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = vec!["hello"];
    /// vec.resize(3, "world");
    /// assert_eq!(vec, vec!["hello", "world", "world"]);
    ///
    /// let mut vec = vec![1i, 2, 3, 4];
    /// vec.resize(2, 0);
    /// assert_eq!(vec, vec![1, 2]);
    /// ```
    #[unstable = "matches collection reform specification; waiting for dust to settle"]
    pub fn resize(&mut self, new_len: usize, value: T) {
        let len = self.len();

        if new_len > len {
            self.extend(repeat(value).take(new_len - len));
        } else {
            self.truncate(new_len);
        }
    }

    /// Appends all elements in a slice to the `Vec`.
    ///
    /// Iterates over the slice `other`, clones each element, and then appends
    /// it to this `Vec`. The `other` vector is traversed in-order.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = vec![1i];
    /// vec.push_all(&[2i, 3, 4]);
    /// assert_eq!(vec, vec![1, 2, 3, 4]);
    /// ```
    #[inline]
    #[unstable = "likely to be replaced by a more optimized extend"]
    pub fn push_all(&mut self, other: &[T]) {
        self.reserve(other.len());

        for i in range(0, other.len()) {
            let len = self.len();

            // Unsafe code so this can be optimised to a memcpy (or something similarly
            // fast) when T is Copy. LLVM is easily confused, so any extra operations
            // during the loop can prevent this optimisation.
            unsafe {
                ptr::write(
                    self.get_unchecked_mut(len),
                    other.get_unchecked(i).clone());
                self.set_len(len + 1);
            }
        }
    }
}
*/

/*
XXX(jtoman): what is this?
 uses unsafe
impl<T: PartialEq> Vec<T> {
    /// Removes consecutive repeated elements in the vector.
    ///
    /// If the vector is sorted, this removes all duplicates.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = vec![1i, 2, 2, 3, 2];
    ///
    /// vec.dedup();
    ///
    /// assert_eq!(vec, vec![1i, 2, 3, 2]);
    /// ```
    #[stable]
    pub fn dedup(&mut self) {
        unsafe {
            // Although we have a mutable reference to `self`, we cannot make
            // *arbitrary* changes. The `PartialEq` comparisons could panic, so we
            // must ensure that the vector is in a valid state at all time.
            //
            // The way that we handle this is by using swaps; we iterate
            // over all the elements, swapping as we go so that at the end
            // the elements we wish to keep are in the front, and those we
            // wish to reject are at the back. We can then truncate the
            // vector. This operation is still O(n).
            //
            // Example: We start in this state, where `r` represents "next
            // read" and `w` represents "next_write`.
            //
            //           r
            //     +---+---+---+---+---+---+
            //     | 0 | 1 | 1 | 2 | 3 | 3 |
            //     +---+---+---+---+---+---+
            //           w
            //
            // Comparing self[r] against self[w-1], this is not a duplicate, so
            // we swap self[r] and self[w] (no effect as r==w) and then increment both
            // r and w, leaving us with:
            //
            //               r
            //     +---+---+---+---+---+---+
            //     | 0 | 1 | 1 | 2 | 3 | 3 |
            //     +---+---+---+---+---+---+
            //               w
            //
            // Comparing self[r] against self[w-1], this value is a duplicate,
            // so we increment `r` but leave everything else unchanged:
            //
            //                   r
            //     +---+---+---+---+---+---+
            //     | 0 | 1 | 1 | 2 | 3 | 3 |
            //     +---+---+---+---+---+---+
            //               w
            //
            // Comparing self[r] against self[w-1], this is not a duplicate,
            // so swap self[r] and self[w] and advance r and w:
            //
            //                       r
            //     +---+---+---+---+---+---+
            //     | 0 | 1 | 2 | 1 | 3 | 3 |
            //     +---+---+---+---+---+---+
            //                   w
            //
            // Not a duplicate, repeat:
            //
            //                           r
            //     +---+---+---+---+---+---+
            //     | 0 | 1 | 2 | 3 | 1 | 3 |
            //     +---+---+---+---+---+---+
            //                       w
            //
            // Duplicate, advance r. End of vec. Truncate to w.

            let ln = self.len();
            if ln < 1 { return; }

            // Avoid bounds checks by using unsafe pointers.
            let p = self.as_mut_ptr();
            let mut r = 1;
            let mut w = 1;

            while r < ln {
                let p_r = p.offset(r as isize);
                let p_wm1 = p.offset((w - 1) as isize);
                if *p_r != *p_wm1 {
                    if r != w {
                        let p_w = p_wm1.offset(1);
                        mem::swap(&mut *p_r, &mut *p_w);
                    }
                    w += 1;
                }
                r += 1;
            }

            self.truncate(w);
        }
    }
}*/

////////////////////////////////////////////////////////////////////////////////
// Internal methods and functions
////////////////////////////////////////////////////////////////////////////////

impl<T> Vec<T> {
    /// Reserves capacity for exactly `capacity` elements in the given vector.
    ///
    /// If the capacity for `self` is already equal to or greater than the
    /// requested capacity, then no action is taken.
    fn grow_capacity(&mut self, capacity: usize) {
        if mem::size_of::<T>() == 0 { return }

        if capacity > self.cap {
            let size =  capacity * mem::size_of::<T>();
            //                  .expect("capacity overflow");
            unsafe {
                let ptr = alloc_or_realloc(self.ptr, self.cap * mem::size_of::<T>(), size);
                if ptr.is_null() { ::alloc::oom() }
                self.ptr = ptr;
            }
            self.cap = capacity;
        }
    }
}

// FIXME: #13996: need a way to mark the return value as `noalias`
#[inline(never)]
unsafe fn alloc_or_realloc<T>(ptr: *mut T, old_size: usize, size: usize) -> *mut T {
    if old_size == 0 {
        allocate(size, mem::min_align_of::<T>()) as *mut T
    } else {
        reallocate(ptr as *mut u8, old_size, size, mem::min_align_of::<T>()) as *mut T
    }
}

#[inline]
unsafe fn dealloc<T>(ptr: *mut T, len: usize) {
    if mem::size_of::<T>() != 0 {
        deallocate(ptr as *mut u8,
                   len * mem::size_of::<T>(),
                   mem::min_align_of::<T>())
    }
}

////////////////////////////////////////////////////////////////////////////////
// Common trait implementations for Vec
////////////////////////////////////////////////////////////////////////////////
/*
 * XXX(jtoman): lot's of bullshit
#[unstable]
impl<T:Clone> Clone for Vec<T> {
    fn clone(&self) -> Vec<T> { ::slice::SliceExt::to_vec(self.as_slice()) }

    fn clone_from(&mut self, other: &Vec<T>) {
        // drop anything in self that will not be overwritten
        if self.len() > other.len() {
            self.truncate(other.len())
        }

        // reuse the contained values' allocations/resources.
        for (place, thing) in self.iter_mut().zip(other.iter()) {
            place.clone_from(thing)
        }

        // self.len <= other.len due to the truncate above, so the
        // slice here is always in-bounds.
        let slice = &other[self.len()..];
        self.push_all(slice);
    }
}

impl<S: hash::Writer + hash::Hasher, T: Hash<S>> Hash<S> for Vec<T> {
    #[inline]
    fn hash(&self, state: &mut S) {
        self.as_slice().hash(state);
    }
}

#[stable]
impl<T> Index<usize> for Vec<T> {
    type Output = T;

    #[inline]
    fn index<'a>(&'a self, index: &usize) -> &'a T {
        &self.as_slice()[*index]
    }
}

#[stable]
impl<T> IndexMut<usize> for Vec<T> {
    type Output = T;

    #[inline]
    fn index_mut<'a>(&'a mut self, index: &usize) -> &'a mut T {
        &mut self.as_mut_slice()[*index]
    }
}


#[stable]
impl<T> ops::Index<ops::Range<usize>> for Vec<T> {
    type Output = [T];
    #[inline]
    fn index(&self, index: &ops::Range<usize>) -> &[T] {
        self.as_slice().index(index)
    }
}
#[stable]
impl<T> ops::Index<ops::RangeTo<usize>> for Vec<T> {
    type Output = [T];
    #[inline]
    fn index(&self, index: &ops::RangeTo<usize>) -> &[T] {
        self.as_slice().index(index)
    }
}
#[stable]
impl<T> ops::Index<ops::RangeFrom<usize>> for Vec<T> {
    type Output = [T];
    #[inline]
    fn index(&self, index: &ops::RangeFrom<usize>) -> &[T] {
        self.as_slice().index(index)
    }
}
#[stable]
impl<T> ops::Index<ops::FullRange> for Vec<T> {
    type Output = [T];
    #[inline]
    fn index(&self, _index: &ops::FullRange) -> &[T] {
        self.as_slice()
    }
}

#[stable]
impl<T> ops::IndexMut<ops::Range<usize>> for Vec<T> {
    type Output = [T];
    #[inline]
    fn index_mut(&mut self, index: &ops::Range<usize>) -> &mut [T] {
        self.as_mut_slice().index_mut(index)
    }
}
#[stable]
impl<T> ops::IndexMut<ops::RangeTo<usize>> for Vec<T> {
    type Output = [T];
    #[inline]
    fn index_mut(&mut self, index: &ops::RangeTo<usize>) -> &mut [T] {
        self.as_mut_slice().index_mut(index)
    }
}
#[stable]
impl<T> ops::IndexMut<ops::RangeFrom<usize>> for Vec<T> {
    type Output = [T];
    #[inline]
    fn index_mut(&mut self, index: &ops::RangeFrom<usize>) -> &mut [T] {
        self.as_mut_slice().index_mut(index)
    }
}
#[stable]
impl<T> ops::IndexMut<ops::FullRange> for Vec<T> {
    type Output = [T];
    #[inline]
    fn index_mut(&mut self, _index: &ops::FullRange) -> &mut [T] {
        self.as_mut_slice()
    }
}

#[stable]
impl<T> ops::Deref for Vec<T> {
    type Target = [T];

    fn deref<'a>(&'a self) -> &'a [T] { self.as_slice() }
}

#[stable]
impl<T> ops::DerefMut for Vec<T> {
    fn deref_mut<'a>(&'a mut self) -> &'a mut [T] { self.as_mut_slice() }
}

#[stable]
impl<T> FromIterator<T> for Vec<T> {
    #[inline]
    fn from_iter<I:Iterator<Item=T>>(mut iterator: I) -> Vec<T> {
        let (lower, _) = iterator.size_hint();
        let mut vector = Vec::with_capacity(lower);
        for element in iterator {
            vector.push(element)
        }
        vector
    }
}

#[unstable = "waiting on Extend stability"]
impl<T> Extend<T> for Vec<T> {
    #[inline]
    fn extend<I: Iterator<Item=T>>(&mut self, mut iterator: I) {
        let (lower, _) = iterator.size_hint();
        self.reserve(lower);
        for element in iterator {
            self.push(element)
        }
    }
}

impl<A, B> PartialEq<Vec<B>> for Vec<A> where A: PartialEq<B> {
    #[inline]
    fn eq(&self, other: &Vec<B>) -> bool { PartialEq::eq(&**self, &**other) }
    #[inline]
    fn ne(&self, other: &Vec<B>) -> bool { PartialEq::ne(&**self, &**other) }
}

macro_rules! impl_eq {
    ($lhs:ty, $rhs:ty) => {
        impl<'b, A, B> PartialEq<$rhs> for $lhs where A: PartialEq<B> {
            #[inline]
            fn eq(&self, other: &$rhs) -> bool { PartialEq::eq(&**self, &**other) }
            #[inline]
            fn ne(&self, other: &$rhs) -> bool { PartialEq::ne(&**self, &**other) }
        }

        impl<'b, A, B> PartialEq<$lhs> for $rhs where B: PartialEq<A> {
            #[inline]
            fn eq(&self, other: &$lhs) -> bool { PartialEq::eq(&**self, &**other) }
            #[inline]
            fn ne(&self, other: &$lhs) -> bool { PartialEq::ne(&**self, &**other) }
        }
    }
}

impl_eq! { Vec<A>, &'b [B] }
impl_eq! { Vec<A>, &'b mut [B] }

impl<'a, A, B> PartialEq<Vec<B>> for CowVec<'a, A> where A: PartialEq<B> + Clone {
    #[inline]
    fn eq(&self, other: &Vec<B>) -> bool { PartialEq::eq(&**self, &**other) }
    #[inline]
    fn ne(&self, other: &Vec<B>) -> bool { PartialEq::ne(&**self, &**other) }
}

impl<'a, A, B> PartialEq<CowVec<'a, A>> for Vec<B> where A: Clone, B: PartialEq<A> {
    #[inline]
    fn eq(&self, other: &CowVec<'a, A>) -> bool { PartialEq::eq(&**self, &**other) }
    #[inline]
    fn ne(&self, other: &CowVec<'a, A>) -> bool { PartialEq::ne(&**self, &**other) }
}

macro_rules! impl_eq_for_cowvec {
    ($rhs:ty) => {
        impl<'a, 'b, A, B> PartialEq<$rhs> for CowVec<'a, A> where A: PartialEq<B> + Clone {
            #[inline]
            fn eq(&self, other: &$rhs) -> bool { PartialEq::eq(&**self, &**other) }
            #[inline]
            fn ne(&self, other: &$rhs) -> bool { PartialEq::ne(&**self, &**other) }
        }

        impl<'a, 'b, A, B> PartialEq<CowVec<'a, A>> for $rhs where A: Clone, B: PartialEq<A> {
            #[inline]
            fn eq(&self, other: &CowVec<'a, A>) -> bool { PartialEq::eq(&**self, &**other) }
            #[inline]
            fn ne(&self, other: &CowVec<'a, A>) -> bool { PartialEq::ne(&**self, &**other) }
        }
    }
}

impl_eq_for_cowvec! { &'b [B] }
impl_eq_for_cowvec! { &'b mut [B] }

#[unstable = "waiting on PartialOrd stability"]
impl<T: PartialOrd> PartialOrd for Vec<T> {
    #[inline]
    fn partial_cmp(&self, other: &Vec<T>) -> Option<Ordering> {
        self.as_slice().partial_cmp(other.as_slice())
    }
}

#[unstable = "waiting on Eq stability"]
impl<T: Eq> Eq for Vec<T> {}

#[unstable = "waiting on Ord stability"]
impl<T: Ord> Ord for Vec<T> {
    #[inline]
    fn cmp(&self, other: &Vec<T>) -> Ordering {
        self.as_slice().cmp(other.as_slice())
    }
}
*/
impl<T> AsSlice<T> for Vec<T> {
    #[inline]
    #[stable]
    fn as_slice<'a>(&'a self) -> &'a [T] {
        unsafe {
            mem::transmute(RawSlice {
                data: self.ptr,
                len: self.len
            })
        }
    }
}

pub fn crust_init() -> (Vec<u32>,) {
    (Vec::with_capacity(10),)
}
/*
#[unstable = "recent addition, needs more experience"]
impl<'a, T: Clone> Add<&'a [T]> for Vec<T> {
    type Output = Vec<T>;

    #[inline]
    fn add(mut self, rhs: &[T]) -> Vec<T> {
        self.push_all(rhs);
        self
    }
}

#[unsafe_destructor]
#[stable]
impl<T> Drop for Vec<T> {
    fn drop(&mut self) {
        // This is (and should always remain) a no-op if the fields are
        // zeroed (when moving out, because of #[unsafe_no_drop_flag]).
        if self.cap != 0 {
            unsafe {
                for x in self.iter() {
                    ptr::read(x);
                }
                dealloc(*self.ptr, self.cap)
            }
        }
    }
}

#[stable]
impl<T> Default for Vec<T> {
    #[stable]
    fn default() -> Vec<T> {
        Vec::new()
    }
}

#[stable]
impl<T: fmt::Debug> fmt::Debug for Vec<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self.as_slice(), f)
    }
}

impl<'a> fmt::Writer for Vec<u8> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.push_all(s.as_bytes());
        Ok(())
    }
}

////////////////////////////////////////////////////////////////////////////////
// Clone-on-write
////////////////////////////////////////////////////////////////////////////////

#[unstable = "unclear how valuable this alias is"]
/// A clone-on-write vector
pub type CowVec<'a, T> = Cow<'a, Vec<T>, [T]>;

#[unstable]
impl<'a, T> FromIterator<T> for CowVec<'a, T> where T: Clone {
    fn from_iter<I: Iterator<Item=T>>(it: I) -> CowVec<'a, T> {
        Cow::Owned(FromIterator::from_iter(it))
    }
}

impl<'a, T: 'a> IntoCow<'a, Vec<T>, [T]> for Vec<T> where T: Clone {
    fn into_cow(self) -> CowVec<'a, T> {
        Cow::Owned(self)
    }
}

impl<'a, T> IntoCow<'a, Vec<T>, [T]> for &'a [T] where T: Clone {
    fn into_cow(self) -> CowVec<'a, T> {
        Cow::Borrowed(self)
    }
}

////////////////////////////////////////////////////////////////////////////////
// Iterators
////////////////////////////////////////////////////////////////////////////////

/// An iterator that moves out of a vector.
#[stable]
pub struct IntoIter<T> {
    allocation: *mut T, // the block of memory allocated for the vector
    cap: usize, // the capacity of the vector
    ptr: *const T,
    end: *const T
}

unsafe impl<T: Send> Send for IntoIter<T> { }
unsafe impl<T: Sync> Sync for IntoIter<T> { }

impl<T> IntoIter<T> {
    #[inline]
    /// Drops all items that have not yet been moved and returns the empty vector.
    #[unstable]
    pub fn into_inner(mut self) -> Vec<T> {
        unsafe {
            for _x in self { }
            let IntoIter { allocation, cap, ptr: _ptr, end: _end } = self;
            mem::forget(self);
            Vec { ptr: NonZero::new(allocation), cap: cap, len: 0 }
        }
    }
}

#[stable]
impl<T> Iterator for IntoIter<T> {
    type Item = T;

    #[inline]
    fn next<'a>(&'a mut self) -> Option<T> {
        unsafe {
            if self.ptr == self.end {
                None
            } else {
                if mem::size_of::<T>() == 0 {
                    // purposefully don't use 'ptr.offset' because for
                    // vectors with 0-size elements this would return the
                    // same pointer.
                    self.ptr = mem::transmute(self.ptr as usize + 1);

                    // Use a non-null pointer value
                    Some(ptr::read(mem::transmute(1u)))
                } else {
                    let old = self.ptr;
                    self.ptr = self.ptr.offset(1);

                    Some(ptr::read(old))
                }
            }
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let diff = (self.end as usize) - (self.ptr as usize);
        let size = mem::size_of::<T>();
        let exact = diff / (if size == 0 {1} else {size});
        (exact, Some(exact))
    }
}

#[stable]
impl<T> DoubleEndedIterator for IntoIter<T> {
    #[inline]
    fn next_back<'a>(&'a mut self) -> Option<T> {
        unsafe {
            if self.end == self.ptr {
                None
            } else {
                if mem::size_of::<T>() == 0 {
                    // See above for why 'ptr.offset' isn't used
                    self.end = mem::transmute(self.end as usize - 1);

                    // Use a non-null pointer value
                    Some(ptr::read(mem::transmute(1u)))
                } else {
                    self.end = self.end.offset(-1);

                    Some(ptr::read(mem::transmute(self.end)))
                }
            }
        }
    }
}

#[stable]
impl<T> ExactSizeIterator for IntoIter<T> {}

#[unsafe_destructor]
#[stable]
impl<T> Drop for IntoIter<T> {
    fn drop(&mut self) {
        // destroy the remaining elements
        if self.cap != 0 {
            for _x in *self {}
            unsafe {
                dealloc(self.allocation, self.cap);
            }
        }
    }
}

/// An iterator that drains a vector.
#[unsafe_no_drop_flag]
#[unstable = "recently added as part of collections reform 2"]
pub struct Drain<'a, T> {
    ptr: *const T,
    end: *const T,
    marker: ContravariantLifetime<'a>,
}

#[stable]
impl<'a, T> Iterator for Drain<'a, T> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<T> {
        unsafe {
            if self.ptr == self.end {
                None
            } else {
                if mem::size_of::<T>() == 0 {
                    // purposefully don't use 'ptr.offset' because for
                    // vectors with 0-size elements this would return the
                    // same pointer.
                    self.ptr = mem::transmute(self.ptr as usize + 1);

                    // Use a non-null pointer value
                    Some(ptr::read(mem::transmute(1u)))
                } else {
                    let old = self.ptr;
                    self.ptr = self.ptr.offset(1);

                    Some(ptr::read(old))
                }
            }
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let diff = (self.end as usize) - (self.ptr as usize);
        let size = mem::size_of::<T>();
        let exact = diff / (if size == 0 {1} else {size});
        (exact, Some(exact))
    }
}

#[stable]
impl<'a, T> DoubleEndedIterator for Drain<'a, T> {
    #[inline]
    fn next_back(&mut self) -> Option<T> {
        unsafe {
            if self.end == self.ptr {
                None
            } else {
                if mem::size_of::<T>() == 0 {
                    // See above for why 'ptr.offset' isn't used
                    self.end = mem::transmute(self.end as usize - 1);

                    // Use a non-null pointer value
                    Some(ptr::read(mem::transmute(1u)))
                } else {
                    self.end = self.end.offset(-1);

                    Some(ptr::read(self.end))
                }
            }
        }
    }
}

#[stable]
impl<'a, T> ExactSizeIterator for Drain<'a, T> {}

#[unsafe_destructor]
#[stable]
impl<'a, T> Drop for Drain<'a, T> {
    fn drop(&mut self) {
        // self.ptr == self.end == null if drop has already been called,
        // so we can use #[unsafe_no_drop_flag].

        // destroy the remaining elements
        for _x in *self {}
    }
}

////////////////////////////////////////////////////////////////////////////////
// Conversion from &[T] to &Vec<T>
////////////////////////////////////////////////////////////////////////////////

/// Wrapper type providing a `&Vec<T>` reference via `Deref`.
#[unstable]
pub struct DerefVec<'a, T> {
    x: Vec<T>,
    l: ContravariantLifetime<'a>
}

#[unstable]
impl<'a, T> Deref for DerefVec<'a, T> {
    type Target = Vec<T>;

    fn deref<'b>(&'b self) -> &'b Vec<T> {
        &self.x
    }
}

// Prevent the inner `Vec<T>` from attempting to deallocate memory.
#[unsafe_destructor]
#[stable]
impl<'a, T> Drop for DerefVec<'a, T> {
    fn drop(&mut self) {
        self.x.len = 0;
        self.x.cap = 0;
    }
}

/// Convert a slice to a wrapper type providing a `&Vec<T>` reference.
#[unstable]
pub fn as_vec<'a, T>(x: &'a [T]) -> DerefVec<'a, T> {
    unsafe {
        DerefVec {
            x: Vec::from_raw_parts(x.as_ptr() as *mut T, x.len(), x.len()),
            l: ContravariantLifetime::<'a>
        }
    }
}
*/
