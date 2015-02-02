#![crate_type = "lib"]
#![no_std]
extern crate core;
use core::prelude::Option;


pub trait Int {
    fn zero() -> Self;
    fn one() -> Self;
    fn min_value() -> Self;
    fn max_value() -> Self;
    fn count_ones(self) -> usize;
    fn leading_zeros(self) -> usize;
    fn trailing_zeros(self) -> usize;
    fn rotate_left(self, n: usize) -> Self;
    fn rotate_right(self, n: usize) -> Self;
    fn swap_bytes(self) -> Self;
    fn checked_add(self, other: Self) -> Option<Self>;
    fn checked_sub(self, other: Self) -> Option<Self>;
    fn checked_mul(self, other: Self) -> Option<Self>;
    fn checked_div(self, other: Self) -> Option<Self>;

    fn count_zeros(self) -> usize;
    fn from_be(x: Self) -> Self;
    fn from_le(x: Self) -> Self;
    fn to_be(self) -> Self;
    fn to_le(self) -> Self;
    fn saturating_add(self, other: Self) -> Self;
    fn saturating_sub(self, other: Self) -> Self;
    fn pow(self, exp: usize) -> Self;
}
