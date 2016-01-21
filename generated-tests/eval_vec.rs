#![crate_type = "lib"]
#![no_std]
#![feature(unsafe_destructor, no_std)]
#![feature(core, alloc, collections)]
extern crate core;
extern crate alloc;
extern crate collections;
extern crate __crust;
extern crate __crust2;


// __crust_test_0() { { let mut v0 = collections::vec::Vec::<()>::new();  let mut v1 = <collections::vec::Vec<()> as core::ops::Drop>::drop((&mut v0));  let mut driver_out = v1;  {  () };  () }; }



// __crust_test_1() { { let mut v0 = collections::vec::Vec::<u8>::new();  let mut v1 = <collections::vec::Vec<u8> as core::ops::Drop>::drop((&mut v0));  let mut driver_out = v1;  {  () };  () }; }



fn __crust_test_2() { { let mut v0 = collections::vec::Vec::<()>::new();  let mut v1 = collections::vec::Vec::<()>::into_iter(v0);  let mut driver_out = v1;  { let mut vr_allocation_0 = (&(&driver_out).allocation);  let mut vr_cap_1 = (&(&driver_out).cap);  let mut vr_ptr_2 = (&(&driver_out).ptr);  let mut vr_end_3 = (&(&driver_out).end);  {  () } };  () }; }



fn __crust_test_3() { { let mut v0 = collections::vec::Vec::<u8>::new();  let mut v1 = collections::vec::Vec::<u8>::into_iter(v0);  let mut driver_out = v1;  { let mut vr_allocation_0 = (&(&driver_out).allocation);  let mut vr_cap_1 = (&(&driver_out).cap);  let mut vr_ptr_2 = (&(&driver_out).ptr);  let mut vr_end_3 = (&(&driver_out).end);  {  () } };  () }; }



fn __crust_test_4() { { let mut v0 = collections::vec::Vec::<()>::new();  let mut v1 = collections::vec::Vec::<()>::truncate((&mut v0), __crust::nondet::<usize>());  let mut driver_out = v1;  {  () };  () }; }



fn __crust_test_5() { { let mut v0 = collections::vec::Vec::<u8>::new();  let mut v1 = collections::vec::Vec::<u8>::truncate((&mut v0), __crust::nondet::<usize>());  let mut driver_out = v1;  {  () };  () }; }



fn __crust_test_6() { { let mut v0 = collections::vec::Vec::<u8>::new();  let mut v1 = collections::vec::Vec::<u8>::shrink_to_fit((&mut v0));  let mut driver_out = v1;  {  () };  () }; }



fn __crust_test_7() { { let mut v0 = collections::vec::Vec::<()>::new();  let mut v1 = collections::vec::Vec::<()>::shrink_to_fit((&mut v0));  let mut driver_out = v1;  {  () };  () }; }



fn __crust_test_8() { { let mut v0 = collections::vec::Vec::<()>::new();  let mut v1 = collections::vec::Vec::<()>::pop((&mut v0));  let mut driver_out = v1;  match driver_out{ core::option::Option::None => {  () }, core::option::Option::Some(ref vr_core_option_Some_0) => {  () },};  () }; }



fn __crust_test_9() { { let mut v0 = collections::vec::Vec::<u8>::new();  let mut v1 = collections::vec::Vec::<u8>::pop((&mut v0));  let mut driver_out = v1;  match driver_out{ core::option::Option::None => {  () }, core::option::Option::Some(ref vr_core_option_Some_0) => {  () },};  () }; }



fn __crust_test_10() { { let mut v0 = collections::vec::Vec::<()>::new();  let mut v1 = collections::vec::Vec::<()>::drain((&mut v0));  let mut driver_out = v1;  { let mut vr_ptr_0 = (&(&driver_out).ptr);  let mut vr_end_1 = (&(&driver_out).end);  let mut vr_marker_2 = (&(&driver_out).marker);  {  () } };  () }; }



fn __crust_test_11() { { let mut v0 = collections::vec::Vec::<u8>::new();  let mut v1 = collections::vec::Vec::<u8>::drain((&mut v0));  let mut driver_out = v1;  { let mut vr_ptr_0 = (&(&driver_out).ptr);  let mut vr_end_1 = (&(&driver_out).end);  let mut vr_marker_2 = (&(&driver_out).marker);  {  () } };  () }; }



fn __crust_test_12() { { let mut v0 = collections::vec::Vec::<u8>::new();  let mut v1 = <collections::vec::Vec<u8> as core::ops::DerefMut>::deref_mut((&mut v0));  let mut driver_out = v1;  { __crust2::assert_not_null::<_>((&driver_out));  () };  () }; }



fn __crust_test_13() { { let mut v0 = collections::vec::Vec::<()>::new();  let mut v1 = <collections::vec::Vec<()> as core::ops::DerefMut>::deref_mut((&mut v0));  let mut driver_out = v1;  { __crust2::assert_not_null::<_>((&driver_out));  () };  () }; }



fn __crust_test_14() { { let mut v0 = collections::vec::Vec::<u8>::new();  let mut v1 = <collections::vec::Vec<u8> as core::ops::Deref>::deref((&v0));  let mut driver_out = v1;  { __crust2::assert_not_null::<_>((&driver_out));  () };  () }; }



fn __crust_test_15() { { let mut v0 = collections::vec::Vec::<()>::new();  let mut v1 = <collections::vec::Vec<()> as core::ops::Deref>::deref((&v0));  let mut driver_out = v1;  { __crust2::assert_not_null::<_>((&driver_out));  () };  () }; }



fn __crust_test_16() { { let mut v0 = collections::vec::Vec::<()>::new();  let mut v1 = collections::vec::Vec::<()>::dedup((&mut v0));  let mut driver_out = v1;  {  () };  () }; }



fn __crust_test_17() { { let mut v0 = collections::vec::Vec::<u8>::new();  let mut v1 = collections::vec::Vec::<u8>::dedup((&mut v0));  let mut driver_out = v1;  {  () };  () }; }



fn __crust_test_18() { { let mut v0 = collections::vec::Vec::<()>::new();  let mut driver_out = v0;  { let mut vr_ptr_0 = (&(&driver_out).ptr);  let mut vr_len_1 = (&(&driver_out).len);  let mut vr_cap_2 = (&(&driver_out).cap);  {  () } };  () }; }



fn __crust_test_19() { { let mut v0 = collections::vec::Vec::<u8>::new();  let mut driver_out = v0;  { let mut vr_ptr_0 = (&(&driver_out).ptr);  let mut vr_len_1 = (&(&driver_out).len);  let mut vr_cap_2 = (&(&driver_out).cap);  {  () } };  () }; }



fn __crust_test_20() { { let mut v0 = collections::vec::Vec::<u8>::new();  let mut v1 = <collections::vec::Vec<u8> as core::ops::Deref>::deref((&v0));  let mut v2 = collections::vec::Vec::<()>::new();  let mut v3 = <collections::vec::Vec<()> as core::ops::DerefMut>::deref_mut((&mut v2));  let mut driver_out = (v1, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



// __crust_test_21() { { let mut v1 = collections::vec::Vec::<u8>::new();  let mut copy0 = v1;  let mut v0 = copy0;  let mut v2 = <collections::vec::Vec<u8> as core::ops::Deref>::deref((&v0));  let mut v3 = copy0;  let mut v4 = <collections::vec::Vec<u8> as core::ops::DerefMut>::deref_mut((&mut v3));  let mut driver_out = (v2, v4);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



fn __crust_test_22() { { let mut v0 = collections::vec::Vec::<u8>::new();  let mut v1 = <collections::vec::Vec<u8> as core::ops::Deref>::deref((&v0));  let mut v2 = collections::vec::Vec::<u8>::new();  let mut v3 = <collections::vec::Vec<u8> as core::ops::DerefMut>::deref_mut((&mut v2));  let mut driver_out = (v1, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



fn __crust_test_23() { { let mut v0 = collections::vec::Vec::<()>::new();  let mut v1 = <collections::vec::Vec<()> as core::ops::Deref>::deref((&v0));  let mut v2 = collections::vec::Vec::<()>::new();  let mut v3 = <collections::vec::Vec<()> as core::ops::Deref>::deref((&v2));  let mut driver_out = (v1, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



fn __crust_test_24() { { let mut v1 = collections::vec::Vec::<()>::new();  let mut copy0 = (&v1);  let mut v0 = copy0;  let mut v2 = <collections::vec::Vec<()> as core::ops::Deref>::deref(v0);  let mut v3 = copy0;  let mut v4 = <collections::vec::Vec<()> as core::ops::Deref>::deref(v3);  let mut driver_out = (v2, v4);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



fn __crust_test_25() { { let mut v1 = collections::vec::Vec::<()>::new();  let mut v2 = <collections::vec::Vec<()> as core::ops::Deref>::deref((&v1));  let mut copy0 = v2;  let mut v0 = copy0;  let mut v3 = copy0;  let mut driver_out = (v0, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



// __crust_test_26() { { let mut v1 = collections::vec::Vec::<()>::new();  let mut copy0 = v1;  let mut v0 = copy0;  let mut v2 = <collections::vec::Vec<()> as core::ops::Deref>::deref((&v0));  let mut v3 = copy0;  let mut v4 = <collections::vec::Vec<()> as core::ops::Deref>::deref((&v3));  let mut driver_out = (v2, v4);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



fn __crust_test_27() { { let mut v0 = collections::vec::Vec::<()>::new();  let mut v1 = <collections::vec::Vec<()> as core::ops::Deref>::deref((&v0));  let mut v2 = collections::vec::Vec::<u8>::new();  let mut v3 = <collections::vec::Vec<u8> as core::ops::Deref>::deref((&v2));  let mut driver_out = (v1, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



fn __crust_test_28() { { let mut v0 = collections::vec::Vec::<u8>::new();  let mut v1 = <collections::vec::Vec<u8> as core::ops::Deref>::deref((&v0));  let mut v2 = collections::vec::Vec::<()>::new();  let mut v3 = <collections::vec::Vec<()> as core::ops::Deref>::deref((&v2));  let mut driver_out = (v1, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



fn __crust_test_29() { { let mut v1 = collections::vec::Vec::<u8>::new();  let mut copy0 = (&v1);  let mut v0 = copy0;  let mut v2 = <collections::vec::Vec<u8> as core::ops::Deref>::deref(v0);  let mut v3 = copy0;  let mut v4 = <collections::vec::Vec<u8> as core::ops::Deref>::deref(v3);  let mut driver_out = (v2, v4);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



fn __crust_test_30() { { let mut v1 = collections::vec::Vec::<u8>::new();  let mut v2 = <collections::vec::Vec<u8> as core::ops::Deref>::deref((&v1));  let mut copy0 = v2;  let mut v0 = copy0;  let mut v3 = copy0;  let mut driver_out = (v0, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



fn __crust_test_31() { { let mut v0 = collections::vec::Vec::<u8>::new();  let mut v1 = <collections::vec::Vec<u8> as core::ops::Deref>::deref((&v0));  let mut v2 = collections::vec::Vec::<u8>::new();  let mut v3 = <collections::vec::Vec<u8> as core::ops::Deref>::deref((&v2));  let mut driver_out = (v1, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



// __crust_test_32() { { let mut v1 = collections::vec::Vec::<u8>::new();  let mut copy0 = v1;  let mut v0 = copy0;  let mut v2 = <collections::vec::Vec<u8> as core::ops::Deref>::deref((&v0));  let mut v3 = copy0;  let mut v4 = <collections::vec::Vec<u8> as core::ops::Deref>::deref((&v3));  let mut driver_out = (v2, v4);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



fn __crust_test_33() { { let mut v0 = collections::vec::Vec::<()>::new();  let mut v1 = <collections::vec::Vec<()> as core::ops::Deref>::deref((&v0));  let mut v2 = collections::vec::Vec::<u8>::new();  let mut v3 = <collections::vec::Vec<u8> as core::ops::DerefMut>::deref_mut((&mut v2));  let mut driver_out = (v1, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



fn __crust_test_34() { { let mut v0 = collections::vec::Vec::<()>::new();  let mut v1 = <collections::vec::Vec<()> as core::ops::Deref>::deref((&v0));  let mut v2 = collections::vec::Vec::<()>::new();  let mut v3 = <collections::vec::Vec<()> as core::ops::DerefMut>::deref_mut((&mut v2));  let mut driver_out = (v1, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



// __crust_test_35() { { let mut v1 = collections::vec::Vec::<()>::new();  let mut copy0 = v1;  let mut v0 = copy0;  let mut v2 = <collections::vec::Vec<()> as core::ops::Deref>::deref((&v0));  let mut v3 = copy0;  let mut v4 = <collections::vec::Vec<()> as core::ops::DerefMut>::deref_mut((&mut v3));  let mut driver_out = (v2, v4);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



fn __crust_test_36() { { let mut v0 = collections::vec::Vec::<()>::new();  let mut v1 = <collections::vec::Vec<()> as core::ops::DerefMut>::deref_mut((&mut v0));  let mut v2 = collections::vec::Vec::<u8>::new();  let mut v3 = <collections::vec::Vec<u8> as core::ops::Deref>::deref((&v2));  let mut driver_out = (v1, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



fn __crust_test_37() { { let mut v0 = collections::vec::Vec::<()>::new();  let mut v1 = <collections::vec::Vec<()> as core::ops::DerefMut>::deref_mut((&mut v0));  let mut v2 = collections::vec::Vec::<()>::new();  let mut v3 = <collections::vec::Vec<()> as core::ops::Deref>::deref((&v2));  let mut driver_out = (v1, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



// __crust_test_38() { { let mut v1 = collections::vec::Vec::<()>::new();  let mut copy0 = v1;  let mut v0 = copy0;  let mut v2 = <collections::vec::Vec<()> as core::ops::DerefMut>::deref_mut((&mut v0));  let mut v3 = copy0;  let mut v4 = <collections::vec::Vec<()> as core::ops::Deref>::deref((&v3));  let mut driver_out = (v2, v4);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



// __crust_test_39() { { let mut v1 = collections::vec::Vec::<u8>::new();  let mut copy0 = v1;  let mut v0 = copy0;  let mut v2 = <collections::vec::Vec<u8> as core::ops::DerefMut>::deref_mut((&mut v0));  let mut v3 = copy0;  let mut v4 = <collections::vec::Vec<u8> as core::ops::Deref>::deref((&v3));  let mut driver_out = (v2, v4);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



fn __crust_test_40() { { let mut v0 = collections::vec::Vec::<u8>::new();  let mut v1 = <collections::vec::Vec<u8> as core::ops::DerefMut>::deref_mut((&mut v0));  let mut v2 = collections::vec::Vec::<u8>::new();  let mut v3 = <collections::vec::Vec<u8> as core::ops::Deref>::deref((&v2));  let mut driver_out = (v1, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



fn __crust_test_41() { { let mut v0 = collections::vec::Vec::<u8>::new();  let mut v1 = <collections::vec::Vec<u8> as core::ops::DerefMut>::deref_mut((&mut v0));  let mut v2 = collections::vec::Vec::<()>::new();  let mut v3 = <collections::vec::Vec<()> as core::ops::Deref>::deref((&v2));  let mut driver_out = (v1, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



fn __crust_test_42() { { let mut v0 = collections::vec::Vec::<()>::new();  let mut v1 = <collections::vec::Vec<()> as core::ops::DerefMut>::deref_mut((&mut v0));  let mut v2 = collections::vec::Vec::<u8>::new();  let mut v3 = <collections::vec::Vec<u8> as core::ops::DerefMut>::deref_mut((&mut v2));  let mut driver_out = (v1, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



// __crust_test_43() { { let mut v1 = collections::vec::Vec::<()>::new();  let mut v2 = <collections::vec::Vec<()> as core::ops::DerefMut>::deref_mut((&mut v1));  let mut copy0 = v2;  let mut v0 = copy0;  let mut v3 = copy0;  let mut driver_out = (v0, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



// __crust_test_44() { { let mut v1 = collections::vec::Vec::<()>::new();  let mut copy0 = v1;  let mut v0 = copy0;  let mut v2 = <collections::vec::Vec<()> as core::ops::DerefMut>::deref_mut((&mut v0));  let mut v3 = copy0;  let mut v4 = <collections::vec::Vec<()> as core::ops::DerefMut>::deref_mut((&mut v3));  let mut driver_out = (v2, v4);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



fn __crust_test_45() { { let mut v0 = collections::vec::Vec::<()>::new();  let mut v1 = <collections::vec::Vec<()> as core::ops::DerefMut>::deref_mut((&mut v0));  let mut v2 = collections::vec::Vec::<()>::new();  let mut v3 = <collections::vec::Vec<()> as core::ops::DerefMut>::deref_mut((&mut v2));  let mut driver_out = (v1, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



// __crust_test_46() { { let mut v1 = collections::vec::Vec::<()>::new();  let mut copy0 = (&mut v1);  let mut v0 = copy0;  let mut v2 = <collections::vec::Vec<()> as core::ops::DerefMut>::deref_mut(v0);  let mut v3 = copy0;  let mut v4 = <collections::vec::Vec<()> as core::ops::DerefMut>::deref_mut(v3);  let mut driver_out = (v2, v4);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



// __crust_test_47() { { let mut v1 = collections::vec::Vec::<u8>::new();  let mut v2 = <collections::vec::Vec<u8> as core::ops::DerefMut>::deref_mut((&mut v1));  let mut copy0 = v2;  let mut v0 = copy0;  let mut v3 = copy0;  let mut driver_out = (v0, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



// __crust_test_48() { { let mut v1 = collections::vec::Vec::<u8>::new();  let mut copy0 = (&mut v1);  let mut v0 = copy0;  let mut v2 = <collections::vec::Vec<u8> as core::ops::DerefMut>::deref_mut(v0);  let mut v3 = copy0;  let mut v4 = <collections::vec::Vec<u8> as core::ops::DerefMut>::deref_mut(v3);  let mut driver_out = (v2, v4);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



fn __crust_test_49() { { let mut v0 = collections::vec::Vec::<u8>::new();  let mut v1 = <collections::vec::Vec<u8> as core::ops::DerefMut>::deref_mut((&mut v0));  let mut v2 = collections::vec::Vec::<u8>::new();  let mut v3 = <collections::vec::Vec<u8> as core::ops::DerefMut>::deref_mut((&mut v2));  let mut driver_out = (v1, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



// __crust_test_50() { { let mut v1 = collections::vec::Vec::<u8>::new();  let mut copy0 = v1;  let mut v0 = copy0;  let mut v2 = <collections::vec::Vec<u8> as core::ops::DerefMut>::deref_mut((&mut v0));  let mut v3 = copy0;  let mut v4 = <collections::vec::Vec<u8> as core::ops::DerefMut>::deref_mut((&mut v3));  let mut driver_out = (v2, v4);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



fn __crust_test_51() { { let mut v0 = collections::vec::Vec::<u8>::new();  let mut v1 = <collections::vec::Vec<u8> as core::ops::DerefMut>::deref_mut((&mut v0));  let mut v2 = collections::vec::Vec::<()>::new();  let mut v3 = <collections::vec::Vec<()> as core::ops::DerefMut>::deref_mut((&mut v2));  let mut driver_out = (v1, v3);  { let mut vr_tuple0_0 = (&(&driver_out).0);  let mut vr_tuple1_1 = (&(&driver_out).1);  {  () } };  () }; }



