#define signed_result(width) __rust_tuple_i##width##_bool
#define unsigned_result(width) __rust_tuple_u##width##_bool

#define __crust_sint(width) rs_i##width
#define __crust_uint(width) rs_u##width

#define __crust_ctlz_op(width) core$intrinsics$ctlz##width

#define CTLZ(width)														\
  __crust_uint(width) __crust_ctlz_op(width)(__crust_uint(width) x) {	\
	__crust_uint(width) y;												\
	__crust_uint(width) n = width;										\
	__crust_uint(width) c = width / 2;									\
	do {																\
	  y = x >> c; if (y != 0) { n = n - c; x = y; }						\
	  c = c >> 1;														\
	} while(c != 0);													\
	return n - x;														\
  }

																
															  



#define __crust_signed_fail(width) \
  {								   \
	signed_result(width) to_ret;   \
	to_ret.field1 = 1;			   \
	return to_ret;				   \
  }

#define __crust_signed_success(width, val) {	\
  signed_result(width) to_ret;					\
  to_ret.field0 = val;							\
  to_ret.field1 = 0;							\
  return to_ret;								\
  }

#define __crust_unsigned_fail(width)			\
  { unsigned_result(width) to_ret; to_ret.field1 = 1; return to_ret; }

#define __crust_unsigned_success(width, val)							\
  {																		\
	unsigned_result(width) to_ret;										\
	to_ret.field0 = val;												\
	to_ret.field1 = 0;													\
	return to_ret;														\
  }

/* These defintions are adapted from: 
 * https://www.securecoding.cert.org/confluence/display/seccode/INT32-C.+Ensure+that+operations+on+signed+integers+do+not+result+in+overflow?showComments=false
 */

#define __crust_intr_op(pref, width, op_name) core$intrinsics$##pref##width##_##op_name##_with_overflow

#define __crust_sub_op(pref, width) __crust_intr_op(pref, width, sub)
#define __crust_add_op(pref, width) __crust_intr_op(pref, width, add)
#define __crust_mul_op(pref, width) __crust_intr_op(pref, width, mul)

// Signed primitive macros

#define SIGNED_ADD(width)										\
  signed_result(width) __crust_add_op(i, width)(__crust_sint(width) a, __crust_sint(width) b) { \
	if(((b > 0) && (a > (INT##width##_MAX - b))) || ((b < 0) && (a < (INT##width##_MIN - b)))) { \
	  __crust_signed_fail(width);										\
	} else {															\
	  __crust_signed_success(width, (a + b));							\
	}																	\
  }

// yikes
#define SIGNED_MUL(width)												\
  signed_result(width) __crust_mul_op(i, width)(__crust_sint(width) si_a, __crust_sint(width) si_b) { \
	if (si_a > 0) {														\
	  if (si_b > 0) {													\
		if (si_a > (INT##width##_MAX / si_b)) {							\
		  __crust_signed_fail(width);									\
		}																\
	  } else {															\
		if (si_b < (INT##width##_MIN / si_a)) {							\
		  __crust_signed_fail(width);									\
		}																\
	  }																	\
	} else {															\
	  if (si_b > 0) {													\
		if (si_a < (INT##width##_MIN / si_b)) {							\
		  __crust_signed_fail(width);									\
		}																\
	  } else {															\
		if ( (si_a != 0) && (si_b < (INT##width##_MAX / si_a))) {		\
		  __crust_signed_fail(width);									\
		}																\
	  }																	\
	}																	\
	__crust_signed_success(width, (a * b));								\
  }																		\
  
#define SIGNED_SUB(width)						\
  signed_result(width) __crust_sub_op(i, width)(__crust_sint(width) si_a, __crust_sint(width) si_b) { \
	if ((si_b > 0 && si_a < INT_MIN + si_b) ||							\
		(si_b < 0 && si_a > INT_MAX + si_b)) {							\
	  __crust_signed_fail(width);										\
	} else {															\
	  __crust_signed_success(width, (si_a - si_b));						\
	}																	\
  }
  
/*
 * These definitions adapted (macro-ified) from:
 * https://www.securecoding.cert.org/confluence/display/seccode/INT30-C.+Ensure+that+unsigned+integer+operations+do+not+wrap
 */

// unsigned primitive macros
#define UNSIGNED_ADD(width)						\
  unsigned_result(width) __crust_add_op(u, width)(__crust_uint(width) ui_a, __crust_uint(width) ui_b) { \
	if(UINT##width##_MAX - ui_a < ui_b) {								\
	  __crust_unsigned_fail(width);										\
	} else {															\
	  __crust_unsigned_success(width, (ui_a + ui_b));					\
	}																	\
  }

#define UNSIGNED_SUB(width)						\
  unsigned_result(width) __crust_sub_op(u, width)(__crust_uint(width) ui_a, __crust_uint(width) ui_b) { \
	if(ui_a < ui_b) {													\
	  __crust_unsigned_fail(width);										\
	} else {															\
	  __crust_unsigned_success(width, (ui_a - ui_b));					\
	}																	\
  }

#define UNSIGNED_MUL(width)						\
  unsigned_result(width) __crust_mul_op(u, width)(__crust_uint(width) ui_a, __crust_uint(width) ui_b) { \
	if(ui_a > UINT##width##_MAX / ui_b) {								\
	  __crust_unsigned_fail(width);										\
	} else {															\
	  __crust_unsigned_success(width, (ui_a + ui_b));					\
	}																	\
  }
