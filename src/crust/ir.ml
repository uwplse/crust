type r_type = Types.r_type
type bin_op = [
	| `BiAdd
	| `BiSub
	| `BiMul
	| `BiDiv
	| `BiRem
	| `BiAnd
	| `BiOr
	| `BiBitXor
	| `BiBitAnd
	| `BiBitOr
	| `BiShl
	| `BiShr
	| `BiEq
	| `BiLt
	| `BiLe
	| `BiNe
	| `BiGe
	| `BiGt
  ]
type un_op = [
	| `UnNot
	| `UnNeg
	| `UnDeref
  ]
type expr_variant = [
  | `Var of string
  | `Literal of string
  | `Struct_Literal of struct_fields
  | `Enum_Literal of (string * int * (expr list))
  | `Match of expr * (match_arm list)
  | `Block of (stmt list) * expr
  | `Struct_Field  of expr * string (* struct field access LHS and RHS *)
  | `Deref of expr
  | `Address_of of expr
  | `Call of string * (Types.lifetime list) * (Types.r_type list) * (expr list)
  | `Unsafe of (stmt list) * expr
  | `Return of expr
  | `Assignment of expr * expr
  | `Cast of expr * r_type
  | `BinOp of bin_op * expr * expr
  | `UnOp of un_op * expr
  | `Tuple of expr list
  | `While of expr * expr
  | `Assign_Op of bin_op * expr * expr
  ]
and expr = r_type * expr_variant
and struct_fields = struct_field list
and struct_field = string * expr (* field binding *)
and stmt = [
  | `Expr of expr
  | `Let of string * Types.r_type * expr option
]
and match_arm = (pattern * expr)
and pattern = (r_type * pattern_variant)
and pattern_variant = [
  | `Bind of string
  | `Enum of Types.variant_name * int * (pattern list)
  | `Wild
  | `Literal of string
  | `Const of string
  | `Tuple of pattern list
  ]

type impl_info = {
  abstract_name : string;
  i_lifetimes: Types.type_param list;
  i_types : Types.r_type list;
  i_self : Types.r_type
}


type fn_def = {
  fn_name : string;
  fn_lifetime_params : Types.lifetime list;
  fn_tparams : Types.type_param list;
  ret_type : Types.r_type;
  fn_body : expr;
  fn_args : (string * Types.r_type) list;
  fn_impl : impl_info option
}

type struct_def = {
	struct_name : string;
	s_lifetime_param : Types.lifetime list;
	s_tparam : Types.type_param list;
	struct_fields : (string * Types.r_type) list;
	drop_fn : string option;
  }

type enum_variant = {
	variant_name : Types.variant_name;
	variant_fields : Types.r_type list;
  }

type enum_def = {
	enum_name: string;
	e_lifetime_param: Types.lifetime list;
	e_tparam: Types.type_param list;
	variants: enum_variant list;
	drop_fn : string option;
  }

				
type type_expr = [
  | `Enum_def of enum_def
  | `Struct_def of struct_def
  ]

type module_expr = [
  | type_expr
  | `Fn of fn_def
  | `Abstract_Fn of string
  | `Assoc_type of Types.associated_type
  | `Abstract_Type of Types.abstract_type_def
]
