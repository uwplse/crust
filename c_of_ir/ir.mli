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
  ]
and expr = r_type * expr_variant
and struct_fields = struct_field list
and struct_field = string * expr (* field binding *)
and stmt = [
  | `Expr of expr
  | `Let of string * Types.r_type * expr
  ]
and match_arm = (pattern * expr)
and pattern = [
  | `Bind of Types.r_type * string
  | `Enum of Types.poly_adt_type * Types.variant_name * int * (pattern list)
  | `Wild
  | `Literal of Types.simple_type * string
  ]
type fn_def = {
	fn_name : string;
	fn_lifetime_params : Types.lifetime list;
	fn_tparams : Types.type_param list;
	ret_type : Types.r_type;
	fn_body : expr;
	fn_args : (string * Types.r_type) list;
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
  ]
