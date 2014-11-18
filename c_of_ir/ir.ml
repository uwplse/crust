type expr = [
  | `Var of string
  | `Literal of simple_type * string
  | `Struct_Literal of (poly_adt_type * struct_fields)
  | `Enum_Literal of (string * poly_adt_type * int * (expr list))
  | `Match of expr * (match_arm list)
  | `Block of (stmt list) * expr
  | `Struct_Field  of expr * string (* struct field access LHS and RHS *)
  | `Deref of expr
  | `Address_of of expr
  | `Call of string * (lifetime list) * (r_type list) * (expr list)
  | `Unsafe of (stmt list) * expr
  | `Return of expr
  | `Assignment of expr * expr
  ]
and struct_fields = struct_field list
and struct_field = string * expr (* field binding *)
and stmt = [
  | `Expr of expr
  | `Let of string * r_type * expr
  ]
and match_arm = (pattern * expr)
and pattern = [
  | `Bind of r_type * string
  | `Enum of string * variant_name * int * (pattern list)
  | `Wild
  ]
type fn_def = {
	fn_name : string;
	fn_lifetime_params : lifetime list;
	fn_tparams : type_param list;
	ret_type : r_type;
	fn_body : expr;
	fn_args : (string * r_type) list;
  }
				
type type_expr = [
  | `Enum_def of enum_def
  | `Struct_def of struct_def
  ]

type module_expr = [
  | type_expr
  | `Fn of fn_def
  ]

type struct_def = {
	struct_name : string;
	s_lifetime_param : lifetime list;
	s_tparam : type_param list;
	struct_fields : (string * r_type) list;
	drop_fn : string option;
  }
					
type enum_variant = {
	variant_name : variant_name;
	variant_fields : r_type list;
  }

type enum_def = {
	enum_name: string;
	e_lifetime_param: lifetime list;
	e_tparam: type_param list;
	variants: enum_variant list;
	drop_fn : string option;
  }
