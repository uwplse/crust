val struct_tag_field : string
val arm_field : (int -> string, unit, string, string, string, string) format6
val field_label : (int -> string, unit, string, string, string, string) format6
val tuple_field : (int -> string, unit, string, string, string, string) format6
val data_field : string

type simple_expr = [
  | `Struct_Field of simple_expr * string
  | `Var of string
  | `Literal of string
  | `Deref of t_simple_expr
  | `Address_of of t_simple_expr
  | `Call of string * (Types.lifetime list) * (Types.r_type list) * (t_simple_expr list)
  | `Assignment of simple_expr * t_simple_expr
  | `BinOp of Ir.bin_op * t_simple_expr * t_simple_expr
  | `UnOp of Ir.un_op * t_simple_expr
  | `Cast of t_simple_expr * Types.r_type
  | `Assign_Op of Ir.bin_op * simple_expr * t_simple_expr
  ]
 and t_simple_expr = Types.r_type * simple_expr
 and 'a complex_expr = [
   | `Block of ('a stmt list) * 'a
   | `Match of t_simple_expr * ('a match_arm list)
   | `While of t_simple_expr * 'a
   | `Return of t_simple_expr
   ]
 and struct_fields = struct_field list
 and struct_field = string * t_simple_expr (* field binding *)
 and 'a stmt = [
   | `Expr of 'a
   | `Let of string * Types.r_type * t_simple_expr
   | `Declare of string * Types.r_type
   ]
(* this is a t_simple_expr but the type will always be `Bool actually *)
 and 'a match_arm = (t_simple_expr * 'a)
type all_expr = Types.r_type * [ all_expr complex_expr | simple_expr ]
type all_complex = all_expr complex_expr

val get_simple_ir : Ir.expr -> all_expr
