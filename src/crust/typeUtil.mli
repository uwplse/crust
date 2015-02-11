module MTSet : Set.S with type elt = Types.mono_type

type inst_flag = [
  | `Adt of string
  | `Tuple
  | `Vec of bool
  | `String of bool
  | `Fixed_Vec of int
]

type type_inst = inst_flag * (Types.mono_type) list

type type_binding = (string * Types.mono_type) list

type inst_result = [
  | `Mismatch
  | `Inst of type_binding list
]

exception StrayDST

exception TyResolutionFailed

val to_monomorph: (string * Types.mono_type) list -> Types.r_type -> Types.mono_type
val get_inst : MTSet.t -> Types.type_param list -> Types.r_type list -> inst_result
val is_inst : Types.mono_type list -> Types.r_type list -> inst_result

val is_move_type : Types.mono_type -> bool
