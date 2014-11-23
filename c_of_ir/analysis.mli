module FISet : Set.S with type elt = string * (Types.mono_type list)
module TISet : Set.S with type elt = string * (Types.mono_type list)
val do_test : Types.mono_type list -> Types.r_type list -> [
  | `Mismatch
  | `Inst of (string * Types.mono_type) list list
  ]
val debug_get_constructors : unit -> string list

(*val extract_instantiations : (Types.type_param * Types.mono_type) list -> string list -> (TISet.t * FISet.t)*)
