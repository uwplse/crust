module FISet : Set.S with type elt = string * (Types.mono_type list)
module TISet : Set.S with type elt = string * (Types.mono_type list)
val extract_instantiations : (Types.type_param * Types.mono_type) list -> string list -> (TISet.t * FISet.t)
