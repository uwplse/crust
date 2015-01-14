val intrinsic_fn : SSet.t

val is_intrinsic_fn : string -> bool
val is_intrinsic_inst : (string * Types.mono_type list) -> bool

val compile_intrinsic_inst : string -> string -> string list -> Buffer.t -> unit
val compile_intrinsic_call : string -> string -> (string list) -> string list -> Buffer.t -> unit
