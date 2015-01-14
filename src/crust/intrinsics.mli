val intrinsic_fn : SSet.t

val is_intrinsic_fn : string -> bool
val is_intrinsic_inst : (string * Types.mono_type list) -> bool

val emit_intrinsic_inst : string -> string -> string list -> Buffer.t -> unit
val emit_intrinsic_call : string -> string -> (string list) -> string list -> Buffer.t -> unit
