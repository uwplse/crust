val is_intrinsic_fn : string -> bool
val is_intrinsic_inst : (string * Types.mono_type list) -> bool

val emit_intrinsic_inst : string -> string -> string list -> Buffer.t -> unit
val emit_intrinsic_call : string -> string -> (string list) -> string list -> Buffer.t -> unit

val need_iheader : (string * Types.r_type list) list -> bool

val is_crust_intrinsic : string -> bool
val intrinsic_name : string -> string
