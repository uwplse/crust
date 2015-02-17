module type Compilation = sig
  type c_types = [
    | Types.simple_type
    | `Tuple of c_types list
    | `Adt_type of c_types Types.adt_type
    | `Ptr of c_types
    | `Ptr_Mut of c_types
    | `Bottom
    | `Fixed_Vec of int * c_types
  ]
  val adt_type_name : Types.mono_type -> string
  val type_to_string : c_types -> string
  val simple_type_repr : Types.simple_type -> string
  val to_monomorph_c_type : (string * c_types) list -> Types.r_type -> c_types
  val int_sizes : int list
  val mangle_fn_name : string -> Types.mono_type list -> string
  val ctype_of_mono : Types.mono_type -> c_types
end

module DriverF(C : Compilation) : sig
  class driver_emission : Buffer.t -> C.c_types list -> object
      method emit_driver : (string * C.c_types list * Analysis.move_info list) list -> unit
    end
end
