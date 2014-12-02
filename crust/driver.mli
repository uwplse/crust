module type Compilation = sig
  type c_types = [
    | Types.simple_type
    | `Tuple of c_types list
    | `Adt_type of c_types Types.adt_type
    | `Ptr of c_types
    | `Ptr_Mut of c_types
    | `Bottom
  ]
  val adt_type_name : c_types -> string
  val type_to_string : c_types -> string
  val simple_type_repr : Types.simple_type -> string
  val to_monomorph_c_type : (string * c_types) list -> Types.r_type -> c_types
  val int_sizes : int list
  val mangle_fn_name : string -> c_types list -> string
end

module DriverF(C : Compilation) : sig
  class driver_emission : Buffer.t -> C.c_types list -> object
      method emit_driver : (string * C.c_types list) list -> unit
    end
end
