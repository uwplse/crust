type lifetime = string
type type_param = string
type variant_name = string
					  
type simple_type = [
  | `Int of int
  | `UInt of int
  | `Bool
  | `Unit
  ]
					 
type 'a mono_r_type = [
  | `Adt_type of 'a adt_type
  | `Ref of lifetime * 'a
  | `Ref_Mut of lifetime * 'a
  | `Ptr of 'a
  | `Ptr_Mut of 'a
  | simple_type
  | `Tuple of 'a list
  | `Bottom
  ]
 and 'a adt_type = {
	 type_name : string;
	 lifetime_param : lifetime list;
	 type_param : 'a list;
   }
					 
type mono_type = mono_type mono_r_type;;
  
type r_type = [
  | `T_Var of type_param
  | r_type mono_r_type
  ]
				
type mono_adt_type = mono_type adt_type
type poly_adt_type = r_type adt_type
val to_monomorph : (string * mono_type) list -> r_type -> mono_type
val type_binding : type_param list -> 'a list -> (type_param * 'a) list

val pp_t :  r_type -> string
