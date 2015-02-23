type lifetime = string
type type_param = string
type variant_name = string

type int_size = [
  | `Ptr_Size
  | `Bit_Size of int
]
					  
type simple_type = [
  | `Int of int_size
  | `UInt of int_size
  | `Bool
  | `Unit
  | `Float of int
  | `Char
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
  | `Fixed_Vec of (int * 'a)
  | `Str
  | `Vec of 'a
]
and 'a adt_type = {
  type_name : string;
  lifetime_param : lifetime list;
  type_param : 'a list;
}

type mono_type = mono_type mono_r_type;;

type abstract_type_def = {
  a_type_name : string;
  a_lifetime_params : lifetime list;
  a_type_params : type_param list
}

type 'a abstract_type = {
  a_name : string;
  a_lifetimes: lifetime list;
  a_params : 'a list
}

  
type r_type = [
  | `T_Var of type_param
  | `Abstract of r_type abstract_type
  | r_type mono_r_type
  ]
				
type mono_adt_type = mono_type adt_type
type poly_adt_type = r_type adt_type

type associated_type = {
  abstract_name : string;
  ty_param : type_param list;
  ty_lifetimes : lifetime list;
  ty_args : r_type list;
  ty_output : r_type
}

type type_binding = (string * mono_type) list

let type_binding tv ty = 
  List.map2 (fun t_name t -> (t_name, t)) tv ty

let string_of_intsize = function
  | `Ptr_Size -> "size"
  | `Bit_Size i -> string_of_int i

let rec pp_t (to_pp : r_type) = match to_pp with
  | `Bottom -> "!"
  | `T_Var t -> "var " ^ t
  | `Int s -> "int" ^ (string_of_intsize s)
  | `UInt s -> "uint" ^ (string_of_intsize s)
  | `Unit -> "()"
  | `Bool -> "bool"
  | `Char -> "char"
  | `Float i -> "float" ^ (string_of_int i)
  | `Ptr t
  | `Ref (_,t) -> "const " ^ (pp_t t) ^ "*"
  | `Ref_Mut (_,t)
  | `Ptr_Mut t -> (pp_t t) ^ "*"
  | `Tuple tl -> "(" ^ (String.concat ", " @@ List.map pp_t tl) ^ ")"
  | `Adt_type p -> 
    let lifetimes = if p.lifetime_param = [] then "" else ("[" ^ (String.concat "," p.lifetime_param) ^ "]") in
	if p.type_param = [] then p.type_name ^ lifetimes
	 else p.type_name ^ lifetimes ^ "<" ^ (String.concat "," @@ List.map pp_t p.type_param) ^ ">"
  | `Abstract a ->
    "<" ^ a.a_name ^ "<" ^ (
      String.concat "," @@ List.map pp_t a.a_params
    ) ^">>"
  | `Str -> "str"
  | `Vec t -> "[" ^ (pp_t t) ^ "]"
  | `Fixed_Vec (n,t) -> (pp_t t) ^ "[" ^ (string_of_int n) ^ "]"

let pp_tb tb = 
  "{ " ^
  (String.concat "," @@
   List.map (fun (t_var, t_type) ->
       t_var ^ " -> " ^ (pp_t (t_type : mono_type :> r_type))
     ) tb
  ) ^ " }"
