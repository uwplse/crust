type lifetime = string
type type_param = string
type variant_name = string
					  
type simple_type = [
  | `Int of int
  | `UInt of int
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

(*
let rec (to_monomorph : (string * mono_type) list -> r_type -> mono_type) = fun t_binding t ->
  match t with
  | `Adt_type a ->
	 let mono_params = List.map (to_monomorph t_binding) a.type_param in
	 `Adt_type { a with type_param = mono_params }
  | `T_Var t_var -> List.assoc t_var t_binding
  | #simple_type as st -> st 
  | `Ref (l,t') -> `Ref (l,(to_monomorph t_binding t'))
  | `Ref_Mut (l, t') -> `Ref_Mut (l,(to_monomorph t_binding t'))
  | `Ptr_Mut t' -> `Ptr_Mut (to_monomorph t_binding t')
  | `Bottom -> `Bottom
  | `Ptr t' -> `Ptr (to_monomorph t_binding t')
  | `Tuple tl -> `Tuple (List.map (to_monomorph t_binding) tl)
  | `Abstract a ->
    let m_args = List.map (to_monomorph t_binding) a.a_params in
    resolve_abstract_type a.a_name m_args
and resolve_abstract_type abstract_name m_args =
  (* stub *)
  `Unit
  *)
type type_binding = (string * mono_type) list

let type_binding tv ty = 
  List.map2 (fun t_name t -> (t_name, t)) tv ty

let rec pp_t (to_pp : r_type) = match to_pp with
  | `Bottom -> "!"
  | `T_Var t -> "var " ^ t
  | `Int s -> "int" ^ (string_of_int s)
  | `UInt s -> "uint" ^ (string_of_int s)
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
	 if p.type_param = [] then p.type_name 
	 else p.type_name ^ "<" ^ (String.concat "," @@ List.map pp_t p.type_param) ^ ">"
  | `Abstract a ->
    "<" ^ a.a_name ^ "<" ^ (
      String.concat "," @@ List.map pp_t a.a_params
    ) ^">>"

let pp_tb tb = 
  "{ " ^
  (String.concat "," @@
   List.map (fun (t_var, t_type) ->
       t_var ^ " -> " ^ (pp_t (t_type : mono_type :> r_type))
     ) tb
  ) ^ " }"

let rust_tuple_name = "__rust_tuple"
