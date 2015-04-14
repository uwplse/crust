module MTSet = Set.Make(struct 
    type t = Types.mono_type
    let compare = Pervasives.compare
  end)

type match_result = [
  | `Bind of Types.type_binding list
  | `Match
  | `Mismatch
]

type inst_result = [
  | `Mismatch
  | `Inst of Types.type_binding list
]

let dummy_lifetime = "_dummy_"

type type_binding = (string * Types.mono_type) list

type _ query_param = 
  | Tuple : int -> (Types.mono_type list) query_param
  | Ptr_Mut : Types.mono_type query_param
  | Ptr : Types.mono_type query_param
  | Ref : bool -> Types.mono_type query_param
  | Ref_Mut : bool -> Types.mono_type query_param
  | Adt : string -> (Types.mono_type list) query_param
  | Fixed_Vec : int -> Types.mono_type query_param
  | Vec : Types.mono_type query_param

(* extend me as needed *)
let primitive_types = 
  MTSet.(
    empty
    |> add (`Int (`Bit_Size 64))
    |> add (`Int (`Bit_Size 32))
    |> add (`Int (`Bit_Size 16))
    |> add (`Int (`Bit_Size 8))
    |> add (`Int `Ptr_Size)
    |> add (`UInt (`Bit_Size 64))
    |> add (`UInt (`Bit_Size 32))
    |> add (`UInt (`Bit_Size 16))
    |> add (`UInt (`Bit_Size 8))
    |> add (`Int `Ptr_Size)
    |> add `Bool
    |> add `Unit
  );;


let find_types : type a.MTSet.t -> a query_param -> a list = 
  fun set query ->
    match query with
    | Tuple n -> MTSet.fold (fun t accum ->
        match t with
        | `Tuple tl when (List.length tl) = n ->
          tl::accum
        | _ -> accum
      ) set []
    | Ptr_Mut -> MTSet.fold (fun t accum  ->
        match t with
        | `Ptr_Mut t ->
          t::accum
        | _ -> accum
      ) set []
    | Ptr -> MTSet.fold (fun t accum  ->
        match t with
        | `Ptr t -> t::accum
        | _ -> accum
      ) set []
    | Ref b -> MTSet.fold (fun t accum ->
        match t with
        | `Ref (_,t) ->
          t::accum
        | `Ref_Mut (_,t) when b ->
          t::accum
        | _ -> accum
      ) set []
    | Ref_Mut b -> MTSet.fold (fun t accum ->
        match t with
        | `Ref_Mut (_,t) -> t::accum
        | `Ref (_,t) when b ->
          t::accum
        | _ -> accum
      ) set []
    | Adt name -> MTSet.fold (fun t accum ->
        match t with
        | `Adt_type p when p.Types.type_name = name ->
          p.Types.type_param::accum
        | _ -> accum
      ) set []
    | Fixed_Vec n -> MTSet.fold (fun t accum ->
        match t with
        | `Fixed_Vec (n',t) when n' = n -> 
          t::accum
        | _ -> accum
      ) set []
    | Vec -> MTSet.fold (fun t accum ->
        match t with
        | `Vec t -> t::accum
        | _ -> accum
      ) set []

class virtual ['a] match_state (fuzzy_ref : bool) (synth_types : bool) = object
  method virtual next_state : 'a match_state
  method virtual get_candidates : MTSet.t
  method synth_types = synth_types
  method fuzzy_ref = fuzzy_ref
end

class set_match_state fuzzy_ref synth_type t_set = object(self)
  method next_state = (self :> set_match_state)
  method get_candidates = t_set
  inherit [MTSet.t] match_state fuzzy_ref synth_type
end

class list_match_state fuzzy_ref synth_type t_list = object(self)
  method next_state = match t_list with
    | [] -> (self :> list_match_state)
    | _::t -> new list_match_state fuzzy_ref synth_type t
  method get_candidates = match t_list with
    | [] -> MTSet.empty
    | h::_ -> MTSet.singleton h
  inherit [Types.mono_type list] match_state fuzzy_ref synth_type
end

let rec uniq_list l = match l with
  | h::h'::t when h = h' ->
    uniq_list (h'::t)
  | h::t -> h::(uniq_list t)
  | [] -> []

(* Not until 4.02 :( *)
let sort_uniq l = 
  let l = List.sort Pervasives.compare l in
  uniq_list l

exception StrayDST;;
exception TyResolutionFailed;;

(* I originally wrote this class because I had this idea that
   sub-classes would fill in the handling of certain types.

   This didn't pan out and all these methods could be made into
   functions, but I like them packaged up like this *)

let dump_tset t_set = 
  "{ " ^
  (String.concat ", " @@ MTSet.fold (fun t accum -> 
       (Types.pp_t (t : Types.mono_type :> Types.r_type))::accum
     ) t_set [])
  ^ " }"

let dump_tlist t_list = 
  "[ " ^ 
  (String.concat ", " @@ List.map (fun x -> Types.pp_t x) t_list) ^ " ]"

let print_tlist t_list = 
  prerr_endline @@ dump_tlist t_list

let print_tset t_set = 
  prerr_endline @@ dump_tset t_set


class type_matcher fuzzy_ref = object(self)
  method get_inst t_set m = 
    self#match_types (new set_match_state fuzzy_ref true t_set) [] m
  method is_inst t_list m = 
    self#match_types (new list_match_state fuzzy_ref false t_list) [] m
  method match_types = fun match_state t_binding to_match ->
    self#p_match_types match_state t_binding [] to_match
  method resolve_abstract_type abstract_ty (mono_ty_args : Types.mono_type list) = 
    let candidates = Env.EnvMap.find Env.associated_types abstract_ty in
    let match_state = new list_match_state fuzzy_ref false mono_ty_args in
    let instantiations = List.fold_left (fun accum c ->
        match self#match_types match_state [] c.Types.ty_args with
        | `Mismatch -> accum
        | `Inst [t] ->
          (self#to_monomorph t c.Types.ty_output)::accum
        | _ -> failwith "ambiguous type instantiation!" (* impossible? *)
      ) [] candidates in
    match instantiations with
    | [] -> raise TyResolutionFailed
    | [t] -> t
    | _ -> failwith "ambiguous type resolution" (* possible? *)
  method to_monomorph t_binding t = 
    match t with
    | `Adt_type a ->
      let mono_params = List.map (self#to_monomorph t_binding) a.Types.type_param in
      `Adt_type { a with Types.type_param = mono_params; Types.lifetime_param = [] }
    | `T_Var t_var -> List.assoc t_var t_binding
    | #Types.simple_type as st -> st 
    | `Ref (_,t') -> `Ref (dummy_lifetime,(self#to_monomorph t_binding t'))
    | `Ref_Mut (_, t') -> `Ref_Mut (dummy_lifetime,(self#to_monomorph t_binding t'))
    | `Ptr_Mut t' -> `Ptr_Mut (self#to_monomorph t_binding t')
    | `Ptr t' -> `Ptr (self#to_monomorph t_binding t')
    | `Bottom -> `Bottom
    | `Tuple tl -> `Tuple (List.map (self#to_monomorph t_binding) tl)
    | `Abstract a ->
      let m_args = List.map (self#to_monomorph t_binding) a.Types.a_params in
      self#resolve_abstract_type a.Types.a_name m_args
    | `Str -> `Str
    | `Vec t -> `Vec (self#to_monomorph t_binding t)
    | `Fixed_Vec (i,t) -> `Fixed_Vec (i,self#to_monomorph t_binding t)

  method private p_match_types = fun state t_binding t_accum to_match ->
    match to_match with 
    (* we found a possible instantiation wooooo! *)
    | [] -> `Inst [t_accum]
    | h::t -> 
      match (self#match_type state t_binding h : match_result) with
      | `Mismatch -> `Mismatch
      | `Match -> let next_state = state#next_state in
        self#p_match_types next_state t_binding t_accum t
      (* We found multiple possible choices for the current type,
         		* recurse on all possible choices
         		*)
      | `Bind b_choices ->
        let next_state = state#next_state in
        (* matches is a list of (complete!) bindings that we could construct
           		   * with any possible choice of (partial) binding choices
           		   *)
        let matches = List.fold_left (fun (accum : type_binding list) b_choice ->
            let new_binding = b_choice @ t_binding in
            let type_accum = b_choice @ t_accum in
            match self#p_match_types next_state new_binding type_accum t with
            (* each concrete choice at this step can
               							lead to multiple possible
               							instantiations *)
            | `Inst complete_binding ->
              complete_binding @ accum
            | `Mismatch -> accum
          ) [] b_choices in
        begin
          match matches with
          | [] -> `Mismatch
          | l -> `Inst l
        end
  method match_single_type state t_binding (t_list : Types.mono_type list) t = 
    let s_state = new set_match_state fuzzy_ref false (List.fold_right MTSet.add t_list MTSet.empty) in
    let bindings = match self#match_type s_state t_binding t with
      | `Mismatch -> []
      | `Match -> [[]]
      | `Bind l -> l
    in
    let bindings = 
      if state#synth_types then
        match self#match_type state t_binding t with
        | `Mismatch -> bindings
        | `Match -> [] :: bindings
        | `Bind l -> l @ bindings
      else
        bindings
    in
    match (sort_uniq bindings) with
    | [] -> `Mismatch
    | [[]] -> `Match
    | l -> `Bind l
  method match_many_type synth_types t_binding candidate_list t_set tl =
    let insts = List.fold_left (fun accum inst_candidate ->
        let t_state = new list_match_state fuzzy_ref synth_types inst_candidate in
        let match_result = self#match_types t_state t_binding tl in
        match match_result with
        | `Mismatch -> accum
        | `Inst l -> l @ accum
      ) [] candidate_list in
    let all_insts = 
      if synth_types then
        let s_match_state = new set_match_state fuzzy_ref true t_set in
        match self#match_types s_match_state t_binding tl with
        | `Mismatch -> insts
        | `Inst synth_inst -> insts @ synth_inst
      else
        insts
    in
    begin
      match (sort_uniq all_insts) with
      | [] -> `Mismatch
      | [[]] -> `Match
      | l -> `Bind l
    end
  method match_type state t_binding (to_match : Types.r_type) = 
    let t_set = state#get_candidates in
    match to_match with
    | #Types.simple_type when state#synth_types -> `Match
    | #Types.simple_type as s -> 
      if MTSet.mem s t_set then
        (`Match : match_result)
      else
        `Mismatch
    | `Tuple tl ->
      let tuple_length = List.length tl in
      let existing_tuples = find_types t_set (Tuple tuple_length) in
      self#match_many_type state#synth_types t_binding existing_tuples t_set tl
    | `Bottom -> `Mismatch
    | `Adt_type p -> 
      let adt_name = p.Types.type_name in
      let type_param = p.Types.type_param in
      let instances = find_types t_set (Adt adt_name) in
      self#match_many_type false t_binding instances t_set type_param
    | `T_Var t when List.mem_assoc t t_binding ->
      let resolved_type = List.assoc t t_binding in
      self#match_type state t_binding (resolved_type : Types.mono_type :> Types.r_type)
    | `T_Var t_var ->
      let t_set = if state#synth_types then MTSet.union primitive_types t_set else t_set in
      let binding_choices = MTSet.fold (fun t accum ->
          [(t_var,t)]::accum
        ) t_set [] in
      `Bind binding_choices
    | `Ptr t -> 
      self#match_single_type state t_binding (find_types t_set Ptr) t
    | `Ref (_,t) ->
      self#match_single_type state t_binding (find_types t_set @@ Ref state#fuzzy_ref) t
    | `Ref_Mut (_,t) ->
      self#match_single_type state t_binding (find_types t_set @@ Ref_Mut state#fuzzy_ref) t
    | `Ptr_Mut t ->
      self#match_single_type state t_binding (find_types t_set Ptr_Mut) t
    | `Str -> 
      if MTSet.mem `Str t_set then
        `Match
      else
        `Mismatch
    | `Fixed_Vec (n,t) -> 
      let state = new set_match_state state#fuzzy_ref false t_set in
      self#match_single_type state t_binding (find_types t_set (Fixed_Vec n)) t
    | `Vec t -> 
      let state = new set_match_state state#fuzzy_ref false t_set in
      self#match_single_type state t_binding (find_types t_set Vec) t
    | `Abstract _ ->
      try
        let t = self#to_monomorph t_binding to_match in
        self#match_type state t_binding (t : Types.mono_type :> Types.r_type)
      with TyResolutionFailed -> `Mismatch
end

let rec get_free_vars accum t = 
    match t with
    | #Types.simple_type -> accum
    | `Bottom -> accum
    | `T_Var t -> SSet.add t accum
    | `Str -> accum
    | `Tuple tl
    | `Adt_type { Types.type_param = tl; _ }
    | `Abstract { Types.a_params = tl; _ } ->
      List.fold_left get_free_vars accum tl
    | `Vec t
    | `Fixed_Vec (_,t)
    | `Ptr t
    | `Ptr_Mut t
    | `Ref (_,t)
    | `Ref_Mut (_,t) ->
      get_free_vars accum t
;;

let rec subst_tys (params : string list) (args : Types.r_type list) (tys : Types.r_type list) =
  let map = Hashtbl.create (List.length params) in
  List.iter2 (Hashtbl.add map) params args;
  let rec go (t : Types.r_type) =
    match t with
    | #Types.simple_type -> t
    | `Bottom -> t
    | `Str -> t
    | `Adt_type adt -> `Adt_type { adt with
        Types.type_param = List.map go adt.Types.type_param }
    | `Ref (l,t) -> `Ref (l, go t)
    | `Ref_Mut (l,t) -> `Ref_Mut (l, go t)
    | `Ptr t -> `Ptr (go t)
    | `Ptr_Mut t -> `Ptr_Mut (go t)
    | `Tuple tys -> `Tuple (List.map go tys)
    | `Fixed_Vec (n, t) -> `Fixed_Vec (n, go t)
    | `Vec ty -> `Vec (go ty)
    | `Abstract abs -> `Abstract { abs with
        Types.a_params = List.map go abs.Types.a_params }
    | `T_Var name -> Hashtbl.find map name
  in
  List.map go tys
;;


let matcher = new type_matcher false;;

let resolution_matcher = new type_matcher false;;

let rec has_abstract (t : Types.r_type) = 
  match t with
  | `T_Var _ -> false
  | #Types.simple_type -> false
  | `Bottom -> false
  | `Adt_type { Types.type_param = tl; _ }
  | `Tuple tl -> 
    List.exists has_abstract tl
  | `Abstract _ -> true
  | `Vec t
  | `Fixed_Vec (_,t)
  | `Ref_Mut (_,t)
  | `Ref (_,t)
  | `Ptr t
  | `Ptr_Mut t ->
    has_abstract t
  | `Str -> false


let sort_types a b = 
  let has_abstract_a = has_abstract a in
  let has_abstract_b = has_abstract b in
  match (has_abstract_a,has_abstract_b) with
  | true,true -> 0
  | false,true -> -1
  | true,false -> 1
  | _,_ ->
    match (a,b) with
    | `T_Var _,`T_Var _ -> 0
    | `T_Var _,_ -> 1
    | _,`T_Var _ -> -1
    | _,_ -> 0


let rev_app f x y = f y x

let get_inst type_univ free_vars (to_match : Types.r_type list) = 
  let appears_in = List.fold_left get_free_vars SSet.empty to_match in
  let all_free = List.fold_right SSet.add free_vars SSet.empty in
  if not (SSet.is_empty @@ SSet.diff all_free appears_in) then
    `Mismatch
  else
    let to_match = 
      if List.exists has_abstract to_match then
        let phantom_args = List.map (fun t_var -> `T_Var t_var) free_vars in
        List.sort sort_types @@ to_match @ phantom_args
      else
        to_match
    in
    matcher#get_inst type_univ to_match

let to_monomorph = matcher#to_monomorph
let is_inst = resolution_matcher#is_inst

type inst_flag = [
  | `Adt of string
  | `Tuple
  | `Vec of bool
  | `String of bool
  | `Fixed_Vec of int
]

type type_inst = inst_flag * (Types.mono_type) list

let handle_dst vec_cb str_cb no_match_cb ty = 
  match ty with
  | `Ptr (`Vec t)
  | `Ref (_,`Vec t) ->
    (vec_cb false t)
  | `Ptr_Mut (`Vec t)
  | `Ref_Mut (_,`Vec t) ->
    (vec_cb true t)

  | `Ref (_,`Str)
  | `Ptr `Str ->
    (str_cb false)

  | `Ptr_Mut `Str
  | `Ref_Mut (_, `Str) ->
    (str_cb true)

  | r -> no_match_cb r

let string_of_inst (n,a) = 
  match a with
  | [] -> n
  | _ -> n ^ "<" ^ (String.concat ", " @@ List.map Types.pp_t (a : Types.mono_type list :> Types.r_type list)) ^ ">"

let pp_inst : type_inst -> string = function
  | `Tuple,m_args -> "TUPLE[" ^ (String.concat "," @@ List.map Types.pp_t (m_args : Types.mono_type list :> Types.r_type list)) ^ "]"
  | `Adt n,m_args -> 
    string_of_inst (n,m_args)
  | `Fixed_Vec n,[m_arg] ->
    (Types.pp_t (m_arg : Types.mono_type :> Types.r_type)) ^ "[" ^ (string_of_int n) ^ "]"
  | `String mut,_ -> (if mut then "mut" else "const") ^ " STRING"
  | `Vec mut,[m_arg] -> 
    (if mut then "mut" else "const") ^ " VEC[" ^ (Types.pp_t (m_arg : Types.mono_type :> Types.r_type)) ^ "]"
  | _ -> assert false
