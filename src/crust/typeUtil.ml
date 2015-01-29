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

type type_binding = (string * Types.mono_type) list

type _ query_param = 
  | Tuple : int -> (Types.mono_type list) query_param
  | Mut_Ptr : Types.mono_type query_param
  | Ptr : Types.mono_type query_param
  | Adt : string -> (Types.mono_type list) query_param

(* extend me as needed *)
let primitive_types = 
  MTSet.(
    empty
    |> add (`Int 64)
    |> add (`Int 32)
    |> add (`Int 16)
    |> add (`Int 8)
    |> add (`UInt 64)
    |> add (`UInt 32)
    |> add (`UInt 16)
    |> add (`UInt 8)
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
    | Mut_Ptr -> MTSet.fold (fun t accum  ->
        match t with
        | `Ptr_Mut t 
        | `Ref_Mut (_,t) ->
          t::accum
        | _ -> accum
      ) set []
    | Ptr -> MTSet.fold (fun t accum  ->
        match t with
        | `Ptr t
        | `Ref (_,t) ->
          t::accum
        | _ -> accum
      ) set []
    | Adt name -> MTSet.fold (fun t accum ->
        match t with
        | `Adt_type p when p.Types.type_name = name ->
          p.Types.type_param::accum
        | _ -> accum
      ) set []

class virtual ['a] match_state (synth_types : bool) = object
  method virtual next_state : 'a match_state
  method virtual get_candidates : MTSet.t
  method synth_types = synth_types
end

class set_match_state synth_type t_set = object(self)
  method next_state = (self :> set_match_state)
  method get_candidates = t_set
  inherit [MTSet.t] match_state synth_type
end

class list_match_state synth_type t_list = object(self)
  method next_state = match t_list with
    | [] -> (self :> list_match_state)
    | _::t -> new list_match_state synth_type t
  method get_candidates = match t_list with
    | [] -> MTSet.empty
    | h::_ -> MTSet.singleton h
  inherit [Types.mono_type list] match_state synth_type
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


(* extend me as needed *)
let primitive_types = 
  MTSet.(
    empty
    |> add (`Int 64)
    |> add (`Int 32)
    |> add (`Int 16)
    |> add (`Int 8)
    |> add (`UInt 64)
    |> add (`UInt 32)
    |> add (`UInt 16)
    |> add (`UInt 8)
    |> add `Bool
    |> add `Unit
  );;

exception TyResolutionFailed;;

(* I originally wrote this class because I had this idea that
   sub-classes would fill in the handling of certain types.

   This didn't pan out and all these methods could be made into
   functions, but I like them packaged up like this *)
class type_matcher = object(self)
  method get_inst t_set m = 
    self#match_types (new set_match_state true t_set) [] m
  method is_inst t_list m = 
    self#match_types (new list_match_state false t_list) [] m
  method match_types = fun match_state t_binding to_match ->
    self#p_match_types match_state t_binding [] to_match
  method resolve_abstract_type abstract_ty (mono_ty_args : Types.mono_type list) = 
     (*prerr_endline @@ "Resolving abstract type: " ^ abstract_ty;*)
    let candidates = Env.EnvMap.find Env.associated_types abstract_ty in
    let match_state = new list_match_state false mono_ty_args in
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
(*    prerr_endline @@ "Monomorphizing: " ^ (Types.pp_t t);
    prerr_endline @@ "\t with args: " ^ (Types.pp_tb t_binding);*)
    match t with
    | `Adt_type a ->
      let mono_params = List.map (self#to_monomorph t_binding) a.Types.type_param in
      `Adt_type { a with Types.type_param = mono_params }
    | `T_Var t_var -> List.assoc t_var t_binding
    | #Types.simple_type as st -> st 
    | `Ref (l,t') -> `Ref (l,(self#to_monomorph t_binding t'))
    | `Ref_Mut (l, t') -> `Ref_Mut (l,(self#to_monomorph t_binding t'))
    | `Ptr_Mut t' -> `Ptr_Mut (self#to_monomorph t_binding t')
    | `Bottom -> `Bottom
    | `Ptr t' -> `Ptr (self#to_monomorph t_binding t')
    | `Tuple tl -> `Tuple (List.map (self#to_monomorph t_binding) tl)
    | `Abstract a ->
      let m_args = List.map (self#to_monomorph t_binding) a.Types.a_params in
      self#resolve_abstract_type a.Types.a_name m_args

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
  method match_single_type state t_binding (t_list : Types.mono_type list) t_set t = 
    let s_state = new set_match_state false (List.fold_right MTSet.add t_list MTSet.empty) in
    let bindings = match self#match_type s_state t_binding t with
      | `Mismatch -> []
      | `Match -> [[]]
      | `Bind l -> l
    in
    let bindings = 
      if state#synth_types then
        let set_match_state = new set_match_state false t_set in
        match self#match_type set_match_state t_binding t with
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
        let t_state = new list_match_state synth_types inst_candidate in
        let match_result = self#match_types t_state t_binding tl in
        match match_result with
        | `Mismatch -> accum
        | `Inst l -> l @ accum
      ) [] candidate_list in
    let all_insts = 
      if synth_types then
        let s_match_state = new set_match_state true t_set in
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
    | `Ptr t
    | `Ref (_,t) ->
      self#match_single_type state t_binding (find_types t_set Ptr) t_set t
    | `Ref_Mut (_,t)
    | `Ptr_Mut t ->
      self#match_single_type state t_binding (find_types t_set Mut_Ptr) t_set t
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
    | `Tuple tl
    | `Adt_type { Types.type_param = tl; _ }
    | `Abstract { Types.a_params = tl; _ } ->
      List.fold_left get_free_vars accum tl
    | `Ptr t
    | `Ptr_Mut t
    | `Ref (_,t)
    | `Ref_Mut (_,t) ->
      get_free_vars accum t
;;

let matcher = new type_matcher;;

let sort_types a b = 
  let to_ret = 
  match (a,b) with
  | `T_Var _,`T_Var _ -> 0
  | `Abstract _,` Abstract _ -> 0
  | _,`Abstract _ -> -1
  | `Abstract _,_ -> 1
  | `T_Var _,_ -> 1
  | _,`T_Var _ -> -1
  | _,_ -> 0 in
(*  prerr_endline @@ "comparing: " ^ (Types.pp_t a) ^ " " ^ (Types.pp_t b);
  prerr_endline @@ "result -> " ^ (string_of_int to_ret);*)
  to_ret

let rev_app f x y = f y x

let get_inst type_univ to_match = 
  let free_vars = SSet.elements @@ List.fold_left get_free_vars SSet.empty to_match in
  let phantom_args = List.map (fun t_var -> `T_Var t_var) free_vars in
  let to_match = List.sort sort_types @@ to_match @ phantom_args in
(*  prerr_endline @@ "Attempting match with " ^ (String.concat ", " @@ List.map Types.pp_t to_match);*)
  matcher#get_inst type_univ to_match

let to_monomorph = matcher#to_monomorph
let is_inst = matcher#is_inst
