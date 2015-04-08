type f_inst = string * (Types.mono_type list)

module TypeInstantiation = struct
  type t = TypeUtil.type_inst
  let compare = Pervasives.compare
end

module FunctionInstantiation = struct
  type t = f_inst
  let compare = Pervasives.compare
end

(* total laziness! *)
open Types
open Ir

module FISet = Set.Make(FunctionInstantiation)
module TISet = Set.Make(TypeInstantiation)
module MTSet = TypeUtil.MTSet

(* function composition (not monads)
 * WARNING: abuse of notation ;)
*)
let (>>=) f g = fun x ->
  g (f x)

let rev_app f = fun x y -> f y x;;

let sequence_len = ref 6;;

exception ResolutionFailed of string;;
exception MonomorphizationFailed of string;;

type walk_state = {
  type_inst : TISet.t;
  fn_inst : FISet.t;
  public_type : MTSet.t;
  public_fn : FISet.t;
  static_var : SSet.t
}

let add_type_instance w_state inst = 
  {
    w_state with type_inst = TISet.add inst w_state.type_inst
  }

let add_fn_instance w_state inst = 
  {
    w_state with fn_inst = FISet.add inst w_state.fn_inst
  }

let add_public_fn w_state inst = 
  {
    w_state with public_fn = FISet.add inst w_state.public_fn
  }
let add_public_type w_state inst = 
  {
    w_state with public_type = MTSet.add inst w_state.public_type
  }

let add_static_var w_state var = 
  {
    w_state with static_var = SSet.add var w_state.static_var
  }

module TySet = Set.Make(struct
    type t = Types.r_type
    let rec compare : t -> t -> int  =
      let rec cmp_lp = function
        | ([],[]) -> 0
        | (h1::t1),(h2::t2) ->
          let cmp = compare h1 h2 in
          if cmp = 0 then cmp_lp (t1,t2) else cmp
        | [],_ -> -1
        | _,[] -> 1
      in fun a b ->
        match a,b with
        | `Vec t1,`Vec t2
        | `Ptr t1,`Ptr t2
        | `Ptr_Mut t1,`Ptr_Mut t2
        | `Ref_Mut (_,t1),`Ref (_,t2)
        | `Ref (_,t1),`Ref (_,t2) ->
          compare t1 t2
        | `Abstract { Types.a_name = tn1; Types.a_params = tp1; _ },
          `Abstract { Types.a_name = tn2; Types.a_params = tp2; _ }
        | `Adt_type { Types.type_name = tn1; Types.type_param = tp1; _ },
          `Adt_type { Types.type_name = tn2; Types.type_param = tp2; _ } ->
          if tn1 = tn2 then
            cmp_lp (tp1,tp2)
          else
            Pervasives.compare tn1 tn2
        | `Tuple tl1,`Tuple tl2 -> cmp_lp (tl1,tl2)
        | _,_ -> Pervasives.compare a b
end)

let resolve_abstract_fn =
  let arg_str s = "(" ^ (String.concat ", " @@ List.map Types.pp_t (s : Types.mono_type list :> Types.r_type list)) ^ ")" in
  let match_types m_args p_args = 
    match TypeUtil.is_inst m_args p_args with
    | `Mismatch -> None
    | `Inst [l] -> Some l
    | _ -> assert false
  in
  let mk_targs tb1 tb2 t_param = 
    if List.mem_assoc t_param tb1 then
      List.assoc t_param tb1
    else if List.mem_assoc t_param tb2 then
      List.assoc t_param tb2
    else `Bottom (* awful *)
  in
  fun abstract_name type_args param_args ->
    let abstract_impls = Env.EnvMap.find Env.abstract_impl abstract_name in
    let possible_insts = 
      List.fold_left (fun accum fn_name ->
          let impl_def = Env.EnvMap.find Env.fn_env fn_name in
          let impl_types = match impl_def.fn_impl with
            | None -> assert false
            | Some ({ Ir.i_types = tl; Ir.i_self = st }) -> tl @ [st]
          in
          let fn_typarams = impl_def.Ir.fn_tparams in
          let arg_types = List.map snd impl_def.Ir.fn_args in
          match (match_types param_args arg_types),(match_types type_args impl_types) with
          | (Some tb1),(Some tb2) ->
            let type_args = List.map (mk_targs tb1 tb2) fn_typarams in
            (type_args,fn_name)::accum
          | _,_ -> accum
        ) [] abstract_impls 
    in
    match possible_insts with 
    | [t] -> t
    | [] -> 
      raise @@ ResolutionFailed ("Failed to discover instantiation for the abstract function " ^ abstract_name ^ " with arguments " ^ (arg_str param_args))
    | l -> 
      let inst_dump = String.concat "/" @@ List.map snd possible_insts in
      let fail_args = arg_str param_args in
      raise @@ ResolutionFailed ("Ambiguous instantiations for abstract function " ^ abstract_name ^ " for arguments " ^ fail_args ^ ". Found instantiations: " ^ inst_dump)


let rec walk_type : walk_state -> Types.mono_type -> walk_state = 
  let walk_rec_inst (w_state : walk_state) inst rec_t =
    if TISet.mem inst w_state.type_inst then
      w_state else walk_type (add_type_instance w_state inst) rec_t
  in
  let walk_type_aux w_state = function
    | `Adt_type a ->
      let instantiation = (`Adt a.type_name),a.type_param in
      if TISet.mem instantiation w_state.type_inst then
        w_state
      else
        let w_state = add_type_instance w_state instantiation in
        walk_type_def_named w_state a.type_name a.type_param
          
    | `Ref (_,t') -> walk_type w_state t'
    | `Ref_Mut (_,t') -> walk_type w_state t'
    | `Tuple tl -> 
      let instantiation = `Tuple,tl in
      if TISet.mem instantiation w_state.type_inst then
        w_state
      else
        let w_state = add_type_instance w_state instantiation in
        List.fold_left walk_type w_state tl
    | `Ptr_Mut t' -> walk_type w_state t' 
    | `Ptr t' -> walk_type w_state t'
    | `Bottom -> w_state
    | #simple_type -> w_state
    | `Fixed_Vec (n,t) ->
      let instantiation = `Fixed_Vec n,[t] in
      if TISet.mem instantiation w_state.type_inst then
        w_state
      else
        walk_type (add_type_instance w_state instantiation) t
    | `Str ->
      raise TypeUtil.StrayDST
    | `Vec _ ->
      raise TypeUtil.StrayDST
  in
  fun w_state ->
    TypeUtil.handle_dst (fun mut t ->
        walk_rec_inst w_state (`Vec mut,[t]) t
      )
      (fun mut -> add_type_instance w_state (`String mut,[]))
      (walk_type_aux w_state)

and walk_type_def_named w_state t_name m_params  = 
  walk_type_def w_state (Env.EnvMap.find Env.adt_env t_name) m_params 
and walk_type_def w_state adt_def m_params =
  let gen_binding = fun t_names -> Types.type_binding t_names m_params in
  match adt_def with
  | `Struct_def sd -> 
    let new_bindings = gen_binding sd.s_tparam in
    let a = (snd >>= (TypeUtil.to_monomorph new_bindings) >>= (rev_app walk_type)) in
    let fold = rev_app a in
    let w_state = List.fold_left fold w_state sd.struct_fields in
    begin
      match sd.Ir.drop_fn with
      | Some df -> walk_fn w_state df m_params
      | None -> w_state
    end
  | `Enum_def ed ->
    let new_bindings = gen_binding ed.e_tparam in
    let m = List.map (fun v -> List.map (TypeUtil.to_monomorph new_bindings) v.variant_fields) ed.variants in
    let w_state  = List.fold_left (List.fold_left walk_type) w_state m in
    begin match ed.Ir.drop_fn with | Some df -> walk_fn w_state df m_params | None -> w_state end
and inst_walk_type t_bindings w_state t = 
  let m_type = TypeUtil.to_monomorph t_bindings t in
  walk_type w_state m_type
and walk_expr t_bindings w_state (expr : Ir.expr) = 
  let w_state = inst_walk_type t_bindings w_state (fst expr) in
  match snd expr with
  | `Block (stmt,e)
  | `Unsafe (stmt,e) ->
    let w_state = (List.fold_left (walk_statement t_bindings) w_state stmt) in
    walk_expr t_bindings w_state e
  | `Call (fn_name,_,t_params,args) ->
    let m_args = List.map (TypeUtil.to_monomorph t_bindings) t_params in
    let w_state = List.fold_left (walk_expr t_bindings) w_state args in
    (*let w_state = List.fold_left walk_type w_state m_args in*)
    if Env.is_abstract_fn fn_name then
      let mono_args = List.map (fst >>= (TypeUtil.to_monomorph t_bindings)) args in
      let (resolved_margs, c_name) = resolve_abstract_fn fn_name m_args mono_args in
      walk_fn w_state c_name resolved_margs
    else 
      walk_fn w_state fn_name m_args
  | `Address_of e | `Deref e -> walk_expr t_bindings w_state e
  | `Struct_Field (e,_) -> walk_expr t_bindings w_state e
  | `Var a -> 
    if Env.is_static_var a then
      walk_static a w_state
    else
      w_state
  | `Literal _ -> w_state
  | `Struct_Literal s_fields ->
    let fold = rev_app (snd >>= (rev_app (walk_expr t_bindings))) in
    List.fold_left fold w_state s_fields
  | `Enum_Literal (_, _, v_fields) ->
    List.fold_left (walk_expr t_bindings) w_state v_fields
  | `Match (e,m) ->
    let w_state = List.fold_left (walk_match_arm t_bindings) w_state m in
    walk_expr t_bindings w_state e
  | `Return e ->
    walk_expr t_bindings w_state e
  | `Assign_Op (_,e1,e2)
  | `BinOp (_,e1,e2)
  | `Assignment (e1,e2) ->
    List.fold_left (walk_expr t_bindings) w_state [e1;e2]
  | `Tuple el -> 
    List.fold_left (walk_expr t_bindings) w_state el
  | `UnOp (_,e1) ->
    walk_expr t_bindings w_state e1
  | `Cast e -> 
    walk_expr t_bindings w_state e
  | `While (e1,e2) ->
    List.fold_left (walk_expr t_bindings) w_state [e1;e2]
  | `Vec e_list ->
    List.fold_left (walk_expr t_bindings) w_state e_list
  | `Break -> w_state
  | `Continue -> w_state
and walk_statement t_bindings w_state stmt = 
  match stmt with
  | `Expr e -> walk_expr t_bindings w_state e
  | `Let (_,v_type,expr) -> 
    let w_state = inst_walk_type t_bindings w_state v_type in
    match expr with
    | Some e -> walk_expr t_bindings w_state e
    | None -> w_state
and walk_static var w_state = 
  if SSet.mem var w_state.static_var then
    w_state 
  else
    let (ty,static_expr) = Env.EnvMap.find Env.static_env var in
    add_static_var w_state var
    |> (rev_app (inst_walk_type [])) ty
    |> (rev_app (walk_expr [])) static_expr
and walk_match_arm t_bindings w_state (patt,expr) = 
  let w_state = walk_pattern t_bindings w_state patt in
  walk_expr t_bindings w_state expr
and walk_pattern t_bindings w_state patt = 
  let w_state = inst_walk_type t_bindings w_state (fst patt) in
  match (snd patt) with
  | `Wild -> w_state
  | `Literal _ -> w_state
  | `Bind _ -> w_state
  | `Const _ -> w_state
  | `Tuple p_list
  | `Enum (_,_,p_list) ->
    List.fold_left (walk_pattern t_bindings) w_state p_list
  | `Addr_of p -> walk_pattern t_bindings w_state p
  | `Ref _ -> w_state
and walk_fn w_state fn_name m_args  = 
  if Intrinsics.is_intrinsic_fn fn_name then 
    add_fn_instance w_state (fn_name,m_args)
  else if  Intrinsics.is_crust_intrinsic fn_name then
    w_state
  else if fn_name = "drop_glue" then
    match m_args with
    | [t] -> begin
        match t with
        | `Tuple tl -> List.fold_left (fun accum t -> walk_fn accum "drop_glue" [t]) w_state tl
        | `Adt_type a -> 
          (match Env.get_adt_drop a.Types.type_name with
           | None -> w_state
           | Some df -> walk_fn w_state df a.Types.type_param
          )
        | _ -> w_state
      end
    | _ -> failwith "Invalid type arity of drop_glue"
  else walk_fn_def w_state (Env.EnvMap.find Env.fn_env fn_name) m_args
and walk_fn_def w_state fn_def m_args = 
  let f_inst = fn_def.fn_name,m_args in
  if FISet.mem f_inst w_state.fn_inst then
    w_state
  else
    let t_bindings = Types.type_binding fn_def.Ir.fn_tparams m_args in
    let ret_type = TypeUtil.to_monomorph t_bindings fn_def.Ir.ret_type in
    let w_state = add_fn_instance w_state f_inst in
    let w_state =  walk_type w_state ret_type in
    walk_expr t_bindings w_state fn_def.Ir.fn_body


let state_size w_state = 
  (MTSet.cardinal w_state.public_type) +
  (FISet.cardinal w_state.public_fn)

type inference_state = {
  restrict_fn : SSet.t;
  fail_inst : FISet.t;
  restrict_types: MTSet.t;
  restrict_adt : SSet.t;
  w_state : walk_state
}


let walk_public_fn =
  let is_restrict_type f_state ty = 
    match ty with
    | `Adt_type { Types.type_name = a; _ } when
        SSet.mem a f_state.restrict_adt ->
      not (MTSet.mem ty f_state.restrict_types)
    |  _ -> false
  in
  fun fn_def f_state m_args ->
  let f_inst = fn_def.fn_name,m_args in
  if FISet.mem f_inst f_state.w_state.public_fn then
    f_state
  else if FISet.mem f_inst f_state.fail_inst then
    f_state
  else begin
    try
      let t_binding = Types.type_binding fn_def.Ir.fn_tparams m_args in
      let mono_ret_type = (TypeUtil.to_monomorph t_binding fn_def.Ir.ret_type) in
      if is_restrict_type f_state mono_ret_type then
        { f_state with fail_inst = FISet.add f_inst f_state.fail_inst }
      else
        let w_state = f_state.w_state in
        let w_state = add_public_fn w_state f_inst in
        let w_state = match mono_ret_type with
          | #simple_type -> w_state (* we don't need to track this crap at runtime *)
          | _ -> add_public_type w_state mono_ret_type 
        in
        let w_state' = walk_fn_def w_state fn_def m_args in
        { f_state with w_state = w_state' }
    with 
    | ResolutionFailed e -> (*prerr_endline ("Method resolution failed: " ^ e);*) { f_state with fail_inst = FISet.add f_inst f_state.fail_inst }
    | TypeUtil.TyResolutionFailed -> (prerr_endline "Type resolution failed"; {
        f_state with fail_inst = FISet.add f_inst f_state.fail_inst
      })
    | Env.Missing_binding e -> (*prerr_endline ("Failed due to missing function " ^ e);*) {
        f_state with fail_inst = FISet.add f_inst f_state.fail_inst
      }
  end

let get_inst t_set fn_def = 
    let arg_types = List.map snd fn_def.Ir.fn_args in
    match TypeUtil.get_inst t_set fn_def.Ir.fn_tparams arg_types with
    | `Inst t_bindings -> 
      Some (List.map (fun t_binding ->
          List.map (t_binding |> rev_app List.assoc) fn_def.Ir.fn_tparams
        ) t_bindings)
    | `Mismatch -> None

let has_inst w_state f_name = 
  FISet.exists (fun (f_name',_) -> 
      f_name = f_name') w_state.fn_inst


let core_ops = [
  Str.regexp ("^" ^ (Str.quote "core$option$Option$1$T$1$$unwrap") ^ "$");
  Str.regexp_string "core$slice$$x5bT$x5d$$ops$x3a$x3aIndexMut$1$uint$1$$index_mut";
  Str.regexp_string "core$slice$$x5bT$x5d$$ops$x3a$x3aIndex$1$uint$1$$index"
]

let pattern_list = ref core_ops;;
let type_filters = ref SSet.empty;;

let rec find_fn ?(iter=0) find_state = 
  if iter = !sequence_len then
    find_state
  else
    let old_size = state_size find_state.w_state in
    let find_state = Env.EnvMap.fold (fun fn_name fn_def f_state ->
        if not (List.exists (fun patt ->
            Str.string_match patt fn_name 0
          ) !pattern_list) then
          f_state
        else if SSet.mem fn_name f_state.restrict_fn then
          f_state
        else begin
          match get_inst f_state.w_state.public_type fn_def with
          | None -> f_state
          | Some inst_list ->
            List.fold_left (walk_public_fn fn_def) f_state inst_list
        end
      ) Env.fn_env find_state in
    if old_size = (state_size find_state.w_state) then
      find_state
    else
      find_fn ~iter:(succ iter) find_state

let build_nopub_fn =
  let is_restrict_fn fn_def = 
    match fn_def.Ir.fn_args with
    | ("self",t)::_ -> begin
        match t with
        | `Ref (_,`Adt_type a)
        | `Ref_Mut (_,`Adt_type a)
        | `Adt_type a when SSet.mem a.Types.type_name !type_filters ->
          true
        | _ -> false
      end
    | _ -> false
  in
  let is_private_fn fn_def = 
    match fn_def.fn_vis with
    | `Private -> true
    | `Public -> false
  in
  let is_drop_fn fn_def =
    match fn_def.Ir.fn_impl with
    | Some { Ir.abstract_name = "core$ops$Drop$drop"; _ } -> true
    | _ -> false
  in
  let is_unsafe_fn fn_def = 
    match snd fn_def.Ir.fn_body with
    | `Unsafe _ -> true
    | _ -> false
  in
  fun () ->
    let accum = (match Env.crust_init_name() with
        | Some s -> SSet.singleton s
        | None -> SSet.empty
      ) |> SSet.add "crust_abort" in
    Env.EnvMap.fold (fun fn_name fn_def accum ->
        if is_restrict_fn fn_def ||
           is_private_fn fn_def ||
           is_drop_fn fn_def ||
           is_unsafe_fn fn_def then
          SSet.add fn_name accum
        else
          accum
      ) Env.fn_env accum

let run_analysis () = 
  let restrict_fn = Env.EnvMap.fold (fun type_name type_def accum ->
      match type_def with
      | `Enum_def ({ drop_fn = Some df; _ } : Ir.enum_def) ->
        SSet.add df accum
      | `Struct_def ({ drop_fn = Some df; _ } : Ir.struct_def)->
        SSet.add df accum
      | _ -> accum
    ) Env.adt_env @@ build_nopub_fn ()
  in
  let (init_def,seed_types) =
    match Env.crust_init_name () with
    | None -> if !Env.init_opt then (None,[]) else failwith "No crust_init definition found!"
    | Some crust_init_name -> begin
        let crust_init_def = Env.EnvMap.find Env.fn_env crust_init_name in
        (match crust_init_def.Ir.fn_tparams with
         | [] -> ()
         | _ -> failwith "crust_init cannot be polymorphic");
        match crust_init_def.Ir.ret_type with
        | `Tuple tl -> (Some crust_init_def,List.map (TypeUtil.to_monomorph []) tl)
        | t -> failwith @@ "crust_init must return a tuple, found: " ^ (Types.pp_t t)
      end
  in
  let init_state = {
    type_inst = TISet.empty;
    fn_inst = FISet.empty;
    public_type = List.fold_right MTSet.add seed_types MTSet.empty;
    public_fn = FISet.empty;
    static_var = SSet.empty
  }
  in
  let w_state = 
    match init_def with
    | None -> init_state
    | Some crust_init_def -> walk_fn_def init_state crust_init_def []
  in
  let (restrict_adt,restrict_type) = List.fold_left (fun (r_a,r_t) ty ->
      match ty with
      | `Adt_type { Types.type_name = a; _ } -> 
        (SSet.add a r_a,MTSet.add ty r_t)
      | _ -> (r_a,r_t)
    ) (SSet.empty,MTSet.empty) seed_types in
  (find_fn {
    restrict_fn = restrict_fn;
    w_state = w_state;
    fail_inst = FISet.empty;
    restrict_adt = restrict_adt;
    restrict_types = restrict_type
  }).w_state

let crust_test_regex = Str.regexp ("^.+" ^ (Str.quote "$") ^ "crust_test_[0-9]+$");;

let run_test_analysis test_chunk_size = 
  let init_state = {
    type_inst = TISet.empty;
    fn_inst = FISet.empty;
    public_type = MTSet.empty;
    public_fn = FISet.empty;
    static_var = SSet.empty
  } in
  Env.EnvMap.fold (fun k v accum ->
      if Str.string_match crust_test_regex k 0 then
        walk_fn_def accum v []
      else 
        accum
    ) Env.fn_env init_state

let compile_glob patt = 
  let patt = Str.quote patt in
  let patt = Str.global_replace (Str.regexp_string @@ Str.quote "*") ".+" patt in
  let patt = "^" ^ patt ^ "$" in
  Str.regexp patt


let init_fn_filter f_name = 
  let in_chan = open_in f_name in
  let patterns = 
    let rec slurp_file accum = 
      try
        let line = input_line in_chan in
        if line = "" then
          slurp_file accum
        else
          slurp_file (line::accum)
      with End_of_file -> close_in in_chan; accum
    in
    slurp_file []
  in
  pattern_list := (List.map (fun patt ->
      compile_glob patt
    ) patterns) @ !pattern_list

let set_fn_filter filter =
  pattern_list := [ compile_glob filter ] @ !pattern_list

let init_type_filter file_name = 
  let f_in = open_in file_name in
  let rec read_loop accum = 
    try
      let l = input_line f_in in
      if l = "" then read_loop accum else (
        read_loop @@ SSet.add l accum
      )
    with End_of_file -> (close_in f_in; accum)
  in
  type_filters := read_loop SSet.empty
