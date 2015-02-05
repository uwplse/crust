type inst = string * (Types.mono_type list)

module TypeInstantiation = struct
  type t = inst
  let compare = Pervasives.compare
end

module FunctionInstantiation = struct
  type t = inst
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

exception ResolutionFailed of string;;
exception MonomorphizationFailed of string;;

type walk_state = {
  type_inst : TISet.t;
  fn_inst : FISet.t;
  public_type : MTSet.t;
  public_fn : FISet.t
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

let rec type_contains_loop src_types = function
  | ((`T_Var _) as s)
  | (#simple_type as s) -> List.mem s src_types
  | `Bottom -> List.mem `Bottom src_types
  | ((`Ref_Mut (_,p)) as t)
  | ((`Ptr_Mut p) as t)
  | ((`Ref (_,p)) as t)
  | ((`Ptr p) as t) -> (List.mem t src_types) ||
                       type_contains_loop src_types p
  | ((`Tuple tl) as t)
  | ((`Adt_type { Types.type_param = tl; _ }) as t) ->
    List.mem t src_types ||
    (let contains = List.map (type_contains_loop src_types) tl in
     List.exists (fun x -> x) contains
    )
  | `Abstract _ -> false (* not really true *)

let rec type_contains src_types target  = 
  match target with
  | `Adt_type { Types.type_param = tl; _ }
  | `Tuple tl -> 
    let contains = List.map (type_contains_loop src_types) tl in
    List.exists (fun x -> x) contains
  | `Abstract _ -> false
  | _ -> false

let find_constructors () = 
  let accum = SSet.empty in
  Env.EnvMap.fold (fun f_name fn_def accum ->
      let arg_types = List.map snd fn_def.Ir.fn_args in
      let ret_type = fn_def.Ir.ret_type in
      if type_contains arg_types ret_type  then
        SSet.add f_name accum
      else accum
    ) Env.fn_env accum

let resolve_abstract_fn =
  let arg_str s = "(" ^ (String.concat ", " @@ List.map Types.pp_t (s : Types.mono_type list :> Types.r_type list)) ^ ")" in
  fun abstract_name param_args ->
  let abstract_impls = Env.EnvMap.find Env.abstract_impl abstract_name in
  let possible_insts = 
    List.fold_left (fun accum fn_name ->
        let impl_def = Env.EnvMap.find Env.fn_env fn_name in
        let arg_types = List.map snd impl_def.Ir.fn_args in
        match TypeUtil.is_inst param_args arg_types with
        | `Mismatch -> accum
        | `Inst l -> (*prerr_endline @@ "found something for " ^ fn_name;*)
          (match l with
            | [t] ->
              let targ_bindings = List.map ((rev_app List.assoc) t) impl_def.Ir.fn_tparams in
              (targ_bindings,impl_def.Ir.fn_name)::accum
            | _ -> assert false
          )
      ) [] abstract_impls in
  match possible_insts with 
  | [t] -> t
  | [] -> 
    raise @@ ResolutionFailed ("Failed to discover instantiation for the abstract function " ^ abstract_name ^ " with arguments " ^ (arg_str param_args))
  | _ -> 
    let inst_dump = String.concat "/" @@ List.map snd possible_insts in
    let fail_args = arg_str param_args in
    raise @@ ResolutionFailed ("Ambiguous instantiations for abstract function " ^ abstract_name ^ " for arguments " ^ fail_args ^ ". Found instantiations: " ^ inst_dump)



let rec walk_type : walk_state -> Types.mono_type -> walk_state = fun w_state t ->
  match t with
  | `Adt_type a ->
    let instantiation = a.type_name,a.type_param in

    if TISet.mem instantiation w_state.type_inst then
      w_state
    else
      let w_state = add_type_instance w_state instantiation in
      walk_type_def_named w_state a.type_name a.type_param
  | `Ref (_,t') -> walk_type w_state t'
  | `Ref_Mut (_,t') -> walk_type w_state t'
  | `Tuple tl -> 
    let instantiation = Types.rust_tuple_name,tl in
    if TISet.mem instantiation w_state.type_inst then
      w_state
    else
      let w_state = add_type_instance w_state instantiation in
      List.fold_left walk_type w_state tl
  | `Ptr_Mut t' -> walk_type w_state t' 
  | `Ptr t' -> walk_type w_state t'
  | `Bottom -> w_state
  | #simple_type -> w_state
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
    let w_state = List.fold_left walk_type w_state m_args in
    if Env.is_abstract_fn fn_name then
      let mono_args = List.map (fst >>= (TypeUtil.to_monomorph t_bindings)) args in
      let (resolved_margs, c_name) = resolve_abstract_fn fn_name mono_args in
      walk_fn w_state c_name resolved_margs
    else 
      walk_fn w_state fn_name m_args
  | `Address_of e | `Deref e -> walk_expr t_bindings w_state e
  | `Struct_Field (e,_) -> walk_expr t_bindings w_state e
  | `Var a -> w_state
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
  | `Cast (e,t) -> 
    let w_state = inst_walk_type t_bindings w_state t in
    walk_expr t_bindings w_state e
  | `While (e1,e2) ->
    List.fold_left (walk_expr t_bindings) w_state [e1;e2]
and walk_statement t_bindings w_state stmt = 
  match stmt with
  | `Expr e -> walk_expr t_bindings w_state e
  | `Let (_,v_type,expr) -> 
    let w_state = inst_walk_type t_bindings w_state v_type in
    walk_expr t_bindings w_state expr
  | `Declare (_,v_type) ->
    inst_walk_type t_bindings w_state v_type
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
and walk_fn w_state fn_name m_args  = 
  if Intrinsics.is_intrinsic_fn fn_name then 
    add_fn_instance w_state (fn_name,m_args)
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
(*  prerr_endline @@ "walking :" ^ fn_def.Ir.fn_name;*)
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
  constructor_fn : SSet.t;
  fail_inst : FISet.t;
  w_state : walk_state
}


let walk_public_fn fn_def f_state m_args = 
  let f_inst = fn_def.fn_name,m_args in
  if FISet.mem f_inst f_state.w_state.public_fn then
    f_state
  else if FISet.mem f_inst f_state.fail_inst then
    f_state
  else
    try
      let w_state = f_state.w_state in
      let t_binding = Types.type_binding fn_def.Ir.fn_tparams m_args in
      let w_state = add_public_fn w_state f_inst in
      let mono_ret_type = (TypeUtil.to_monomorph t_binding fn_def.Ir.ret_type) in
      let w_state = match mono_ret_type with
        | #simple_type -> w_state (* we don't need to track this crap at runtime *)
        | _ -> add_public_type w_state mono_ret_type 
      in
      let w_state' = walk_fn_def w_state fn_def m_args in
      { f_state with w_state = w_state' }
    with 
    | ResolutionFailed _ -> { f_state with fail_inst = FISet.add f_inst f_state.fail_inst }
    | TypeUtil.TyResolutionFailed -> {
        f_state with fail_inst = FISet.add f_inst f_state.fail_inst
      }

let get_inst t_set fn_def = 
    let arg_types = List.map snd fn_def.Ir.fn_args in
    match TypeUtil.get_inst t_set fn_def.Ir.fn_tparams arg_types with
    | `Inst t_bindings -> 
(*      prerr_endline @@  "found instantiation for " ^ fn_def.Ir.fn_name;
      List.iter (fun t_binding ->
          let to_print = "{ " ^ (String.concat ", " @@ List.map (fun (t_name,t) ->
              t_name ^ " -> " ^ (Types.pp_t (t : Types.mono_type :> Types.r_type))
                ) t_binding) ^ "}" in
          prerr_endline to_print
        ) t_bindings;*)
      Some (List.map (fun t_binding ->
          List.map (t_binding |> rev_app List.assoc) fn_def.Ir.fn_tparams
        ) t_bindings)
    | `Mismatch -> None

let has_inst w_state f_name = 
  FISet.exists (fun (f_name',_) -> 
      f_name = f_name') w_state.fn_inst

let pattern_list = ref [Str.regexp ""];;

let rec find_fn find_state = 
  let old_size = state_size find_state.w_state in
  let find_state = Env.EnvMap.fold (fun fn_name fn_def f_state ->
      if not (List.exists (fun patt ->
          Str.string_match patt fn_name 0
        ) !pattern_list) then
        f_state
      else if (SSet.mem fn_name f_state.constructor_fn) && has_inst f_state.w_state fn_name then
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
    find_fn find_state

let build_nopub_fn () = 
  Env.EnvMap.fold (fun fn_name fn_def accum ->
      match fn_def.Ir.fn_args with
      | ("self",t)::_ -> begin
          match t with
          | `Ref (_,`Adt_type a)
          | `Ref_Mut (_,`Adt_type a)
          | `Adt_type a when Env.EnvSet.mem Env.type_infr_filter a.Types.type_name ->
            SSet.add fn_def.Ir.fn_name accum
          | _ -> accum
        end
      | _ -> accum
    ) Env.fn_env @@ SSet.singleton "crust_abort"

let run_analysis () = 
  let constructor_fn = find_constructors () |> SSet.add "crust_init" in
  let restrict_fn = Env.EnvMap.fold (fun type_name type_def accum ->
      match type_def with
      | `Enum_def ({ drop_fn = Some df; _ } : Ir.enum_def) ->
        SSet.add df accum
      | `Struct_def ({ drop_fn = Some df; _ } : Ir.struct_def)->
        SSet.add df accum
      | _ -> accum
    ) Env.adt_env @@ build_nopub_fn ()
  in
  (*let crust_init_patt = Str.regexp "crust_init$" in
  let found_init_fn = Env.EnvMap.fold (fun fn_name _ accum ->
      try
        let _ = Str.search_forward crust_init_patt fn_name 0 in
        fn_name::accum
      with Not_found -> accum
    ) Env.EnvMap.fn_name in
  let (init_fn,has_init) = 
    match found_init_fn with
    | [] -> ("",false)
    | [h] -> (h,true)
    | _ -> failwith @@ "troublingly many init functions found! " ^ (String.concat ", " found_init_fn)
  in*)
  let (init_def,seed_types) =
    if !Env.init_opt && not (Env.EnvMap.mem Env.fn_env "crust_init") then (None,[])
    else begin
      let crust_init_def = Env.EnvMap.find Env.fn_env "crust_init" in
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
    public_fn = FISet.empty
  }
  in
  let w_state = 
    match init_def with
    | None -> init_state
    | Some crust_init_def -> walk_fn_def init_state crust_init_def []
  in
  (find_fn {
    restrict_fn = restrict_fn;
    constructor_fn = constructor_fn;
    w_state = w_state;
    fail_inst = FISet.empty
  }).w_state

(* borrow analysis *)
let indexed_fold_left f accum l = 
  let rec fold_loop i accum l = 
    match l with
    | [] -> accum
    | h::t -> 
      let new_accum = f i accum h in
      fold_loop (succ i) new_accum t
  in
  fold_loop 0 accum l


type borrow_nested = [
  | `Tuple of int  * borrow_nested
  | `MutableBorrow of int
  | `ImmutableBorrow of int
]

type borrow_info = [
  | borrow_nested
  | `NoBorrow
]

let find_lifetime ty =
  let rec find_lifetime_loop path nested = function
    | `Abstract _ -> []
    | `Ref_Mut (l,t) ->
      (l,nested,true,path)::(find_lifetime_loop path true t)
    | `Ref (l,t) -> 
      (l,nested,false,path)::(find_lifetime_loop path true t)
    | `Ptr_Mut t
    | `Ptr t ->
      find_lifetime_loop path nested t
    | #Types.simple_type -> []
    | `Bottom -> []
    | `T_Var _ -> []
    | `Adt_type p ->
      let new_nested = nested || (p.Types.lifetime_param <> []) in
      let new_accum = List.flatten @@ List.map (find_lifetime_loop path new_nested) p.Types.type_param in
      let l = List.map (fun a_lifetime -> (a_lifetime,nested,false,path)) p.Types.lifetime_param in
      l @ new_accum
    | `Tuple tl ->
      indexed_fold_left (fun i accum t ->
          (find_lifetime_loop (i::path) nested t) @ accum
        ) [] tl
  in
  find_lifetime_loop [] false ty

let arg_borrow_info = 
  let make_borrow mut ind = 
    if mut then
      `MutableBorrow ind
    else
      `ImmutableBorrow ind
  in
  fun i accum ty ->
    let extracted_lifetimes = find_lifetime ty in
    match extracted_lifetimes with
    | [] -> accum
    | l -> 
      accum @ (List.map (fun (lifetime,nested,mut,path) ->
          match path with
          | [] -> 
            (lifetime,(nested,(make_borrow mut i)))
          | root::rest ->
            let lifetime_info = List.fold_left (fun accum ind ->
                `Tuple (ind,accum)
              ) (make_borrow mut root) rest
            in
            (lifetime,(nested,(`Tuple (i,lifetime_info))))
        ) l)

let assoc_all a l = 
  List.fold_left (fun accum (key,v) ->
      if key = a then
        v::accum
      else
        accum
    ) [] l

(* To make modelling the borrow checker in C tractable the analysis
   imposes the following constraints:
   - Lifetimes cannot be nested (no &'a &'b T OR &'a &'a T)
   - Lifetimes cannot be on multiple arguments
   - Return types must have one or zero lifetimes

   Here are some allowed lifetime usages:
   - &(int,int)
   - Foo<'a, T>
   - &int
   Here are some disallowed lifetime usages:
   - &(int,&int)
   - Foo<'a,Bar<'b,T>>
   - & &int
*)

let borrow_analysis fn_def = 
  let lifetime_mapping = indexed_fold_left arg_borrow_info [] @@ List.map snd fn_def.Ir.fn_args in
  let return_lifetimes = find_lifetime fn_def.Ir.ret_type in
  if (List.length return_lifetimes) > 1 then
    failwith "Unsupported shape of return type!"
  else if (List.length return_lifetimes) = 0 then
    `NoBorrow
  else begin
    let (l_param,_,_,_) = List.hd return_lifetimes in
    let mapped_args = assoc_all l_param lifetime_mapping in
    if (List.length mapped_args) > 1 then
      failwith "Multiple borrows not supported"
    else if (List.length mapped_args) = 0 then
      failwith "No lifetime found!"
    else if (fst @@ List.hd mapped_args) then
      failwith "Borrowing of nested params not supported"
    else
      snd @@ List.hd mapped_args
  end

type move_info = [
  | `Move_val of int
  | `Move_tuple of int * (move_info list)
]

let rec move_analysis m_args =
  let rec move_aux i accum r = 
    match r with
    | [] -> accum
    | t::rest ->
      if TypeUtil.is_move_type t then
        let m = 
          match t with 
          | `Tuple tl -> `Move_tuple (i,(move_analysis tl))
          | _ -> `Move_val i
        in
        move_aux (succ i) (m::accum) rest
      else
        move_aux (succ i) accum rest
  in
  move_aux 0 [] m_args


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
  pattern_list := List.map (fun patt ->
      let patt = Str.global_replace (Str.regexp_string "*") ".+" patt in
      let patt = "^" ^ patt ^ "$" in
      Str.regexp patt
    ) patterns
