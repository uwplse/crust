module TypeInstantiation = struct
	type t = string * (Types.mono_type list)
	let compare = Pervasives.compare
  end
							 
module FunctionInstantiation = struct
	type t = string * (Types.mono_type list)
	let compare = Pervasives.compare
  end

open Types
open Ir
								 
module FISet = Set.Make(FunctionInstantiation)
module TISet = Set.Make(TypeInstantiation)
let rec walk_type : TISet.t -> Types.mono_type -> TISet.t = fun t_set t ->
  match t with
  | `Adt_type a ->
	 let instantiation = a.type_name,a.type_param in
	 if TISet.mem instantiation t_set then
	   t_set
	 else
	   walk_type_def 
		 (TISet.add instantiation t_set) 
		 (Hashtbl.find Env.adt_env a.type_name)
		 a.type_param
  | `Ref (_,t') -> walk_type t_set t'
  | `Ref_Mut (_,t') -> walk_type t_set t'
  | `Tuple tl -> 
	 let instantiation = "__rust_tuple",tl in
	 if TISet.mem instantiation t_set then
	   t_set
	 else
	   let t_set' = TISet.add instantiation t_set in
	   List.fold_left walk_type t_set' tl
  | `Bottom -> t_set
  | `Ptr_Mut t' -> walk_type t_set t'
  | `Ptr t' -> walk_type t_set t'
  | #simple_type -> t_set
and walk_type_def t_set adt_def m_params =
  let gen_binding = fun t_names -> Types.type_binding t_names m_params in
  match adt_def with
  | `Struct_def sd -> 
	 let new_bindings = gen_binding sd.s_tparam in
	 List.fold_left (fun t_set' (_,t) -> 
					 walk_type t_set' (Types.to_monomorph new_bindings t)
					) t_set sd.struct_fields
  | `Enum_def ed ->
	 let new_bindings = gen_binding ed.e_tparam in
	 let m = List.map (fun v -> List.map (Types.to_monomorph new_bindings) v.variant_fields) ed.variants in
	 List.fold_left (List.fold_left walk_type) t_set m
					
let inst_walk_type t_bindings t_set t = 
  let m_type = Types.to_monomorph t_bindings t in
  walk_type t_set m_type
			
let rec walk_fn (t_set,f_set) fn_name m_params = 
  let fn_def = Hashtbl.find Env.fn_env fn_name in
  let bindings = Types.type_binding fn_def.fn_tparams m_params in
  let arg_types = List.map snd fn_def.fn_args in
  let fn_types = fn_def.ret_type::arg_types in
  let t_set' = List.fold_left (inst_walk_type bindings) t_set fn_types in
  walk_expr bindings (t_set',f_set) fn_def.fn_body
and walk_expr t_bindings ((t_set,f_set) as s) expr = 
  let t_set' = inst_walk_type t_bindings t_set (fst expr) in
  match snd expr with
  | `Block (stmt,e)
  | `Unsafe (stmt,e) ->
	 let s' = (List.fold_left (walk_statement t_bindings) (t_set',f_set) stmt) in
	 walk_expr t_bindings s' e
  | `Call (fn_name,_,t_params,args) ->
	 let m_args = List.map (Types.to_monomorph t_bindings) t_params in
	 let (t_set'',f_set') = List.fold_left (walk_expr t_bindings) (t_set,f_set) args in
	 let t_set''' = List.fold_left walk_type t_set'' m_args in
	 let f_instantiation = fn_name,m_args in
	 if FISet.mem f_instantiation f_set' then
	   (t_set'',f_set')
	 else
	   let f_set'' = FISet.add f_instantiation f_set' in
	   walk_fn (t_set''',f_set'') fn_name m_args
  | `Address_of e | `Deref e -> walk_expr t_bindings (t_set',f_set) e
  | `Struct_Field (e,_) -> walk_expr t_bindings (t_set',f_set) e
  | `Var a -> s
  | `Literal _ -> s
  | `Struct_Literal s_fields ->
	 List.fold_left (fun s (_,e) -> walk_expr t_bindings s e) (t_set',f_set) s_fields
  | `Enum_Literal (_, _, v_fields) ->
	 List.fold_left (walk_expr t_bindings) (t_set',f_set) v_fields
  | `Match (e,m) ->
	 let s' = List.fold_left (walk_match_arm t_bindings) (t_set',f_set) m in
	 walk_expr t_bindings s' e
  | `Return e ->
	 walk_expr t_bindings s e
  | `Assignment (e1,e2) ->
	 walk_expr t_bindings (walk_expr t_bindings s e1) e2
  | `Tuple el -> 
	 List.fold_left (walk_expr t_bindings) (t_set',f_set) el
  | `BinOp (_,e1,e2) -> 
	 let s' = (t_set',f_set) in
	 List.fold_left (walk_expr t_bindings) s' [e1;e1]
  | `UnOp (_,e1) ->
	 walk_expr t_bindings (t_set',f_set) e1
  | `Cast (e,t) -> 
	 let t_set'' = inst_walk_type t_bindings t_set' t in
	walk_expr t_bindings (t_set'',f_set) e
and walk_statement t_bindings ((t_set,f_set) as s) stmt = 
  match stmt with
  | `Expr e -> walk_expr t_bindings s e
  | `Let (_,v_type,expr) -> 
	 let t_set' = inst_walk_type t_bindings t_set v_type in
	 walk_expr t_bindings (t_set',f_set) expr
and walk_match_arm t_bindings s (patt,expr) = 
  let s' = walk_pattern t_bindings s patt in
  walk_expr t_bindings s' expr
and walk_pattern t_bindings ((t_set,f_set) as s) patt = 
  match patt with
  | `Wild -> s
  | `Literal _ -> s
  | `Bind (t,_) -> 
	 let t_set' = inst_walk_type t_bindings t_set t in
	 (t_set',f_set)
  | `Enum (_,_,_,p_list) ->
	 List.fold_left (walk_pattern t_bindings) s p_list
					
let inst_walk_fn t_bindings s f_name = 
  let fn_def = Hashtbl.find Env.fn_env f_name in
  let m_args = List.map (fun t -> List.assoc t t_bindings) fn_def.fn_tparams in
  walk_fn s f_name m_args
		  
let extract_instantiations t_binding e_points = 
  let f_set = FISet.empty in
  let t_set = TISet.empty in
  List.fold_left (inst_walk_fn t_binding) (t_set,f_set) e_points
