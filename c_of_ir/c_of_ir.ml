(*
-L /opt/rust/lib/rustlib/x86_64-unknown-linux-gnu/lib/	
-A warnings
--crate-type lib
input.rs
 *)

(*
let rec (adt_type_name : mono_type -> string) = function
  | `Adt_type a -> mangle_adt_name a
  | `Ref (_,t) -> (type_name t) ^ "_ptr"
  | `Ref_Mut (_,t) -> "const_" ^ (type_name t) ^ "_ptr"
  | `Int _ -> "int"
  | `UInt _ -> "uint"
  | `Bool -> "int"
  | `Unit -> "void"
and mangle_adt_name t = 
  if t.type_param = [] then
	t.type_name ^ "_t"
  else
	t.type_name ^ 
	  (String.concat "_" (List.map adt_type_name t.type_param))
	  ^ "_t"
and mangle_fn_name fn_name mono_args = 
  match mono_args with
  | [] -> fn_name
  | _ -> fn_name ^ (String.concat "_" (List.map adt_type_name mono_args))*)

module Compilation = struct
	type r_type = Types.r_type
	type simple_type = Types.simple_type
	type lifetime = Types.lifetime
	let rec instrument_return = function
	  | `Unsafe (s,e) ->
		 `Unsafe (s,(instrument_return e))
	  | `Block (s,e) ->
		 `Block (s,(instrument_return e))
	  | `Return e -> `Return e
	  | e -> `Return e
	type simple_expr = [
	  | `Struct_Field of simple_expr * string
	  | `Var of string
	  | `Literal of string
	  | `Deref of t_simple_expr
	  | `Address_of of t_simple_expr
	  | `Call of string * (lifetime list) * (r_type list) * (t_simple_expr list)
	  | `Return of t_simple_expr
	  | `Assignment of simple_expr * t_simple_expr
	  | `BinOp of Ir.bin_op * t_simple_expr * t_simple_expr
	  | `UnOp of Ir.un_op * t_simple_expr * t_simple_expr
	  ]
	 and t_simple_expr = r_type * simple_expr
	 and 'a complex_expr = [
	   | `Block of ('a stmt list) * 'a
(*	   | `Unsafe of ('a stmt list) * 'a*)
	   | `Match of simple_expr * ('a match_arm list)
	   ]
	 and struct_fields = struct_field list
	 and struct_field = string * t_simple_expr (* field binding *)
	 and 'a stmt = [
	   | `Expr of 'a
	   | `Let of string * r_type * t_simple_expr
	   | `Declare of string * r_type
	   ]
	and 'a match_arm = (simple_expr * 'a)
	and pattern = [
	  | `Bind of r_type * string
	  | `Enum of string * Types.variant_name * int * (pattern list)
	  | `Wild
	  ]
	type all_expr = r_type * [ all_expr complex_expr | simple_expr ]
	type all_complex = all_expr complex_expr
	type r_all_expr = [ all_expr complex_expr | simple_expr ]
	exception Break of all_expr
	let is_complex (_,e) = match e with
	  | #complex_expr -> true
	  | _ -> false
	let counter = ref 0;;
	let fresh_temp () = 
	  let new_id = !counter in
	  counter := !counter + 1;
	  Printf.sprintf "__temp_%d" new_id
	let rec push_assignment (lhs : simple_expr) (e : all_expr) = 
	  match (snd e) with
	  | #simple_expr as s -> 
		 let assign = `Assignment (lhs,((fst e),s)) in
		 ((`Unit,assign) : all_expr)
	  | `Match (e,m_arms) ->
		 let m_arms' = List.map (fun (patt,m_arm) -> 
								 (patt,(push_assignment lhs m_arm))
								) m_arms in
		 (`Unit,`Match (e,m_arms'))
	  | `Block (s,e) -> (`Unit,(`Block (s,(push_assignment lhs e))))
(*	  | `Unsafe (s,e) ->
		 (`Unit,(`Unsafe (s,(push_assignment lhs e))))*)
	let (lift_complex : all_expr complex_expr -> (string * all_expr)) = fun expr ->
	  match expr with
	  | `Block (s,e) 
(*	  | `Unsafe (s,e)*) ->
		 let out_var = fresh_temp () in
		 let e' = push_assignment (`Var out_var) e in
		 (out_var,(`Unit,`Block (s,e')))
	  | `Match (e,m_arms) ->
		 let out_var = fresh_temp () in
		 let m_arms' = List.map (fun (patt,m_arm) -> (patt,(push_assignment (`Var out_var) m_arm))) m_arms in
		 (out_var,(`Unit,`Match (e,m_arms')))
	let rec apply_lift_cb : 'a. Ir.expr -> (all_expr stmt list -> t_simple_expr -> 'a) -> 'a = 
	  fun expr cb ->
	  let expr' = simplify_ir expr in
	  let e_type = fst expr' in
	  match (snd expr') with
	  | #simple_expr as s -> cb [] (e_type,s)
	  | #all_complex as c ->
		 let (out_var,lifted) = lift_complex c in
		 let declaration = `Declare (out_var,e_type) in
		 let assign_block = `Expr lifted in
		 let replacement = e_type,(`Var out_var) in
		 cb [declaration ; assign_block] replacement
	and apply_lift e_type sub_expr cb =
	  apply_lift_cb sub_expr (fun stmt out_var -> 
							  let op = cb out_var in
							  let block_e = (e_type,op) in
							  let block = `Block (stmt,block_e) in
							  (e_type,block)
							 )
	and simplify_adt : 'a. r_type -> 'a list -> ?post:(t_simple_expr -> all_expr list) -> (t_simple_expr -> int -> 'a -> simple_expr) -> (int -> 'a -> Ir.expr) -> all_expr = fun e_type components ?(post=(fun _ -> [])) lhs rhs ->
	  let out_var = fresh_temp () in
	  let adt_var = e_type,`Var out_var in
	  let declare = `Declare (out_var,e_type) in
	  let stmts = List.mapi (fun i comp ->
							 let assign_lhs = lhs adt_var i comp in
							 let e = rhs i comp in
							 apply_lift_cb e (fun stmts e' ->
											  let assignment = `Assignment (assign_lhs,e') in
											  stmts @ [`Expr (`Unit,assignment)]
											 )
							) components in
	  let post_stmts = List.map (fun e -> `Expr e) (post adt_var) in
	  let stmts' = (declare :: (List.flatten stmts)) @ post_stmts in
	  (e_type,(`Block (stmts',adt_var)))
	and (simplify_ir : Ir.expr -> all_expr) = fun expr ->
	  match (snd expr) with
	  | `Call (f_name,l,t,args) ->
		 let (s,args') = List.fold_right 
							(fun arg (s_accum,arg_accum) ->
							 apply_lift_cb arg (fun stmt a -> (stmt @ s_accum,a::arg_accum))
							) args ([],[]) in
		 if s = [] then
		   (fst expr,`Call (f_name,l,t,args'))
		 else
		   (fst expr,`Block (s,(fst expr,`Call (f_name,l,t,args'))))
	  | `Address_of t -> 
		 apply_lift (fst expr) t (fun e -> `Address_of e)
	  | `Deref t -> 
		 apply_lift (fst expr) t (fun e -> `Deref e)
	  | `Var s -> (fst expr,(`Var s))
	  | `Literal s -> (fst expr,`Literal s)
	  | `Return r -> apply_lift (fst expr) r (fun e -> `Return e)
	  | `Struct_Field (s,f) -> apply_lift (fst expr) s (fun e -> `Struct_Field (snd e,f))
	  | `Tuple t_fields ->
		 let tuple_type = fst expr in
		 let lhs = fun adt_var f_index _ ->
		   `Struct_Field ((snd adt_var),(Printf.sprintf "field%d" f_index))
		 in
		 let rhs = fun _ f -> f in
		 simplify_adt tuple_type t_fields lhs rhs
	  | `Struct_Literal s_fields ->
		 let struct_type = fst expr in
		 let lhs = fun adt_var _ (f,_) ->
		   `Struct_Field (snd adt_var,f)
		 in
		 let rhs = fun _ (_,e) -> e in
		 simplify_adt struct_type s_fields lhs rhs
	  | `Enum_Literal (_,tag,exprs) ->
		 let lhs = fun adt_var f_index _ ->
		   let data = `Struct_Field (snd adt_var,"data") in
		   let tag_field = `Struct_Field (data,(Printf.sprintf "tag%d" tag)) in
		   let e_field = `Struct_Field (tag_field,(Printf.sprintf "field%d" tag)) in
		   e_field
		 in
		 let rhs = fun _ e -> e in
		 let post = fun adt_var ->
		   let tag_rhs = (`Int 4,(`Literal (string_of_int tag))) in
		   let discriminant_field = `Struct_Field (snd adt_var,"discr") in
		   let assignment = `Assignment (discriminant_field,tag_rhs) in
		   [(`Unit,assignment)] in
		 simplify_adt (fst expr) exprs ~post:post lhs rhs
	  | `Unsafe (s,e) 
	  | `Block (s,e) ->
		 let b_type = fst e in
		 let stmt_frag  = List.flatten (List.map simplify_stmt s) in
		 apply_lift_cb e (fun stmt e' ->
						  let all_stmt = stmt_frag @ stmt in
						  let block = `Block (all_stmt,(e' :> all_expr)) in
						  (b_type,block))
	and simplify_stmt : Ir.stmt -> all_expr stmt list = function
	  | `Let (v_name,v_type,expr) ->
		 let expr' = simplify_ir expr in
		 let e_type = fst expr' in
		 begin
		   match (snd expr') with
		   | #simple_expr as s -> [`Let (v_name,v_type,(e_type,s))]
		   | #complex_expr as c -> 
			  let c' = push_assignment (`Var v_name) (e_type,c) in
			  [`Declare (v_name,v_type); `Expr c']
		 end
	  | `Expr e ->
		 let e' = simplify_ir e in
		 [`Expr e']
(*		 apply_lift (fst expr) e (fun e' -> `Block ([],(e' :> all_expr)))*)
(*	let compile_fn_inst (f_name,mono_args) = 
	  let fn_def = Hashtbl.find fn_env f_name in
	  let bindings = List.map2 (fun t_name t_val -> (t_name,t_val)) fn_def.fn_tparams mono_args in
	  let f_name = mangle_fn_name fn_def.fn_name mono_args in
	  let buf = Buffer.create 100 in
	  let ret_type = to_monomorph bindings fn_def.ref_type in
	  Buffer.add_string buf (string_of_type (ret_type));
	  Buffer.add_string buf " ";
	  Buffer.add_string buf f_name;
	  Buffer.add_string buf "(";
	  let dump_arg (arg_name, arg_type) = 
		Buffer.add_string buf (string_of_type (to_monomorph bindings arg_type));
		Buffer.add_string buf " ";
		Buffer.add_string buf arg_name
	  in
	  let rec arg_dump_loop = function ->
									 | [] -> ()
									 | a::[] -> dump_arg a
									 | h::t -> dump_arg h; Buffer.add_string buf ","; arg_dump_loop t
	  in
	  arg_dump_loop fn_def.fn_args;
	  Buffer.add_string buf "{\n";
	  let to_compile = 
		if ret_type = `Unit then 
		  fn_def.fn_body 
		else 
		  instrument_return fn_def.fn_body
	  in
	  compile_expr buf bindings 1 to_compile;
	  Buffer.add_string buf "}\n";
	  *)
end

