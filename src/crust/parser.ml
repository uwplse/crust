let slurp_file close_chan f = 
  let buf = Buffer.create 100 in
  let rec loop () = 
	try
	  let l = input_line f in
	  if (String.length l) == 0 then
		loop ()
	  else if l.[0] = '#' then
		loop ()
	  else (Buffer.add_string buf l; 
			Buffer.add_string buf " "; 
			loop ())
	with End_of_file -> (
	  (if close_chan then close_in f else ()); 
	  Buffer.contents buf
	)
  in
  loop ();;

let split_regex = "[][{}<> \t;:()]";;

exception Parse_failure of (string * (string list));;
exception Unexpected_stream_end of string

let consume_to_end tokens parse_fn cb = 
  let rec consume_loop t accum =
	match t with
	| [] -> cb (List.rev accum)
	| _ -> parse_fn t (fun item rest -> 
     consume_loop rest (item::accum))
  in
  consume_loop tokens []

let consume_lifetime l cb = 
  match l with
  | h::t -> cb (h : Types.lifetime) t
  | _ -> raise (Unexpected_stream_end "consume_lifetime")

let parse_n parse_fn l cb = 
  let rec parse_loop i n t accum = 
	if i == n then
	  cb (List.rev accum) t
	else
	  parse_fn t (fun e rest -> parse_loop (succ i) n rest (e::accum))
  in
  let n_items = 
	try
	  (int_of_string (List.hd l)) 
	with Failure _ -> raise (Parse_failure ("parse_n",l))
  in
  parse_loop 0 n_items (List.tl l) []

let maybe_parse parse_fn tokens cb = 
  match tokens with
  | "1"::t -> parse_fn t (fun a rest ->
						  cb (Some a) rest
						 )
  | "0"::t -> cb None t
  | _ -> raise (Parse_failure ("maybe_parse",tokens))

let parse_lifetimes tokens cb = 
  parse_n consume_lifetime tokens cb

let consume_type_param l cb = 
  match l with
  | h::t -> 
	 cb (h : Types.type_param) t
  | _ -> raise (Unexpected_stream_end "consume_type_param")

let parse_type_params tokens cb = 
  parse_n consume_type_param tokens cb

type tokens = string list

let (>>) a b = 
  (fun tokens cb ->
   a tokens (fun a_res tokens' ->
			 b tokens' (fun b_res tokens'' ->
						cb (a_res,b_res) tokens''
					   )
			)
  )

let consume_name tokens cb = 
  cb (List.hd tokens) (List.tl tokens)
let consume_word = consume_name

let consume_int tokens cb = match tokens with
  | h::t -> 
	 begin
	   try 
		 cb (int_of_string h) t
	   with Failure _ -> raise @@ Parse_failure ("consume_int",tokens)
	 end
  | _ -> raise (Unexpected_stream_end "consume_int")

let parse_int_size tokens cb = match tokens with
  | "size"::t -> cb `Ptr_Size t
  | _ -> consume_int tokens (fun b -> cb (`Bit_Size b))

let parse_simple_type tokens cb = match tokens with
  | "unit"::t -> cb `Unit t
  | "int"::t -> 
    parse_int_size t (fun size -> cb (`Int size))
  | "uint"::t -> 
    parse_int_size t (fun size -> cb (`UInt size))
  | "bool"::t -> cb `Bool t
  | "var"::t_var::t -> cb (`T_Var t_var) t
  | "bottom"::t -> cb `Bottom t
  | "float"::w::t -> cb (`Float (int_of_string w)) t
  | "char"::t -> cb (`Char) t
  | _ -> raise (Parse_failure ("parse_simple_type", tokens))

let rec parse_adt_type tokens cb = match tokens with
  | "adt"::name::t -> 
	 let p_fun = (parse_lifetimes) >> (parse_n parse_type) in
	 p_fun t (fun (l,t) rest ->
			  cb {
				  Types.type_name = name;
				  Types.lifetime_param = l;
				  Types.type_param = t;
				} rest
			 )
  | _ -> (raise (Parse_failure ("parse_adt_type",tokens)))
and parse_type tokens cb = 
  match tokens with
  | "adt"::_ -> parse_adt_type tokens (fun a rest ->
									   cb (`Adt_type a) rest
									  )
  | "var"::t_var::rest -> cb (`T_Var (t_var :> Types.type_param)) rest
  | "tuple"::t ->
	 (parse_n parse_type) t (fun t_types rest ->
							 cb (`Tuple t_types) rest
							)
  | "ref"::lifetime::t ->
	 parse_type t (fun r_type rest ->
				   cb (`Ref (lifetime,r_type)) rest
				  )
  | "ref_mut"::lifetime::t ->
	 parse_type t (fun r_type rest ->
				   cb (`Ref_Mut (lifetime,r_type)) rest
				  )
  | "ptr"::t ->
	 parse_type t (fun r_type rest ->
				   cb (`Ptr r_type) rest
				  )
  | "ptr_mut"::t ->
	 parse_type t (fun r_type rest ->
				   cb (`Ptr_Mut r_type) rest
				  )
  | "fn"::_ -> raise @@ Parse_failure ("unsupported function type: parse_type", tokens)
  | "abstract"::name::t ->
    let parse_fn = parse_lifetimes >> (parse_n parse_type) in
    parse_fn t (fun (lifetimes, types) ->
        cb (`Abstract {
            Types.a_name = name;
            Types.a_lifetimes = lifetimes;
            Types.a_params = types
        })
      )
  | "vec"::rest ->
    parse_type rest (fun t ->
        cb (`Vec t)
      )
  | "str"::rest ->
    cb `Str rest
  | "fixed_vec"::rest ->
    (consume_int >> parse_type) rest (fun l ->
        cb (`Fixed_Vec l)
      )
  | _ -> parse_simple_type tokens cb

let parse_binop tokens cb = match tokens with
	| "BiAdd"::t -> cb `BiAdd t
	| "BiSub"::t -> cb `BiSub t
	| "BiMul"::t -> cb `BiMul t
	| "BiDiv"::t -> cb `BiDiv t
	| "BiRem"::t -> cb `BiRem t
	| "BiAnd"::t -> cb `BiAnd t
	| "BiOr"::t -> cb `BiOr t
	| "BiBitXor"::t -> cb `BiBitXor t
	| "BiBitAnd"::t -> cb `BiBitAnd t
	| "BiBitOr"::t -> cb `BiBitOr t
	| "BiShl"::t -> cb `BiShl t
	| "BiShr"::t -> cb `BiShr t
	| "BiEq"::t -> cb `BiEq t
	| "BiLt"::t -> cb `BiLt t
	| "BiLe"::t -> cb `BiLe t
	| "BiNe"::t -> cb `BiNe t
	| "BiGe"::t -> cb `BiGe t
	| "BiGt"::t -> cb `BiGt t
	| [] -> raise (Unexpected_stream_end "parse_binop")
	| b::_ -> failwith @@ "bad biop " ^ b
let parse_unop tokens cb = match tokens with
  | "UnNot"::t -> cb `UnNot t
  | "UnNeg"::t -> cb `UnNeg t
  | "UnDeref"::t -> cb `UnDeref t
  | [] -> raise (Unexpected_stream_end "parse_unop")
  | u::_ -> failwith @@ "bad unop " ^ u

let rec parse_patt tokens cb = (parse_type >> parse_patt_variant) tokens cb
and parse_patt_variant tokens cb = 
  match tokens with
  | "var"::name::t ->
	 cb (`Bind name) t
  | "const"::name::t ->
	 cb (`Const name) t
  | "wild"::t ->
	 cb `Wild t
  | "simple_literal"::w::t ->
	 cb (`Literal w) t
  | "tuple"::t ->
	 (parse_n parse_patt) t (fun tl rest ->
							 cb (`Tuple tl) rest
							)
  | "enum"::variant_name::variant_tag::t ->
	 (parse_n parse_patt) t (fun patts rest ->
							 cb (`Enum (variant_name,(int_of_string variant_tag),patts)) rest
							)
  | "addr_of"::t ->
    parse_patt t (fun p ->
        cb (`Addr_of p)
      )
  | "ref_var"::name::t ->
    cb (`Ref name) t
  | _ -> raise (Parse_failure ("parse_patt_variant",tokens))
		   
let rec parse_expr_var tokens cb = match tokens with
  | "var"::n::rest -> cb (`Var n) rest
  | "simple_literal"::l::rest -> cb (`Literal l) rest
  | "struct_literal"::t ->
	 parse_n (consume_name >> parse_expr) t (fun fields rest ->
											 cb (`Struct_Literal fields) rest
											)
  | "enum_literal"::v_name::v_index::t ->
	 let i_v_index = int_of_string v_index in
	 parse_n parse_expr t (fun e rest ->
						   cb (`Enum_Literal (v_name,i_v_index,e)) rest
						  )
  | "tuple_literal"::t ->
	 parse_n parse_expr t (fun el rest ->
						   cb (`Tuple el) rest
						  )
  | "match"::t ->
	 (parse_expr >> (parse_n parse_match_arm)) t (fun m rest ->
												  cb (`Match m) rest
												 )
  | "block"::t ->
	 let p_fun = (parse_n parse_stmt) >> parse_expr in
	 p_fun t (fun b rest -> cb (`Block b) rest)
  | "field"::t ->
	 (parse_expr >> consume_name) t (fun f rest -> cb (`Struct_Field f) rest)
  | "deref"::t ->
	 parse_expr t (fun e rest -> cb (`Deref e) rest)
  | "addr_of"::t ->
	 parse_expr t (fun e rest -> cb (`Address_of e) rest)
  | "cast"::t ->
	 parse_expr t (fun c rest -> cb (`Cast c) rest)
  | "binop"::t ->
	 (parse_binop >> parse_expr >> parse_expr) t (fun ((op,lhs),rhs) rest ->
												  cb (`BinOp (op,lhs,rhs)) rest
												 )
  | "unop"::t ->
	 (parse_unop >> parse_expr) t (fun uo rest -> cb (`UnOp uo) rest)
  | "call"::f_name::t ->
	 let p_fun = (parse_lifetimes >> (parse_n parse_type) >> (parse_n parse_expr)) in
	 p_fun t (fun ((l,t_inst),args) rest ->
			  cb (`Call (f_name,l,t_inst,args)) rest
			 )
  | "unsafe"::t ->
	 let p_fun = ((parse_n parse_stmt) >> parse_expr) in
	 p_fun t (fun u rest -> cb (`Unsafe u) rest)
  | "assign"::t ->
	 (parse_expr >> parse_expr) t (fun a rest -> cb (`Assignment a) rest)
  | "return"::t ->
	 parse_expr t (fun expr rest -> cb (`Return expr) rest)
  | "while"::t ->
    (parse_expr >> parse_expr) t (fun w rest ->
        cb (`While w) rest
      )
  | "assign_op"::t ->
    (parse_binop >> parse_expr >> parse_expr) t (fun ((op,lhs),rhs) rest ->
        cb (`Assign_Op (op,lhs,rhs)) rest
      )
  | "vec"::t ->
    (parse_n parse_expr) t (fun e_list ->
        cb (`Vec e_list)
      )
  | _ -> raise (Parse_failure ("parse_expr_var",tokens))
and parse_stmt tokens cb = match tokens with
  | "expr"::t -> parse_expr t (fun expr rest -> cb (`Expr expr) rest)
  | "let"::t ->
	 (parse_patt >> (maybe_parse parse_expr)) t (fun ((typ,binding_pat),expr) rest ->
        match binding_pat with
        | `Bind r ->
		  cb (`Let (r,typ,expr)) rest
        | _ -> raise (Parse_failure ("Found unsupported pattern in let position",tokens))
	  )
  | _ -> raise (Parse_failure ("parse_stmt",tokens))
and parse_match_arm = fun tokens cb ->
  (parse_patt >> parse_expr) tokens cb
and parse_expr = fun tokens cb ->
  (parse_type >> parse_expr_var) tokens cb



let parse_fn =
  let arg_counter = ref 0 in
  let consume_arg = fun tokens cb ->
    parse_patt tokens (fun (p_ty,p_var) rest ->
        match p_var with
        | `Bind i -> cb (i,p_ty) rest
        | `Wild -> 
          let arg_name = "__crust_unused_" ^ (string_of_int !arg_counter) in
          incr arg_counter;
          cb (arg_name,p_ty) rest
        | _ -> raise @@ Parse_failure ("Unsupported pattern type found in argument position", tokens)
      )
  in
  let parse_args = fun tokens cb ->
	match tokens with 
	| "args"::t -> parse_n consume_arg t cb
	| _ -> (raise (Parse_failure ("parse_args",tokens)))
  in
  let parse_return tokens cb =
	match tokens with
	| "return"::rest -> parse_type rest cb
	| _ -> (raise (Parse_failure ("parse_return",tokens)))
  in
  let parse_body tokens cb = 
	match tokens with
	| "body"::rest -> parse_expr rest cb 
	| _ -> (raise (Parse_failure ("parse_body",tokens)))
  in
  let parse_impl tokens cb = 
    match tokens with
    | abstract_name::rest ->
      (parse_lifetimes >> (parse_n parse_type)) rest (fun (lifetimes,ty) ->
          let rty = List.rev ty in
          let self_type = List.hd rty in
          let t_params = List.tl rty |> List.rev in
          cb {
            Ir.abstract_name = abstract_name;
            Ir.i_types = t_params;
            Ir.i_self = self_type;
            Ir.i_lifetimes = lifetimes
          }
        )
    | _ -> raise (Parse_failure ("parse_impl", tokens))
  in
  fun tokens cb ->
    arg_counter := 0;
    match tokens with
    | "fn"::fn_name::t ->
	 let parse_function = parse_lifetimes >> parse_type_params >> parse_args >> parse_return >> (maybe_parse parse_impl) >> parse_body in
	 parse_function t (fun (((((lifetime,t_params),args),ret_type),impl_info),body) ->
	  cb (`Fn {
						 Ir.fn_name = fn_name;
						 Ir.fn_lifetime_params = lifetime;
						 Ir.fn_tparams = t_params;
						 Ir.ret_type = ret_type;
						 Ir.fn_args = args;
						 Ir.fn_body = body;
						 Ir.fn_impl = impl_info
					   })
	)
    | "abstract_fn"::fn_name::t ->
      let parse_function = parse_lifetimes >> parse_type_params >> parse_args >> parse_return in
      parse_function t (fun _ -> cb (`Abstract_Fn fn_name))
    | _ -> raise (Parse_failure ("parse_fn", tokens))

let parse_struct_def tokens cb = match tokens with
  | "struct"::name::t ->
	 let p_fun = (parse_lifetimes >> parse_type_params >> (parse_n (consume_name >> parse_type)) >> (maybe_parse consume_name)) in
	 p_fun t (fun (((lifetimes,t_params),struct_fields),drop_fn) rest ->
			  cb (`Struct_def {
				Ir.struct_name = name;
				Ir.s_lifetime_param = lifetimes;
				Ir.s_tparam = t_params;
				Ir.struct_fields = struct_fields;
				Ir.drop_fn = drop_fn
			  }) rest
			 )
  | _ -> (raise (Parse_failure ("struct_def",tokens)))

let parse_variant_def tokens cb = 
  let p_fun = (consume_name >> (parse_n parse_type)) in
  p_fun tokens (fun (v_name,fields) rest ->
				cb {
				  Ir.variant_name = v_name;
				  Ir.variant_fields = fields
				} rest
			   )

let parse_enum_def tokens cb = match tokens with
  | "enum"::name::t ->
	 (* this is what it should be? *)
	 let p_fun = (parse_lifetimes >> parse_type_params >> (parse_n parse_variant_def) >> (maybe_parse consume_name)) in
	 p_fun t (fun (((lifetimes,t_params),v_def),drop_fn) rest ->
			  cb (`Enum_def {
				  Ir.enum_name = name;
				  Ir.e_lifetime_param = lifetimes;
				  Ir.e_tparam = t_params;
				  Ir.variants = v_def;
				  Ir.drop_fn = drop_fn
				}) rest
			 )
  | _ -> assert false

let parse_assoc_type tokens cb =
  match tokens with
  | "associated_type"::rest ->
    let p_fn = 
      (parse_lifetimes) >> (parse_type_params) >>
      consume_name >> (parse_lifetimes) >> (parse_n parse_type) >>
      (parse_type) 
    in
    p_fn rest (fun (((((lifetimes,t_params),name),_),input_types),ret_type) t ->
        cb (`Assoc_type {
          Types.abstract_name = name;
          Types.ty_param = t_params;
          Types.ty_lifetimes = lifetimes;
          Types.ty_args = input_types;
          Types.ty_output = ret_type
        }) t
      )
  | _ -> (raise (Parse_failure ("parse_assoc_type", tokens)))

let parse_abstract_type tokens cb = 
  match tokens with
  | "abstract_type"::name::t ->
    let parse_fn = parse_lifetimes >> parse_type_params in
    parse_fn t (fun (lifetimes,tp) rest ->
        cb (`Abstract_Type {
            Types.a_type_name = name;
            Types.a_lifetime_params = lifetimes;
            Types.a_type_params = tp
          }) rest
      )
  | _ -> raise (Parse_failure ("parse_abstract_type",tokens))

let parse_static tokens cb = 
  match tokens with
  | "static"::t ->
    (consume_name >> parse_type >> parse_expr) t (fun ((name,ty),expr) ->
        cb (`Static (name,ty,expr))
      )
  | _ -> raise (Parse_failure ("parse_static",tokens))

let rec parse_module tokens cb = 
  match tokens with
  | "abstract_fn"::_ 
  | "fn"::_ -> parse_fn tokens cb
  | "enum"::_ -> parse_enum_def tokens cb
  | "struct"::_ -> parse_struct_def tokens cb
  | "abstract_type"::_ -> parse_abstract_type tokens cb
  | "associated_type"::_ -> parse_assoc_type tokens cb
  | "const"::name::t ->
    (parse_type >> parse_expr) t (fun _ rest ->
        parse_module rest cb
      )
  | "static"::_ -> parse_static tokens cb
  | _ -> (raise (Parse_failure ("parse_module",tokens)))

let parse_string s = 
  (* terrible memory!!!!!!! *)
  let tokens = List.filter (fun s -> s <> "") (Str.split (Str.regexp split_regex) s) in
  consume_to_end tokens parse_module (fun s -> s)

let parse_channel ?(close=true) c = 
  parse_string (slurp_file close c)

