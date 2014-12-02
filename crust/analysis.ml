type inst = string * (Types.mono_type list)

module TypeInstantiation = struct
	type t = inst
	let compare = Pervasives.compare
  end
							 
module FunctionInstantiation = struct
	type t = inst
	let compare = Pervasives.compare
  end

module MonoType = struct
	type t = Types.mono_type
	let compare = Pervasives.compare
end

(* total laziness! *)
open Types
open Ir
								 
module FISet = Set.Make(FunctionInstantiation)
module TISet = Set.Make(TypeInstantiation)
module MTSet = Set.Make(MonoType)

(* function composition (not monads)
 * WARNING: abuse of syntax ;)
 *)
let (>>=) f g = fun x ->
  g (f x)

let rev_app f = fun x y -> f y x;;

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
  walk_type_def w_state (Hashtbl.find Env.adt_env t_name) m_params 
and walk_type_def w_state adt_def m_params =
  let gen_binding = fun t_names -> Types.type_binding t_names m_params in
  match adt_def with
  | `Struct_def sd -> 
	 let new_bindings = gen_binding sd.s_tparam in
	 let a = (snd >>= (Types.to_monomorph new_bindings) >>= (rev_app walk_type)) in
	 let fold = rev_app a in
	 List.fold_left fold w_state sd.struct_fields
  | `Enum_def ed ->
	 let new_bindings = gen_binding ed.e_tparam in
	 let m = List.map (fun v -> List.map (Types.to_monomorph new_bindings) v.variant_fields) ed.variants in
	 List.fold_left (List.fold_left walk_type) w_state m 
					
let inst_walk_type t_bindings w_state t = 
  let m_type = Types.to_monomorph t_bindings t in
  walk_type w_state m_type

let rec walk_expr t_bindings w_state (expr : Ir.expr) = 
  let w_state = inst_walk_type t_bindings w_state (fst expr) in
  match snd expr with
  | `Block (stmt,e)
  | `Unsafe (stmt,e) ->
	 let w_state = (List.fold_left (walk_statement t_bindings) w_state stmt) in
	 walk_expr t_bindings w_state e
  | `Call (fn_name,_,t_params,args) ->
	 let m_args = List.map (Types.to_monomorph t_bindings) t_params in
	 let w_state = List.fold_left (walk_expr t_bindings) w_state args in
	 let w_state = List.fold_left walk_type w_state m_args in
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
and walk_statement t_bindings w_state stmt = 
  match stmt with
  | `Expr e -> walk_expr t_bindings w_state e
  | `Let (_,v_type,expr) -> 
	 let w_state = inst_walk_type t_bindings w_state v_type in
	 walk_expr t_bindings w_state expr
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
  walk_fn_def w_state (Hashtbl.find Env.fn_env fn_name) m_args
and walk_fn_def w_state fn_def m_args = 
  let f_inst = fn_def.fn_name,m_args in
  if FISet.mem f_inst w_state.fn_inst then
	w_state
  else
	let t_bindings = Types.type_binding fn_def.Ir.fn_tparams m_args in
	let ret_type = Types.to_monomorph t_bindings fn_def.Ir.ret_type in
	let w_state = add_fn_instance w_state f_inst in
	let w_state =  walk_type w_state ret_type in
	walk_expr t_bindings w_state fn_def.Ir.fn_body

module SSet = Set.Make(String)

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

let rec type_contains src_types target  = 
  match target with
  | `Adt_type { Types.type_param = tl; _ }
  | `Tuple tl -> 
	 let contains = List.map (type_contains_loop src_types) tl in
	 List.exists (fun x -> x) contains
  | _ -> false

let find_constructors () = 
  let accum = SSet.empty in
  Hashtbl.fold (fun f_name fn_def accum ->
				let arg_types = List.map snd fn_def.Ir.fn_args in
				let ret_type = fn_def.Ir.ret_type in
				if type_contains arg_types ret_type  then
				  SSet.add f_name accum
				else accum
			   ) Env.fn_env accum

type type_binding = (string * Types.mono_type) list

type match_result = [
  | `Bind of type_binding list
  | `Match
  | `Mismatch
  ]

type inst_result = [
  | `Mismatch
  | `Inst of type_binding list
  ]

type _ query_param = 
  | Tuple : int -> (Types.mono_type list) query_param
  | Mut_Ptr : Types.mono_type query_param
  | Ptr : Types.mono_type query_param
  | Adt : string -> (Types.mono_type list) query_param

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
	|> add (`Int 32)
	|> add (`UInt 32)
	|> add `Bool
	|> add `Unit
  );;

(* I originally wrote this class because I had this idea that
 sub-classes would fill in the handling of certain types.

This didn't pan out and all these methods could be made into
 functions, but I like them packaged up like this *)
class type_matcher = object(self)
  method get_inst t_set m = 
	self#match_types (new set_match_state true t_set) [] m
  method match_types = fun match_state t_binding to_match ->
	self#p_match_types match_state t_binding [] to_match
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
	| #simple_type when state#synth_types -> `Match
	| #simple_type as s -> 
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
end


let state_size w_state = 
  (MTSet.cardinal w_state.public_type) +
	(FISet.cardinal w_state.public_fn)

let walk_public_fn fn_def w_state m_args = 
  let f_inst = fn_def.fn_name,m_args in
  if FISet.mem f_inst w_state.public_fn then
	w_state
  else
	let t_binding = Types.type_binding fn_def.Ir.fn_tparams m_args in
	let w_state = add_public_fn w_state f_inst in
	let mono_ret_type = (Types.to_monomorph t_binding fn_def.Ir.ret_type) in
	let w_state = match mono_ret_type with
	  | #simple_type -> w_state (* we don't need to track this crap at runtime *)
	  | _ -> add_public_type w_state mono_ret_type 
	in
	walk_fn_def w_state fn_def m_args

let get_inst = 
  let matcher = new type_matcher in
  fun t_set fn_def ->
  let arg_types = List.map snd fn_def.Ir.fn_args in
  match matcher#get_inst t_set arg_types with
  | `Inst t_bindings -> 
	 Some (List.map (fun t_binding ->
			   List.map (t_binding |> rev_app List.assoc) fn_def.Ir.fn_tparams
			  ) t_bindings)
  | `Mismatch -> None

let has_inst w_state f_name = 
  FISet.exists (fun (f_name',_) -> 
				f_name = f_name') w_state.fn_inst

let rec find_fn constructor_fn w_state = 
  let old_size = state_size w_state in
  let w_state = Hashtbl.fold (fun fn_name fn_def w_state ->
							  if (SSet.mem fn_name constructor_fn) &&
								   has_inst w_state fn_name then
								w_state
							  else begin
								  match get_inst w_state.public_type fn_def with
								  | None -> w_state
								  | Some inst_list ->
									 List.fold_left (walk_public_fn fn_def) w_state inst_list
								end
							 ) Env.fn_env w_state in
  if old_size = (state_size w_state) then
	w_state
  else
	find_fn constructor_fn w_state

let run_analysis () = 
  let constructor_fn = find_constructors () |> SSet.add "crust_init" in
  let crust_init_def = Hashtbl.find Env.fn_env "crust_init" in
  let init_state = {
	  type_inst = TISet.empty;
	  fn_inst = FISet.empty;
	  public_type = MTSet.empty;
	  public_fn = FISet.empty
	}
  in
  (match crust_init_def.Ir.fn_tparams with
  | [] -> ()
  | _ -> failwith "crust_init cannot be polymorphic");
  let seed_types = 
	match crust_init_def.Ir.ret_type with
	| `Tuple tl -> List.map (Types.to_monomorph []) tl
	| t -> failwith @@ "crust_init must return a tuple, found: " ^ (Types.pp_t t)
  in
  let w_state = {
	  init_state with public_type = List.fold_right MTSet.add seed_types init_state.public_type
	}
  in
  let with_init_state = walk_fn_def w_state crust_init_def [] in
  find_fn constructor_fn with_init_state

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
