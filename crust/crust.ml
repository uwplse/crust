let get_sets () = 
  let t_set = Hashtbl.fold (fun t _ accum ->
							Analysis.TISet.add (t,[]) accum
						   ) Env.adt_env Analysis.TISet.empty in
  let f_set = Hashtbl.fold (fun f _ accum ->
							Analysis.FISet.add (f,[]) accum
						   ) Env.fn_env Analysis.FISet.empty in
  (t_set,f_set)

let do_it f = 
  Printexc.record_backtrace true;
  let (input,close) = 
	if f = "-" then
	  stdin,false
	else
	  (open_in f),true
  in
  let ast =
  try
	Parser.parse_channel ~close:close input 
  with Parser.Parse_failure (f_name,tokens) ->
	failwith @@ "Parse failure in " ^ f_name ^ " on input " ^ (String.concat " " tokens)
  in
  Env.set_env ast;
  let w_state = Analysis.run_analysis () in
(*
  print_endline "public types:";
  Analysis.MTSet.iter (fun s -> print_endline @@ Types.pp_t (s : Types.mono_type :> Types.r_type))  w_state.Analysis.public_type;
  print_endline "function instantiations:";
  Analysis.FISet.iter (fun (f_name,t) ->
					   print_endline @@ "function name:  " ^ f_name;
					   print_endline @@ "type params: [" ^ (String.concat ", " @@ List.map Types.pp_t (t : Types.mono_type list :> Types.r_type list)) ^ "]"
					  ) w_state.Analysis.fn_inst;
  print_endline "public functions:";
  Analysis.FISet.iter (fun (f_name,t) ->
					   print_endline @@ "function name:  " ^ f_name;
					   print_endline @@ "type params: [" ^ (String.concat ", " @@ List.map Types.pp_t (t : Types.mono_type list :> Types.r_type list)) ^ "]"
					  ) w_state.Analysis.public_fn
 *)
  Compilation.emit stdout 
    w_state.Analysis.public_type
    w_state.Analysis.public_fn
    w_state.Analysis.type_inst
    w_state.Analysis.fn_inst

let _ = do_it Sys.argv.(1)
