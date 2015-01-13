let get_sets () = 
  let t_set = Env.EnvMap.fold (fun t _ accum ->
							Analysis.TISet.add (t,[]) accum
						   ) Env.adt_env Analysis.TISet.empty in
  let f_set = Env.EnvMap.fold (fun f _ accum ->
							Analysis.FISet.add (f,[]) accum
						   ) Env.fn_env Analysis.FISet.empty in
  (t_set,f_set)

let do_it f = 
  Printexc.record_backtrace true;
  let input_file = ref "" in
  let arg_spec = [
    ("-gcc", Arg.Set Env.gcc_mode, "Turn on gcc mode");
    ("-", Arg.Unit (fun () -> input_file := "-"), "Read from stdin");
  ] in
  Arg.parse arg_spec (fun s -> input_file := s) "Compile Rust IR to C";
  let (input,close) = 
	if !input_file = "-" then
	  stdin,false
	else
	  (open_in !input_file),true
  in
  let ast =
  try
	Parser.parse_channel ~close:close input 
  with Parser.Parse_failure (f_name,tokens) ->
	failwith @@ "Parse failure in " ^ f_name ^ " on input " ^ (String.concat " " tokens)
  in
  Env.set_env ast;
  let w_state = Analysis.run_analysis () in
  Compilation.emit stdout 
    w_state.Analysis.public_type
    w_state.Analysis.public_fn
    w_state.Analysis.type_inst
    w_state.Analysis.fn_inst

let _ = do_it Sys.argv.(1)
