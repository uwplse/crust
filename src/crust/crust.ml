let do_it f = 
  Printexc.record_backtrace true;
  let input_file = ref "" in
  let arg_spec = [
    ("-gcc", Arg.Set Env.gcc_mode, "Turn on gcc mode");
    ("-infer-filter", Arg.String (fun f_name -> Env.init_inference_filter f_name), "Read a list of types for which to filter public function inference from file f (one per line)");
    ("-", Arg.Unit (fun () -> input_file := "-"), "Read from stdin");
    ("-optional-init", Arg.Set Env.init_opt, "Make crust_init optional (for testing purposes only!)");
    ("-api-filter", Arg.String (fun f_name -> Analysis.init_fn_filter f_name), "Read a list of glob patterns")
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
    w_state.Analysis.static_var

let _ = do_it Sys.argv.(1)
