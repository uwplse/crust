let get_sets () = 
  let t_set = Env.EnvMap.fold (fun t _ accum ->
							Analysis.TISet.add (t,[]) accum
						   ) Env.adt_env Analysis.TISet.empty in
  let f_set = Env.EnvMap.fold (fun f _ accum ->
							Analysis.FISet.add (f,[]) accum
						   ) Env.fn_env Analysis.FISet.empty in
  (t_set,f_set)

let filter_file = ref None;;

let filter_ast ast = 
  let read_filter_rules f_name = 
    let f_in = open_in f_name in
    let rec read_loop accum = 
      try
        let l = input_line f_in in
        if l = "" then
          accum
        else
          read_loop @@ SSet.add l accum
      with End_of_file -> close_in f_in; accum
    in
    read_loop SSet.empty
  in
  match !filter_file with
  | None -> ast
  | Some f_file ->
    let filter_fn = read_filter_rules f_file in
    List.filter (function
        | `Fn { Ir.fn_name = n; _ } ->
          not (SSet.mem n filter_fn)
        | _ -> true
      ) ast
;;

let do_it f = 
  Printexc.record_backtrace true;
  let input_file = ref "" in
  let arg_spec = [
    ("-gcc", Arg.Set Env.gcc_mode, "Turn on gcc mode");
    ("-filter", Arg.String (fun f_name -> filter_file := Some f_name), "Filter function names");
    ("-infer-filter", Arg.String (fun f_name -> Env.init_inference_filter f_name), "Read a list of types for which to filter public function inference from file f (one per line)");
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
  (try
	Parser.parse_channel ~close:close input 
  with Parser.Parse_failure (f_name,tokens) ->
	failwith @@ "Parse failure in " ^ f_name ^ " on input " ^ (String.concat " " tokens)
  ) |> filter_ast
  in
  Env.set_env ast;
  let w_state = Analysis.run_analysis () in
  Compilation.emit stdout 
    w_state.Analysis.public_type
    w_state.Analysis.public_fn
    w_state.Analysis.type_inst
    w_state.Analysis.fn_inst

let _ = do_it Sys.argv.(1)
