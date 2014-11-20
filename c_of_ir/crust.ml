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
  (* for now, let's optimistically assume all methods and types
  all already monomorphic *)
  let (t_set,f_set) = get_sets () in
  Compilation.emit stdout t_set f_set

let _ = do_it Sys.argv.(1)
