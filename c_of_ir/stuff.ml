type 'a foo = Bar of 'a | Baz of 'a foo list | Gorp

let rec walk_list f l cb = 
  match l with
  | h::t -> 
	 walk_foo f h (fun h' a ->
				   walk_list f t (fun l' a' ->
                                  cb (h'::l') (a + a')
								 )
				  )
  | [] -> cb [] 0
and walk_foo f e cb = 
  match e with 
  | Bar x -> 
	 let x' = f x in
	 cb (Bar x') 1
  | Gorp -> cb e 1
  | Baz fl ->
	 walk_list f fl (fun fl' a' ->
                     cb (Baz fl') (a' + 1)
					)
