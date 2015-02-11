let handle_dst vec_cb str_cb no_match_cb ty = 
  match ty with
  | `Ptr (`Vec t)
  | `Ref (_,`Vec t) ->
    (vec_cb false t)
  | `Ptr_Mut (`Vec t)
  | `Ref_Mut (_,`Vec t) ->
    (vec_cb true t)

  | `Ref (_,`Str)
  | `Ptr `Str ->
    (str_cb false)

  | `Ptr_Mut `Str
  | `Ref_Mut (_, `Str) ->
    (str_cb true)

  | r -> no_match_cb r

