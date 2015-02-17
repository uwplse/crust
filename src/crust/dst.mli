(* TODO(jtoman): migrate to typeutil? *)
val handle_dst : 
  (bool ->'a -> 'b) -> (bool -> 'b) -> ('a -> 'b) ->
  ([> `Ptr of [> `Str | `Vec of 'a ]
   | `Ptr_Mut of [> `Str | `Vec of 'a ]
   | `Ref of Types.lifetime * [> `Str | `Vec of 'a ]
   | `Ref_Mut of Types.lifetime * [> `Str | `Vec of 'a ] ] as 'a) -> 
  'b
