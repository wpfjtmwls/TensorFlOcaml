(* Graph state object *) 
type st
type nodeid = string 
val empty : st
val add_placeholder: nodeid -> string -> st
val add_variable : nodeid -> string -> st