open Owl

(* Graph state object *)
module type GraphState = sig 
  type st
  type nodeid = string 
  val empty : st
  val add_placeholder: nodeid -> Arr.arr -> st
  val add_variable : nodeid -> Arr.arr -> st
end
