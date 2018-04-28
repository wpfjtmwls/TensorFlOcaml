open Owl

(* Graph state object *)
module type GraphState = sig 
  type st
  type nodeid = string 
  val empty : st
  val add_placeholder: nodeid -> Arr.arr -> st
  val add_variable : nodeid -> Arr.arr -> st
end


(* Graph state object *)
module GraphState : GraphState = struct
  type st = string
  type nodeid = string 
  let empty = ""
  let add_placeholder s m = ""
  let add_variable s m = "" 
end
