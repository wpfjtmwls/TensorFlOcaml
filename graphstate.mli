open Owl

(* Graph state object *)
module type Graphstate = sig 
  type t 
  type nodeid = string 
  val empty : t
  val add_placeholder: nodeid -> Arr.arr -> t
  val add_variable : nodeid -> Arr.arr -> t
end
