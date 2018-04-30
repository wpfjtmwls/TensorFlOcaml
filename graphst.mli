open Owl
open Node

(* Graph state object *)
module GraphState : sig 

  (* Graph state object. *)
  type st
  
  (* [empty] initializes an empty mapping *)
  val empty : st
  
  (* [add_node] adds a node to the current mapping 
   * [requires] : node name, corresponding matrix, current graphstate
   * [returns] : new graphstate with the node added
   *)
  val add_node : node -> Arr.arr -> st -> st
  
  (* [get_node] gets a node from the current mapping 
   * [requires] : node name and the current graphstate
   * [returns] : corresponding matrix
   *)
  val get_node : node -> st -> Arr.arr
  
  (* [merge_graphstates] merges the two graphstates accordingly
   * [requires] : two graphsates to be merged and accumulator (which shouold be [] initially)
   * [returns] : updated graphstate with successful merge
   * [usage] : new_graphstate = merge_graphstates [new_state1; new_state2] old_state
   *)
  val merge_graphstates : st list -> st -> st

 (* Returns string representation of state *)
 val graphst_to_string : st -> (Arr.arr -> string) -> string
end
