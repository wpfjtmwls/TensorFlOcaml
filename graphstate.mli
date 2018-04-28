open Owl

(* Graph state object *) 
type st

type nodeid = string 

(* [empty] initializes an empty mapping *)
val empty : st


(* [add_node] adds a node to the current mapping 
 * [requires] : node name, corresponding matrix, current graphstate
 * [returns] : new graphstate with the node added
 *)
val add_node : nodeid -> Arr.arr -> st -> st

(* [get_node] gets a node from the current mapping 
 * [requires] : node name and the current graphstate
 * [returns] : corresponding matrix
 *)
val get_node : nodeid -> st -> Arr.arr