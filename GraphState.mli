(* Graph state object *)
module type GraphState = sig 
  type t 
  type nodeid = string 
  (* Placeholder for now. Eventually use owl matrix? *)
  type matrix

  val empty : unit -> t
  
  val add_placeholder: nodeid -> matrix -> t
  
  val add_variable : nodeid -> matrix -> t
end