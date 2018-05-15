(*
  Allows the user to chain together multiples Tensorflowcaml nueral nets. 
  Defines a functor that takes in two computational graphs,
  using this, a user can recursively chain as many graphs as they want. 
*)
open Tf

module Chain 
  : functor (SB_1 : SUBGRAPH) 
    -> functor (SB_2 : SUBGRAPH) 
    -> SUBGRAPH
