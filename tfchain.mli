open Tf

module Chain 
  : functor (SB_1 : SUBGRAPH) 
    -> functor (SB_2 : SUBGRAPH) 
    -> SUBGRAPH
