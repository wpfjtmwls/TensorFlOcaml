open Tfsubgraph

module type Chain = 
  functor (SB_1 : SUBGRAPH) (SB_2 : SUBGRAPH) -> sig
    val chaingraphs : node list -> node
  end
