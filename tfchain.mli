open Tfnode
open Tfgraph
open Tfgraphst

module type Chain = 
  functor (SB_1 : SUBGRAPH) (SB_2 : SUBGRAPH) -> sig
    val chaingraphs : node list -> (node * Graph.t * GraphState.st)
  end
