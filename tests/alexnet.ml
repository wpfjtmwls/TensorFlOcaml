open Owl
open Tf
open Tfgraph
open Tfgraphst
open Tfnode

module type Chain = 
  functor (SB_1 : SUBGRAPH) (SB_2 : SUBGRAPH) -> (*SUBGRAPH *)
  sig
    val create : node list -> (node * Graph.t * GraphState.st)
  end


