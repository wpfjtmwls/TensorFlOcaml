open Tfnode
open Tfgraph
open Tfgraphst

module type SUBGRAPH =
  sig
    val create : node list -> (node * Graph.t * GraphState.st)
  end