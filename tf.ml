(*
  Defines definition for a subgraph. Allows the user to build a computational 
  graph from a list of inputs which are represented by a node list.
  The graph is created under the specified namespace.  
*)
open Tfnode
open Tfgraph
open Tfgraphst

module type SUBGRAPH =
  sig
    (* [create] takes a node list of inputs and creates a computational graph under the namespace provided.
     * Requires: [inputs] : node list, a list of nodes that is required by the subgraph
     *           [namespace] : string, a name for the graph to be used for the namespace
     * Outputs: (node, graph, graphstate) : outputs the output node for the subgraph, an updated graph object
     *           and an updated graphstate for initialized parameters *)
    val create : node list -> string -> Graph.t -> GraphState.st -> (node * Graph.t * GraphState.st)

    (* [default_name] is the default name of the subgraph, to be used for naming purposes *)
    val default_name : string
  end