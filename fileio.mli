open Owl
open Tfgraph
open Tfnode

module Fileio : sig

  val export_graph : string -> Graph.t -> node -> unit

  val import_graph : string -> (Graph.t * node)

end