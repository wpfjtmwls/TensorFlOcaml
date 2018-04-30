open Owl
open Tfgraph
open Tfnode

module Fileio : sig

  val read_image : string -> Arr.arr

  val export_graph : string -> Graph.t -> node -> unit

  val import_graph : string -> (Graph.t * node)

end