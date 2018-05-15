(*
Module for file input/output. 
Can be used to read graphs from files or export trained networks to 
files for later acess. 
*)
open Owl
open Tfgraph
open Tfnode

module Fileio : sig
  (* Given a graph id, graph, and graph node, export to a file *)
  val export_graph : string -> Graph.t -> node -> unit

  (* Given the name of a file with the specified Tensorflowcaml format, 
     constructs the computational graph from that. *)
  val import_graph : string -> (Graph.t * node)

end