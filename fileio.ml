open Owl
open Tfgraph
open Tfnode

module Fileio = struct

  let read_image str = Arr.ones [|1|]

  let export_graph str gr n = ()

  let import_graph str = Graph.empty, Tfnode.empty

end