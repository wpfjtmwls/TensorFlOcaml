open Tfnode

module type SUBGRAPH =
  sig
    val create : node list -> node
  end