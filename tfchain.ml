open Tfnode
open Tfgraph
open Tfgraphst

module Chain = functor (SB_1 : SUBGRAPH) -> functor (SB_2 : SUBGRAPH) -> 
  struct

    let create in_list name graph graphst = 
      let (output1, graph, graphst) = SB_1.create in_list (SB_1.default_name) graph graphst in
      let (output2, graph, graphst) = SB_2.create [output1] (SB_2.default_name) graph graphst in
      (output2, graph, graphst)

    let default_name = "To be Updated"

  end 
