open Owl
open Tf
open Tfgraph
open Tfgraphst
open Tfnode

module AlexNet : SUBGRAPH = struct
  let default_name = "ALEXNET"

  let create in_list name graph graphst =
      if List.length in_list <> 1 then failwith "Wrong number of arguments" else
      let input = List.nth in_list 0 in
      let num_dims = (List.nth input.size 0) in
      let (label, graph) = graph |> Graph.placeholder [num_dims;1] ~prefix:name in
      let (loss, graph) =  graph |> Graph.squared_loss input label ~prefix:name in

      let graphstate = GraphState.(graphst
                   |> add_node label (Arr.(ones [|5;1|]))
      ) in
      (loss, graph, graphstate)
end

