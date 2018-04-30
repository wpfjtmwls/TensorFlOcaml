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
      let (a1, graph) = graph |> Graph.variable ~prefix:name [num_dims;1] in
      let (h1, graph) = graph |> Graph.matmul ~prefix:name input a1 in

      let graphstate = GraphState.(graphst
                      |> add_node a1 (Arr.mul_scalar (Arr.ones [|num_dims;1|]) 2)
      ) in
      (h1, graph, graphstate)
end

