open Owl
open Tf
open Tfgraph
open Tfgraphst
open Tfnode

module JayNet : SUBGRAPH = struct
    let default_name = "JAYNET"

    let create in_list name graph graphst =
        if List.length in_list <> 1 then failwith "Wrong number of arguments" else
        let input = List.nth in_list 0 in
        let num_dims = (List.nth input.size 0) in
        let (a1, graph) = graph |> Graph.variable [num_dims;10] in
        let (h1, graph) = graph |> Graph.matmul input a1 in
        let (s1, graph) = graph |> Graph.sigmoid h1 in
        let (a2, graph) = graph |> Graph.variable [10;1] in
        let (h2, graph) = graph |> Graph.matmul s1 a2 in
        let (s2, graph) = graph |> Graph.sigmoid h2 in
        let graphstate = GraphState.(graphst
                        |> add_node a1 (Arr.mul_scalar (Arr.ones [|num_dims;10|]) 0.5)
                        |> add_node a2 (Arr.(mul_scalar (ones [|10;1|]) 0.5))
        ) in
        (s2, graph, graphstate)
end