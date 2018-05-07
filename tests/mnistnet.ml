open Owl
open Tf
open Tfgraph
open Tfgraphst
open Tfnode

module MnistNet : SUBGRAPH = struct
    let default_name = "MNISTNET"

    let create in_list name graph graphst =
        if List.length in_list <> 2 then failwith "Wrong number of arguments" else
        let input = List.nth in_list 0 in
        let labels = List.nth in_list 1 in
        let num_dims = (List.nth input.size 1) in
        let (a1, graph) = graph |> Graph.variable ~prefix:name [num_dims;100] in
        let (h1, graph) = graph |> Graph.matmul ~prefix:name input a1 in
        let (s1, graph) = graph |> Graph.sigmoid ~prefix:name h1 in
        let (a2, graph) = graph |> Graph.variable ~prefix:name [100;100] in
        let (h2, graph) = graph |> Graph.matmul ~prefix:name s1 a2 in
        let (s2, graph) = graph |> Graph.sigmoid ~prefix:name h2 in
        let (a3, graph) = graph |> Graph.variable ~prefix:name [100;10] in
        let (smax, graph) = graph |> Graph.softmax ~prefix:name a3 in
        let (loss, graph) = graph |> Graph.crossentropyloss ~prefix:name smax in
        let graphstate = GraphState.(graphst
                        |> add_node a1 (Arr.mul_scalar (Arr.ones [|num_dims;100|]) 0.5)
                        |> add_node a2 (Arr.(mul_scalar (ones [|100;100|]) 0.5))
                        |> add_node a3 (Arr.(mul_scalar (ones [|100;100|]) 0.5))
        ) in
        (s2, graph, graphstate)
end