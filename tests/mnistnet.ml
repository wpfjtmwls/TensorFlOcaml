open Owl
open Tf
open Tfgraph
open Tfgraphst
open Tfnode

module MnistNet : SUBGRAPH = struct
    let default_name = "MNISTNET"

    let create in_list name graph graphst =
        let get_init fan_in fan_out =
            (* let r = 4. *. (sqrt (6. /. ((float_of_int fan_in) +. (float_of_int  fan_out)))) in *)
            let r = 0.01 in
            Arr.uniform [|fan_in;fan_out|] ~a:(-1. *. r) ~b:(r)
        in
        if List.length in_list <> 2 then failwith "Wrong number of arguments" else
        let input = List.nth in_list 0 in
        let labels = List.nth in_list 1 in
        let n_dims = (List.nth input.size 1) in
        let num_data = (List.nth input.size 0) in
        let (a1, graph) = graph |> Graph.variable ~prefix:name [n_dims;300] in
        let (b1, graph) = graph |> Graph.variable ~prefix:name [num_data;1] in
        let (h1, graph) = graph |> Graph.matmul ~prefix:name input a1 in
        let (h1b, graph)= graph |> Graph.add ~prefix:name h1 b1 in
        let (s1, graph) = graph |> Graph.sigmoid ~prefix:name h1b in
        let (a2, graph) = graph |> Graph.variable ~prefix:name [300;100] in
        let (b2, graph) = graph |> Graph.variable ~prefix:name [num_data;1] in
        let (h2, graph) = graph |> Graph.matmul ~prefix:name s1 a2 in
        let (h2b, graph)= graph |> Graph.add ~prefix:name h2 b2 in
        let (s2, graph) = graph |> Graph.sigmoid ~prefix:name h2b in
        let (a3, graph) = graph |> Graph.variable ~prefix:name [100;10] in
        let (b3, graph) = graph |> Graph.variable ~prefix:name [num_data;1] in
        let (h3, graph) = graph |> Graph.matmul ~prefix:name s2 a3 in
        let (h3b, graph)= graph |> Graph.add ~prefix:name h3 b3 in
        let (smax, graph) = graph |> Graph.softmax ~prefix:name h3b in
<<<<<<< HEAD
        let (loss, graph) = graph |> Graph.crossentropyloss ~prefix:name smax labels in
=======
        let logger = Some {filename="mnist-loss.log"; interval=10; counter=ref 0} in
        let (loss, graph) = graph |> Graph.squared_loss ~prefix:name ~logger:logger smax labels in
>>>>>>> graphsave
        let graphstate = GraphState.(graphst    
                        |> add_node a1 (get_init n_dims 300)
                        |> add_node a2 (get_init 300 100)
                        |> add_node a3 (get_init 100 10)
                        |> add_node b1 (Arr.uniform [|num_data;1|])
                        |> add_node b2 (Arr.uniform [|num_data;1|])
                        |> add_node b3 (Arr.uniform [|num_data;1|])
        ) in
        (loss, graph, graphstate)
end