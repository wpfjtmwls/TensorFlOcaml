(*
Example test if one were to want to demo Tensorflowcaml 

*)
open OUnit2
open Owl
open Tfgraph
open Tfgraphst
open Mnistnet
open Random

(* Pull data *)
let xtrain, _, ytrain = Dataset.load_mnist_train_data ()
let xtrain, ytrain = (Dense.Matrix.Generic.cast_s2d xtrain, Dense.Matrix.Generic.cast_s2d ytrain)
let xtest, _, ytest = Dataset.load_mnist_test_data ()
let xtest, ytest = (Dense.Matrix.Generic.cast_s2d xtest, Dense.Matrix.Generic.cast_s2d ytest)

(* Normalize data *)
let mean = Arr.mean ~axis:0 xtrain
let std= Arr.std ~axis:0 xtrain
let std = Arr.map (fun x -> if x = 0. then 1. else x) std
let xtrain = Arr.div (Arr.sub xtrain mean) std
let xtest = Arr.div (Arr.sub xtest mean) std

(* Extra dimensions for bias *)
let xtrain = Arr.concat_horizontal xtrain (Arr.ones [|(Arr.shape xtrain).(0);1|])
let xtest = Arr.concat_horizontal xtest (Arr.ones [|(Arr.shape xtest).(0);1|])

(* Split data in batches of size batchsize *)
let batchsize = 32
let xtrainbatches = Arr.split ~axis:0 (Array.of_list (List.init (60000 / batchsize) (fun _ -> batchsize))) xtrain
let ytrainbatches = Arr.split ~axis:0 (Array.of_list (List.init (60000 / batchsize) (fun _ -> batchsize))) ytrain
let trainbatches = List.combine (Array.to_list xtrainbatches) (Array.to_list ytrainbatches)
let xtestbatches = Arr.split ~axis:0 (Array.of_list (List.init (3200 / batchsize) (fun _ -> batchsize))) (Arr.get_slice [[0;3199];[]] xtest)
let ytestbatches = Arr.split ~axis:0 (Array.of_list (List.init (3200 / batchsize) (fun _ -> batchsize))) (Arr.get_slice [[0;3199];[]] ytest)

(* Graph construction *)
let graph = Graph.empty
let graphst = GraphState.empty
let (x, graph) = graph |> Graph.placeholder (Array.to_list (Dense.Ndarray.Generic.shape xtrainbatches.(0)))
let (y, graph) = graph |> Graph.placeholder (Array.to_list (Dense.Ndarray.Generic.shape ytrainbatches.(0)))
let (loss, graph, graphst) = MnistNet.create [x;y] (MnistNet.default_name) graph graphst
let (opt, graph) = graph |> Graph.grad_descent loss 0.01

(* Load graph and graph state files *)
let graphst = GraphState.load_graphst "tests/saved-graphstates-mnist-final"


(* Get Preds and Truths *)
let xVal , yVal = List.hd (Array.to_list (Array.sub xtestbatches 0 1)), List.hd (Array.to_list (Array.sub ytestbatches 0 1))
let graphst = GraphState.(graphst
                  |> add_node x (xVal)
                  |> add_node y (yVal)
  )
let (loss_val, graphst) = Graph.forward loss graph graphst
let smax_val = GraphState.(graphst |> get_node_by_id "MNISTNET_SOFTMAX_0") 
let preds = Dense.Matrix.Generic.fold_rows (fun acc row -> let i = (snd (Arr.max_i row)).(1) in i::acc) [] smax_val 
let truths = Dense.Matrix.Generic.fold_rows (fun acc row -> let i = (snd (Arr.max_i row)).(1) in i::acc) [] yVal 
let () = print_endline (string_of_int (List.length preds))
let () = print_endline (string_of_int (List.length truths))

(* Plot *)
