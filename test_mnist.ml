open OUnit2
open Owl
open Tfgraph
open Tfgraphst
open Jaynet
open Jalexnet
open Mnistnet


(* Pull data *)
let batchsize = 32
let xtrain, _, ytrain = Dataset.load_mnist_train_data ()
let xtrain, ytrain = (Dense.Matrix.Generic.cast_s2d xtrain, Dense.Matrix.Generic.cast_s2d ytrain)
let mean = Arr.mean ~axis:0 xtrain
let std= Arr.std ~axis:0 xtrain
let std = Arr.map (fun x -> if x = 0. then 1. else x) std
let xtrain = Arr.div (Arr.sub xtrain mean) std
let xtrain = Arr.concat_horizontal xtrain (Arr.ones [|(Arr.shape xtrain).(0);1|]) (* Give features extra dimension for bias *)
let xtrainbatches = Arr.split ~axis:0 (Array.of_list (List.init (60000 / batchsize) (fun _ -> batchsize))) xtrain
let ytrainbatches = Arr.split ~axis:0 (Array.of_list (List.init (60000 / batchsize) (fun _ -> batchsize))) ytrain
let trainbatches = List.combine (Array.to_list xtrainbatches) (Array.to_list ytrainbatches)
let trainbatches = [List.nth trainbatches 0]
let xtest, _, ytest = Dataset.load_mnist_test_data ()
let xtest, ytest = (Dense.Matrix.Generic.cast_s2d xtest, Dense.Matrix.Generic.cast_s2d ytest)
let xtest = Arr.div (Arr.sub xtest mean) std
let xtest = Arr.concat_horizontal xtest (Arr.ones [|(Arr.shape xtest).(0);1|]) (* Give features extra dimension for bias *)
let xtest, ytest = ((Arr.get_slice [[0;(batchsize - 1)]] xtest), (Arr.get_slice [[0;(batchsize - 1)]] ytest))

(* let _ = Printf.printf "%s\n" (Array.fold_left (fun acc i -> acc ^ "x" ^ (string_of_int i)) "" (Arr.shape xtrainbatches.(0)))
let _ = Printf.printf "%s\n" (Array.fold_left (fun acc i -> acc ^ "x" ^ (string_of_int i)) "" (Arr.shape xtest)) *)

(* Graph construction *)
let graph = Graph.empty
let graphst = GraphState.empty
let (x, graph) = graph |> Graph.placeholder (Array.to_list (Dense.Ndarray.Generic.shape xtrainbatches.(0)))
let (y, graph) = graph |> Graph.placeholder (Array.to_list (Dense.Ndarray.Generic.shape ytrainbatches.(0)))
let (loss, graph, graphst) = MnistNet.create [x;y] (MnistNet.default_name) graph graphst
let (opt, graph) = graph |> Graph.grad_descent loss 0.01

(* Evaluation helper *)
let get_accuracy xVal yVal graph graphst =
  let graphst = GraphState.(graphst
                  |> add_node x (xVal)
                  |> add_node y (yVal)
  ) in
  let (loss_val, graphst) = Graph.forward loss graph graphst in
  let smax_val = GraphState.(graphst |> get_node_by_id "MNISTNET_SOFTMAX_0") in
  let preds = Dense.Matrix.Generic.fold_rows (fun acc row -> let i = (snd (Arr.max_i row)).(1) in i::acc) [] smax_val in
  let truth = Dense.Matrix.Generic.fold_rows (fun acc row -> let i = (snd (Arr.max_i row)).(1) in i::acc) [] yVal in
  let total = float_of_int (Arr.shape yVal).(0) in
  float_of_int (List.fold_left2 (fun acc i1 i2 -> if i1 = i2 then acc + 1 else acc) 0 preds truth) /. total

(* Evaluating the graph *)
let _ = Printf.printf "Starting Accuracy on trg set: %.5f\n" (get_accuracy xtrainbatches.(0) ytrainbatches.(0) graph graphst)
let _ = Printf.printf "Starting Accuracy on test set: %.5f\n" (get_accuracy xtest ytest graph graphst)
(* Training the graph *)
let run_backward (st, accloss) xTr yTr =
  let graphstate = GraphState.(st
                    |> add_node x (xTr)
                    |> add_node y (yTr)
  ) in
  let (new_st, losslist) = Graph.backward opt graph graphstate ~max_iter:1 in
  new_st, (accloss @ losslist)
let (graphst, losses) = List.fold_left 
  (fun (st, accloss) (xTr, yTr) -> run_backward (st, accloss) xTr yTr)
  (graphst, [])
  (List.concat (List.init 100 (fun _ -> trainbatches)))

let _ = Arr.print ~max_row:10 ~max_col:10 (GraphState.(graphst |> get_node_by_id "MNISTNET_SIGM_0"))
let _ = GraphState.save_graphst graphst "tests/saved-graphstates-mnist"

(* Evaluating the graph *)
let _ = Printf.printf "Ending Accuracy on trg set: %.5f\n" (get_accuracy xtrainbatches.(0) ytrainbatches.(0) graph graphst)
let _ = Printf.printf "Ending Accuracy on test set: %.5f\n" (get_accuracy xtest ytest graph graphst)

(* let _ = Arr.print ~max_row:10 ~max_col:10 (GraphState.(graphst |> get_node_by_id "MNISTNET_MM_0"))
let _ = Arr.print ~max_row:10 ~max_col:10 (GraphState.(graphst |> get_node_by_id "MNISTNET_SIGM_0")) *)