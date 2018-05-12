open OUnit2
open Owl
open Tfgraph
open Tfgraphst
open Jaynet
open Jalexnet
open Mnistnet


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

(* Evaluation helper *)
let get_accuracy_helper xVal yVal graph graphst =
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
let get_accuracy xValList yValList graph graphst =
  let xValList, yValList = Array.to_list xValList, Array.to_list yValList in
  let all = List.fold_left2 (fun acc xVal yVal -> (get_accuracy_helper xVal yVal graph graphst)::acc) [] xValList yValList in
  (List.fold_left (+.) 0. all) /. (float_of_int (List.length all))

let get_loss xVal yVal graph graphst =
  let graphst = GraphState.(graphst
                  |> add_node x (xVal)
                  |> add_node y (yVal)
  ) in
  let (loss_val, graphst) = Graph.forward loss graph graphst in
  (Arr.get_index loss_val [|[|0;0|]|]).(0)

(* Evaluating the graph *)
let _ = Printf.printf "Starting Accuracy on trg set: %.5f\n" (get_accuracy (Array.sub xtrainbatches 0 10) (Array.sub ytrainbatches 0 10) graph graphst)
let _ = Printf.printf "Starting Accuracy on test set: %.5f\n" (get_accuracy (Array.sub xtestbatches 0 10) (Array.sub ytestbatches 0 10) graph graphst)
let _ = Printf.printf "Starting loss on training set: %.5f\n" (get_loss xtrainbatches.(0) ytrainbatches.(0) graph graphst)
let _ = Printf.printf "Starting loss on test set: %.5f\n" (get_loss xtestbatches.(0) ytestbatches.(0) graph graphst)

(* Training the graph *)
let (graphst, losslist) = Graph.train opt graph [(x, (Array.to_list xtrainbatches)); (y, (Array.to_list ytrainbatches))] ~max_iter:10 ~delta:0.01 ~log_loss_every_ith:10 graphst

(* let _ = Arr.print ~max_row:10 ~max_col:10 (GraphState.(graphst |> get_node_by_id "MNISTNET_VAR_0")) *)
let _ = GraphState.save_graphst graphst "tests/saved-graphstates-mnist"

(* Evaluating the graph *)
let _ = Printf.printf "Ending Accuracy on trg set: %.5f\n" (get_accuracy (Array.sub xtrainbatches 0 10) (Array.sub ytrainbatches 0 10) graph graphst)
let _ = Printf.printf "Ending Accuracy on test set: %.5f\n" (get_accuracy (Array.sub xtestbatches 0 10) (Array.sub ytestbatches 0 10) graph graphst)
let _ = Printf.printf "Ending loss on training set: %.5f\n" (get_loss xtrainbatches.(0) ytrainbatches.(0) graph graphst)
let _ = Printf.printf "Ending loss on test set: %.5f\n" (get_loss xtestbatches.(0) ytestbatches.(0) graph graphst)

(* let _ = Arr.print ~max_row:10 ~max_col:10 (GraphState.(graphst |> get_node_by_id "MNISTNET_MM_0"))
let _ = Arr.print ~max_row:10 ~max_col:10 (GraphState.(graphst |> get_node_by_id "MNISTNET_SIGM_0")) *)