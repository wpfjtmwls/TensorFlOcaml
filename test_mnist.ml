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
let xtrain = Arr.concat_horizontal xtrain (Arr.ones [|(Arr.shape xtrain).(0);1|]) (* Give features extra dimension for bias *)
let xtrainbatches = Arr.split ~axis:0 (Array.of_list (List.init 100 (fun _ -> 600))) xtrain
let ytrainbatches = Arr.split ~axis:0 (Array.of_list (List.init 100 (fun _ -> 600))) ytrain
let trainbatches = List.combine (Array.to_list xtrainbatches) (Array.to_list ytrainbatches)
let xtest, _, ytest = Dataset.load_mnist_test_data ()
let xtest, ytest = (Dense.Matrix.Generic.cast_s2d xtest, Dense.Matrix.Generic.cast_s2d ytest)

(* Graph construction *)
let graph = Graph.empty
let graphst = GraphState.empty
let (x, graph) = graph |> Graph.placeholder (Array.to_list (Dense.Ndarray.Generic.shape xtrainbatches.(0)))
let (y, graph) = graph |> Graph.placeholder (Array.to_list (Dense.Ndarray.Generic.shape ytrainbatches.(0)))
let (loss, graph, graphst) = MnistNet.create [x;y] (MnistNet.default_name) graph graphst
let (opt, graph) = graph |> Graph.grad_descent loss 0.01


(* Running the graph *)
let graphstate = GraphState.(graphst
                    |> add_node x (xtrainbatches.(0))
                    |> add_node y (ytrainbatches.(0))
)
let _ = Arr.print (fst (Graph.forward loss graph graphstate))

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
  (List.concat (List.init 2 (fun _ -> trainbatches)))

let _ = GraphState.save_graphst graphst "tests/saved-graphstates-mnist"
let _ = Arr.print (fst (Graph.forward loss graph graphst))
(* 
let x = Mat.linspace 0. 300. 200
let y0 = (Arr.zeros [|(List.length losses)|])
let _ = List.mapi (fun i l -> Arr.set_index y0 [|[|i|]|] [|(Arr.get_index l [|[|0;0|]|]).(0)|]) losses
let h = Plot.create ""
let _ = Plot.(plot ~h ~spec:[ RGB (255,0,0); LineStyle 1; Marker "#[0x2299]"; MarkerSize 8. ] x y0)
let _ = Plot.output h *)