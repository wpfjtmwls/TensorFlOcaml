open OUnit2
open Owl
open Tfgraph
open Tfgraphst
open Jaynet
open Jalexnet
open Mnistnet


(* OCaml allows {|...|} as a syntax for strings in which the ...
   can contain unescaped quotes.  This is super useful for
   constructing test cases, as shown below. *)

let xtrain, _, ytrain = Dataset.load_mnist_train_data ()
let xtrain, ytrain = (Dense.Matrix.Generic.cast_s2d xtrain, Dense.Matrix.Generic.cast_s2d ytrain)
let xtrainbatches = Arr.split ~axis:0 (Array.of_list (List.init 100 (fun _ -> 600))) xtrain
let ytrainbatches = Arr.split ~axis:0 (Array.of_list (List.init 100 (fun _ -> 600))) ytrain
(* let trainbatches = List.combine (Array.to_list xtrainbatches) (Array.to_list ytrainbatches) *)
let trainbatches = [(xtrainbatches.(0), ytrainbatches.(0))]
let xtest, _, ytest = Dataset.load_mnist_test_data ()
let xtest, ytest = (Dense.Matrix.Generic.cast_s2d xtest, Dense.Matrix.Generic.cast_s2d ytest)

let graph = Graph.empty
let graphst = GraphState.empty
let (x, graph) = graph |> Graph.placeholder (Array.to_list (Dense.Ndarray.Generic.shape xtrainbatches.(0)))
let (y, graph) = graph |> Graph.placeholder (Array.to_list (Dense.Ndarray.Generic.shape ytrainbatches.(0)))
let (loss, graph, graphst) = MnistNet.create [x;y] (MnistNet.default_name) graph graphst
let (opt, graph) = graph |> Graph.grad_descent loss 0.0001

let run_backward (st, accloss) xTr yTr =
  let graphstate = GraphState.(st
                    |> add_node x (xTr)
                    |> add_node y (yTr)
  ) in
  let (new_st, losslist) = Graph.backward opt graph graphstate ~max_iter:100 in
  new_st, (accloss @ losslist)

let (graphst, losses) = List.fold_left 
  (fun (st, accloss) (xTr, yTr) -> run_backward (st, accloss) xTr yTr)
  (graphst, [])
  trainbatches
  (* (List.concat (List.init 1 (fun _ -> trainbatches))) *)

let _ = GraphState.save_graphst graphst "tests/saved-graphstates-mnist"
let _ = List.map (fun l -> Arr.print (Arr.sum l)) losses

let _ = Arr.print (fst (Graph.forward loss graph graphst))

let x = Mat.linspace 0. 300. 300
let y0 = (Arr.zeros [|(List.length losses)|])
let _ = List.mapi (fun i l -> Arr.set_index y0 [|[|i|]|] [|(Arr.get_index l [|[|0;0|]|]).(0)|]) losses
let h = Plot.create ""
let _ = Plot.(plot ~h ~spec:[ RGB (255,0,0); LineStyle 1; Marker "#[0x2299]"; MarkerSize 8. ] x y0)
let _ = Plot.output h