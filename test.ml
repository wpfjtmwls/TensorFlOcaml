(*
  "Basic" test for people wanting to create their own nueral net
  and train it on a certain type of data. 

  The file uses various nueral nets defined within the "tests" folder.
  These nets are described within their respective ".ml" files. 
*)
open OUnit2
open Owl
open Tfgraph
open Tfnode
open Tfgraphst
open Jaynet
open Jalexnet


(* OCaml allows {|...|} as a syntax for strings in which the ...
   can contain unescaped quotes.  This is super useful for
   constructing test cases, as shown below. *)

(* Simple graph of sigmoid(A1x) = s1, sigmoid(A2s1) = s2, (s2-label)^2 = loss *)
let graph = Graph.empty
let graphst = GraphState.empty
let (x, graph) =          graph |> Graph.placeholder [5;4]
let (s2, graph, graphst) = JayNet.create [x] (JayNet.default_name) graph graphst
let (label, graph) =      graph |> Graph.placeholder [5;1]
let logger = Some {filename="test-loss.log"; interval=10; counter=ref 0}
let (loss, graph) =       graph |> Graph.squared_loss s2 label ~logger:logger
(*let (loss, graph) =       graph |> Graph.squared_loss s2 label*)
let (testol, graph) =     graph |> Graph.sigmoid x (* used for testing graph.t.ol *)
let (optimizer, graph) =  graph |> Graph.grad_descent loss 0.01
let graphstate = GraphState.(graphst
                   |> add_node x (Arr.ones [|5;4|])
                   |> add_node label (Arr.(ones [|5;1|]))
)
let loss_test, st = Graph.forward loss graph graphstate
let y_pred, st = Graph.forward s2 graph graphstate
let y_actual, st = Graph.forward label graph graphstate
let new_st, losses = Graph.train optimizer graph 
  [
    (x, [(Arr.ones [|5;4|])]); 
    (label, [(Arr.(ones [|5;1|]))])
  ] graphstate ~max_iter:100000 ~delta:0.000000001
let loss_trained, st_losstrained = Graph.forward loss graph new_st

(* test for softmax *)
(* let sm_graph = Graph.empty
let sm_graphst = GraphState.empty
let sm_x, sm_graph = sm_graph |> Graph.placeholder [1;4]
let sm_sm, sm_graph = sm_graph |> Graph.softmax sm_x
let sm_gd, sm_graph = sm_graph |> Graph.grad_descent sm_sm 0.01
let sm_graphst = GraphState.(sm_graphst |> add_node sm_x (Arr.ones [|1;4|]))
let (sm_res, sm_graphst) = Graph.forward sm_sm sm_graph sm_graphst
let (sm_back, sm_graphst) = Graph.backward sm_gd sm_graph sm_graphst *)

(* Tests for saving of simple graphstate *)
let _ = GraphState.save_graphst new_st "tests/saved-graphstates"
let loaded_graphstate_losstrained = GraphState.load_graphst "tests/saved-graphstates"

let _ = Graph.save graph "tests/saved-graphstates/graph"

(* Tests for loading of graph *)
let gr1 = Graph.load "tests/saved-graphstates/graph.tfgraph" 
let _ = Graph.save gr1 "tests/saved-graphstates/loaded_graph"

(* Plotting test *)
let () =
let h = Plot.create "plot_003.png" in

let x = Mat.create 1 3 0. in
let y = Mat.create 1 3 0. in
let () = Mat.set x 0 0 1. in
let () = Mat.set x 0 1 2. in
let () = Mat.set x 0 2 3. in
let () = Mat.set y 0 0 1. in
let () = Mat.set y 0 1 2. in
let () = Mat.set y 0 2 3. in

Plot.set_foreground_color h 0 0 0;
Plot.set_background_color h 255 255 255;
Plot.set_title h "Loss Function Plot";
Plot.set_xlabel h "x-axis";
Plot.set_ylabel h "y-axis";
Plot.set_font_size h 8.;
Plot.set_pen_size h 3.;
let () = Plot.plot ~h x y in

Plot.output h;;


(* Simple chained graph replicating the above simple graph with chaining *)
let graph = Graph.empty
let graphst = GraphState.empty
let (x, graph) =          graph |> Graph.placeholder [5;4]
let (loss, graph, graphst) = Jalexnet.create [x] (Jalexnet.default_name) graph graphst
let graphstate = GraphState.(graphst
                   |> add_node x (Arr.ones [|5;4|])
)
let loss_test2, st = Graph.forward loss graph graphstate


let mat_to_string ar =
  let array = Arr.to_array ar in
  Array.fold_left (fun acc i -> acc ^ " " ^ (string_of_float i)) "" array

let state_to_string st =
  GraphState.graphst_to_string st mat_to_string

let tests_mat = [
  (* ("Hidden_1", (h1_test, "A=[4x10],x=[5x4], xA=[5x10]"), " 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2."); *)
  (* ("Sigmoid_1", (s1_test, "H1=[5x10], s1=[5x10]"), " 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978"); *)
  ("loss_1", (loss_test, "s2=[5x1],label=[5x1],loss=[5x1]") , " 0.000145945182956 0.000145945182956 0.000145945182956 0.000145945182956 0.000145945182956");
  ("y_actual", (y_actual, "y_actual placeholder=[5x1]"), " 1. 1. 1. 1. 1.");
  ("y_pred", (y_pred, "y_pred=[5x1]"), " 0.987919222585 0.987919222585 0.987919222585 0.987919222585 0.987919222585");
  ("trained_loss", (loss_trained, "trained loss"), " 5.66264501612e-05 5.66264501612e-05 5.66264501612e-05 5.66264501612e-05 5.66264501612e-05");

  (* test for Chaining *)
  ("loss_2", (loss_test2, "s2=[5x1],label=[5x1],loss=[5x1]") , " 0.000145945182956 0.000145945182956 0.000145945182956 0.000145945182956 0.000145945182956");

  (*softmax*)
  (* ("softmax", (sm_res, "sm_sm=[4x1]"), " 0.25 0.25 0.25 0.25"); *)

]

let tests_state = [
  (* ("backward", (new_st, "new_state"), ""); *)
  ("save-loading graphstates", (loaded_graphstate_losstrained, "graphstate before saving and loading = graphstate after saving and loading"), (state_to_string new_st));
]

let make_tests t (result, in_str) out_str stringify =
  ("\n########################### " ^ t ^ " ##################################\n\n    "
  ^ in_str ^ " ========= EVALUATED TO ======> \n" ^ (stringify result) ^ "   \n\n"
  ^ "   EXPECTED OUTPUT = \n" ^ out_str ^ "   \n\n"
  ^ "#####################################################################\n"
  >:: (fun _ -> assert_equal out_str (stringify result)))

let _ = run_test_tt_main ("suite" >::: 
  (List.map (fun (t, i, o) -> make_tests t i o mat_to_string) tests_mat) @ 
  (List.map (fun (t, i, o) -> make_tests t i o state_to_string) tests_state)
  )