open OUnit2
open Owl
open Tfgraph
open Tfgraphst
open Jaynet

(* OCaml allows {|...|} as a syntax for strings in which the ...
   can contain unescaped quotes.  This is super useful for
   constructing test cases, as shown below. *)

(* Simple graph of sigmoid(A1x) = s1, sigmoid(A2s1) = s2, (s2-label)^2 = loss *)
let graph = Graph.empty
let graphst = GraphState.empty
let (x, graph) =          graph |> Graph.placeholder [5;4]
let (s2, graph, graphst) = JayNet.create [x] (JayNet.default_name) graph graphst
let (label, graph) =      graph |> Graph.placeholder [5;1]
let (loss, graph) =       graph |> Graph.squared_loss s2 label
let (optimizer, graph) =  graph |> Graph.grad_descent loss
let graphstate = GraphState.(graphst
                   |> add_node x (Arr.ones [|5;4|])
                   |> add_node label (Arr.(ones [|5;1|]))
)
let loss_test, st = Graph.forward loss graph graphstate
let y_pred, st = Graph.forward s2 graph graphstate
let y_actual, st = Graph.forward label graph graphstate
let new_st, losslist = Graph.backward optimizer graph graphstate
let loss_trained, st = Graph.forward loss graph new_st

let tests_mat = [
  (* ("Hidden_1", (h1_test, "A=[4x10],x=[5x4], xA=[5x10]"), " 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2."); *)
  (* ("Sigmoid_1", (s1_test, "H1=[5x10], s1=[5x10]"), " 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978 0.880797077978"); *)
  ("loss_1", (loss_test, "s2=[5x1],label=[5x1],loss=[5x1]") , " 0.000145945182956 0.000145945182956 0.000145945182956 0.000145945182956 0.000145945182956");
  ("y_actual", (y_actual, "y_actual placeholder=[5x1]"), " 1. 1. 1. 1. 1.");
  ("y_pred", (y_pred, "y_pred=[5x1]"), " 0.987919222585 0.987919222585 0.987919222585 0.987919222585 0.987919222585");
  ("trained_loss", (loss_trained, "trained loss"), " 0.000145941911639 0.000145941911639 0.000145941911639 0.000145941911639 0.000145941911639");
]

let tests_state = [
  (* ("backward", (new_st, "new_state"), ""); *)
]

let mat_to_string ar =
  let array = Arr.to_array ar in
  Array.fold_left (fun acc i -> acc ^ " " ^ (string_of_float i)) "" array

let state_to_string st =
  GraphState.graphst_to_string st mat_to_string

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