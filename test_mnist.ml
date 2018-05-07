open OUnit2
open Owl
open Tfgraph
open Tfgraphst
open Jaynet
open Jalexnet


(* OCaml allows {|...|} as a syntax for strings in which the ...
   can contain unescaped quotes.  This is super useful for
   constructing test cases, as shown below. *)

(* Simple graph of sigmoid(A1x) = s1, sigmoid(A2s1) = s2, (s2-label)^2 = loss *)
let graph = Graph.empty
let graphst = GraphState.empty
let x, _, y = Dataset.load_mnist_train_data ()

let graph = Graph.empty
let graphst = GraphState.empty
let (x, graph) = graph |> Graph.placeholder (Array.to_list (Dense.Ndarray.Generic.shape x))
let (s2, graph, graphst) = Mnistnet.create [x] (Mnistnet.default_name) graph graphst

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
]

let tests_state = [
  (* ("backward", (new_st, "new_state"), ""); *)
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