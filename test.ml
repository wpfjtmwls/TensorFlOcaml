open OUnit2
open Graphst
open Owl
open Grapho
(* OCaml allows {|...|} as a syntax for strings in which the ...
   can contain unescaped quotes.  This is super useful for
   constructing test cases, as shown below. *)
let graph = Graph.empty
let (a1, graph) = graph |> Graph.variable [4;10]
let (x, graph) = graph |> Graph.placeholder [5;4]
let (h1, graph) = graph |> Graph.matmul x a1
let (s1, graph) = graph |> Graph.sigmoid h1
let (a2, graph) = graph |> Graph.variable [10;1]
let (h2, graph) = graph |> Graph.matmul s1 a2
let (s2, graph) = graph |> Graph.sigmoid h2
let (label, graph) = graph |> Graph.placeholder [5;1]
let (loss, graph) = graph |> Graph.squared_loss s2 label

(* Easy test *)
let graphstate = GraphState.(empty
                   |> add_node x.id (Arr.ones [|5;4|])
                   |> add_node a1.id (Arr.(mul_scalar (ones [|4;10|]) 0.5))
                   |> add_node a2.id (Arr.(mul_scalar (ones [|10;1|]) 0.5))
                   |> add_node label.id (Arr.(ones [|5;1|]))
)
let easy_matmul, st = Graph.forward h1 graph graphstate

 
let tests = [
  ("easy matmul", (easy_matmul, "A=[1],x=[1], Ax=1"), " 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2.");
]
let to_string ar =
  let array = Arr.to_array ar in
  Array.fold_left (fun acc i -> acc ^ " " ^ (string_of_float i)) "" array

let make_tests t (result, in_str) out_str =
  ("\n########################### " ^ t ^ " ##################################\n\n    "
  ^ in_str ^ " ========= EVALUATED TO ======> \n" ^ (to_string result) ^ "   \n\n"
  ^ "   EXPECTED OUTPUT = \n" ^ out_str ^ "   \n\n"
  ^ "#####################################################################\n"
  >:: (fun _ -> assert_equal out_str (to_string result)))
let _ = run_test_tt_main ("suite" >::: 
  List.map (fun (t, i, o) -> make_tests t i o) tests)