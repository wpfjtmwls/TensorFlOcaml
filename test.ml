open OUnit2
open Graph
open Owl

(* OCaml allows {|...|} as a syntax for strings in which the ...
   can contain unescaped quotes.  This is super useful for
   constructing test cases, as shown below. *)

let graph = Graph.empty
let (A, graph) = graph |> Graph.create_variable (1, 1)
let (x, graph) = graph |> Graph.create_placeholder(1,1)
let (y, graph) = graph |> Graph.matmul A x
let graphstate = GraphState.(empty
                   |> add_placeholder x.id Mat.ones 1 1
                   |> add_variable A.id Mat.ones 1 1)           
let easy_matmul = Graph.forward y graph graphstate
 
let tests = [
  ("easy matmul", (easy_matmul, "A=[1],x=[1], Ax=1"), "1")
]

let make_tests t (result, in_str) out_str =
  ("\n########################### " ^ t ^ " ##################################\n\n    "
  ^ in_str ^ " ========= EVALUATED TO ======> " ^ (print result) ^ "   \n\n"
  ^ "   EXPECTED OUTPUT = " ^ out_str ^ "   \n\n"
  ^ "#####################################################################\n"
  >:: (fun _ -> assert_equal out_str (print result)))

let _ = run_test_tt_main ("suite" >::: 
  List.map (fun (t, i, o) -> make_tests t i o) tests)
