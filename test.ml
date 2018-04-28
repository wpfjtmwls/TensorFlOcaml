open OUnit2
open Graphst
open Grapho
open Owl


(* OCaml allows {|...|} as a syntax for strings in which the ...
   can contain unescaped quotes.  This is super useful for
   constructing test cases, as shown below. *)

let graph = Grapho.Graph.empty
let (a, graph) = graph |> Grapho.Graph.variable [1;1]
let (x, graph) = graph |> Grapho.Graph.placeholder [1;1]
let (y, graph) = graph |> Grapho.Graph.matmul a x
let graphstate = GraphState.(empty
                   |> add_node x.id (Arr.ones [|1|])
                   |> add_node a.id (Arr.ones [|1|]))           
let easy_matmul = Grapho.Graph.forward y graph graphstate
 
let tests = [
  ("easy matmul", (easy_matmul, "A=[1],x=[1], Ax=1"), "1")
]

let make_tests t (result, in_str) out_str =
  ("\n########################### " ^ t ^ " ##################################\n\n    "
  ^ in_str ^ " ========= EVALUATED TO ======> " ^ ("result") ^ "   \n\n"
  ^ "   EXPECTED OUTPUT = " ^ out_str ^ "   \n\n"
  ^ "#####################################################################\n"
  >:: (fun _ -> assert_equal out_str ("result")))

let _ = run_test_tt_main ("suite" >::: 
  List.map (fun (t, i, o) -> make_tests t i o) tests)
