open OUnit2

(* OCaml allows {|...|} as a syntax for strings in which the ...
   can contain unescaped quotes.  This is super useful for
   constructing test cases, as shown below. *)
   
(* all of these tests will currently fail, because you have not
   yet implemented interpretation of any of these syntactic forms *)
let tests = [
  "test-name", "input", "expected output";
]

let run s = 
  (* TODO: Implement for testing *) s

let make_interp_expr_test t in_str out_str =
  "\n########################### " ^ t ^ " ##################################\n\n    "
  ^ in_str ^ "========= EVALUATED TO ======>" ^ (run in_str) ^ "   \n\n"
  ^ "   EXPECTED OUTPUT = " ^ out_str ^ "   \n\n"
  ^ "#####################################################################\n"
  >:: (fun _ -> assert_equal out_str (run in_str))

let _ = run_test_tt_main ("suite" >::: 
  List.map (fun (t, i, o) -> make_interp_expr_test t i o) tests)
