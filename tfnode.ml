open Owl

type dims = int list

type oper =
  | MatMul of (node * node)
  | Add of (node * node)
  | Minus of (node * node)
  | SquareLoss of (node * node)
  | Sigmoid of node
  | T of node
  | Pow of (node * float)

and optm = 
  | GradDesc of float

and nodetype =
  | Placeholder
  | Variable
  | Operation of oper
  | Optimizer of (optm * node)

and node = {id: string; nodetype: nodetype; size: dims}

(* Helper function. Convert dimension to string *)
let string_of_dims dims =
  let s = List.fold_left (fun ac el -> ac ^ "x" ^ (string_of_int el)) "" dims in
  String.sub s 1 (String.length s - 1)

(* Helper function. Convert Arr.shape to string *)
let string_of_shape sh =
  let s = Array.fold_left (fun ac el -> ac ^ "x" ^ (string_of_int el)) "" sh in
  String.sub s 1 (String.length s - 1)

(* Helper function. Converts Arr.shape to dims *)
let dims_of_shape sh =
  let f = fun ac el -> ac @ [el] in
  Array.fold_left f [] sh

(* Return true iff n.size matches the shape of ar *)
let matches_array_shape n ar =
  ar |> Arr.shape |> dims_of_shape = n.size