open Owl
open Printf

type dims = int list

type oper =
  | MatMul of (node * node)
  | Add of (node * node)
  | Minus of (node * node)
  | SquareLoss of (node * node)
  | Sigmoid of node
  | T of node
  | Pow of (node * float)
  | Softmax of node
  | Negative of node
  | ReduceSum of (node * int)
  | Mul of (node * node)
  | Log of (node)
  | Broadcast of (node * int * int * bool)

and optm =
  | GradDesc of float

and nodetype =
  | Placeholder
  | Variable
  | Operation of oper
  | Optimizer of (optm * node)

and logger = {filename: string; interval: int; counter: int ref}

and node = {id: string; nodetype: nodetype; size: dims; log: logger option}

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

let update_log (n:node) =
  match n.log with
  | Some l -> let l' = {l with counter = l.counter + 1} in
    let () = if l'.counter mod l'.interval = 0 then
      (*write to the file*)
      let oc = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o666 l'.filename in
      fprintf oc "hello world\n";
      close_out oc; in
    {n with log = Some l'}
  | None -> n

let empty = {id="";nodetype=Placeholder;size=[];log=None;}
