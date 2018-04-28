open Owl
open Graphst

module Graph = struct
type node_counts = {
  nVar: int;
  nPlaceholder: int; 
  nMatmul: int;
  nAdd: int;
  nSquareLoss: int;
  nSigmoid: int;
  nGradDesc: int;}

type dims = int list

type oper =
  | MatMul of (node * node)
  | Add of (node * node)
  | SquareLoss of (node * node)
  | Sigmoid of node

and optm = 
  | GradDesc of node

and nodetype =
  | Placeholder of dims
  | Variable of dims
  | Operation of oper
  | Optimizer of optm

and node = {id: string; nodetype: nodetype}

type t = {nc: node_counts;}

let empty ={nc= {nVar= 0;
            nPlaceholder= 0;
            nMatmul= 0;
            nAdd= 0;
            nSquareLoss= 0;
            nSigmoid= 0;
            nGradDesc= 0;}}

(* ------------ Helper Functions --------------- *)

(* Helper function. Converts nodetype to string. *)    
let to_string = function
  | Placeholder _ -> "PH"
  | Variable _ -> "VAR"
  | Operation o -> (match o with
    | MatMul _ -> "MM"
    | Add _ -> "Add"
    | SquareLoss _ -> "SL"
    | Sigmoid _ -> "SIG")
  | Optimizer o -> (match o with
    | GradDesc _ -> "GD")

(* Helper function. Gets appropriate value from node_counts *)
let get_node_count nc = function
| Placeholder _ -> nc.nPlaceholder
  | Variable _ -> nc.nVar
  | Operation o -> (match o with
    | MatMul _ -> nc.nMatmul
    | Add _ -> nc.nAdd
    | SquareLoss _ -> nc.nSquareLoss
    | Sigmoid _ -> nc.nSigmoid)
  | Optimizer o -> (match o with
    | GradDesc _ -> nc.nGradDesc
    | _ -> failwith "Unimplemented")

(* Helper function. Returns nc with the appropriate value incremented *)
let incr_node_count nc = function
| Placeholder _ -> {nc with nPlaceholder = nc.nPlaceholder + 1}
  | Variable _ -> {nc with nVar = nc.nVar + 1}
  | Operation o -> (match o with
    | MatMul _ -> {nc with nMatmul = nc.nMatmul + 1}
    | Add _ -> {nc with nAdd = nc.nAdd + 1}
    | SquareLoss _ -> {nc with nSquareLoss = nc.nSquareLoss + 1}
    | Sigmoid _ -> {nc with nSigmoid = nc.nSigmoid + 1})
  | Optimizer o -> (match o with
    | GradDesc _ -> {nc with nGradDesc = nc.nGradDesc + 1})

(* Helper function. Converts nodetype and graph to id and new graph *)
let gen_id nt gr =
  to_string nt ^ "_" ^ string_of_int (get_node_count gr.nc nt), {nc= incr_node_count gr.nc nt}

(* ------------ Node Creation --------------- *)

let variable dims gr =
  let nodetype = Variable dims in
  let (id, gr') = gen_id nodetype gr in
  ({id=id; nodetype=nodetype;}, gr')

let placeholder dims gr =
  let nodetype = Placeholder dims in
  let (id, gr') = gen_id nodetype gr in
  ({id=id; nodetype=nodetype;}, gr')

let matmul n1 n2 gr =
  let nodetype = Operation (MatMul (n1, n2)) in
  let (id, gr') = gen_id nodetype gr in
  ({id=id; nodetype=nodetype;}, gr')

let add n1 n2 gr =
  let nodetype = Operation (Add (n1, n2)) in
  let (id, gr') = gen_id nodetype gr in
  ({id=id; nodetype=nodetype;}, gr')

let squared_loss n1 n2 gr =
  let nodetype = Operation (SquareLoss (n1, n2)) in
  let (id, gr') = gen_id nodetype gr in
  ({id=id; nodetype=nodetype;}, gr')

let sigmoid n gr =
  let nodetype = Operation (Sigmoid n) in
  let (id, gr') = gen_id nodetype gr in
  ({id=id; nodetype=nodetype;}, gr')

let grad_descent n gr =
  let nodetype = Optimizer (GradDesc n) in
  let (id, gr') = gen_id nodetype gr in
  ({id=id; nodetype=nodetype;}, gr')

  (* ------------ Runners --------------- *)

let rec forward n gr st =
  match n.nodetype with
  | Placeholder _ | Variable _ -> get_node n.id st, st
  | Operation o -> (match o with
    | MatMul (n1,n2) ->
      let (a1, st1) = forward n1 st in
      let (a2, st2) = forward n2 st in
      (*let ndims1 = Arr.num_dims a1 in
      let ndims2 = Arr.num_dims a2 in*)
      let ar = Arr.mul a1 a2 in
      (ar, (merge_graphstates [st1; st2] st |> add_node n.id ar))
    | Add n1 n2 ->
      let (a1, st1) = forward n1 st in
      let (a2, st2) = forward n2 st in
      let ar = Arr.add a1 a2 in
      ar, (merge_graphstates [st1; st2] st |> add_node n.id ar)
    | SquareLoss n1 n2 -> 
      let (a1, st1) = forward n1 st in
      let (a2, st2) = forward n2 st in
      let ar = Arr.mul a1 a2 in
      ar, (merge_graphstates [st1; st2] st |> add_node n.id ar)
    | Sigmoid n1 ->
      let (a1, st1) = forward n1 st in
      let ar = Arr.sigmoid a1 in
      ar, add_node n.id ar st1)
  | Optimizer _ -> failwith "Cannot call forward on an optimizer node"

let backward n gr st =
  GraphState.empty

end

(* TODO validate dimensions of new nodes. Don't let anything add to optimizer. Keep in mind, matmul is 2-dimensional, batch ops add a dimension,
TODO batch operations *)