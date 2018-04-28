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
  | GradDesc of float

and nodetype =
  | Placeholder of dims
  | Variable of dims
  | Operation of oper
  | Optimizer of (optm * node)

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
  | Optimizer (o, _) -> (match o with
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
  | Optimizer (o, _) -> (match o with
    | GradDesc _ -> nc.nGradDesc)

(* Helper function. Returns nc with the appropriate value incremented *)
let incr_node_count nc = function
| Placeholder _ -> {nc with nPlaceholder = nc.nPlaceholder + 1}
  | Variable _ -> {nc with nVar = nc.nVar + 1}
  | Operation o -> (match o with
    | MatMul _ -> {nc with nMatmul = nc.nMatmul + 1}
    | Add _ -> {nc with nAdd = nc.nAdd + 1}
    | SquareLoss _ -> {nc with nSquareLoss = nc.nSquareLoss + 1}
    | Sigmoid _ -> {nc with nSigmoid = nc.nSigmoid + 1})
  | Optimizer (o, _) -> (match o with
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
  let nodetype = Optimizer (GradDesc(0.1), n) in (* TODO: change learning rate *)
  let (id, gr') = gen_id nodetype gr in
  ({id=id; nodetype=nodetype;}, gr')

  (* ------------ Runners --------------- *)

let forward n gr st =
  (* match n with
  | Placeholder dimens -> 
  | Variable dimens -> 
  | Operation o -> (match o with
    | MatMul n1 n2 -> 
    | Add n1 n2 -> 
    | SquareLoss n1 n2 -> 
    | Sigmoid n1 -> )
  | Optimizer o -> (match o with
    | GradDesc n1 -> ) *)
  Arr.ones [|1|]

let backward n gr st =
  (* Helper to backprop for gradient descent *)
  let rec backprop_graddesc node grad lr st =
    match node.nodetype with
    | Placeholder _ -> st (* Placeholders do not update on backprop *)
    | Variable _ -> 
      let var_val = st |> Graphstate.get_node node.id in
      let new_val = var_val - (lr) .* grad in
      st |> Graphstate.add_node node.id new_val
    | Operation op -> begin
      match op with
      | MatMul (a, b) ->
        let b_val = st |> Graphstate.get_node b.id in
        let a_val = st |> Graphstate.get_node a.id in
        let st1 = backprop_graddesc a (Arr.mul grad b_val) lr st in
        let st2 = backprop_graddesc b (Arr.mul grad a_val) lr st in
        Graphstate.merge st [st1; st2]
      | Add (a, b) ->
        let st1 = backprop_graddesc a (grad) lr st in
        let st2 = backprop_graddesc b (grad) lr st in
        Graphstate.merge st [st1; st2]
      | Sigmoid a ->
        let sig_val = st |> Graphstate.get_node node.id in
        backprop_graddesc a (Arr.mul a dsig_dl) lr st
      | SquareLoss (a, b) ->
    end
    | Optimizer _ ->  failwith "Should not be backpropping on optimizer"
  in
  (* Run backward pass *)
  match n with
  | Placeholder _ | Variable _ | Operation _ -> 
    failwith "Unable to run backward iteration on non-optimizer node"
  | Optimizer (opt, loss_node) -> begin
    let (st_after_run, loss_val) = forward loss_node gr st in
    match opt with
    | GradDesc lr ->
      backprop_graddesc loss_node lr st_after_run
  end
