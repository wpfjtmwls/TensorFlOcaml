open Owl
open Graphst.GraphState

let testmode = true

module Graph = struct
type node_counts = {
  nVar: int;
  nPlaceholder: int; 
  nMatmul: int;
  nAdd: int;
  nSquareLoss: int;
  nSigmoid: int;
  nGradDesc: int;
  nT: int;
  nMinus: int;
  nPow: int;
}

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

and node = {id: string; nodetype: nodetype; size: dims;}

type t = {nc: node_counts;}

let empty ={nc = {
              nVar = 0;
              nPlaceholder = 0;
              nMatmul = 0;
              nAdd = 0;
              nSquareLoss = 0;
              nSigmoid = 0;
              nGradDesc = 0;
              nT = 0;
              nMinus = 0;
              nPow = 0;
            }}

(* ------------ Helper Functions --------------- *)

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

(* Helper function. Converts nodetype to string. *)    
let to_string = function
  | Placeholder -> "PH"
  | Variable -> "VAR"
  | Operation o -> begin
    match o with
    | MatMul _ -> "MM"
    | Add _ -> "ADD"
    | SquareLoss _ -> "SL"
    | Sigmoid _ -> "SIGM"
    | T _ -> "T"
    | Minus _ -> "MINUS"
    | Pow _ -> "POW"
  end
  | Optimizer (o, _) -> (match o with
    | GradDesc _ -> "GD")

(* Helper function. Gets appropriate value from node_counts *)
let get_node_count nc = function
  | Placeholder -> nc.nPlaceholder
  | Variable -> nc.nVar
  | Operation o -> begin
    match o with
    | MatMul _ -> nc.nMatmul
    | Add _ -> nc.nAdd
    | SquareLoss _ -> nc.nSquareLoss
    | Sigmoid _ -> nc.nSigmoid
    | T _ -> nc.nT
    | Minus _ -> nc.nMinus
    | Pow _ -> nc.nPow
  end
  | Optimizer (o, _) -> (match o with
    | GradDesc _ -> nc.nGradDesc)

(* Helper function. Returns nc with the appropriate value incremented *)
let incr_node_count nc = function
  | Placeholder -> {nc with nPlaceholder = nc.nPlaceholder + 1}
  | Variable  -> {nc with nVar = nc.nVar + 1}
  | Operation o -> begin
    match o with
    | MatMul _ -> {nc with nMatmul = nc.nMatmul + 1}
    | Add _ -> {nc with nAdd = nc.nAdd + 1}
    | SquareLoss _ -> {nc with nSquareLoss = nc.nSquareLoss + 1}
    | Sigmoid _ -> {nc with nSigmoid = nc.nSigmoid + 1}
    | T _ -> {nc with nT = nc.nT + 1}
    | Minus _ -> {nc with nMinus = nc.nMinus + 1}
    | Pow _ -> {nc with nPow = nc.nPow + 1}
  end
  | Optimizer (o, _) -> (match o with
    | GradDesc _ -> {nc with nGradDesc = nc.nGradDesc + 1})

(* Helper function. Converts nodetype and graph to id and new graph *)
let gen_id nt gr =
  to_string nt ^ "_" ^ string_of_int (get_node_count gr.nc nt), {nc= incr_node_count gr.nc nt}

(* Helper function. Returns true iff a nodetype in nodetypes is an optimizer *)
let contains_optimizer nodetypes =
  let is_opt nt =
    match nt with
    | Optimizer _ -> true
    | _ -> false in
  List.exists is_opt nodetypes

(* ------------ Node Creation --------------- *)

let variable dims gr =
  let nodetype = Variable in
  let (id, gr') = gen_id nodetype gr in
  ({id=id; nodetype=nodetype; size=dims}, gr')

let placeholder dims gr =
  let nodetype = Placeholder in
  let (id, gr') = gen_id nodetype gr in
  ({id=id; nodetype=nodetype; size=dims}, gr')

let matmul n1 n2 gr =
  if contains_optimizer [n1.nodetype; n2.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Operation (MatMul (n1, n2)) in
  let (id, gr') = gen_id nodetype gr in
  let size = begin
    if List.length n1.size = 2 && List.length n2.size = 2 && (List.nth n1.size 1) = (List.hd n2.size)
    then [List.hd n1.size; List.nth n2.size 1]
    else failwith "Invalid dimensions"
  end in
  ({id=id; nodetype=nodetype; size=size}, gr')

let add n1 n2 gr =
  if contains_optimizer [n1.nodetype; n2.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Operation (Add (n1, n2)) in
  let (id, gr') = gen_id nodetype gr in
  let size = begin
    if List.length n1.size = 2 && n1.size = n2.size
    then n1.size
    else failwith "Invalid dimensions"
  end in
  ({id=id; nodetype=nodetype; size=size}, gr')

let squared_loss n1 n2 gr =
  if contains_optimizer [n1.nodetype; n2.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Operation (SquareLoss (n1, n2)) in
  let (id, gr') = gen_id nodetype gr in
  let size = begin
    if List.length n1.size = 2 && n1.size = n2.size
    then n1.size
    else failwith "Invalid dimensions"
  end in
  ({id=id; nodetype=nodetype; size=size}, gr')

let sigmoid n gr =
  if contains_optimizer [n.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Operation (Sigmoid n) in
  let (id, gr') = gen_id nodetype gr in
  ({id=id; nodetype=nodetype; size=n.size}, gr')

let trans n gr =
  if contains_optimizer [n.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Operation (T n) in
  let (id, gr') = gen_id nodetype gr in
  let size = begin
    if List.length n.size = 2
    then List.rev n.size
    else failwith "Invalid dimensions"
  end in
  ({id=id; nodetype=nodetype; size=size}, gr')

let minus n1 n2 gr =
  if contains_optimizer [n1.nodetype; n2.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Operation (Minus (n1, n2)) in
  let (id, gr') = gen_id nodetype gr in
  let size = begin
    if List.length n1.size = 2 && n1.size = n2.size
    then n1.size
    else failwith "Invalid dimensions"
  end in
  ({id=id; nodetype=nodetype; size=size}, gr')

let pow n power gr =
  if contains_optimizer [n.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Operation (Pow (n, power)) in
  let (id, gr') = gen_id nodetype gr in
  let size = n.size in
  ({id=id; nodetype=nodetype; size=size}, gr')

let grad_descent n gr =
  if contains_optimizer [n.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Optimizer (GradDesc(0.1), n) in (* TODO: change learning rate *)
  let (id, gr') = gen_id nodetype gr in
  ({id=id; nodetype=nodetype; size=[]}, gr')

  (* ------------ Runners --------------- *)

let rec forward n gr st =
  match
    match n.nodetype with
    | Placeholder | Variable -> get_node n.id st, st
    | Optimizer _ -> failwith "Cannot call forward on an optimizer node"
    | Operation o -> begin
      match o with
      | MatMul (n1,n2) ->
        let (a1, st1) = forward n1 gr st in
        let (a2, st2) = forward n2 gr st in
        (* let _ = Printf.printf "Running Matmul on %s matmul %s = %s\n" n1.id n2.id n.id; in *)
        let ar = Arr.dot a1 a2 in
        (ar, ((merge_graphstates [st1; st2] st) |> add_node n.id ar))
      | Add (n1, n2) ->
        let (a1, st1) = forward n1 gr st in
        let (a2, st2) = forward n2 gr st in
        (* let _ = Printf.printf "Running Add on %s + %s = %s \n" n1.id n2.id n.id; in *)
        let ar = Arr.add a1 a2 in
        ar, ((merge_graphstates [st1; st2] st) |> add_node n.id ar)
      | Minus (n1, n2) ->
        let (a1, st1) = forward n1 gr st in
        let (a2, st2) = forward n2 gr st in
        (* let _ = Printf.printf "Running Minus on %s - %s = %s\n" n1.id n2.id n.id; in *)
        let ar = Arr.(a1 - a2) in
        ar, ((merge_graphstates [st1; st2] st) |> add_node n.id ar)
      | SquareLoss (n1, n2) -> 
        let (a1, st1) = forward n1 gr st in
        let (a2, st2) = forward n2 gr st in
        (* let _ = Printf.printf "Running Squareloss on sqloss(pred=%s, actual=%s) = %s\n" n1.id n2.id n.id; in *)
        let ar = Arr.(pow_scalar (a1 - a2) 2.) in
        ar, ((merge_graphstates [st1; st2] st) |> add_node n.id ar)
      | Sigmoid n1 ->
        let (a1, st1) = forward n1 gr st in
        (* let _ = Printf.printf "Running Sigmoid on sigmoid(%s) = %s\n" n1.id n.id; in *)
        let ar = Arr.sigmoid a1 in
        ar, add_node n.id ar st1
      | T a ->
        let (a_val, st1) = forward a gr st in
        (* let _ = Printf.printf "Running T on %s.T %s\n" a.id n.id; in *)
        let ar = Arr.transpose a_val in
        ar, add_node n.id ar st1
      | Pow (a, p) ->
        let (a_val, st1) = forward a gr st in
        (* let _ = Printf.printf "Running Pow on %s ** %s = %s\n" a.id (string_of_float p) n.id; in *)
        let ar = Arr.scalar_pow p a_val in
        ar, add_node n.id ar st1
    end
  with ar, st ->
    let () = assert begin
      if testmode then
        let ar_shape = Arr.shape ar in
        dims_of_shape ar_shape = n.size
      else true
    end in ar, st

let backward n gr st =
  (* Helper to backprop for gradient descent *)
  let rec backprop_graddesc node grad lr st =
    let _ = Printf.printf "--Backprop-- %s\n" node.id; in
    match node.nodetype with
    | Optimizer _ ->  failwith "Should not be backpropping on optimizer"
    | Placeholder -> st (* Placeholders do not update on backprop *)
    | Variable -> 
      let var_val = st |> get_node node.id in
      let new_val = Arr.(var_val - (mul_scalar grad lr)) in
      st |> add_node node.id new_val
    | Operation op -> begin
      match op with
      | MatMul (a, b) ->
        let b_val = st |> get_node b.id in
        let a_val = st |> get_node a.id in
        (* let _ = Printf.printf "Running Backprop on Matmul:  %s matmul %s = %s\n" a.id b.id node.id; in
        let _ = Printf.printf "A %s * B %s = C %s\n" (string_of_dims a.size) (string_of_dims b.size) (string_of_dims node.size); in
        let _ = Printf.printf "G = %s\n" (grad |> Arr.shape |> string_of_shape); in
        let _ = Printf.printf "G = %s | Bt = %s | Agrad %s\n" (grad |> Arr.shape |> string_of_shape) (b_val |> Arr.transpose |> Arr.shape |> string_of_shape) (a_val |> Arr.shape |> string_of_shape); in
        let _ = Printf.printf "At = %s | G = %s | Bgrad %s\n" (a_val |> Arr.transpose |> Arr.shape |> string_of_shape) (grad |> Arr.shape |> string_of_shape) (b_val |> Arr.shape |> string_of_shape); in *)
        let st1 = backprop_graddesc a (Arr.dot grad (b_val |> Arr.transpose)) lr st in
        let st2 = backprop_graddesc b (Arr.dot (a_val |> Arr.transpose) grad) lr st in
        let _ = if node.id = "MM_0" then Arr.print (get_node "VAR_0" (merge_graphstates [st1; st2] st)) else (); in
        merge_graphstates [st1; st2] st
      | Add (a, b) ->
        let st1 = backprop_graddesc a (grad) lr st in
        let st2 = backprop_graddesc b (grad) lr st in
        merge_graphstates [st1; st2] st
      | Minus (a, b) ->
        let st1 = backprop_graddesc a (grad) lr st in
        let st2 = backprop_graddesc b (Arr.mul_scalar grad (-1.)) lr st in
        merge_graphstates [st1; st2] st
      | Sigmoid a ->
        let sig_val = st |> get_node node.id in
        let a_val = st |> get_node a.id in
        let _ = if node.id = "SIGM_0" then Arr.print (get_node "VAR_0" (backprop_graddesc a (Arr.mul a_val sig_val) lr st)) else (); in
        backprop_graddesc a (Arr.mul a_val sig_val) lr st
      | SquareLoss (pred, truth) ->
        let pred_val = st |> get_node pred.id in
        let truth_val = st |> get_node truth.id in
        backprop_graddesc pred Arr.(mul_scalar (pred_val - truth_val) 2.) lr st
      | T a ->
        backprop_graddesc a (Arr.transpose grad) lr st
      | Pow (a, p) ->
        let a_val = st |> get_node a.id in
        let p_minus_1 = p -. 1. in
        backprop_graddesc a Arr.(pow_scalar (mul_scalar a_val p) (p_minus_1)) lr st
    end
  in
  (* Run backward pass *)
  match n.nodetype with
  | Placeholder | Variable | Operation _ -> 
    failwith "Unable to run backward iteration on non-optimizer node"
  | Optimizer (opt, loss_node) -> begin
    let (loss_val, st_after_run) = forward loss_node gr st in
    match opt with
    | GradDesc lr ->
      backprop_graddesc loss_node (Arr.ones [|1|]) lr st_after_run
  end

end