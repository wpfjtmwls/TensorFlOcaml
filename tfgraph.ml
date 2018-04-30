open Owl
open Tfgraphst.GraphState
open Tfnode

let testmode = true

module Graph = struct

(* maps string_of_node values to number of occurances (0 actually means 1 occurance) *)
type node_counts = (string * int) list 

type t = {nc: node_counts}

let empty = {nc = []}

(* ------------ Helper Functions --------------- *)

(* Helper function. Converts nodetype to string. *)    
let string_of_nodetype = function
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

(* Helper function. Converts nodetype and graph to id and new graph.
 * If prefix is set, the id will be prefixed by the prefix followed *)
let gen_id nt ?(prefix="") gr =
  let name = string_of_nodetype nt in
  let num = match List.assoc_opt name gr.nc with
    | None -> 0
    | Some x -> x + 1
  in
  let f = fun (k,v) ->
      if k = name then (k, num)
      else (k,v)
  in
  let gr' = if num = 0 then {nc = (name,0)::gr.nc} else
    {nc = (List.map f (gr.nc))}
  in
  ((prefix ^ name ^ "_" ^ (string_of_int num)), gr')

(* Helper function. Returns true iff a nodetype in nodetypes is an optimizer *)
let contains_optimizer nodetypes =
  let is_opt nt =
    match nt with
    | Optimizer _ -> true
    | _ -> false in
  List.exists is_opt nodetypes

(* ------------ Node Creation --------------- *)

let variable dims ?(prefix="") gr =
  let nodetype = Variable in
  let (id, gr') = gen_id nodetype gr ~prefix:prefix in
  ({id=id; nodetype=nodetype; size=dims}, gr')

let placeholder dims ?(prefix="") gr =
  let nodetype = Placeholder in
  let (id, gr') = gen_id nodetype gr ~prefix:prefix in
  ({id=id; nodetype=nodetype; size=dims}, gr')

let matmul n1 n2 ?(prefix="") gr =
  if contains_optimizer [n1.nodetype; n2.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Operation (MatMul (n1, n2)) in
  let (id, gr') = gen_id nodetype gr ~prefix:prefix in
  let size = begin
    if List.length n1.size = 2 && List.length n2.size = 2 && (List.nth n1.size 1) = (List.hd n2.size)
    then [List.hd n1.size; List.nth n2.size 1]
    else failwith "Invalid dimensions"
  end in
  ({id=id; nodetype=nodetype; size=size}, gr')

let add n1 n2 ?(prefix="") gr =
  if contains_optimizer [n1.nodetype; n2.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Operation (Add (n1, n2)) in
  let (id, gr') = gen_id nodetype gr ~prefix:prefix in
  let size = begin
    if List.length n1.size = 2 && n1.size = n2.size
    then n1.size
    else failwith "Invalid dimensions"
  end in
  ({id=id; nodetype=nodetype; size=size}, gr')

let squared_loss n1 n2 ?(prefix="") gr =
  if contains_optimizer [n1.nodetype; n2.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Operation (SquareLoss (n1, n2)) in
  let (id, gr') = gen_id nodetype gr ~prefix:prefix in
  let size = begin
    if List.length n1.size = 2 && n1.size = n2.size
    then n1.size
    else failwith "Invalid dimensions"
  end in
  ({id=id; nodetype=nodetype; size=size}, gr')

let sigmoid n ?(prefix="") gr =
  if contains_optimizer [n.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Operation (Sigmoid n) in
  let (id, gr') = gen_id nodetype gr ~prefix:prefix in
  ({id=id; nodetype=nodetype; size=n.size}, gr')

let trans n ?(prefix="") gr =
  if contains_optimizer [n.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Operation (T n) in
  let (id, gr') = gen_id nodetype gr ~prefix:prefix in
  let size = begin
    if List.length n.size = 2
    then List.rev n.size
    else failwith "Invalid dimensions"
  end in
  ({id=id; nodetype=nodetype; size=size}, gr')

let minus n1 n2 ?(prefix="") gr =
  if contains_optimizer [n1.nodetype; n2.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Operation (Minus (n1, n2)) in
  let (id, gr') = gen_id nodetype gr ~prefix:prefix in
  let size = begin
    if List.length n1.size = 2 && n1.size = n2.size
    then n1.size
    else failwith "Invalid dimensions"
  end in
  ({id=id; nodetype=nodetype; size=size}, gr')

let pow n power ?(prefix="") gr =
  if contains_optimizer [n.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Operation (Pow (n, power)) in
  let (id, gr') = gen_id nodetype gr ~prefix:prefix in
  let size = n.size in
  ({id=id; nodetype=nodetype; size=size}, gr')

let grad_descent n ?(prefix="") gr =
  if contains_optimizer [n.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Optimizer (GradDesc(0.0001), n) in (* TODO: change learning rate *)
  let (id, gr') = gen_id nodetype gr ~prefix:prefix in
  ({id=id; nodetype=nodetype; size=[]}, gr')

  (* ------------ Runners --------------- *)

let rec forward n gr st =
  match n.nodetype with
  | Placeholder | Variable -> get_node n st, st
  | Optimizer _ -> failwith "Cannot call forward on an optimizer node"
  | Operation o -> begin
    match o with
    | MatMul (n1,n2) ->
      let (a1, st1) = forward n1 gr st in
      let (a2, st2) = forward n2 gr st in
      (* let _ = Printf.printf "Running Matmul on %s matmul %s = %s\n" n1.id n2.id n.id; in *)
      let ar = Arr.dot a1 a2 in
      (ar, ((merge_graphstates [st1; st2] st) |> add_node n ar))
    | Add (n1, n2) ->
      let (a1, st1) = forward n1 gr st in
      let (a2, st2) = forward n2 gr st in
      (* let _ = Printf.printf "Running Add on %s + %s = %s \n" n1.id n2.id n; in *)
      let ar = Arr.add a1 a2 in
      ar, ((merge_graphstates [st1; st2] st) |> add_node n ar)
    | Minus (n1, n2) ->
      let (a1, st1) = forward n1 gr st in
      let (a2, st2) = forward n2 gr st in
      (* let _ = Printf.printf "Running Minus on %s - %s = %s\n" n1.id n2.id n; in *)
      let ar = Arr.(a1 - a2) in
      ar, ((merge_graphstates [st1; st2] st) |> add_node n ar)
    | SquareLoss (n1, n2) -> 
      let (a1, st1) = forward n1 gr st in
      let (a2, st2) = forward n2 gr st in
      (* let _ = Printf.printf "Running Squareloss on sqloss(pred=%s, actual=%s) = %s\n" n1.id n2.id n; in *)
      let ar = Arr.(pow_scalar (a1 - a2) 2.) in
      ar, ((merge_graphstates [st1; st2] st) |> add_node n ar)
    | Sigmoid n1 ->
      let (a1, st1) = forward n1 gr st in
      (* let _ = Printf.printf "Running Sigmoid on sigmoid(%s) = %s\n" n1.id n; in *)
      let ar = Arr.sigmoid a1 in
      ar, add_node n ar st1
    | T a ->
      let (a_val, st1) = forward a gr st in
      (* let _ = Printf.printf "Running T on %s.T %s\n" a.id n; in *)
      let ar = Arr.transpose a_val in
      ar, add_node n ar st1
    | Pow (a, p) ->
      let (a_val, st1) = forward a gr st in
      (* let _ = Printf.printf "Running Pow on %s ** %s = %s\n" a.id (string_of_float p) n; in *)
      let ar = Arr.scalar_pow p a_val in
      ar, add_node n ar st1
  end

  (* Backward runners *)
  (* Helper to backprop for gradient descent *)
  let rec backprop_graddesc node grad lr st =
    match node.nodetype with
    | Optimizer _ ->  failwith "Should not be backpropping on optimizer"
    | Placeholder -> st (* Placeholders do not update on backprop *)
    | Variable -> 
      let var_val = st |> get_node node in
      let new_val = Arr.(var_val - (mul_scalar grad lr)) in
      st |> add_node node new_val
    | Operation op -> begin
      match op with
      | MatMul (a, b) ->
        let b_val = st |> get_node b in
        let a_val = st |> get_node a in
        (* let _ = Printf.printf "Running Backprop on Matmul:  %s matmul %s = %s\n" a.id b.id node.id; in
        let _ = Printf.printf "A %s * B %s = C %s\n" (string_of_dims a.size) (string_of_dims b.size) (string_of_dims node.size); in
        let _ = Printf.printf "G = %s\n" (grad |> Arr.shape |> string_of_shape); in
        let _ = Printf.printf "G = %s | Bt = %s | Agrad %s\n" (grad |> Arr.shape |> string_of_shape) (b_val |> Arr.transpose |> Arr.shape |> string_of_shape) (a_val |> Arr.shape |> string_of_shape); in
        let _ = Printf.printf "At = %s | G = %s | Bgrad %s\n" (a_val |> Arr.transpose |> Arr.shape |> string_of_shape) (grad |> Arr.shape |> string_of_shape) (b_val |> Arr.shape |> string_of_shape); in *)
        let st1 = backprop_graddesc a (Arr.dot grad (b_val |> Arr.transpose)) lr st in
        let st2 = backprop_graddesc b (Arr.dot (a_val |> Arr.transpose) grad) lr st in
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
        let sig_val = st |> get_node node in
        backprop_graddesc a Arr.(mul grad (mul sig_val ((ones [|1|]) - sig_val))) lr st
      | SquareLoss (pred, truth) ->
        let pred_val = st |> get_node pred in
        let truth_val = st |> get_node truth in
        backprop_graddesc pred Arr.(mul_scalar (pred_val - truth_val) 2.) lr st
      | T a ->
        backprop_graddesc a (Arr.transpose grad) lr st
      | Pow (a, p) ->
        let a_val = st |> get_node a in
        let p_minus_1 = p -. 1. in
        backprop_graddesc a Arr.(pow_scalar (mul_scalar a_val p) (p_minus_1)) lr st
    end

let rec backward_helper opt loss_node graph graphstate loss_list n =
  let loss, st_after_run = forward loss_node graph graphstate in
  (* TODO: terminate on delta *)
  if n = 0 then (graphstate, loss::loss_list) else
  let backpropped =
    match opt with
    | GradDesc lr ->
      backprop_graddesc loss_node (Arr.ones [|1|]) lr st_after_run
  in
  backward_helper opt loss_node graph (backpropped) (loss::loss_list) (n-1)

let backward n gr ?(max_iter=10) ?(delta=0.001) st =
  match n.nodetype with
  | Placeholder | Variable | Operation _ -> 
    failwith "Unable to run backward iteration on non-optimizer node"
  | Optimizer (opt, loss_node) ->
    backward_helper opt loss_node gr st [] max_iter

end