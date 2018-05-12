open Owl
open Tfgraphst.GraphState
open Tfnode
open Yojson.Basic.Util

let testmode = true

module Graph = struct

(* maps node-types (string_of_node values) to number of occurances (0 actually means 1 occurance) *)
type node_counts = (string * int) list

(* nc is a node_counts representing the 'namespace'
 * ol is a list of output nodes
 * nm is a nodemap (maps id to node) *)
type t = {nc: node_counts; ol: node list; nm : (string * node) list}

let empty = {nc = []; ol = []; nm = []}

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
    | Softmax _ -> "SOFTMAX"
    | Negative _ -> "NEG"
    | ReduceSum _ -> "REDUCESUM"
    | Mul _ -> "ELMUL"
    | Log _ -> "LOG"
    | Broadcast _ -> "BROADCAST"
  end
  | Optimizer (o, _) -> (match o with
    | GradDesc _ -> "GD")

(************************* helpers for load ***************************************)

(* Helper function to get nodetype string from node map 
 * [str_nt] : string representation of nodetype 
 * [nm] : namespace for nodemap 
 * precondition : the str_nt has already been declared *)
let nt_from_nm str_nt nm = 
  if List.mem_assoc str_nt nm then List.assoc str_nt nm 
  else failwith "Exception: string Node has not been added to the nodemap"

(* Reverse function of string_of_nodetype. Converts string to nodetype *)
let nodetype_from_string str_nt params nm = 
  match str_nt with 
  | "PH" -> Placeholder
  | "VAR" -> Variable
  | "MM" -> 
    let str_nt1 = List.nth params 0 in
    let str_nt2 = List.nth params 1 in
    let nd1 = nt_from_nm str_nt1 nm in 
    let nd2 = nt_from_nm str_nt2 nm in
    Operation (MatMul (nd1, nd2))
  | "ADD" -> 
    let str_nt1 = List.nth params 0 in
    let str_nt2 = List.nth params 1 in
    let nd1 = nt_from_nm str_nt1 nm in 
    let nd2 = nt_from_nm str_nt2 nm in
    Operation (Add (nd1, nd2))
  | "MINUS" -> 
    let str_nt1 = List.nth params 0 in
    let str_nt2 = List.nth params 1 in
    let nd1 = nt_from_nm str_nt1 nm in 
    let nd2 = nt_from_nm str_nt2 nm in
    Operation (Minus (nd1, nd2))
  | "SL" -> 
    let str_nt1 = List.nth params 0 in
    let str_nt2 = List.nth params 1 in
    let nd1 = nt_from_nm str_nt1 nm in 
    let nd2 = nt_from_nm str_nt2 nm in
    Operation (SquareLoss (nd1, nd2))
  | "SIGH" -> Operation (Sigmoid (nt_from_nm (List.hd params) nm))
  | "T" -> Operation (T (nt_from_nm (List.hd params) nm))
  | "POW" -> failwith "Pow Unimplemented"
  | "SOFTMAX" -> Operation (Softmax (nt_from_nm (List.hd params) nm))
  | "GD" -> failwith "GD Unimplemented"
  | _ -> failwith "Not a valid string representation of nodetype"
  
(* Define type for each node object expressed in json to be loaded *)
type load_node = {
  load_nodeid : string;
  load_nodetype : string;
  load_size : int list;
  load_params : string list;
}

(* Load one single node object from json file *)
let load_single_node obj = {
  load_nodeid = obj |> member "node_id" |> to_string;
  load_nodetype = obj |> member "nodetype" |> to_string;
  load_size = obj |> member "size" |> to_list |> List.map to_int;
  load_params = obj |> member "params" |> to_list |> List.map to_string;
}

(* parse json file to a list of load_node objects *)
let load_nodes j = 
  try j |> member "graphs" |> to_list |> List.map load_single_node
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s) 

(* Create a node from load node information *)
let make_node ln nm = {
  id = ln.load_nodeid;
  nodetype = (nodetype_from_string ln.load_nodetype ln.load_params nm);
  size = ln.load_size;
}

(* Configure the new ol by checking if anynode in output node list is one of the params for other nodes *)
let rec config_ol old_ol acc_params = 
  List.fold_left (fun acc nd -> if List.exists (fun x -> x=nd.id) acc_params then acc else nd::acc) [] old_ol

(* Update node count map according to the string nodetype *)
let update_nc old_nc str_nt =
    if List.mem_assoc str_nt old_nc then (str_nt,((List.assoc str_nt old_nc) + 1))::(List.remove_assoc str_nt old_nc) 
    else (str_nt,0)::old_nc 

(* Recursively make a graph from parsed load_node list *)
let rec make_graph ln_lst acc_nc acc_ol acc_nm acc_params = 
  match ln_lst with
  | [] -> {nc = acc_nc; ol = config_ol acc_ol acc_params; nm = acc_nm;}
  | ln::t -> 
    (* make a node from load node *)
    let nd = make_node ln acc_nm in
    let new_nc = update_nc acc_nc nd.id in
    let new_ol = nd::(acc_ol) in
    let new_nm = (nd.id,nd)::(acc_nm) in
    let new_params = ln.load_params @ acc_params in
    make_graph t new_nc new_ol new_nm new_params

(* Preprocessing helper function for [load] converts the path to json and parse *)
let load_helper path = 
  let json = Yojson.Basic.from_file path in
  let ln_lst = load_nodes json in
  make_graph ln_lst [] [] [] []

(************************* end of helpers for load ***************************************)

(* Helper function. Converts nodetype and nodecounts to id and new nodecounts.
 * If prefix is set, the id will be prefixed by the prefix followed *)
let gen_id nt ?(prefix="") (nc:node_counts) =
  let name = string_of_nodetype nt in
  let num = match List.assoc_opt name nc with
    | None -> 0
    | Some x -> x + 1 in
  let f = fun (k,v) ->
      if k = name then (k, num)
      else (k,v) in
  let nc' = if num = 0 then (name,0)::nc else
    List.map f nc in
  let prefix' = if prefix = "" then "" else prefix ^ "_" in
  ((prefix' ^ name ^ "_" ^ (string_of_int num)), nc')

(* Helper function. Returns true iff a nodetype in nodetypes is an optimizer *)
let contains_optimizer nodetypes =
  let is_opt nt =
    match nt with
    | Optimizer _ -> true
    | _ -> false in
  List.exists is_opt nodetypes

(* Returns a modified version of output_list after adding node n to the graph.
 * in_list represents the list of nodes feeding into n as inputs. *)
let new_output_list (n:node) (in_lst:node list) (ol:node list) =
  let p = fun x ->
    let e = fun y -> x = y in
    not (List.exists e in_lst) in
  n :: List.filter p ol

(* Helper function. Returns nodes in order required for saving and loading
 * Returns: [n1, n2, ...] where the nodes required to instantiate n_i have
 *          indices < i *)
let nodes_in_save_order (output_nodes:node list) =
  let rec nodes_in_save_order_singlenode node =
    match node.nodetype with
    | Placeholder -> [node]
    | Variable -> [node]
    | Optimizer (opt, loss) -> node::(nodes_in_save_order_singlenode loss)
    | Operation o -> begin
      match o with
      | MatMul(n1, n2) -> node::((nodes_in_save_order_singlenode n1) @ (nodes_in_save_order_singlenode n2))
      | Add(n1, n2) -> node::((nodes_in_save_order_singlenode n1) @ (nodes_in_save_order_singlenode n2))
      | Minus(n1, n2) -> node::((nodes_in_save_order_singlenode n1) @ (nodes_in_save_order_singlenode n2))
      | SquareLoss(n1, n2) -> node::((nodes_in_save_order_singlenode n1) @ (nodes_in_save_order_singlenode n2))
      | Sigmoid n1 -> node::(nodes_in_save_order_singlenode n1)
      | T n1 -> node::(nodes_in_save_order_singlenode n1)
      | Pow (n1, _) -> node::(nodes_in_save_order_singlenode n1)
      | Softmax n1 -> node::(nodes_in_save_order_singlenode n1)
      | Negative n1 -> node::(nodes_in_save_order_singlenode n1)
      | ReduceSum (n1, _) -> node::(nodes_in_save_order_singlenode n1)
      | Mul (n1, n2) -> node::((nodes_in_save_order_singlenode n1) @ (nodes_in_save_order_singlenode n2))
      | Log (n1) -> node::(nodes_in_save_order_singlenode n1)
      | Broadcast (n1, _, _, _) -> node::(nodes_in_save_order_singlenode n1)
    end
  in
  let merge_nodelists oldlist newlist =
    (List.fold_right (fun node acc -> if not (List.mem node oldlist) then node::acc else acc) newlist [])
    @ oldlist
  in
  List.rev (List.fold_left 
    (fun acc output_node -> merge_nodelists acc (nodes_in_save_order_singlenode output_node)) [] output_nodes)

(* Helper that builds a broadcast node if possible from two
 * nodes, one is the target (big) and one the broadcastee (small) *)
let broadcast n1 n2 ?(prefix="") gr =
  (* Identify which node to broadcase (small node) *)
  let (small, big, reversed) = 
    if (List.length n1.size) = (List.length n2.size) then
      let n1sum = List.fold_left (+) 0 n1.size in
      let n2sum = List.fold_left (+) 0 n2.size in
      if n1sum > n2sum then (n2, n1, true) else (n1, n2, false)
    else if (List.length n1.size) > (List.length n2.size) then
      (n2, n1, true)
    else (n1, n2, false)
  in
  let return small big reversed gr =
    if reversed then big, small, gr else small, big, gr
  in
  if big.size = small.size then return small big reversed gr else
  let paddedsmall = 
    if (List.length big.size) > (List.length small.size) then
      List.mapi (fun i x -> if i < (List.length small.size) then  List.nth small.size i else 1) big.size
    else small.size
  in
  (* Check if broadcast possible *)
  if List.fold_left2 
    (fun acc s1 s2 -> if s1 <> s2 && s1 <> 1 then false else acc && true) false paddedsmall big.size 
    then failwith "Unable to broadcast!"
  else
  (* Broadcast possible, run broadcasting *)
  let make_node s_small s_big (n, index_from_right, graph) =
    let sizelength = (List.length n.size) in
    if s_big <> s_small && s_small <> 1 then failwith "FATAL: Broadcasting error!"
    else if s_small = s_big && 
      (index_from_right < sizelength) then 
        (n, (index_from_right+1), graph)
    else
      let nodetype = if index_from_right < sizelength then
        Operation(Broadcast(n, index_from_right, s_big, false))
        else Operation(Broadcast(n, index_from_right, s_big, true))
      in
      let newsize = if index_from_right < sizelength then
        List.mapi (fun i x -> if i = (sizelength - index_from_right - 1) then s_big else x) n.size
        else s_big::n.size
      in
      let (id, nc') = gen_id nodetype gr.nc ~prefix:prefix in
      let node = {id=id; nodetype=nodetype; size=newsize} in
      (node, 
      (index_from_right+1),
      {nc=nc'; nm=(id,node)::graph.nm; ol= new_output_list node [n] gr.ol})
  in
  let broadcasted, _, graph = List.fold_right2 (make_node) paddedsmall big.size (small, 0, gr) in
  return broadcasted big reversed gr

(* ------------ Load and Save --------------- *)

  let save gr path =
    let to_json_node (n:node) : Yojson.json =
      let params:Yojson.json = match n.nodetype with
      | Placeholder | Variable -> `List []
      | Optimizer o -> `List [`String (snd o).id]
      | Operation o -> begin
        match o with
        | MatMul (n1,n2) -> `List [`String n1.id; `String n2.id]
        | Add (n1, n2) -> `List [`String n1.id; `String n2.id]
        | Minus (n1, n2) -> `List [`String n1.id; `String n2.id]
        | SquareLoss (n1, n2) -> `List [`String n1.id; `String n2.id]
        | Sigmoid n1 -> `List [`String n1.id]
        | T n1 -> `List [`String n1.id]
        | Pow (n1, p) -> `List [`String n1.id; `Float p]
        | Softmax n1 -> `List [`String n1.id]
        | Negative n1 -> `List [`String n1.id]
        | ReduceSum (n1, axis) -> `List [`String n1.id; `Int axis]
        | Mul (n1, n2) -> `List [`String n1.id; `String n2.id]
        | Log (n1) -> `List [`String n1.id]
        | Broadcast (n1, index_from_right, size, squeezed) -> `List [`String n1.id; `Int index_from_right; `Int size; `Bool squeezed ]
      end in
      let json_of_dims d = `List (List.map (fun x -> `Int x) d) in
      `Assoc [("node_id", `String n.id);("nodetype", `String (string_of_nodetype n.nodetype));
      ("size", json_of_dims n.size);("params", params)] in
    let ns = nodes_in_save_order gr.ol in
    let json : Yojson.json = `Assoc [("graph", `List (List.map to_json_node ns))] in
    Yojson.to_file (path ^ ".tfgraph") json

  let load path = 
    load_helper path
    
(* ------------ Node Creation --------------- *)

let variable dims ?(prefix="") gr =
  let nodetype = Variable in
  let (id, nc') = gen_id nodetype gr.nc ~prefix:prefix in
  let node = {id=id; nodetype=nodetype; size=dims} in
  (node, {nc=nc'; nm = (id,node)::gr.nm; ol = new_output_list node [] gr.ol})

let placeholder dims ?(prefix="") gr =
  let nodetype = Placeholder in
  let (id, nc') = gen_id nodetype gr.nc ~prefix:prefix in
  let node = {id=id; nodetype=nodetype; size=dims} in
  (node, {nc=nc'; nm = (id,node)::gr.nm; ol = new_output_list node [] gr.ol})

let matmul n1 n2 ?(prefix="") gr =
  if contains_optimizer [n1.nodetype; n2.nodetype]
  then failwith "Cannot do a matmul on an optimizer"
  else
  let nodetype = Operation (MatMul (n1, n2)) in
  let (id, nc') = gen_id nodetype gr.nc ~prefix:prefix in
  let size = begin
    if List.length n1.size = 2 && List.length n2.size = 2 && (List.nth n1.size 1) = (List.hd n2.size)
    then [List.hd n1.size; List.nth n2.size 1]
    else failwith ("Invalid dimensions for matmul " ^ n1.id ^ " " ^ n2.id)
  end in
  let node = {id=id; nodetype=nodetype; size=size} in
  (node, {nc=nc'; nm = (id,node)::gr.nm; ol = new_output_list node [n1;n2] gr.ol})

let add n1 n2 ?(prefix="") gr =
  if contains_optimizer [n1.nodetype; n2.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let (n1, n2, gr) = broadcast n1 n2 ~prefix:prefix gr in
  let nodetype = Operation (Add (n1, n2)) in
  let (id, nc') = gen_id nodetype gr.nc ~prefix:prefix in
  let size = begin
    if List.length n1.size = 2 && n1.size = n2.size
    then n1.size
    else failwith  ("Invalid dimensions for add " ^ n1.id ^ " " ^ n2.id)
  end in
  let node = {id=id; nodetype=nodetype; size=size} in
  (node, {nc=nc'; nm = (id,node)::gr.nm; ol = new_output_list node [n1;n2] gr.ol})

let squared_loss n1 n2 ?(prefix="") gr =
  if contains_optimizer [n1.nodetype; n2.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Operation (SquareLoss (n1, n2)) in
  let (id, nc') = gen_id nodetype gr.nc ~prefix:prefix in
  let size = begin
    if List.length n1.size = 2 && n1.size = n2.size
    then n1.size
    else failwith  ("Invalid dimensions for sqloss " ^ n1.id ^ " " ^ n2.id)
  end in
  let node = {id=id; nodetype=nodetype; size=size} in
  (node, {nc=nc'; nm = (id,node)::gr.nm; ol = new_output_list node [n1;n2] gr.ol})

let softmax n1 ?(prefix="") gr =
  if contains_optimizer [n1.nodetype]
  then failwith "Cannot calculate softmax of optimizer"
  else
  let nodetype = Operation (Softmax (n1)) in
  let (id, nc') = gen_id nodetype gr.nc ~prefix:prefix in
  let size = n1.size in
  let node = {id=id; nodetype=nodetype; size=size} in
  (node, {nc=nc'; nm=(id,node)::gr.nm; ol=new_output_list node [n1] gr.ol})

let sigmoid n ?(prefix="") gr =
  if contains_optimizer [n.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Operation (Sigmoid n) in
  let (id, nc') = gen_id nodetype gr.nc ~prefix:prefix in
  let node = {id=id; nodetype=nodetype; size=n.size} in
  (node, {nc=nc'; nm = (id,node)::gr.nm; ol = new_output_list node [n] gr.ol})

let trans n ?(prefix="") gr =
  if contains_optimizer [n.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Operation (T n) in
  let (id, nc') = gen_id nodetype gr.nc ~prefix:prefix in
  let size = begin
    if List.length n.size = 2
    then List.rev n.size
    else failwith "Invalid dimensions"
  end in
  let node = {id=id; nodetype=nodetype; size=size} in
  (node, {nc=nc'; nm = (id,node)::gr.nm; ol = new_output_list node [n] gr.ol})

let minus n1 n2 ?(prefix="") gr =
  if contains_optimizer [n1.nodetype; n2.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let (n1, n2, gr) = broadcast n1 n2 ~prefix:prefix gr in
  let nodetype = Operation (Minus (n1, n2)) in
  let (id, nc') = gen_id nodetype gr.nc ~prefix:prefix in
  let size = begin
    if List.length n1.size = 2 && n1.size = n2.size
    then n1.size
    else failwith  ("Invalid dimensions for minus" ^ n1.id ^ " " ^ n2.id)
  end in
  let node = {id=id; nodetype=nodetype; size=size} in
  (node, {nc=nc'; nm = (id,node)::gr.nm; ol = new_output_list node [n1;n2] gr.ol})

let pow n power ?(prefix="") gr =
  if contains_optimizer [n.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Operation (Pow (n, power)) in
  let (id, nc') = gen_id nodetype gr.nc ~prefix:prefix in
  let size = n.size in
  let node = {id=id; nodetype=nodetype; size=size} in
  (node, {nc=nc'; nm = (id,node)::gr.nm; ol = new_output_list node [n] gr.ol})

let neg n ?(prefix="") gr =
  if contains_optimizer [n.nodetype]
  then failwith "Cannot take negation of optimizer"
  else
  let nodetype = Operation (Negative (n)) in
  let (id, nc') = gen_id nodetype gr.nc ~prefix:prefix in
  let size = n.size in
  let node = {id=id; nodetype=nodetype; size=size} in
  (node, {nc=nc'; nm = (id,node)::gr.nm; ol = new_output_list node [n] gr.ol})

let reducesum n axis ?(prefix="") gr =
  if contains_optimizer [n.nodetype]
  then failwith "Cannot reduce sum on optimizer"
  else
  let nodetype = Operation (ReduceSum (n, axis)) in
  let (id, nc') = gen_id nodetype gr.nc ~prefix:prefix in
  let size = List.mapi (fun ind el -> if ind <> axis then el else 1) n.size in
  let node = {id=id; nodetype=nodetype; size=size} in
  (node, {nc=nc'; nm = (id,node)::gr.nm; ol = new_output_list node [n] gr.ol})

let mul n1 n2 ?(prefix="") gr =
  if contains_optimizer [n1.nodetype;n2.nodetype]
  then failwith "Cannot multiply an optimizer"
  else
  let (n1, n2, gr) = broadcast n1 n2 ~prefix:prefix gr in
  let nodetype = Operation (Mul (n1, n2)) in
  let (id, nc') = gen_id nodetype gr.nc ~prefix:prefix in
  let size = begin
    if List.length n1.size = 2 && n1.size = n2.size
    then n1.size
    else 
      let n1_size = List.fold_left (fun acc i -> acc ^ (string_of_int i) ^ "x") "" n1.size in
      let n2_size = List.fold_left (fun acc i -> acc ^ (string_of_int i) ^ "x") "" n2.size in
      failwith  ("Invalid dimensions for multiply " ^ n1_size ^ n1.id ^ " " ^ n2_size ^ n2.id)
  end in
  let node = {id=id; nodetype=nodetype; size=size} in
  (node, {nc=nc'; nm = (id,node)::gr.nm; ol = new_output_list node [n1;n2] gr.ol})

let log n ?(prefix="") gr =
  if contains_optimizer [n.nodetype]
  then failwith "Cannot take negation of optimizer"
  else
  let nodetype = Operation (Log (n)) in
  let (id, nc') = gen_id nodetype gr.nc ~prefix:prefix in
  let size = n.size in
  let node = {id=id; nodetype=nodetype; size=size} in
  (node, {nc=nc'; nm = (id,node)::gr.nm; ol = new_output_list node [n] gr.ol})


let crossentropyloss pred truth ?(prefix="") gr =
  if contains_optimizer [pred.nodetype;truth.nodetype]
  then failwith "Cannot run cross entropy loss on optimizer"
  else
  let lognode, gr = log pred ~prefix:prefix gr in
  let mulnode, gr = mul truth lognode ~prefix:prefix gr in
  let reducesumnode1, gr = reducesum mulnode 1 ~prefix:prefix gr in
  let reducesumnode2, gr = reducesum reducesumnode1 0 ~prefix:prefix gr in
  neg reducesumnode2 ~prefix:prefix gr

let grad_descent n lr ?(prefix="") gr =
  if contains_optimizer [n.nodetype]
  then failwith "Cannot add node to optimizer"
  else
  let nodetype = Optimizer (GradDesc(lr), n) in (* TODO: change learning rate *)
  let (id, nc') = gen_id nodetype gr.nc ~prefix:prefix in
  let node = {id=id; nodetype=nodetype; size=[]} in
  (node, {nc=nc'; nm = (id,node)::gr.nm; ol = new_output_list node [n] gr.ol})

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
    | Softmax (a) ->
      let (a_val, st1) = forward a gr st in
      let e_a_val = Arr.exp a_val in
      let total_val = Arr.(sum ~axis:1 (e_a_val)) in
      let ar = Arr.(e_a_val / total_val) in
      ar, add_node n ar st1
    | Negative (a) ->
      let (a_val, st1) = forward a gr st in
      let ar = Arr.neg a_val in
      ar, add_node n ar st1
    | ReduceSum (a, ax) ->
      let (a_val, st1) = forward a gr st in
      let ar = Arr.sum_reduce ~axis:[|ax|] a_val in
      ar, add_node n ar st1
    | Mul (n1, n2) ->
      let (a1, st1) = forward n1 gr st in
      let (a2, st2) = forward n2 gr st in
      let ar = Arr.mul a1 a2 in
      ar, ((merge_graphstates [st1;st2] st |> add_node n ar))
    | Log (n1)->
      let (a_val, st1) = forward n1 gr st in
      let ar = Arr.log a_val in
      ar, add_node n ar st1
    | Broadcast (n1, index_from_right, broadcasted_size, expanded) ->
      let (a_val, st1) = forward n1 gr st in
      if expanded then
        let ar = (Arr.repeat ~axis:0 (Arr.expand a_val (index_from_right + 1)) broadcasted_size) in
        ar, add_node n ar st1
      else
        let current_dims = Array.length (Arr.shape a_val) in
        let ar = Arr.repeat ~axis:(current_dims - index_from_right - 1) a_val broadcasted_size in
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
        backprop_graddesc a Arr.(grad * sig_val * ((ones [|1|]) - sig_val)) lr st
      | SquareLoss (pred, truth) ->
        let pred_val = st |> get_node pred in
        let truth_val = st |> get_node truth in
        backprop_graddesc pred Arr.(div_scalar (mul_scalar (pred_val - truth_val) 2.) (float_of_int (Arr.shape pred_val).(0))) lr st
      | T a ->
        backprop_graddesc a (Arr.transpose grad) lr st
      | Pow (a, p) ->
        let a_val = st |> get_node a in
        let p_minus_1 = p -. 1. in
        backprop_graddesc a Arr.(pow_scalar (mul_scalar a_val p) (p_minus_1)) lr st
      | Softmax a ->
        let sm = st |> get_node node in
        backprop_graddesc a Arr.((grad - (sum (grad * sm) ~axis:1)) * sm) lr st
      | Negative a ->
        backprop_graddesc a (Arr.neg grad) lr st
      | ReduceSum (a, ax) ->
        let axsize = List.nth (a.size) ax in
        let tilearray = if ax = 0 then [|axsize;1|] else [|1;axsize|] in
        backprop_graddesc a (Arr.tile grad tilearray) lr st
      | Mul (a, b) ->
        let a_val = st |> get_node a in
        let b_val = st |> get_node b in
        let st1 = backprop_graddesc a (Arr.mul grad b_val) lr st in
        let st2 = backprop_graddesc b (Arr.mul grad a_val) lr st in
        merge_graphstates [st1;st2] st
      | Log a ->
        let log_val = st |> get_node node in
        backprop_graddesc a (Arr.div grad log_val) lr st
      | Broadcast (a, index_from_right, size, expanded) ->
        let index = (List.length node.size) - index_from_right - 1 in
        let unbroadcasted_gradient = 
          let inside = Arr.((div_scalar (sum ~axis:index grad) (float_of_int (List.nth node.size index)))) in
          if expanded then Arr.(squeeze ~axis:[|index|] inside) else inside
        in
        backprop_graddesc a unbroadcasted_gradient lr st
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
    match backward_helper opt loss_node gr st [] max_iter with
    | (st, losses) -> st, (List.rev losses)

end