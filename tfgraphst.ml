open Owl
open List
open Tfnode

(* Graph state object *)
module GraphState = struct
  
  (* [nodeid] type is the naming convention for nodes *)
  type nodeid = string 
  
  (* [Graphstate.st] is a mapping from nodeid to the corresponding owl matrix.
   * RI: for any node n, if n is mapped to Arr.arr a, then n.dims matches a.shape *)
  type st = (nodeid * Arr.arr) list 
  
  (* [Empty] is an empty mapping *)
  let empty = []
  
  let add_node n matrix state = 
    if (matches_array_shape n matrix) then 
      match mem_assoc n.id state with
      | true -> (n.id, matrix)::(remove_assoc n.id state)
      | false -> (n.id, matrix)::state
    else 
      failwith "Matrix Dimensions Mismatch. Check yo matrix sizes!"
  
  let get_node n state = match mem_assoc n.id state with 
    | true -> assoc n.id state
    | false -> failwith "Exception : No such node id exists in Graph State"

  let get_node_by_id nodeid state = match mem_assoc nodeid state with
  | true -> assoc nodeid state
  | false -> failwith "Exception : No such node id in graphstate"
  
  (* (* [update_state] updates the old state with new state 
   * [requires] : new state, old state, and ids that have been updated
   * [returns] : (updated state, updated ids) as a tuple
   *)
  let rec update_state new_st update_st acc = match new_st with
    | [] -> (update_st, acc)
    | (id, new_matrix)::t -> 
      begin match assoc_opt id update_st with
      | None -> 
        let new_update_st = (id, new_matrix)::update_st in
        let new_acc = id::acc in
        update_state t new_update_st new_acc
      | Some old_matrix -> 
        begin match new_matrix = old_matrix with
        | true -> update_state t update_st acc 
        | false -> 
          begin 
            if mem id acc then update_state t update_st acc
            else 
            let new_update_st = (id, new_matrix)::(remove_assoc id update_st) in
            let new_acc = id::acc in
            update_state t new_update_st new_acc
          end
        end
      end
  
  (* [merge_graphstates_helper] iterates through a list of new states and updates the old state accordingly
   * [requires] : a list of new states, old state, and ids that have been updated
   * [returns] : updated list after the iteration of the new states 
   *)
  let rec merge_graphstates_helper new_st_lst update_st acc = 
    match new_st_lst with 
    | [] -> update_st
    | new_st::t -> 
      let (new_update_st, new_acc) = update_state new_st update_st acc in
      merge_graphstates_helper t new_update_st new_acc
  
  let merge_graphstates new_st_lst old_st = 
    merge_graphstates_helper new_st_lst old_st [] *)

  let merge_graphstates (new_st_lst : st list) (old_st : st) =
    (* Fold over list of new_st and accum (state, updated_ids) *)
    let updated_state =
      List.fold_left (
        fun acc_st new_st ->
          (* Fold over individual new_st and accum (state, updated_ids) *)
          List.fold_left (fun acc_st (id, mat) -> 
            match (List.assoc_opt id old_st), (List.assoc_opt id acc_st) with
            | None, None -> (id, mat)::acc_st
            | Some old_mat, Some acc_mat when Arr.(old_mat <> mat)-> (id, mat)::(List.remove_assoc id acc_st)
            | Some _, Some _ -> acc_st
            | Some _, None -> (id, mat)::acc_st
            | None, Some _ -> acc_st
          )
          acc_st new_st
      )
      old_st new_st_lst
    in updated_state

  let graphst_to_string st mat_to_string =
    let sorted = List.sort (fun (id1,_) (id2,_) -> String.compare id1 id2) st in
    List.fold_left (fun str (id, mat) -> str ^ id ^ ":\n" ^ (mat_to_string mat) ^ "\n\n")
    "" sorted

  let save_graphst st folderpath =
    let _ = List.map (fun (id, mat) -> (Mat.save mat (folderpath ^ "/" ^ id ^ ".tfgraphst"))) st in ()

  let load_graphst folderpath =
    Sys.readdir folderpath |> 
    Array.to_list |> 
    List.fold_left (fun acc path -> 
    if Filename.extension path = ".tfgraphst" then
      Filename.((remove_extension path), (Mat.load (concat folderpath path)))::acc
    else acc) []
end