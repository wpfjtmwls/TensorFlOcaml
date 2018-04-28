(* Graph state object *)
module GraphState = struct
  open Owl
  open List
  
  (* [nodeid] type is the naming convention for nodes *)
  type nodeid = string 
  
  (* [Graphstate.st] is a mapping from nodeid to the corresponding owl matrix *)
  type st = (nodeid * Arr.arr) list 
  
  (* [Empty] is an empty mapping *)
  let empty = [] 
  
  let add_node id matrix state = match mem_assoc id state with
    | true -> (id, matrix)::(remove_assoc id state)
    | false -> (id, matrix)::state
  
  let get_node id state = match mem_assoc id state with 
    | true -> assoc id state
    | false -> failwith "Exception : No such node id exists in Graph State"
  
  (* [update_state] updates the old state with new state 
   * [requires] : new state, old state, and ids that have been updated
   * [returns] : (updated state, updated ids) as a tuple
   *)
  let rec update_state new_st update_st acc = match new_st with
    | [] -> (update_st, acc)
    | tup::t -> 
      let id = fst tup in 
      let new_matrix = snd tup in
      begin match assoc_opt id update_st with 
      | None -> failwith "Exception : New Graph states lost a mapping"
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
    merge_graphstates_helper new_st_lst old_st []
    
end