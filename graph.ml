module Graph = struct

  type node_counts = {nVar: int;
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

  (* type node is a record with id and nodetype *)
  and node = {id: string; nodetype: nodetype}

  type t = {nc: node_counts;}

  let empty ={nVar= 0;
              nPlaceholder= 0;
              nMatmul= 0;
              nAdd= 0;
              nSquareLoss= 0;
              nSigmoid= 0;
              nGradDesc= 0;}


  (* Helper function. Converts nodetype to string. *)    
  let to_string = function
    | Placeholder -> "PH"
    | Variable -> "VAR"
    | Operation o -> (match o with
      | MatMul _ -> "MM"
      | Add _ -> "Add"
      | SquareLoss _ -> "SL"
      | Sigmoid _ -> "SIG")
    | Optimizer o -> (match o with
      | GradDesc _ -> "GD")

  (* Helper function. Gets appropriate value from node_counts *)
  let get_node_count nc = function
  | Placeholder -> nc.nPlaceholder
    | Variable -> nc.nVar
    | Operation o -> (match o with
      | MatMul _ -> nc.nMatmul
      | Add _ -> nc.nAdd
      | SquareLoss _ -> nc.nSquareLoss
      | Sigmoid _ -> nc.nSigmoid)
    | Optimizer o -> (match o with
      | GradDesc _ -> nc.nGradDesc)

  (* Helper function. Returns nc with the appropriate value incremented *)
  let incr_node_count nc = function
  | Placeholder -> {nc with nPlaceholder = nc.nPlaceholder + 1}
    | Variable -> {nc with nVar = nc.nVar + 1}
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

end