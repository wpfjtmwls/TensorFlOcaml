open GraphState

module Graph = struct

  type node_counts = {nVar: int;
                      nPlaceholder: int; 
                      nMatmul: int;
                      nAdd: int;
                      nSquareLoss: int;
                      nSigmoid: int;
                      nGradDesc: int;}

  type oper =
    | MatMul
    | Add
    | SquareLoss
    | Sigmoid

  and optm = 
    | GradDesc

  and nodetype =
    | Placeholder
    | Variable
    | Operation of oper
    | Optimizer of optm

  (* type node is a record with id and nodetype *)
  type node = {id: string; nodetype: nodetype}

  type t = (node option * node_counts) (* the node is the root *)

  let empty = (None, {nVar= 0;
                    nPlaceholder= 0;
                    nMatmul= 0;
                    nAdd= 0;
                    nSquareLoss= 0;
                    nSigmoid= 0;
                    nGradDesc= 0;})

  

end