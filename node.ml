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