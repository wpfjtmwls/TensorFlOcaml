open Graphstate
module type Graph = sig 
  (* type t is a type of Graph *)
  type t 

  (* type empty is empty type of Graph *)
  val empty : t

  (* type nodetype is a type of node eg Placeholder, Variable, Operation, Optimizer *)
  type nodetype

  (* type node is a record with id and nodetype *)
  type node = {id: string; nodetype: nodetype}

  (* ------------ Operation --------------- *)

  (* [matmul] takes two nodes and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : two nodes and a graph 
  * [outputs] : a new node that is multiplication of two nodes and a graph where the new node was added
  *)
  val matmul : node -> node -> t -> (node * t)

  (* [add] takes two nodes and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : two nodes and a graph 
  * [outputs] : a new node that is addition of two nodes and a graph where the new node was added
  *)
  val add : node -> node -> t -> (node * t)

  (* [squared_loss] takes two nodes and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : two nodes and a graph 
  * [outputs] : a new node that is the calculation of loss function between two nodes and a graph where the new node was added
  *)
  val squared_loss : node -> node -> t -> (node * t)

  (* [sigmoid] takes a node and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : a node  and a graph 
  * [outputs] : a new node that is the output of sigmoid function of the input node and a graph where the new node was added
  *)
  val sigmoid : node -> t -> (node * t)

  (* ------------ Node Creation --------------- *)

  (* [create_variable] a matrix dimension as int list and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : int list representing matrix dimensions and a graph
  * [outputs] : a new variable node with initial matrix depending on the matrix dimension and a graph where the new node was added
  *)
  val create_variable : int list -> t -> (node * t)

  (* [create_placeholder] a matrix dimension as int list and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : int list representing matrix dimensions and a graph
  * [outputs] : a new placeholder node with initial matrix depending on the matrix dimension and a graph where the new node was added
  *)
  val create_placeholder : int list -> t -> (node * t)


  (* ------------ Optimizer Creation --------------- *)

  (* [create_grad_descent] takes a node and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : a node  and a graph 
  * [outputs] : a new node with a specific optimizer and a graph where the new node was added
  *)
  val create_grad_descent : node -> t -> (node * t)

  (* ------------ Runners --------------- *)

  (* [forward] takes a node, a graph, and a graph state inputs and outputs the resulting matrix
  * [requires] : a node, a graph, and a graph state 
  * [outputs] : the resulting matrix from forward pass of previous nodes
  *)
  val forward : node -> t -> Graphstate.t -> matrix 

  (* [backword] takes a node, a graph, and a graph state inputs (max_iters and delta are optional arguments) and outputs the resulting matrix
  * [requires] : a node, a graph, and a graph state 
  * [outputs] : changed GraphState from backward pass into previous nodes and update the according mutable fields
  *)
  val backward : node -> t -> Graphstate.t -> ?max_iters:int -> ?delta:int -> Graphstate.t

end
