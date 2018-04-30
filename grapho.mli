open Owl
open Graphst
open Node

module Graph : sig
(* type t is a type of Graph *)
  type t 

  (* type empty is empty type of Graph *)
  val empty : t

  (* ------------ Node Creation --------------- *)

  (* [variable] a matrix dimension as int list and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : int list representing matrix dimensions and a graph
  * [outputs] : a new variable node with initial matrix depending on the matrix dimension and a graph where the new node was added
  *)
  val variable : int list -> t -> (node * t)

  (* [placeholder] a matrix dimension as int list and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : int list representing matrix dimensions and a graph
  * [outputs] : a new placeholder node with initial matrix depending on the matrix dimension and a graph where the new node was added
  *)
  val placeholder : int list -> t -> (node * t)

  (* [matmul] takes two nodes and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : two nodes and a graph. Both nodes are two-dimensional and the second dimension of the
  * first node equals the first dimension of the second.
  Neither node can be an optimizer.
  * [outputs] : a new node that is multiplication of two nodes and a graph where the new node was added
  *)
  val matmul : node -> node -> t -> (node * t)

  (* [add] takes two nodes and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : two nodes and a graph. Both nodes are two-dimensional and have the same shape.
  * Neither node can be an optimizer.
  * [outputs] : a new node that is addition of two nodes and a graph where the new node was added
  *)
  val add : node -> node -> t -> (node * t)

  (* [squared_loss] takes two nodes and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : two nodes and a graph. Both nodes are two-dimensional and have the same shape.
  * Neither node can be an optimizer.
  * [outputs] : a new node that is the calculation of loss function between two nodes and a graph where the new node was added
  *)
  val squared_loss : node -> node -> t -> (node * t)

  (* [sigmoid] takes a node and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : a node and a graph. The node cannot be an optimizer.
  * [outputs] : a new node that is the output of sigmoid function of the input node and a graph where the new node was added
  *)
  val sigmoid : node -> t -> (node * t)

  (* [trans] takes a node and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : a node and a graph. The node is two-dimensional and cannot be an optimizer.
  * [outputs] : a new node that is the output of transpose of the input node and a graph where the new node was added
  *)
  val trans : node -> t -> (node * t)

  (* [minus] takes two nodes and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : two nodes and a graph. Both nodes are two-dimensional and have the same shape.
  * Neither node can be an optimizer.
  * [outputs] : a new node that is subtraction of two nodes and a graph where the new node was added
  *)
  val minus : node -> node -> t -> (node * t)

  (* [pow] takes a node, a float, and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : a node, a float, and a graph. The node cannot be an optimizer.
  * [outputs] : a new node that is float power of node and a graph where the new node was added
  *)
  val pow : node -> float -> t -> (node * t)

  (* [grad_descent] takes a node and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : a node and a graph. The node cannot be an optimizer.
  * [outputs] : a new node with a specific optimizer and a graph where the new node was added
  *)
  val grad_descent : node -> t -> (node * t)

  (* ------------ Runners --------------- *)

  (* [forward] takes a node, a graph, and a graph state inputs and outputs the resulting matrix
  * [requires] : a node, a graph, and a graph state 
  * [outputs] : the resulting matrix from forward pass of previous nodes and the new graphstate
  *)
  val forward : node -> t -> GraphState.st -> Arr.arr * GraphState.st

  (* [backword] takes a node, a graph, and a graph state inputs (max_iters and delta are optional arguments) and outputs the resulting matrix
  * [requires] : a node, a graph, and a graph state 
  * [outputs] : changed GraphState from backward pass into previous nodes and update the according mutable fields
  *)
  val backward : node -> t -> GraphState.st -> GraphState.st

end