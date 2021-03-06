(*
  Defines the actual computational graph object, and operations
  that act on the graph as a whole, such as load, forward, 
  backwards, etc. 
*)
open Owl
open Tfgraphst
open Tfnode
open Yojson.Basic.Util

module Graph : sig
  
  (* type t is a type of Graph *)
  type t

  val empty : t

  (* ------------ Load and Save --------------- *)

  (* [save gr path] writes gr to a save file at path *)
  val save : t -> string -> unit

  (* [load path] loads the save file at path and returns a graph 
   * precondition : json should be valid file generated by [save] function
  *)
  val load : string -> t

  (* ------------ Node Creation --------------- *)

  (* [variable] a matrix dimension as int list and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : int list representing matrix dimensions and a graph
  * [outputs] : a new variable node with initial matrix depending on the matrix dimension and a graph where the new node was added
  *)
  val variable : int list -> ?prefix:string -> ?logger:logger option -> t -> (node * t)

  (* [placeholder] a matrix dimension as int list and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : int list representing matrix dimensions and a graph
  * [outputs] : a new placeholder node with initial matrix depending on the matrix dimension and a graph where the new node was added
  *)
  val placeholder : int list -> ?prefix:string -> ?logger:logger option -> t -> (node * t)

  (* [matmul] takes two nodes and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : two nodes and a graph. Both nodes are two-dimensional and the second dimension of the
  * first node equals the first dimension of the second.
  Neither node can be an optimizer.
  * [outputs] : a new node that is multiplication of two nodes and a graph where the new node was added
  *)
  val matmul : node -> node -> ?prefix:string -> ?logger:logger option -> t -> (node * t)

  (* [add] takes two nodes and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : two nodes and a graph. Both nodes are two-dimensional and have the same shape.
  * Neither node can be an optimizer.
  * [outputs] : a new node that is addition of two nodes and a graph where the new node was added
  *)
  val add : node -> node -> ?prefix:string -> ?logger:logger option -> t -> (node * t)

  (* [squared_loss] takes two nodes and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : two nodes and a graph. Both nodes are two-dimensional and have the same shape.
  * Neither node can be an optimizer.
  * [outputs] : a new node that is the calculation of loss function between two nodes and a graph where the new node was added
  *)
  val squared_loss : node -> node -> ?prefix:string -> ?logger:logger option -> t -> (node * t)

  (* [softmax] takes one node and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : one node and a graph. Node should be [n;1] dimensional
  * Node cannot be optimizer.
  * [outputs] : a new node that is the calculation of the softmax function between two nodes and a graph where the new node was added
  *)
  val softmax : node -> ?prefix:string -> ?logger:logger option -> t -> (node * t)

  (* [sigmoid] takes a node and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : a node and a graph. The node cannot be an optimizer.
  * [outputs] : a new node that is the output of sigmoid function of the input node and a graph where the new node was added
  *)
  val sigmoid : node -> ?prefix:string -> ?logger:logger option -> t -> (node * t)

  (* [trans] takes a node and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : a node and a graph. The node is two-dimensional and cannot be an optimizer.
  * [outputs] : a new node that is the output of transpose of the input node and a graph where the new node was added
  *)
  val trans : node -> ?prefix:string -> ?logger:logger option -> t -> (node * t)

  (* [minus] takes two nodes and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : two nodes and a graph. Both nodes are two-dimensional and have the same shape.
  * Neither node can be an optimizer.
  * [outputs] : a new node that is subtraction of two nodes and a graph where the new node was added
  *)
  val minus : node -> node -> ?prefix:string -> ?logger:logger option -> t -> (node * t)

  (* [pow] takes a node, a float, and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : a node, a float, and a graph. The node cannot be an optimizer.
  * [outputs] : a new node that is float power of node and a graph where the new node was added
  *)
  val pow : node -> float -> ?prefix:string -> ?logger:logger option -> t -> (node * t)

  (* [neg] takes a node, and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : a node and a graph. The node cannot be an optimizer.
  * [outputs] : a new node that is the negation of the node and a graph where the new node was added
  *)
  val neg : node -> ?prefix:string -> ?logger:logger option -> t -> (node * t)

  (* [reducesum] takes a node, an axis, and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : a node and a graph. The node cannot be an optimizer.
  * [outputs] : a new node that is the sum of the old node along the specified axis
       and a graph where the new node was added
  *)
  val reducesum : node -> int -> ?prefix:string -> ?logger:logger option -> t -> (node * t)

  (* [reducesum] takes two nodes and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : two nodes of same dimensions and a graph. The nodes cannot be an optimizer.
  * [outputs] : a new node that is the element-wise multiplication of the input nodes
       and a graph where the new node was added
  *)
  val mul : node -> node -> ?prefix:string -> ?logger:logger option -> t -> (node * t)

  (* [log] takes a node and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : The node cannot be an optimizer.
  * [outputs] : a new node that is the log of the input node
       and a graph where the new node was added
  *)
  val log : node -> ?prefix:string -> ?logger:logger option -> t -> (node * t)

  (* [crossentropyloss] takes two nodes and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : first node is the output prediction and second node is the input label. Cannot be optimizer
    nodes
  * [outputs] : a new node that is the cross-entropy loss and a graph where the new node was added
  *)
  val crossentropyloss : node -> node -> ?prefix:string -> ?logger:logger option -> t -> (node * t)

  (* [grad_descent] takes a node, a learning rate and a graph as inputs and outputs a new node and graph as a tuple
  * [requires] : a node and a graph. The node cannot be an optimizer.
  * [outputs] : a new node with a specific optimizer and a graph where the new node was added
  *)
  val grad_descent : node -> float -> ?prefix:string -> ?logger:logger option -> t -> (node * t)

  (* ------------ Runners --------------- *)

  (* [forward] takes a node, a graph, and a graph state inputs and outputs the resulting matrix
  * [requires] : a node, a graph, and a graph state 
  * [outputs] : the resulting matrix from forward pass of previous nodes and the new graphstate
  *)
  val forward : node -> t -> GraphState.st -> Arr.arr * GraphState.st

  (* [backword] takes a node, a graph, and a graph state inputs and outputs the resulting matrix
  * [requires] : a node, a graph, and a graph state 
  * [outputs] : changed GraphState from backward pass into previous nodes and update the according mutable fields
  *)
  val backward : node -> t -> GraphState.st -> GraphState.st

  (* [train] takes an optimizer node, graph and a list of graph state inputs and outputs the resulting
   *         state after training iteratively on each element of the graph state inputs
   * [requires] : lengths of each graph state input lists to be equal, each element represents one training batch and
                  has to have the same number of rows
   * [outputs] : GraphState.st after training and a list of floats representing the loss function over time
  *)
  val train : node -> t -> (node * (Arr.arr list)) list -> ?max_iter:int -> ?delta:float -> ?log_loss_every_ith:int -> GraphState.st  -> (GraphState.st * (float list))

end