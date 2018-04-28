open GraphState

(* type t is a type of Graph *)
type t 

(* type nodetype is a type of node eg Placeholder, Variable, Operation, Optimizer *)
type nodetype

(* type node is a record with id and nodetype *)
type node = {id: string ; nodetype: nodetype}

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
val 