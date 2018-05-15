(*
  Defines the model of a computational graph node, which are the nodes within 
  the computational graph that you define. The model is defined as a .ml file
  solely so that the types and definitions are exposed easily to the other 
  parts of Tensorflowcaml. 

  NODE DEFINITION 
  The nodes are defined as a record with four fields. The fields are as follows 
  - ID : string 
      The unique ID of the node. Used to access the node and its elements 
      later on.
  - Nodetype : nodetype 
      Computational nodes can be of four different general types 
      as described in further detail below. 
  - Size : dims 
      The dimensions of the node matrix
  - Log : logger option 
      The desired log of the node. The process of logging is outputting
      the nodes data when training the graph. The logger data type is 
      defined as a record with the following fields 
      LOGGER DEFINITION
        - filename : string
            The desired file that the loggers data should be outputted to 
        - interval : int
            On what interval should the logger write data to the file
        - counter : int ref 
            Pointer to a counter holding the number of the times that 
            the logger has written to file. Used when outputting to the file
      
NODETYPES
There are four general nodetypes for the user to choose from.
- Placeholder :
    Placeholder nodes are used to hold placeholder values used during other operations
- Variable :
    Variable nodes are nodes that hold variables to equations. ex. x in Ax+b
- Operation : 
      an operation node can consist of one of the following operations
      - Matrix Multiplication : Takes two nodes contaning matrices and multiplies
          them together.
      - Addition : Adds the values of two nodes together.
      - Square Loss : Calculates the square loss of two nodes.
      - Sigmoid : Calculates the sigmoid function given a node value, where
          the sigmoid function is 1 / (1 + exp{-x}) 
      - T : Calculates the transpose of a given nodes matrix
      - Power : Takes in a node and a float, and raises the nodes value to that
          power
      - Softmax : Calculates softmax regression on a given node 
      - Negation : Negates (- x) the value of a given node 
      - ReduceSum : Reduces the sum value of the node by given int
      - Multiplication : Takes two nodes (not containing matrices) and multiplies
          their values toegether 
      - Log : Takes the log of a given nodes value
      - Broadcast : Does the same as numpy broadcast 
      - CrossEntropyLoss : Calculates the cross entropy loss between two given nodes.
- Optimizer : 
    Takes in two nodes,  the optimizer and the optimizee. The optimizer will
    work to optimize the optimizee.
  
*)
open Owl
open Printf

(* Type definitions *)
(* Dimensions of the node matrix *)
type dims = int list

(* Node operations *)
type oper =
  | MatMul of (node * node)
  | Add of (node * node)
  | Minus of (node * node)
  | SquareLoss of (node * node)
  | Sigmoid of node
  | T of node
  | Pow of (node * float)
  | Softmax of node
  | Negative of node
  | ReduceSum of (node * int)
  | Mul of (node * node)
  | Log of (node)
  | Broadcast of (node * int * int * bool)
  | CrossEntropyLoss of (node * node)

(* Different types of Node Optimizers *)
and optm =
  | GradDesc of float

(* The specific node type *)
and nodetype =
  | Placeholder
  | Variable
  | Operation of oper
  | Optimizer of (optm * node)

(* Definition of the logger record *)
and logger = {filename: string; interval: int; counter: int ref}

(* Definition of the logger. If *)
and node = {id: string; nodetype: nodetype; size: dims; log: logger option}

(* Helper function. Convert dimension to string *)
let string_of_dims dims =
  let s = List.fold_left (fun ac el -> ac ^ "x" ^ (string_of_int el)) "" dims in
  String.sub s 1 (String.length s - 1)

(* Helper function. Convert Arr.shape to string *)
let string_of_shape sh =
  let s = Array.fold_left (fun ac el -> ac ^ "x" ^ (string_of_int el)) "" sh in
  String.sub s 1 (String.length s - 1)

(* Helper function. Converts Arr.shape to dims *)
let dims_of_shape sh =
  let f = fun ac el -> ac @ [el] in
  Array.fold_left f [] sh

(* Return true iff n.size matches the shape of ar *)
let matches_array_shape n ar =
  ar |> Arr.shape |> dims_of_shape = n.size

(* Updates the node log by writing to file *)
let update_node_log (n:node) (value:float) =
  match n.log with
  | Some l -> 
    l.counter := !(l.counter) + 1;
    if !(l.counter) mod l.interval = 0 then
      (* write to file *)
      let oc = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o666 l.filename in
      fprintf oc "%s|%d|%f\n" n.id !(l.counter) value;
      close_out oc;
  | None -> ()

(* Definition for an empty placeholder node *)
let empty = {id="";nodetype=Placeholder;size=[];log=None;}
