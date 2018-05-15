# TensorFlOcaml

## Final Project for CS3110 (Functional Programming) at Cornell
Our goal was to define a Tensorflow like Api using the OCaml language. Users can use the TensorFlOwcaml api to define their own nueral nets, and train them on specific data. 

## Dependencies:
* Ocaml >= 4.06.0
* Opam
* Utop  
* Cmake
* Camlimages (for image loading)
* Pkg-config
* Plplot
* Owl Numerical Library
    * alcotest
    * core
    * ctypes
    * eigen 
    * ocaml-compiler-libs
    * openblas


## Setup:
1. First update Opam. This is critical because some older versions of opam may cause errors. 
```bash 
opam update && opam upgrade
```
2. Install OpenBlas onto your system. If you're using OSX and have homebrew then you can do this with 
```bash
brew install openblas
```
3. You'll then want to get the latest version of the owl repo. 
```bash 
git clone https://github.com/owlbarn/owl.git
```
I put the repo in '~/.opam/packages' but you can put it wherever you want really.
You'll then want to follow the instructions on the owl github page. 
First we want to see if we have any outside dependencies missing. Run the following command while inside the owl repo to see 
what you're missing.
```bash 
jbuilder external-lib-deps --missing @install
```
4. Most likely you'll still be missing some dependencies. One is called Plplot. Confusion may stem from trying to install Plplot via opam. Turns out, ocamls plplot
depends on the plplot software, which you'll need to install from http://plplot.sourceforge.net/downloads.php or if you're on mac. 
```bash 
brew install cmake
brew install pkg-config

opam install ocaml-compiler-libs
opam install core
opam install eigen
opam install alcotest
opam install ctypes

brew install plplot
```
If you install via brew, you'll most likely need to run the following commands 
```bash
echo 'export PATH="/usr/local/opt/icu4c/bin:$PATH"' >> ~/.zshrc
echo 'export PATH="/usr/local/opt/icu4c/sbin:$PATH"' >> ~/.zshrc
```
Replace ".zshrc" with ".bashrc" if you use bash instead of zshrc. 
5. Once you've done that, run
```bash
opam install plplot
opam install camlimages
```
should work. 
6. Finally run within the owl directory
```bash
cd ~/.opam/packages/owl
make && make install
```
7. If you get issues during this such as "Assertion Failed Backtrace", then run 
```bash
jbuilder build @install
```
and see if it returns any external libraries. If it does, you'll need to install those. 

8. If this works you should be able to run 
```bash
#require "owl-top"
```
in utop succsessfully. 
9. You made it! You're ready to get started with TensorFlowcaml

## Compilation (Makefile):
`make test` to compile and then run test.ml

`make check` to run checktypes.sh

`make zip` to archive the binaries

`make clean` to remove binaries



## Use
Now that you've installed Tensorflowcaml, lets get you set up with your own nueral net!
First you'll want to create a graph and graphstate. You can then fill those with the nodes you want.
An example from "test_mnist" is 
```ocaml
let graph = Graph.empty
let graphst = GraphState.empty
let (x, graph) = graph |> Graph.placeholder (Array.to_list (Dense.Ndarray.Generic.shape xtrainbatches.(0)))
let (y, graph) = graph |> Graph.placeholder (Array.to_list (Dense.Ndarray.Generic.shape ytrainbatches.(0)))
let (loss, graph, graphst) = MnistNet.create [x;y] (MnistNet.default_name) graph graphst
let (opt, graph) = graph |> Graph.grad_descent loss 0.01
```
After creating the computational graph you can train it!
```ocaml
let (graphst, losslist) = Graph.train opt graph [(x, (Array.to_list xtrainbatches)); (y, (Array.to_list ytrainbatches))] ~max_iter:100 ~delta:0.001 ~log_loss_every_ith:10 graphst
```
To print how your net performs, lets print some stats

```ocaml
let _ = Printf.printf "Ending Accuracy on trg set: %.5f\n" (get_accuracy (Array.sub xtrainbatches 0 10) (Array.sub ytrainbatches 0 10) graph graphst)
let _ = Printf.printf "Ending Accuracy on test set: %.5f\n" (get_accuracy (Array.sub xtestbatches 0 10) (Array.sub ytestbatches 0 10) graph graphst)
let _ = Printf.printf "Ending loss on training set: %.5f\n" (get_loss xtrainbatches.(0) ytrainbatches.(0) graph graphst)
let _ = Printf.printf "Ending loss on test set: %.5f\n" (get_loss xtestbatches.(0) ytestbatches.(0) graph graphst)
```

And thats it! In pure Ocaml fashion be sure to look at the example nets, created within the tests folder and the three test files to get a feel 
for creating nets. We hope you enjoy!!

## Tests
Various tests are contained within the tests folder. 
Each test (Alexnet, Jaynet, ...) defines a nueral net which you can use in other ml 
files or tests with 
```ocaml
open Alexnet 
```

Saved graphstates will also deposited in the tests folder. 