#!/bin/bash
# DO NOT EDIT THIS FILE

ocamlbuild -use-ocamlfind -I tests check.cmo

if [[ $? -ne 0 ]]; then
  cat <<EOF
===========================================================
WARNING

Your code currently does not compile. Please allow 20 
minutes for crying before attempting a solution. Check the
error messages above carefully to determine what is wrong.
See stackoverflow for help if you cannot determine what is
wrong.
===========================================================
EOF
  exit 1
fi
# fileio.cmo jaynet.ml test.ml tf.ml tfchain.mli tfgraph.cmo tfgraphst.cmo tfnode.ml
# tfgraphst.cmo tfgraph.cmo tfnode.cmo test.cmo test.byte tf.cmo jaynet.cmo