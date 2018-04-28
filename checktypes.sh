#!/bin/bash
# DO NOT EDIT THIS FILE

ocamlbuild -use-ocamlfind graph.cmo graphst.cmo test.cmo test.byte
if [[ $? -ne 0 ]]; then
  cat <<EOF
===========================================================
WARNING

Your code currently does not compile.  You a little bitch. Check the
error messages above carefully to determine what is wrong.
See stackoverflow for help if you cannot determine what is
wrong.
===========================================================
EOF
  exit 1
fi

if ocamlbuild -use-ocamlfind checktypes.byte ; then
  cat <<EOF
===========================================================
Your function names and types look good to me.
Congratulations!
===========================================================
EOF
else
  cat <<EOF
===========================================================
WARNING

Your function names and types look broken to me.  The code
that you submit is garbage.  Please fix your names and
types.  Check the error messages above carefully to
determine what is wrong.  See your local doctor for help if you
cannot determine what is wrong.
===========================================================
EOF
fi

