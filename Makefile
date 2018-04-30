test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

compile:
	ocamlbuild -use-ocamlfind fileio.cmo jaynet.ml test.ml tf.ml tfchain.mli tfgraph.cmo tfgraphst.cmo tfnode.ml

check:
	bash checktypes.sh

zip:
	zip tensorflocaml.zip *.ml*
	
clean:
	ocamlbuild -clean
	rm -f tensorflocaml.zip