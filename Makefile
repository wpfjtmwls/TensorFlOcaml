test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

compile:
	ocamlbuild -use-ocamlfind graph.cmo graphstate.cmo

check:
	bash checkenv.sh && bash checktypes.sh

zip:
	zip tensorflocaml.zip *.ml*
	
clean:
	ocamlbuild -clean
	rm -f tensorflocaml.zip