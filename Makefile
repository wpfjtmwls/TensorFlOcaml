test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

compile:
	ocamlbuild -use-ocamlfind graphst.cmo grapho.cmo

check:
	bash checkenv.sh && bash checktypes.sh

zip:
	zip tensorflocaml.zip *.ml*
	
clean:
	ocamlbuild -clean
	rm -f tensorflocaml.zip