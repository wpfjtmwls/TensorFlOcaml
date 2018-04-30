test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

compile:
	ocamlbuild -use-ocamlfind -I tests graphst.cmo grapho.cmo

check:
	bash checktypes.sh

zip:
	zip tensorflocaml.zip *.ml*
	
clean:
	ocamlbuild -clean
	rm -f tensorflocaml.zip