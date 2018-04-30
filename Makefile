test:
	ocamlbuild -use-ocamlfind -I tests test.byte && ./test.byte

check:
	bash checktypes.sh

zip:
	zip tensorflocaml.zip *.ml*
	
clean:
	ocamlbuild -clean
	rm -f tensorflocaml.zip