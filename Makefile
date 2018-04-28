test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

zip:
	zip tensorflocaml.zip *.ml*
	
clean:
	ocamlbuild -clean
	rm -f tensorflocaml.zip
