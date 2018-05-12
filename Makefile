test:
	ocamlbuild -use-ocamlfind -I tests test.byte && ./test.byte

test-mnist:
	ocamlbuild -use-ocamlfind -I tests test_mnist.byte && ./test_mnist.byte

check:
	bash checktypes.sh

zip:
	zip tensorflocaml.zip *.ml*
	
clean:
	ocamlbuild -clean
	rm -f tensorflocaml.zip
