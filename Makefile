.PHONY: test
test: all test.sh
	./test.sh tests/test-*.tl

.PHONY: all
all : tenlab.native

tenlab.native:
	opam config exec -- \
	ocamlbuild -I src -use-ocamlfind tenlab.native

.PHONY: clean
clean:
	rm -rf *.cmi *.cmo
	rm -rf parser.ml parser.mli
	rm -rf scanner.ml
	rm -rf tenlab