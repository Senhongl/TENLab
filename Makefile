.PHONY: test
test: all test.sh
	./test.sh

.PHONY: all
all: tenlab.native

tenlab.native:
	opam config exec -- \
	ocamlbuild -I src -use-ocamlfind tenlab.native

.PHONY: clean
clean:
	rm -rf *.ll *.out *.s *.diff *.exe
	rm -rf tenlab.native
	rm -rf _build