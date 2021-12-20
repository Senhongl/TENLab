.PHONY: test
test: all generate.sh test.sh
	./generate.sh ./tests/pe-test/*.tl
	./test.sh ./tests/pe-test/*.tl

.PHONY: all
all: tenlab.native

tenlab.native:
	opam config exec -- \
	ocamlbuild -I src -use-ocamlfind tenlab.native

.PHONY: clean
clean: cleandir
	rm -rf *.ll *.out *.s *.diff *.exe *.err
	rm -rf tenlab.native
	rm -rf _build
	rm -rf pe.o

cleandir :
	@if [ -d build ]; then make -C build clean; \
	else echo "build not exist"; fi
