.PHONY: test
test: all generate.sh test.sh
	# ./generate.sh ./tests/*.tl
	# ./test.sh ./tests/*.tl
	./generate.sh ./tests/test-build-in.tl
	./test.sh ./tests/test-build-in.tl

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
