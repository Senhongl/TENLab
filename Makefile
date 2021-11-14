dim.out : tensordim.native dim.tb
	./tensordim.native < dim.tb > dim.out

tensordim.native : tensorscanner.mll tensorparser.mly tensorast.mli tensordim.ml
	ocamlbuild tensordim.native

.PHONY : clean
clean :
	rm -rf *.out _build *.native