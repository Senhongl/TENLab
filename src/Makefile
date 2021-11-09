# all: tenlab.out clean

# tenlab : ast.cmo parser.cmo scanner.cmo sast.cmo semant.cmo codegen.cmo tenlab.cmo
# 	ocamlc -w A -o tenlab $^

# %.cmo : %.ml
# 	ocamlc -w A -c $<

# %.cmi : %.mli
# 	ocamlc -w A -c $<

# scanner.ml : scanner.mll
# 	ocamllex $^

# parser.ml parser.mli : parser.mly
# 	ocamlyacc $^

# tenlab.out : tenlab tenlab.tb
# 	./tenlab < tenlab.tb > tenlab.out

# # Depedencies from ocamldep
# tenlab.cmo : scanner.cmo parser.cmi ast.cmo sast.cmo semant.cmo codegen.cmo
# tenlab.cmx : scanner.cmx parser.cmx ast.cmo sast.cmo semant.cmo codegen.cmo
# parser.cmo : ast.cmo parser.cmi
# parser.cmx : ast.cmo parser.cmi
# scanner.cmo : parser.cmi
# scanner.cmx : parser.cmx
.PHONY: all
all : tenlab.native builtin.o

tenlab.native:
	opam config exec -- \
	ocamlbuild -use-ocamlfind tenlab.native

builtin : builtin.c
	cc -o builtin builtin.c

.PHONY: clean
clean:
	rm -rf *.cmi *.cmo
	rm -rf parser.ml parser.mli
	rm -rf scanner.ml
	rm -rf tenlab