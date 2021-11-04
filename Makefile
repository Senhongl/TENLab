all: tenlab.out

tenlab : ast.cmo parser.cmo scanner.cmo tenlab.cmo
	ocamlc -w A -o tenlab $^

%.cmo : %.ml
	ocamlc -w A -c $<

%.cmi : %.mli
	ocamlc -w A -c $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc $^

tenlab.out : tenlab tenlab.tb
	./tenlab < tenlab.tb > tenlab.out

# Depedencies from ocamldep
tenlab.cmo : scanner.cmo parser.cmi ast.cmo
tenlab.cmx : scanner.cmx parser.cmx ast.cmo
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmo parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx