target = tensorcodegen

dim.out : $(target).native dim.tb add.o mult.o
	./$(target).native < dim.tb > dim.ll

add.o : add.c tensor.h
	cc -c add.c

mult.o : mult.c tensor.h
	cc -c mult.c

$(target).native : tensorscanner.mll tensorparser.mly tensorast.mli \
					tensorsemant.ml tensorsast.mli tensorcodegen.ml
	ocamlbuild $(target).native -pkgs llvm,llvm.analysis

.PHONY : clean
clean :
	rm -rf *.out _build *.native *.o *.ll *.s

# clang -emit-llvm -S -c test.c -o test.ll