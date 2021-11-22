compiler = tensorcodegen

test.out : test
	./$< > $@

test : test.s
	@if [ -d build ]; then echo "build exist"; else mkdir build; fi
	cd build && cmake .. && make

test.s : $(compiler).native test.tl
	./$(compiler).native < test.tl > test.ll; \
	llc -relocation-model=pic test.ll > test.s

$(compiler).native : tensorscanner.mll tensorparser.mly tensorast.mli \
		tensorsemant.ml tensorsast.mli tensorcodegen.ml
	ocamlbuild $(compiler).native -pkgs llvm,llvm.analysis

.PHONY : clean
clean : cleandir
	rm -rf *.out _build build *.native *.o *.ll *.s
cleandir :
	@if [ -d build ]; then make -C build clean; \
	else echo "build not exist"; fi

# clang -emit-llvm -S -c test.c -o test.ll