target = tensorcodegen
link-dir = func
link-obj = add mult print

test.out : test.exe
	./$< > $@

test.exe : test.s $(foreach exe, $(link-obj), $(link-dir)/$(exe).o)
	cc -o $@ $^

test.s : $(target).native test.tl
	./$(target).native < test.tl > test.ll; \
	llc -relocation-model=pic test.ll > test.s

$(target).native : tensorscanner.mll tensorparser.mly tensorast.mli \
					tensorsemant.ml tensorsast.mli tensorcodegen.ml
	ocamlbuild $(target).native -pkgs llvm,llvm.analysis

.PHONY : clean
clean : cleandir
	rm -rf *.out _build *.native *.o *.ll *.s *.exe
cleandir :
	make -C func clean

# clang -emit-llvm -S -c test.c -o test.ll