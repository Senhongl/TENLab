# Path to the LLVM interpreter
LLI="lli"

# Path to the LLVM compiler
LLC="llc"

# Path to the C compiler
CC="cc"

# Path to the tenlab compiler.
TENLAB="./tenlab.native"

Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}

Generate() {
    basename=`echo $1 | sed 's/.*\\///
                             s/.tl//'`

    echo -n "$basename..."

    echo 1>&2
    echo "###### Generating $basename.s" 1>&2

    Run "$TENLAB" "./$1" ">" "${basename}.ll" &&
    Run "$LLC" "-relocation-model=pic" "${basename}.ll" ">" "${basename}.s"
}

Test() {
	error=0
	basename=`echo $1 | sed 's/.*\\///
                             s/.tl//'`

    echo -n "$basename..."
	echo "###### Testing $basename" 1>&2

    generatedfiles=""

    if [ -d build ]; then echo "build exist"; else mkdir build; fi 

    generatedfiles="$generatedfiles ${basename}.ll ${basename}.s ${basename}.exe ${basename}.out" &&
    Run "cd build" &&
    Run "cmake .." "-DSOURCE_FILE:FILEPATH=${basename}" &&
    Run "make" &&
    Run "cd .." &&
    Run "./${basename}.exe"

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
	rm -f $generatedfiles
	echo "OK"
    else
	echo "###### FAILED" 1>&2
    fi
}

TimeTest() {
	error=0
	basename=`echo $1 | sed 's/.*\\///
                             s/.tl//'`

    echo -n "$basename..."
	echo "###### Testing $basename" 1>&2

    generatedfiles=""

    if [ -d build ]; then echo "build exist"; else mkdir build; fi 

    generatedfiles="$generatedfiles ${basename}.ll ${basename}.s ${basename}.exe ${basename}.out" &&
    eval "cd build" &&
    eval "cmake .." "-DSOURCE_FILE:FILEPATH=${basename}" &&
    eval "make" &&
    eval "cd .." &&
    Run "time ./${basename}.exe"

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
	rm -f $generatedfiles
	echo "OK"
    else
	echo "###### FAILED" 1>&2
    fi
}


if [ $# -eq 1 ]; then
	Generate $1
	Test $1
else
	Generate $1
	TimeTest $1
fi
