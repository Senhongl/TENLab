# Path to the LLVM interpreter
LLI="lli"

# Path to the LLVM compiler
LLC="llc"

# Path to the C compiler
CC="cc"

# Path to the tenlab compiler.
TENLAB="./tenlab.native"

globallog=testall.log
rm -f $globallog

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}

Compare() {
    generatedfiles="$generatedfiles $3"
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
	SignalError "$1 differs"
	echo "FAILED $1 differs from $2" 1>&2
    }
}

Generate() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.tl//'`
    reffile=`echo $1 | sed 's/.tl$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Generating $basename.s" 1>&2

    Run "$TENLAB" "./$1" ">" "${basename}.ll" &&
    Run "$LLC" "-relocation-model=pic" "${basename}.ll" ">" "${basename}.s"
}

CheckFail() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.tl//'`
    reffile=`echo $1 | sed 's/.tl$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${basename}.err ${basename}.diff" &&
    RunFail "$TENLAB" "<" $1 "2>" "${basename}.err" ">>" $globallog &&
    Compare ${basename}.err ${reffile}.err ${basename}.diff

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
	    rm -f $generatedfiles
	echo "OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}

RunFail() {
    echo $* 1>&2
    eval $* && {
	SignalError "failed: $* did not report an error"
	return 1
    }
    return 0
}

if [ $# -ge 1 ]
then
    files=$@
else
    files="tests/test-*.tl tests/fail-*.tl"
fi

for file in $files
do
    case $file in
	*test-*)
	    Generate $file 2
	    ;;
	*fail-*)
	    CheckFail $file 2 >> $globallog
	    ;;
	*)
	    echo "unknown file type $file"
	    globalerror=1
	    ;;
    esac
done
