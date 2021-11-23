# Path to the LLVM interpreter
LLI="lli"

# Path to the LLVM compiler
LLC="llc"

# Path to the C compiler
CC="cc"

# Path to the tenlab compiler.
TENLAB="./tenlab.native"

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
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
	    Generate $file 2
	    ;;
	*)
	    echo "unknown file type $file"
	    globalerror=1
	    ;;
    esac
done