#!/bin/bash

dir=./
up=0
extension=*

function printUsage {
    echo "Usage: $gfind [OPTION] TARGET [EXTENSION]"
    echo "Finds occurrences of TARGET in files in the current directory tree."
    echo "If optional argument EXTENSION is provided, it searches only files with"
    echo "filename extension EXTENSION."
    echo ""
    echo "Options:"
    echo "  -E ext      Another way to specify file extension, typically used by alias."
    echo "  -u N        Searches the directory tree N levels up from current directory."
    echo "  -d M        Limit the search to directories M levels below current directory."
    echo "  -v          Verbose mode"
}

while getopts "hE:u:d:v" arg; do
    case $arg in
	h)
	    printUsage
	    exit 0
	    ;;
	E)
	    extension=$OPTARG
	    ;;
	u)
	    up=$OPTARG
	    ;;
	d)
	    down=$OPTARG
	    ;;
	v)
	    verbose=1
	    ;;
	\?)
	    echo "Run $script -h for help" >&2
	    exit 0
	    ;;
    esac
done

shift $(( OPTIND-1 ))

target=$1
extension=${2:-$extension}
echo ${verbose+Verbose mode is on}
echo ${verbose+Target is \'$target\'}
echo ${verbose+Extension is \'$extension\'}

if [ -z "$target" ]; then
    printUsage
    exit 0
fi

for i in `seq $up`
do
    dir="../$dir"
done

echo ${verbose+Searching directories \'$dir\' ...}

find "$dir" \
     ${down+-maxdepth $down}\
     -type f \
     -name "*.$extension" \
     -print0 \
    | xargs -0 -r grep -a -i --color=auto -n -s "$target"
