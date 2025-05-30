#!/bin/bash

dir=./
up=0
extension=*
ignoreCase=
extensionModifier=""
grepContext=""
grepAfter=""

function printUsage {
    echo "Usage: gfind [OPTION] TARGET [EXTENSION]"
    echo "Finds occurrences of TARGET in files in the current directory tree."
    echo "If optional argument EXTENSION is provided, it searches only files with"
    echo "filename extension EXTENSION."
    echo ""
    echo "Options:"
    echo "  -E EXT      Another way to specify file extension, typically used by alias."
    echo "  -e EXT      Do not search files with EXTENSION. Issues a warning if no EXTENSION is provided"
    echo "  -u NUM      Searches the directory tree NUM levels up from current directory."
    echo "  -d NUM      Limit the search to directories NUM levels below current directory."
    echo "  -C NUM      Print NUM lines of output context. See grep manual for more info."
    echo "  -A NUM      Print also NUM lines of output context. See grep manual for more info."
    echo "  -b          Do not search files with EXTENSION. Issues a warning if no EXTENSION is provided"
    echo "  -i          Ignore case"
    echo "  -v          Verbose mode"
}

while getopts "hE:e:u:d:C:A:iv" arg; do
    case $arg in
	h)
	    printUsage
	    exit 0
	    ;;
	e)
	    extension=$OPTARG
	    extensionModifier="-not "
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
	C)
	    grepContext="-C $OPTARG"
	    ;;
	A)
	    grepAfter="-A $OPTARG"
	    ;;
	i)
	    ignoreCase=" -i"
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
echo ${verbose+ignoreCase is \'$ignoreCase\'}
echo ${verbose+extensionModifier is \'$extensionModifier\'}

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
     $extensionModifier-name "*.$extension" \
     -print0 \
    | xargs -0 -r grep $ignoreCase $grepContext $grepAfter -a --color=auto -n -s "$target"

echo ${verbose+find "$dir" \
     ${down+-maxdepth $down}\
     -type f \
     $extensionModifier-name "*.$extension" \
     -print0 \
    | xargs -0 -r grep $ignoreCase $grepContext $grepAfter -a --color=auto -n -s "$target"
}
