#!/bin/bash

defaultContextBeforeArgument=6
defaultContextAfterArgument=1
numberOfShownEntries=5

function printUsage {
    echo "Usage: gitgrep ADA-NUMBER [CONTEXT1] [CONTEXT2]"
    echo "Finds occurrences of ADA-NUMBER in git log. CONTEXT1 is the -C option argument in grep."
    echo "By default it is set to $numberOfShownEntries."
    echo "If CONTEXT2 is provided, CONTEXT1 and CONTEXT2 are the"
    echo "-B and -A option arguments in grep, respectively."
    echo ""
    echo "ADA-NUMBER might be truncated. Then the remaining digits will be filled out"
    echo "according to the template $(adatagtemplate). See util adatagtemplate for more information."
    echo ""
    echo "Options:"
    echo "  -n          Number of shown log entries. Default is $numberOfShownEntries"
    echo "  -v          Verbose mode"
    echo "  -h          Help"
}

while getopts "hv" arg; do
    case $arg in
	n)
	    numberOfShownEntries=$arg
	    ;;
	h)
	    printUsage
	    exit 0
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

adaTag=`adatagtemplate $1`

contextBeforeArgument=${2:-$defaultContextBeforeArgument}
contextAfterArgument=${3:-$defaultContextAfterArgument}

if [ -n "$verbose" ]; then
    echo Verbose mode is on
    echo Script argument is \'$1\'
    echo adaTag is \'$adaTag\'
    echo contextBeforeArgument is \'$contextBeforeArgument\'
    echo contextAfterArgument is \'$contextAfterArgument\'
fi

if [ -n "$verbose" ]; then
    echo ""
    echo "git log | grep $adaTag -B $contextBeforeArgument -A $contextAfterArgument"
fi

git log | grep $adaTag -B $contextBeforeArgument -A $contextAfterArgument
