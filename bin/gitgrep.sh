#!/bin/bash

defaultContextBeforeArgument=4
defaultContextAfterArgument=1
adaTagTemplate="ADA-13500"

function printUsage {
    echo "Usage: gitgrep ADA-NUMBER [CONTEXT1] [CONTEXT2]"
    echo "Finds occurrences of ADA-NUMBER in git log. CONTEXT1 is the -C option argument in grep."
    echo "By default it is set to 5. If CONTEXT2 is provided, CONTEXT1 and CONTEXT2 are the"
    echo "-B and -A option arguments in grep, respectively."
    echo ""
    echo "ADA-NUMBER might be truncated. Then the remaining digits will be filled out"
    echo "according to the template 12500."
    echo ""
    echo "Options:"
    echo "  -v          Verbose mode"
}

while getopts "hv" arg; do
    case $arg in
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

adaNumber=$1

if [ -z "$adaNumber" ]; then
    printUsage
    exit 0
fi

contextBeforeArgument=${2:-$defaultContextBeforeArgument}
contextAfterArgument=${3:-$defaultContextAfterArgument}

adaTag="${adaTagTemplate:0:$((${#adaTagTemplate} - ${#adaNumber}))}$adaNumber"

if [ -n "$verbose" ]; then
    echo Verbose mode is on
    echo adaTag is \'$adaTag\'
    echo adaNumber \(script argument\) is \'$adaNumber\'
    echo adaTagTemplate is \'$adaTagTemplate\'
    echo contextBeforeArgument is \'$contextBeforeArgument\'
    echo contextAfterArgument is \'$contextAfterArgument\'
fi

if [ -n "$verbose" ]; then
    echo ""
    echo "git log | grep $adaTag -B $contextBeforeArgument -A $contextAfterArgument"
fi

git log | grep $adaTag -B $contextBeforeArgument -A $contextAfterArgument
