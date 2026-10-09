#!/bin/bash

function printUsage {
    echo "Usage: cdadatask ADA-NUMBER"
    echo "Move to ~/ada-tasks/ADATAG/README, where ADA-JIRANUMBER is created from ADATAG with adatagtemplate."
    echo ""
    echo "See also adatagtemplate, mkadatask"
    echo ""
    echo "Options:"
    echo "  -v          Verbose mode"
}

if [ $# -eq 0 ]; then
    echo "No arguments provided."
    echo
    printUsage
fi

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

if [ -n "$verbose" ]; then
    echo Script argument is \'$1\'
    echo "Verbose mode is on"
fi

adaTag=$(adatagtemplate "$1")
targetPath="$HOME/ada-tasks/$adaTag"

[ -n "$verbose" ] && echo "targetPath is '$targetPath'"

cd "$targetPath" || return
