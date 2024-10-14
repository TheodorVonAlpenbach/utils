#!/bin/bash

function printUsage {
    echo "Usage: adaswitch ADA-NUMBER"
    echo "Expand ADA-NUMBER to a legal Jira task number and switch git to corresponding branch."
    echo ""
    echo "If two branches correspond to this Jira number, i.e. it is the prefix of both have names,"
    echo "the script lists these two branches."
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

adaTag=`adatagtemplate $1`
adaBranchOrigin=`git branch -r --list "*$adaTag*"`
adaBranch=`echo $adaBranchOrigin | cut -d '/' -f 2`

if [ -n "$verbose" ]; then
    echo Verbose mode is on
    echo Script argument is \'$1\'
    echo adaTag is \'$adaTag\'
    echo adaBranchOrigin is \'$adaBranchOrigin\'
    echo adaBranch is \'$adaBranch\'
fi

if [ -n "$verbose" ]; then
    echo ""
    echo "git switch $adaBranch"
fi

git switch $adaBranch
