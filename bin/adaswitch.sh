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
adaBranch=`git branch --list "$adaTag"*`

if [ -n "$verbose" ]; then
    echo Verbose mode is on
    echo Script argument is \'$1\'
    echo adaTag is \'$adaTag\'
fi

if [ -n "$verbose" ]; then
    echo ""
    echo "git switch $adaBranch"
fi

git switch $adaBranch
