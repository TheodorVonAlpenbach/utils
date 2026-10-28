#!/bin/bash

# Default tag
adaTagTemplate="ADA-17100"
lastMillenial=1$(( 10#${adaTagTemplate: -4:-3} - 1))000 # E.g. 15000
lastMillenialLimit=$(( 10#${adaTagTemplate: -3} + 200 )) # E.g. ADA-16300 -> 500

# However, if the input argument is a number greater than 500
# we use another 
if [[ $1 =~ ^[0-9]+$ ]] && [ $1 -ge $lastMillenialLimit ]; then
    adaTagTemplate="ADA-$lastMillenial"
fi

function expandTag {
    adaNumber=$1
    echo "${adaTagTemplate:0:$((${#adaTagTemplate} - ${#adaNumber}))}$adaNumber"
}

function printUsage {
    echo "Usage: adatagtemplate ADA-NUMBER"
    echo "Expand ADA-NUMBER to a legal Jira tag based on template $adaTagTemplate."
    echo ""
    echo "Options:"
    echo "  -v          Verbose mode"
    echo ""
    echo "Examples:"
    echo ""
    echo "adatagtemplate 1234"
    echo "=> $(expandTag 1234)"
    echo ""
    echo "adatagtemplate 12"
    echo "=> $(expandTag 12)"
    echo ""
    echo "adatagtemplate"
    echo "=> $(expandTag)"
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

adaSuffix=${1:-$adaTagTemplate}

if [ -n "$verbose" ]; then
    echo Verbose mode is on
    echo Script argument is \'$1\'
    echo adaTagTemplate is \'$adaTagTemplate\'
    echo lastMillenial is \'$lastMillenial\'
    echo lastMillenialLimit is \'$lastMillenialLimit\'
    echo adaSuffix is \'$adaSuffix\'
fi

expandTag $adaSuffix
