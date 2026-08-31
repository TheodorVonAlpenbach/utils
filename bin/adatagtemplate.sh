#!/bin/bash

# Default tag
adaTagTemplate="ADA-16800"
lastMillenial=1$(( 10#${adaTagTemplate: -4:-3} - 1))000 # E.g. 15000
lastMillenialLimit=$(( 10#${adaTagTemplate: -3} + 200 )) # E.g. ADA-16300 -> 500

# However, if the input argument is a number greater than 500
# we use another 
if [[ $1 =~ ^[0-9]+$ ]] && [ $1 -ge $lastMillenialLimit ]; then
    adaTagTemplate="ADA-$lastMillenial"
fi

adaSuffix=${1:-$adaTagTemplate}

function expandTag {
    adaNumber=$1
    echo "${adaTagTemplate:0:$((${#adaTagTemplate} - ${#adaNumber}))}$adaNumber"
}

function printUsage {
    echo "Usage: adatagtemplate ADA-NUMBER"
    echo "Expand ADA-NUMBER to a legal Jira tag based on template $adaTagTemplate."
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

while getopts "h" arg; do
    case $arg in
	h)
	    printUsage
	    exit 0
	    ;;
	\?)
	    echo "Run $script -h for help" >&2
	    exit 0
	    ;;
    esac
done

shift $(( OPTIND-1 ))

expandTag $adaSuffix
