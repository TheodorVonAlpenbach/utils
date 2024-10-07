#!/bin/bash

adaTagTemplate="ADA-13500"

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
