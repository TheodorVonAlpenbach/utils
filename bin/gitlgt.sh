#!/usr/bin/env bash

show_logs=false

if [[ "$1" == "-l" ]]; then
    show_logs=true
    shift
fi

if [[ $# -ne 2 ]]; then
    echo "Usage: gitl [-l] <commit1> <commit2>"
    exit 1
fi

c1=$(git rev-parse --verify "${1}^{commit}") || {
    echo "Unknown commit: $1"
    exit 1
}

c2=$(git rev-parse --verify "${2}^{commit}") || {
    echo "Unknown commit: $2"
    exit 1
}

if git merge-base --is-ancestor "$c2" "$c1"; then
    result=true
elif git merge-base --is-ancestor "$c1" "$c2"; then
    result=false
else
    echo "Commits are not comparable (different branches?)"
    exit 2
fi

echo "$result"

if $show_logs; then
    if [[ "$result" == "true" ]]; then
	first="$c1"
	second="$c2"
    else
	first="$c2"
        second="$c1"
    fi

    echo

    git show -s --format='%h  %ad  %an%n    %s' \
        --date=short "$first"

    echo

    git show -s --format='%h  %ad  %an%n    %s' \
        --date=format:'%Y-%m-%d %H:%M:%S' "$second"
fi
