#!/bin/bash

# Path settings
output=~/.MBTAGS
mbdir=~/git/utils/elisp
libdir=~/.emacs.d/
projdir=~/sources/imms/src/elisp/

# Build regular expression.
# Note that there is no backslash prefix for grouping characters (|)
# in the current version of etags. However, this may vary.
SPC="[[:space:]]*"
NSPC="[^ \t]*"
NSPC="[^[:space:]]*"
SYMBOL=$NSPC
FN_SYMBOL="\<cl-def(un|macro)\>"

regex="/^$SPC\($SPC$FN_SYMBOL$SPC($SYMBOL)$SPC/\2/"

echo "Generating tags table for Emacs lisp files using regular expression"
echo "$regex"
echo

echo "Clearing previous tags..."
rm -f "$output"
echo "Done."
echo

echo "Generating tags in $mbdir ..."
find "$mbdir" -type f -regex ".*\.el$" -and -not -regex ".*[/-]old.*" -print0 \
    | xargs -0 etags -o $output --regex-lisp="$regex"
echo "Done."
echo

echo "Generating tags in $libdir ..."
find "$libdir" -type f -regex ".*\.el$" -and -not -regex ".*[/-]old.*" -print0 | \
    xargs -0 etags -a -o $output --regex-lisp="$regex"
echo "Done."
