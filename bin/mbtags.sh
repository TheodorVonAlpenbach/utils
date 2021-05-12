#!/bin/bash

# Path settings
output=~/.MBTAGS
mbdir=~/git/utils/elisp
libdir=~/.emacs.d/

echo "Clearing previous tags..."
rm -f "$output"
echo "Done."
echo

echo "Generating tags in $mbdir ..."
find "$mbdir" -type f -regex ".*\.el$" -and -not -regex ".*[/-]old.*" -print0 \
    | xargs -0 etags -o $output
echo "Done."
echo

echo "Generating tags in $libdir ..."
find "$libdir" -type f -regex ".*\.el$" -and -not -regex ".*[/-]old.*" -print0 | \
    xargs -0 etags -a -o $output --regex-lisp="$regex"
echo "Done."
