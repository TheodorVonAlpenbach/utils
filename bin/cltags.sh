#!/bin/bash

# Path settings
output=~/.CLTAGS
yamaldir=/cygdrive/c/Users/eier/Google\ Drive/projects/yamal/lisp/
localdir=~/sources/imms/src/lisp/
utilsdir=~/projects/utils/lisp/

echo "Generating tags table for Emacs lisp files using regular expression"
echo "$regex"
echo

echo "Clearing previous tags..."
rm -f "$output"
echo "Done."
echo

echo "Generating local tags..."
echo find "$localdir" -type f -regex ".*\.lisp$" -print0 "| xargs -0 etags -o $output"
find "$localdir" -type f -regex ".*\.lisp$" -print0 | xargs -0 etags -o $output
echo "Done."
echo

echo "Adding library tags..."
echo find "$yamaldir" -type f -regex ".*\.lisp$" -print0 "| xargs -0 etags -a -o $output"
find "$yamaldir" -type f -regex ".*\.lisp$" -print0 | xargs -0 etags -a -o $output
echo "Done."

echo "Adding utils tags..."
echo find "$utilsdir" -type f -regex ".*\.lisp$" -print0 "| xargs -0 etags -a -o $output"
find "$utilsdir" -type f -regex ".*\.lisp$" -print0 | xargs -0 etags -a -o $output
echo "Done."
