#!/bin/bash
yamal=/cygdrive/c/Users/eier/Google\ Drive/projects/yamal/lisp
ls=/cygdrive/c/Users/eier/Google\ Drive/Contango-MB/Light\ Structures/lisp
find $yamal $ls -type f -name "*.lisp" -print0 | xargs -0 etags -o ~/CLTAGS

