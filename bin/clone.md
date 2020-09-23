* TODO
- Rewrite with argparse
- use git-style commands
-- search


* Usage
clone [OPTIONS] OLD NEW [DIR]

* OPTIONS
Consider implementing some of these

-h, --help               show usage with options
-v, --verbose            print action states
-1, --printold           print OLD*, see clone.md for a definition of OLD*
-2, --printnew           print NEW*, see clone.md for a definition of NEW*
-p, --printall           short for -12 or --printold --printnew
-d, --dry                do not do anything in verbose mode
-a, --anomalities        print anomality report
-A, ---no-anomalities    do not anomality report

* Convert OLD to variants of OLD in the following way
  old_string
  oldstring
  old-string
  oldString
  OldString
  OLDSTRING
  OLD-STRING
  OLD_STRING
  
  (Consider accepting any of these forms as the OLD argument
  old_string
  old-string
  oldString
  OldString
  OLD-STRING
  OLD_STRING)
  
  Denote this collection as OLD*, and an arbitrary string matching some
  string in OLD* as SUPEROLD. 

* Convert NEW in the same way as OLD, and denote NEW* and SUPERNEW in
  the same way as above.

* Copy every file matching a SUPEROLD with the corresponding SUPERNEW

* Substitute content in the new files: In every file matching
  SUPERNEW, substitute every occurrence of OLD* with the corresponding
  form in NEW*.
      
* Anomalities report Print every path in DIR with a name not matching
  some OLD*, but with some occurrence of some OLD*.

  These files are ripe for further scrutiny.