(require 'compile)
(require 'cl)

(setq compilation-window-height 12)

(defconst *mb-default-makefile* nil) ;(setq *mb-default-makefile* nil)
(unless *mb-default-makefile*
  (setq *mb-default-makefile* "e:/projects/VaR/main/Calc/Calc.mak"))


(defun mb-make (command) "" 
  (interactive
   (if (or compilation-read-command current-prefix-arg)
     (list (read-from-minibuffer "Compile command: "
				 compile-command nil nil
				 '(compile-history . 1)))
     (list compile-command)))

  (save-excursion
;    (let ((make-buffer (find-file-noselect (mb-current-makefile))))
    (let ((make-buffer (find-file-noselect *mb-default-makefile*)))
      (set-buffer make-buffer)
      (message (buffer-name))
      (setq compile-command command)
      (save-some-buffers (not compilation-ask-about-save) nil)
      (compile-internal compile-command "No more errors"))))

; Returns the name of makefile in the current buffer's directory if there
; exists a makefile there. Else returns the same as when last called
; (default first time makefile is 'mb-current-makefile) .

(lexical-let ((current-makefile *mb-default-makefile*))
  (defun mb-current-makefile ()
    (let ((current-directory (file-name-directory (buffer-file-name (current-buffer)))))
      (aif (makefile-in-dir current-directory)
	(setq current-makefile it)
	current-makefile))))
;;(mb-current-makefile)

(defun makefile-in-dir (directory)
  "Returns the name of makefile in DIRECTORY if exists or nil."
  (concat 
   directory 
   (if (equal (elt directory (1- (length directory))) ?/)
       "" "/")
   (or (find "Makefile" (directory-files directory) :test 'string=)
       (find "makefile" (directory-files directory) :test 'string=))))
;;(makefile-in-dir "e:/projects/rp/sverige/rp_lib/rp_data")

;(find "a" '("a" "b") :test 'string=)
;(makefile-in-dir mb-current-makefile-dir)

(setq compilation-directory-regexp 
      "\\([a-zA-Z]:\\)?\\([^:\\/\t\n]*[\\/]\\)*")
(setq compilation-filename-regexp 
      "[^: \t\n]+")
(setq compilation-path-regexp 
      (concat "\\(" compilation-directory-regexp
	      compilation-filename-regexp "\\)"))

(setq compilation-borland-regexp
       (concat "\
\n\\(Error E[0-9]+\\|Warning W[0-9]+\\)\ "
compilation-path-regexp
"\ \\([0-9]+\\):\
\\([^0-9\n]*\\)" ))

;; borland
(setq compilation-error-regexp-alist
      (cons (list compilation-borland-regexp 2 5) 
	    compilation-error-regexp-alist))

(setq db-string "
//f/projects/rp/apps/rp_test/../../libs/rp_routeplanner/RpRoutePlanner.cpp:23: undefined reference to `RpLink::successors(void)'")

(setq compilation-collect2-regexp "
\\(//[^\n:]+\\)\
\\(\\.\\./\\.\\./[^\n:]+\\):\
\\([0-9]+\\): \
\\([^\n]*\\)\
")
	      
(string-match compilation-collect2-regexp db-string)
;(match-string 4 db-string)

(setq compilation-error-regexp-alist ; g++ linker error
      (cons (list compilation-collect2-regexp 2 3) 
	    compilation-error-regexp-alist))

(defun compilation-reset-error-regexp-alist () "" (interactive)
  (setq compilation-error-regexp-alist '(
    ;; NOTE!  See also grep-regexp-alist, below.

    ;; 4.3BSD grep, cc, lint pass 1:
    ;; 	/usr/src/foo/foo.c(8): warning: w may be used before set
    ;; or GNU utilities:
    ;; 	foo.c:8: error message
    ;; or HP-UX 7.0 fc:
    ;; 	foo.f          :16    some horrible error message
    ;; or GNU utilities with column (GNAT 1.82):
    ;;   foo.adb:2:1: Unit name does not match file name
    ;;
    ;; We'll insist that the number be followed by a colon or closing
    ;; paren, because otherwise this matches just about anything
    ;; containing a number with spaces around it.
    ("\n\
\\([a-zA-Z]?:?[^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\)\\([) \t]\\|\
:\\([^0-9\n]\\|\\([0-9]+:\\)\\)\\)" 1 2 5)

    ;; Microsoft C/C++:
    ;;  keyboard.c(537) : warning C4005: 'min' : macro redefinition
    ;;  d:\tmp\test.c(23) : error C2143: syntax error : missing ';' before 'if'
    ("\n\\(\\([a-zA-Z]:\\)?[^:( \t\n-]+\\)[:(][ \t]*\\([0-9]+\\)[:)\t]" 1 3)

    ;;NMAKE : warning U4010: '..\CorbaUtils.cpp' : build failed; /K specified, continuing ...
    ("\nNMAKE : warning \\(\\([a-zA-Z]:\\)?[^:( \t\n-]+\\)[:(][ \t]*\\([0-9]+\\)[:) \t]" 1 3)

    ;; Borland C++:
    ;;  Error ping.c 15: Unable to open include file 'sys/types.h'
    ;;  Warning ping.c 68: Call to function 'func' with no prototype
    ("\n\\(Error\\|Warning\\) \\([a-zA-Z]?:?[^:( \t\n]+\\)\
 \\([0-9]+\\)\\([) \t]\\|:[^0-9\n]\\)" 2 3)

    ("\n\\(Error E2092\\|Warning\\) \\([a-zA-Z]?:?[^:( \t\n]+\\)\
 \\([0-9]+\\)\\([) \t]\\|:[^0-9\n]\\)" 2 3)

    ;; 4.3BSD lint pass 2
    ;; 	strcmp: variable # of args. llib-lc(359)  ::  /usr/src/foo/foo.c(8)
    ("[ \t:]\\([a-zA-Z]?:?[^:( \t\n]+\\)[:(](+[ \t]*\\([0-9]+\\))[:) \t]*$"
     1 2)

    ;; 4.3BSD lint pass 3
    ;; 	bloofle defined( /users/wolfgang/foo.c(4) ), but never used
    ;; This used to be
    ;; ("[ \t(]+\\([a-zA-Z]?:?[^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]+" 1 2)
    ;; which is regexp Impressionism - it matches almost anything!
    ("([ \t]*\\([a-zA-Z]?:?[^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\))" 1 2)

    ;; MIPS lint pass<n>; looks good for SunPro lint also
    ;;  TrimMask (255) in solomon.c may be indistinguishable from TrimMasks (93) in solomon.c due to truncation
    ("[^ ]+ (\\([0-9]+\\)) in \\([^ ]+\\)" 2 1)
    ;;  name defined but never used: LinInt in cmap_calc.c(199)
    ("in \\([^(]+\\)(\\([0-9]+\\))$" 1 2)

    ;; Ultrix 3.0 f77:
    ;;  fort: Severe: addstf.f, line 82: Missing operator or delimiter symbol
    ;; Some SGI cc version:
    ;;  cfe: Warning 835: foo.c, line 2: something
    ("\n\\(cfe\\|fort\\): [^:\n]*: \\([^ \n]*\\), line \\([0-9]+\\):" 2 3)
    ;;  Error on line 3 of t.f: Execution error unclassifiable statement
    ;; Unknown who does this:
    ;;  Line 45 of "foo.c": bloofle undefined
    ;; Absoft FORTRAN 77 Compiler 3.1.3
    ;;  error on line 19 of fplot.f: spelling error?
    ;;  warning on line 17 of fplot.f: data type is undefined for variable d
    ("\\(\n\\|on \\)[Ll]ine[ \t]+\\([0-9]+\\)[ \t]+\
of[ \t]+\"?\\([a-zA-Z]?:?[^\":\n]+\\)\"?:" 3 2)

    ;; Apollo cc, 4.3BSD fc:
    ;;	"foo.f", line 3: Error: syntax error near end of statement
    ;; IBM RS6000:
    ;;  "vvouch.c", line 19.5: 1506-046 (S) Syntax error.
    ;; Unknown compiler:
    ;;  File "foobar.ml", lines 5-8, characters 20-155: blah blah
    ;; Microtec mcc68k:
    ;;  "foo.c", line 32 pos 1; (E) syntax error; unexpected symbol: "lossage"
    ;; GNAT (as of July 94):
    ;;  "foo.adb", line 2(11): warning: file name does not match ...
    ;; IBM AIX xlc compiler:
    ;;  "src/swapping.c", line 30.34: 1506-342 (W) "/*" detected in comment.
    ("\"\\([^,\" \n\t]+\\)\", lines? \
\\([0-9]+\\)\\([\(.]\\([0-9]+\\)\)?\\)?[:., (-]" 1 2 4)

    ;; MIPS RISC CC - the one distributed with Ultrix:
    ;;	ccom: Error: foo.c, line 2: syntax error
    ;; DEC AXP OSF/1 cc
    ;;  /usr/lib/cmplrs/cc/cfe: Error: foo.c: 1: blah blah
    ("rror: \\([^,\" \n\t]+\\)[,:] \\(line \\)?\\([0-9]+\\):" 1 3)

    ;; IBM AIX PS/2 C version 1.1:
    ;;	****** Error number 140 in line 8 of file errors.c ******
    ("in line \\([0-9]+\\) of file \\([^ \n]+[^. \n]\\)\\.? " 2 1)
    ;; IBM AIX lint is too painful to do right this way.  File name
    ;; prefixes entire sections rather than being on each line.

    ;; Lucid Compiler, lcc 3.x
    ;; E, file.cc(35,52) Illegal operation on pointers
    ("\n[EW], \\([^(\n]*\\)(\\([0-9]+\\),[ \t]*\\([0-9]+\\)" 1 2 3)

    ;; GNU messages with program name and optional column number.
    ("\n[a-zA-Z]?:?[^0-9 \n\t:]+:[ \t]*\\([^ \n\t:]+\\):\
\\([0-9]+\\):\\(\\([0-9]+\\)[: \t]\\)?" 1 2 4)

    ;; Cray C compiler error messages
    ("\n\\(cc\\| cft\\)-[0-9]+ c\\(c\\|f77\\): ERROR \\([^,\n]+, \\)* File = \\([^,\n]+\\), Line = \\([0-9]+\\)" 4 5)

    ;; IBM C/C++ Tools 2.01:
    ;;  foo.c(2:0) : informational EDC0804: Function foo is not referenced.
    ;;  foo.c(3:8) : warning EDC0833: Implicit return statement encountered.
    ;;  foo.c(5:5) : error EDC0350: Syntax error.
    ("\n\\([^( \n\t]+\\)(\\([0-9]+\\):\\([0-9]+\\)) : " 1 2 3)

    ;; Sun ada (VADS, Solaris):
    ;;  /home3/xdhar/rcds_rc/main.a, line 361, char 6:syntax error: "," inserted
    ("\n\\([^, ]+\\), line \\([0-9]+\\), char \\([0-9]+\\)[:., \(-]" 1 2 3)
    )))

(provide 'c++-compile)

;; garbage
; (defun set-current-makefile-to-this-old (n) 
;   (interactive "p")
;   (if (< n 0) 
;       (setq mb-current-makefile nil)
;       (setq mb-current-makefile mb-current-makefile-default)))

