(require 'c++-include)

(defun mb-grep-filter-dirs (dirs match match-not)
  (let ((case-fold-search nil))
    (cl-remove-if (disjoin (if match
			     (complement (bind #'string-match match 1))
			     #'never)
			   (if match-not
			     (bind #'string-match match-not 1)
			     #'never))
     dirs)))
;;(mb-grep-filter-dirs '("abc" "bcd") "b" (regexp-or "i" "d"))

(cl-defun mb-grep-basic (&key (target (if (use-region-p)
					(buffer-substring-no-properties
					 (region-beginning) (region-end))
					(thing-at-point 'symbol)))
			   (directories
			    (list (file-name-directory (buffer-file-name))))
			   (types
			    (list
			     (or (file-name-extension (buffer-file-name))
				 (file-name-nondirectory (buffer-file-name)))))
			   (match nil)
			   (match-not nil))
  "Convenient grep according to file type."
  (save-excursion
    (when (stringp target)
      (let ((globs (cl-loop for x in (listify types) collect
			    (format "/%s%s" (if (eql (char x 0) ?.) "" "*") x))))
	(compilation-start
	 (format "grep -nHs -E '%s' %s"
	   (substring-no-properties target)
	   (concat* (mapcar #'file-relative-name
		      (mb-grep-filter-dirs
		       (combine (list directories globs)
				:key #'concat)
		       match match-not))
	     :in " "))
	 'grep-mode nil t)))))
;;(mb-grep-basic :target "mb-grep-basic" :types '("el"))

(defun finddirs (rootdir &optional depth)
  "Return a list of all subdirectories under ROOTDIR.
If optional arguement depth is a non-negative integer, the result
is restricted to depth levels, just like unix utility find's
-maxdepth."
  (mapcar #'file-name-as-directory
    (split-string
     (if (and (integerp depth) (not (minusp depth)))
       (call-process* "find" rootdir "-type" "d" "-maxdepth" (sstring depth))
       (call-process* "find" rootdir "-type" "d"))
     "\n")))
;;(finddirs (expand-directory-name +mb-lisp-dir+))

(cl-defun subdirs (rootdir &optional depth flatten-p)
  "Return a tree of all directories under ROOTDIR.
If optional argument DEPTH is a non-negative integer, the result
is restricted to subdirectories DEPTH levels below rootdir. If
optional argument FLATTEN-P is non-nil the result is flattened to
a list.

Note! In this version the flatten-p mechanism is not implemented.
The current implementation always returns a flattened list."
  (cl-set-difference
      (finddirs rootdir depth)
      '("" ".." "../" "." "./")
    :test #'string=))
;;(subdirs (expand-directory-name +mb-lisp-dir+))

(cl-defun mb-grep-dirs-1 (up maxdepth dir)
  (subdirs (if (zerop up)
	     dir
	     (parent-directory (expand-directory-name dir) up))
	   maxdepth))
;;(mb-grep-dirs-1 1 0 "~/")

(cl-defun mb-grep-dirs (prefix &optional (dir "."))
  "Return a list of directories according to prefix.
If prefix is nil or zero, it returns all the subdirectories,
recursively, in the current directory. If PREFIX is less than 10,
it returns all subdirectories, recursively, below the PREFIXth
superdirectory of the current directory. For higher values of
PREFIX it splits prefix in to MAXDEPTH and UP, where MAXDEPTH is
defined as (floor PREFIX 10), and UP is (mod PREFIX 10). The UP
part defines the root directory for the search as described
above. MAXDEPTH restricts the search to MAXDEPTH levels of
subdirectories below the root directory, just like the -maxdepth
option in the sh utility find.

With a prefix the roles of MAXDEPTH and UP is reversed, see the
examples below.

Examples:

PREFIX   DIRS
nil      ./ and all its subdirectories
0        ./ and all its subdirectories
1        ../ and all its subdirectories
2        ../../ and all its subdirectories
...
9        ../../../../../../../../../ and all its subdirectories

10       ../ only
20       ../../ only
11       ../ and its direct subdirectories only
21       ../../ and its direct subdirectories only 

35       ../../../ and its subdirectories at maximum five levels down      

- or -0  ./ only
-1       ./ and its direct subdirectories only
-5       ./ and its subdirectories at maximum five levels down
-14      same as 41
-72      same as 27
"
  (if (equal prefix "-")
    (mb-grep-dirs-1 0 0 dir)
    (if (or (null prefix) (zerop prefix))
      (mb-grep-dirs-1 0 nil dir)
      (let (maxdepth up)
	(if (minusp prefix)
	  (if (> prefix -10)
	    (setf up 0 maxdepth (abs prefix))
	    (cl-multiple-value-setq (maxdepth up) (cl-floor (abs prefix) 10)))
	  (if (< prefix 10)
	    (setf up prefix maxdepth nil)
	    (cl-multiple-value-setq (up maxdepth) (cl-floor prefix 10))))
	(mb-grep-dirs-1 up maxdepth dir)))))
;;(mb-grep-dirs 1)

(cl-defun mb-grep-interactive ()
  "Convenient grep according to file type."
  (interactive)
  (mb-grep-basic
   :target (read-from-minibuffer "Grep string: ")
   :directories (mb-grep-dirs current-prefix-arg)))

(cl-defun mb-grep ()
  "Convenient grep according to file type."
  (interactive)
  (cl-case major-mode
    (emacs-lisp-mode (mb-elisp-grep))
    (mbscilab-mode (mb-scilab-grep))
    (t (mb-gen-grep))))

(cl-defun mb-gen-grep ()
  "Convenient grep according to file type."
  (mb-grep-basic :directories (mb-grep-dirs (or current-prefix-arg 1))))

(cl-defun mb-elisp-grep ()
  "Convenient grep for Emacs lisp mode."
  (mb-grep-basic :directories (mb-grep-dirs (or current-prefix-arg 1))
		 :types "el"
		 :match-not "/old/"))

(cl-defun mb-scilab-grep ()
  "Convenient grep for mbscilab mode."
  (mb-grep-basic :directories (mb-grep-dirs (or current-prefix-arg 2))
		 :types '("sci" "sce")
		 :match-not (regexp-or "CVS" "etc" "help")))

(cl-defun mb-js-grep ()
  "Convenient grep on JS files."
  (interactive)
  (mb-grep-basic :types "js"))

(cl-defun mb-c++-grep ()
  "Convenient grep according to file type."
  (interactive)
  (mb-grep-basic :types "{h,cpp}"))

(cl-defun mb-lisp-grep ()
  "Convenient grep according to file type."
  (interactive)
  (let ((sexp (thing-at-point 'symbol)))
    (setq grep-command 
	  (cons (format "grep -n '%s' *.lsp ../utils/*.lsp" sexp)
		(+ 9 (length sexp))))
    (call-interactively 'grep)))

(cl-defun clisp-grep ()
  "Convenient grep according to file type."
  (interactive)
  (let ((sexp (thing-at-point 'symbol)))
    (setq grep-command 
	  (cons (format "grep -n '%s' c:/unix/clisp/src/*.lisp" sexp)
		(+ 9 (length sexp))))
    (call-interactively 'grep)))

(provide 'mb-grep)

