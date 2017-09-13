(require 'c++-include)

(cl-defun mb-grep-basic (&key (target (if (use-region-p)
					(buffer-substring-no-properties
					 (region-beginning) (region-end))
					(thing-at-point 'symbol)))
			      (directories
			       (list (file-name-directory (buffer-file-name))))
			      (types (list (file-name-extension (buffer-name)))))
  "Convenient grep according to file type."
  (when (stringp target)
    (let ((globs (loop for x in (listify types) collect (concat "/*." x))))
      (compilation-start
       (format "grep -nH -E '%s' %s"
	 (substring-no-properties target)
	 (concat* (combine (list directories globs) :key #'concat) :in " "))
       'grep-mode))))
;;(mb-grep-basic :target "mb-grep-basic" :types '("el"))

(cl-defun subdirs (rootdir &optional (depth 1))
  "Return a tree of all directories under ROOTDIR.
If FLATTEN-P is non-nil the result is flattened to a list."
  (cl-set-difference
   (split-string (call-process* "find" rootdir "-type" "d" "-maxdepth" (sstring depth)) "\n")
   '("" ".." "./" ".")
   :test #'string=))
;;(subdirs "..")

(cl-defun mb-grep-dirs (prefix)
  "Return a list of directories according to prefix.
If prefix is nil or zero, it returns the directories in the
current directory. If PREFIX is less than 10, it returns all
directories below the PREFIXth superdirectory of the current
directory. For higher values of PREFIX it splits prefix in to
MAXDEPTH and UP, where MAXDEPTH is defined as (floor PREFIX 10),
and UP is (mod PREFIX 10). The UP part defines the root directory
for the search as described above. MAXDEPTH restricts the search
to MAXDEPTH levels of subdirectories below the root directory,
just like the -maxdepth option in the sh utility find."
  (if (null prefix)
    (subdirs "../")
    (destructuring-bind (maxdepth up) (cl-floor prefix 10)
      (subdirs (if (zerop up) "./" (concat* (make-list up "../"))) maxdepth))))
;;(mb-grep-dirs 13)

(cl-defun mb-grep-interactive ()
  "Convenient grep according to file type."
  (interactive)
  (mb-grep-basic
   :target (read-from-minibuffer "Grep string: ")
   :directories (mb-grep-dirs current-prefix-arg)))

(cl-defun mb-grep ()
  "Convenient grep according to file type."
  (interactive)
  (mb-grep-basic :directories (mb-grep-dirs current-prefix-arg)))

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

