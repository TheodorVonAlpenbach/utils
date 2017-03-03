(require 'c++-include)

(cl-defun mb-grep-basic (&key (target (thing-at-point 'symbol))
			      (directories
			       (list (file-name-directory (buffer-file-name))))
			      (types (list (file-name-extension (buffer-name)))))
  "Convenient grep according to file type."
  (when (stringp target)
    (let ((globs (loop for x in (listify types) collect (concat "*." x))))
      (compilation-start
       (format "grep -nH -E '%s' %s"
	 (substring-no-properties target)
	 (concat* (combine (list directories globs) :key #'concat) :in " "))
       'grep-mode))))
;;(mb-grep-basic :target "mb-grep-basic" :types '("el"))

(cl-defun mb-grep ()
  "Convenient grep according to file type."
  (interactive)
  (mb-grep-basic))

(cl-defun mb-grep1 ()
  "Convenient grep according to file type. Assumes a directory pyramid
of height 1."
  (interactive)
  (mb-grep-basic :directories "../*/*/"))

(cl-defun mb-grep2 (&optional (type (file-name-extension (buffer-name))))
  "Convenient grep according to file type. Assumes a directory pyramid
of height 0, ie. searches only in the same directory."
  (interactive)
  (mb-grep-basic :directories "../../*/*/*/"))

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

