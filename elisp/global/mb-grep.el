(require 'c++-include)

(defun* mb-grep (&optional (type (file-name-extension (buffer-name))))
  "Convenient grep according to file type."
  (interactive)
  (let ((sexp (thing-at-point 'symbol)))
    (setq grep-command 
	  (cons (format "grep -n '%s' *.%s" sexp type)
		(+ 9 (length sexp))))
    (call-interactively 'grep)))

(defun* mb-grep1 (&optional (type (file-name-extension (buffer-name))))
  "Convenient grep according to file type. Assumes a directory pyramid
of height 1."
  (interactive)
  (let ((sexp (thing-at-point 'symbol)))
    (setq grep-command 
	  (cons (format "grep -n '%s' ../*/*.%s" sexp type)
		(+ 9 (length sexp))))
    (call-interactively 'grep)))

(defun* mb-grep2 (&optional (type (file-name-extension (buffer-name))))
  "Convenient grep according to file type. Assumes a directory pyramid
of height 0, ie. searches only in the same directory."
  (interactive)
  (let ((sexp (thing-at-point 'symbol)))
    (setq grep-command 
	  (cons (format "grep -n '%s' ../../*/*/*.%s" sexp type)
		(+ 9 (length sexp))))
    (call-interactively 'grep)))

(defun* mb-c++-grep ()
  "Convenient grep according to file type."
  (interactive)
  (let ((sexp (thing-at-point 'symbol)))
    (setq grep-command 
	  (cons (format "grep -n '%s' *.cpp *.h" sexp)
		(+ 9 (length sexp))))
    (call-interactively 'grep)))

(defun* mb-lisp-grep ()
  "Convenient grep according to file type."
  (interactive)
  (let ((sexp (thing-at-point 'symbol)))
    (setq grep-command 
	  (cons (format "grep -n '%s' *.lsp ../utils/*.lsp" sexp)
		(+ 9 (length sexp))))
    (call-interactively 'grep)))

(defun* clisp-grep ()
  "Convenient grep according to file type."
  (interactive)
  (let ((sexp (thing-at-point 'symbol)))
    (setq grep-command 
	  (cons (format "grep -n '%s' c:/unix/clisp/src/*.lisp" sexp)
		(+ 9 (length sexp))))
    (call-interactively 'grep)))

(provide 'mb-grep)

