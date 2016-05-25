;;;; Derived xml mode
(define-derived-mode mbxml-mode sgml-mode
   "mbXML"
   "Major mode for editing SGML documents.
   \\{sgml-mode-map}"
   (set (make-local-variable 'forward-sexp-function) 'mbxml-forward-sexp)
   (define-key mode-specific-map "\C-\M-d" 'mbxml-down-sexp))

;;; for moving around, could we just define forward-sexp-function?
;;; then the stanard lisp functions for moving around in sexps should
;;; do the rest

(defun mbxml-tag-regexp (&optional end-tag)
   (format "[[:space:]]*<%s[A-z0-9]+>[[:space:]]*" (if end-tag "/" "")))
;;(mbxml-tag-regexp t)

(defun mbxml-looking-at-tag (&optional end-tag backward)
  "If point is looking at an sgml tag, it returns the name
of that tag. Else it return nil. If BACKWARDS is not nil then it
looks backward from point."
  (funcall (if backward #'looking-back #'looking-at)
	   (mbxml-tag-regexp end-tag)))

(defun mbxml-tag-region (&optional backward)
  "Returns a list (START END) defining the buffer region of the
XML tag point is looking at."
  (let ((forward-sexp-function nil)
	(start (point)))
    (save-excursion
      (forward-sexp (if backward -1 1))
      (if backward 
	(list (point) start)
	(list start (point))))))

(defun mbxml-skip-tag (n)
  "Skips N tags, backwards if N is negative."
  (if (plusp n)
    (sgml-skip-tag-forward n)
    (sgml-skip-tag-backward (- n))))

(defun* mbxml-down-sexp (&optional (n 1))
  (if (/= n 0)
    (let ((backward (minusp n))
	  (inc (signum n)))
      (while (not (mbxml-looking-at-tag backward backward))
	(forward-sexp inc))
      (let ((forward-sexp-function nil))
	  (forward-sexp inc))
      (mbxml-down-sexp (- n inc)))))

(defun* mbxml-forward-sexp (&optional (n 1))
  "XML version of forward-sexp"
  (let ((backward (minusp n)))
    (if (mbxml-looking-at-tag (not backward) backward)
      ;; very important: the two last arguments should be start end
      ;; for the blocking up "parenthesis" expression (an XML start or end tag)
      (signal 'scan-error (cons "Containing expression ends prematurely" 
				(mbxml-tag-region backward)))
      (if (mbxml-looking-at-tag backward backward)
	(mbxml-skip-tag n)
	(let ((forward-sexp-function nil))
	  (forward-sexp n))))))

;;; outlining XML: back to Emacs basics...
