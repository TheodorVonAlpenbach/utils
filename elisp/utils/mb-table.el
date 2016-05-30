;;; Methods for producing nice looking text tables

(defun table-clean (beg end)
  "Assumes lines in region is a table and cleans that table."
  (interactive "r")
  (with-syntax-table emacs-lisp-mode-syntax-table
    (save-excursion
      (let* ((beg-line (point-2-line beg))
	     (end-line (point-2-line end))
	     (maxlengths (apply #'mapcar* #'max 
				(table-collect-sexp-lengths
				 beg-line end-line)))
	     (offsets (loop for y = 0 then (+ x y 2)
			    for x in maxlengths
			    collect y)))
	(loop for line from beg-line to end-line
	      do (table-clean-line line offsets))))))

(defun table-clean-paragraph ()
  "Assumes lines in region is a table and cleans that table."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'paragraph)))
    (table-clean (1+ (car bounds)) (1- (cdr bounds)))))

(defun* goto-begin-sexp (&optional (n 1))
  (interactive "p")
  (forward-sexp n)
  (forward-sexp -1)
  (princ (point)))

(defun table-clean-line (line offsets)
  "Fills in or removes whitespace according to OFFSETS at LINE.
OFFSETS is a list of integers."
  (goto-line line)
  (let ((point (point)))
      (dolist (offset offsets)
	(delete-char (- (point) (goto-begin-sexp)))
	(insert-char ?\  (- offset (- (point) point)))
	(forward-sexp 1)
	(when (/= (point-2-line) line)
	  (error "number of sexp in line %d doesn't match offsets argument")))))

(defun* point-2-line (&optional (point (point)))
  (interactive "d")
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (1+ (count-lines 1 (point)))))
;;(point-2-line)

(defun table-collect-sexp-lengths (start-line end-line)
  "Assumes lines in region is a table and cleans that table. TODO:
lines should be input instead."
  (interactive "r")

  (save-excursion
    (goto-line start-line)
    ;; start with point immediately behind first sexp on line
    (forward-sexp 1)
    (loop for line from start-line to end-line
	  collect (loop for bounds = (bounds-of-thing-at-point 'sexp)
			while (= (point-2-line) line)
			collect (- (cdr bounds) (car bounds))
			do (forward-sexp 1)))))

(defun table-collect-sexp-lengths (start-line end-line)
  "Assumes lines in region is a table and cleans that table."
  (interactive "r")
  (goto-line start-line)
  (backward-char 1)
  (loop for line from start-line to end-line
	  collect (loop for start = (progn (forward-whitespace 1)
					   (point))
			for end = (progn (forward-whitespace 1)
					 (forward-whitespace -1)
					 (point))
			while (= (point-2-line) line)
			collect (princ (- end start)))))

(defun table-item-lengths-line (line)
  "Returns a ordered list of lengths of all non-whitespace items in
LINE. TODO: fix if LINE is last."
  (interactive "P")
  (goto-line line)
  (backward-char 1)
  (princ (loop for start = (progn (forward-whitespace 1)
				  (point))
	for end = (progn (forward-whitespace 1)
			 (forward-whitespace -1)
			 (point))
	while (and (= (point-2-line) line))
	collect (princ (- end start))
	do (forward-whitespace 1))))

(defun table-item-lengths-line (line)
  "Returns a ordered list of lengths of all non-whitespace items in
LINE. TODO: fix if LINE is last."
  (goto-line line)
  (loop while (and (re-search-forward "[^ \t\n]+" nil t)
		   (= (point-2-line) line))
	collect (length (match-string 0))))

(defun table-item-lengths-line* ()
  (interactive)
  (princ (table-item-lengths-line (point-2-line))))

(provide 'mb-table)