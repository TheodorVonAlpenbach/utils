(require 'mb-utils-div)

(defun goto-first-sexp ()
  "Go safely back to beginning of first sexp at point's level."
  (interactive)				;or not?
  (while (not (condition-case nil
		    (forward-sexp -1)
		(error t)))))

(defun count-sexps-current-level ()
  "Counts number of sexps at current level \(at point\)"
  (interactive)
  (save-excursion
    (goto-first-sexp)
    ;; then go safe forward to the last sexp increasing count at each step
    (let ((n 0))
      (while (not (condition-case nil
		      (forward-sexp 1)
		    (error t)))
	(cl-incf n))
      (princ n))))
;;(count-sexps sdfd)

(defun count-sexps-region* (start end)
  "Return the number of sexps between START and END."
  (interactive "r")
  (message "%d" (count-sexps-region start end)))

;; Calling scan-sexps may have three different types of outcome

;; 1. It returns an integer. This means that the scan was ok: it found
;; a sexp to the right of POINT which ends at the integer it returned
;; 2. It returns nil. This means that it did not find any sexps after
;; POINT. Probably there is only whitespace or other non-sexp
;; characters left in the buffer
;; 3. It fails and returns a condition. This means that we are at the
;; end of a sexp contained in another sexp

(defun count-sexps-region (start end)
  "Return the number of sexps between START and END."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (1- (cl-loop for p = (point-min) then (condition-case nil
					     (scan-sexps p 1)
					   (error nil))
		while p count 1
		do (goto-char p))))))

(cl-defun lift-sexp (&optional (n 1))
  "Replace sexp at the above level with the N next sexps. N must be
positive or zero. If zero, it just removes the surrounding
paranthesis."
  (interactive "p")
  (if (> n 0)
    (kill-sexp n)
    (progn
      (goto-first-sexp)
      (kill-sexp (count-sexps-current-level))))  
  (backward-up-list 1)
  (kill-sexp 1)
  (yank 2))
;;(a b (c d))

(defconst +defun-symbols+
  '(defun cl-defun
    defmacro cl-defmacro
    defparameter defconstant
    ert-deftest))

(defconst +defun-regexp+
  (regexp-opt (mapcar #'sstring +defun-symbols+)))

(cl-defun defun-symbol (&optional (point (point)) (defun-symbols +defun-symbols+))
  "Returns the defun symbol at POINT."
  (when (in-defun-p)
    (save-excursion
     (bod)
     (forward-thing 'symbol 1)
     (when (member (sexp-at-point) defun-symbols)
       (forward-sexp 1)
       (sexp-at-point)))))

(cl-defun in-defun-p (&optional (point (point))
				  (defun-regexp +defun-regexp+))
  "Returns not nil if POINT is inside a defun form."
  (awhen (thing-at-point 'defun)
    (string-match* (format "\\_<%s" defun-regexp) it)))

(definteractive defun-symbol)

(cl-defun defun-symbols (&optional (buffer (current-buffer)))
  "Returns all the defun symbols in BUFFER."
  (save-excursion
    (goto-char (point-min))
    (cl-loop for symbol = (defun-symbol)
	  while (< (point) (point-max))
	  when symbol collect symbol
	  do (end-of-defun))))
(definteractive defun-symbols)

(defun insert-defun-symbols ()
  (interactive)
  (insert (format "%S" (defun-symbols))))

(provide 'mb-sexp)
