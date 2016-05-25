(require 'mb-utils-div)

(defun goto-first-sexp ()
  "Go safely back to beginning of first sexp at point's level."
  (interactive)				;or not?
  (while (not (condition-case nil
		    (forward-sexp -1)
		(error t)))))

(defun count-sexps ()
  "Counts number of sexps at current level \(at point\)"
  (interactive)
  (save-excursion
    (goto-first-sexp)
    ;; then go safe forward to the last sexp increasing count at each step
    (let ((n 0))
      (while (not (condition-case nil
		      (forward-sexp 1)
		    (error t)))
	(incf n))
      (princ n))))
;;(count-sexps sdfd)

(defun* lift-sexp (&optional (n 1))
  "Replace sexp at the above level with the N next sexps. N must be
positive or zero. If zero, it just removes the surrounding
paranthesis."
  (interactive "p")
  (if (> n 0)
    (kill-sexp n)
    (progn
      (goto-first-sexp)
      (kill-sexp (count-sexps))))  
  (backward-up-list 1)
  (kill-sexp 1)
  (yank 2))
;;(a b (c d))

(defun* defun-symbol (&optional (point (point)))
  "Returns the defun symbol at POINT."
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (down-list 1)
    (when (member (sexp-at-point)
		  '(defun defun* defmacro defmacro* defparameter defconstant))
      (forward-sexp 2)
      (sexp-at-point))))
(definteractive defun-symbol)

(defun* defun-symbols (&optional (buffer (current-buffer)))
  "Returns all the defun symbols in BUFFER."
  (save-excursion
    (goto-char (point-min))
    (loop for symbol = (defun-symbol)
	  while (< (point) (point-max))
	  when symbol collect symbol
	  do (end-of-defun))))
(definteractive defun-symbols)

(defun insert-defun-symbols ()
  (interactive)
  (insert (format "%S" (defun-symbols))))

(provide 'mb-sexp)