;;; This file is above all devoted to following modification of
;;; forward-sentence.

(provide 'mb-paragraph)

(setq sentence-end "[.?!:][]\"')}]*\\([ \t\n]+[A-W]\\)")
(setq sentence-end-double-space nil) ; fill commands use one space

;(debug-on-entry 'forward-sentence)
;(cancel-debug-on-entry 'forward-sentence)
(defun forward-sentence (&optional arg)
  "Move forward to next `sentence-end'.  With argument, repeat.
With negative argument, move backward repeatedly to `sentence-beginning'.

The variable `sentence-end' is a regular expression that matches ends of
sentences.  Also, every paragraph boundary terminates sentences as
well.

This version is modified by mb. See canonical behaviour definition in
*(emacs)Sentences lines 29-32."
  
  (interactive "p")
  ;; are there other ways of turning this off temporary?
  (let ((old-case-fold-search case-fold-search))
    (setq case-fold-search nil)
    (or arg (setq arg 1))
    (while (< arg 0)
      (let ((par-beg (save-excursion (start-of-paragraph-text) (point))))
	(if (re-search-backward (concat sentence-end "[^ \t\n]") par-beg t)
	    (goto-char (2- (match-end 0)))
	  (goto-char par-beg)))
      (setq arg (1+ arg)))
    (while (> arg 0)
      (let ((par-end (save-excursion (end-of-paragraph-text)
				     (point))))
	(princ par-end)
	(princ sentence-end)
	(if (re-search-forward sentence-end par-end t)
	    (progn (backward-char 1)
		   (skip-chars-backward " \t\n"))
	  (goto-char par-end)))
      (setq arg (1- arg)))
    (setq case-fold-search old-case-fold-search)))

(defun mark-whole-word () (interactive)
  (forward-word 1)
  (push-mark nil t t)
  (backward-word 1))

(defun mark-whole-sexp () (interactive)
  (forward-sexp 1)
  (push-mark nil t t)
  (backward-sexp 1))

(defun 2- (number)
  "Return NUMBER minus two.  NUMBER may be a number or a marker.
Markers are converted to integers." (- number 2))
;(2- 4)

(defun 3- (number)
  "Return NUMBER minus three. NUMBER may be a number or a marker.
Markers are converted to integers." (- number 3))