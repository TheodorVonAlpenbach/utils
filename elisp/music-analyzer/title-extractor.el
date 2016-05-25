;;;; Perhaps this file should be located elsewhere

(defconst title-regexp "Op\\.? *\\([0-9]+\\)\\( */? *\\([0-9]+\\)\\)?\\.?\\(.*\\)?")
;;(string-match* "Op\\.? *\\([0-9]+\\)\\( */? *\\([0-9]+\\)\\)?\\.?" "Op2. V" '(1 3 4))
;;(string-match* "Op\\.? *\\([0-9]+\\)\\( */? *\\([0-9]+\\)\\)?\\.?\\(.*\\)?" "Op2. Variations in B, 'La ci darem la mano'" '(1 3 4))

(defun extract-opus-number (title)
  (string-match* title-regexp title '(1 3 4)))
;;(extract-opus-number "Op11/1. Klavierkonzert Nr. 1 in e: 1. Allegro maestoso")
;;(extract-opus-number "Op2. Variations in B, 'La ci darem la mano'")

(defun clean-opus-number-in-title (title)
  (let ((op (extract-opus-number title)))
    (if (and (listp op )
	     (first op)			;op
	     (third op))		;rest title
      (if (second op) 
	;; op/subopus
	(format "Op. %d/%d: %s"
	  (string-to-number (first op))
	  (string-to-number (second op))
	  (string-trim (third op)))
	;; op only
	(format "Op. %d: %s"
	  (string-to-number (first op))
	  (string-trim (third op))))
      ;; no op, just return title as was
      title)))
;;(clean-opus-number-in-title "Op11/1. Klavierkonzert Nr. 1 in e: 1. Allegro maestoso")

(defun extract-subopus-number-line ()
  (interactive)
  (move-beginning-of-line 1)
  (let* ((op (extract-opus-number (current-line-as-string)))
	 (sub-opus (or (and (listp op)
			    (second op))
		       "")))
    (kill-line 1)
    (insert sub-opus)
    (insert "\n")))
