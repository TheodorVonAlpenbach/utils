(require 'chord-skeleton)

(cl-defun chord-new ()
  (chromes-from-string '(C E G)))
;;(chord-new)

(cl-defun chord-p (x)
  "Partly because of the necessity of this function, consider
chord to be a struct."
  (and (listp x) (cl-every #'chrome-p x)))

(cl-defun chord-transpose (chord chrome-interval &optional (n 1))
  "N is unused until the general `bind*' is finished"
  (mapcar (bind #'chrome-transpose chrome-interval) chord))
;;(chord-transpose (chord-new) (chrome-new 1))

(cl-defun chord-invert (chord &optional (n 1))
  (rotate-list chord n))
;;(chord-invert (chord-new))


;;; Conversions
(cl-defun chord-from-chosk (chord-skeleton &optional (inversion 0) (chrome-reference (chrome-new)))
  (chord-invert (chord-transpose (cons (chrome-new) 
				       (mapcar (compose #'p-chrome #'i-from-symbol) chord-skeleton))
				 chrome-reference)
		(- inversion)))
;;(chord-from-chosk (chosk-from-chord-type "d") 1 (chrome-from-string "D"))

(cl-defun chord-from-chordx (chordx)
  (chord-from-chosk (chordx-chosk chordx) (chordx-inversion chordx) (chordx-root chordx)))
;;(chord-from-chordx (chordx-new))

(cl-defun chord-from-schordx (schordx &optional (reference-chrome (chrome-new)))
  (chord-from-chordx (chordx-from-schordx schordx reference-chrome)))

(cl-defun chord-from-schord (schord &optional (reference-chrome (chrome-new)))
  (chord-from-chordx (chordx-from-schordx (schord-to-schordx schord)
					  reference-chrome)))
;;(chord-from-schord '(7 2 10))

(cl-defun chord-to-chordx (chord)
  (let* ((schordx (schordx-from-chord chord))
	 (root (nth* (- (schordx-inversion schordx)) chord)))
    (schordx-to-chordx schordx root)))
;;(chord-to-chordx (chromes-from-string "F# A D"))

(cl-defun chord-root (chord)
  (chordx-root (chord-to-chordx chord)))
;;(mapcar #'chord-root (mapcar (bind #'chord-invert (chord-new) 1) (0-n 2)))

(cl-defun chord-to-string (chord &optional (print-style mu-default-print-style))
  "TODO: there should be a short cut here somewhere"
  (chordx-to-string (chord-to-chordx chord) print-style))
;;(chord-to-string (chord-from-string "Bb") 'english-chord)

(cl-defun chords-to-string (chords &optional (print-style mu-default-print-style))
  "TODO: there should be a short cut here somewhere"
  (mapcar (bind #'chord-to-string print-style) chords))
;;(chords-to-string (list (chord-from-string "Em") (chord-from-string "Em")))

(cl-defun chord-from-string (s &optional (print-style mu-default-print-style))
  (let* ((chrome-string (string-match* (chrome-regexp print-style) s))
	 (chosk-info (chosk-info-from-chord-abbrev (substring s (length chrome-string))))
	 (inversion 0)) ;;always 0 in this version
    (chord-from-chosk (chosk-info-chosk chosk-info) inversion (chrome-from-string chrome-string))))
;;(chord-from-string "Cm")

(provide 'chord)
