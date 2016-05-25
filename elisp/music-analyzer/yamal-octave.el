(defun octave-to-string-lilypond (o)
  (make-string (abs (- o 3)) (if (> o 3) ?' ?,)))
;;(mapcar #'octave-to-string-lilypond (a-b 0 8))

(defun* octave-to-string (o &optional (print-style mu-default-print-style))
  (case print-style
    (lilypond (octave-to-string-lilypond o))
    (t (int-to-string o))))
;;(mapcar #'octave-to-string (a-b 0 8))

(defun octave-from-string-lilypond (o-string)
  (let ((o-ups (count ?' o-string))
	(o-downs (count ?, o-string)))
    (if (nor (zerop o-ups) (zerop o-downs))
      (error "Octave string '%s' is not in Lilypond format" o-stringa)
      (+ 3 (- o-ups o-downs)))))
;;(octave-from-string-lilypond "")

(defun* octave-from-string (o-string &optional (print-style mu-default-print-style))
  (case print-style
    (lilypond (octave-from-string-lilypond o-string))
    (t (if (string-match "[[:digit:]]" o-string)
	 (string-to-number o-string)
	 (error "Input octave string is not a number")))))
;;(octave-from-string "1")

(provide 'yamal-octave)
