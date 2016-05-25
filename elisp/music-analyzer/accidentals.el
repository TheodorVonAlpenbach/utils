(defconst accidentals-print-styles
  '((english " sharp" " flat")
    (german "is" "es")
    (lilypond german)
    (english-chord "#" "b")
    (symbol "#" "b")
    (norwegian "iss" "ess")))

(defun accidentals-print-style (print-style)
  (let ((res (assoc print-style accidentals-print-styles)))
    (if (symbolp (second res))
      (accidentals-print-style (second res))
      res)))
;;(accidentals-print-style 'lilypond)

(defun sharps-print-style (print-style)
  (second (accidentals-print-style print-style)))
;;(sharps-print-style 'lilypond)

(defun flats-print-style (print-style)
  (third (accidentals-print-style print-style)))
;;(sharps-print-style 'symbol)

(defun accidental-print-style (is-flat print-style)
  (if is-flat (flats-print-style print-style) (sharps-print-style print-style)))
;;(funcall #'(lambda (n) (format " %s %s" (n-tuple-to-string (abs n)) (if (> n 0) "sharp" "flat"))) 2)

(defun* a-transpose (a a-transposer &optional (n 1))
  (* (+ a a-transposer) n))

(provide 'accidentals)