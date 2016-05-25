;;;; Short-cut: CHORDX
;;;; '(chosk inversion root)
;;;; chosk:         see chosk.el
;;;; inversion:     0 (root-position), 1, ... 
;;;; root: chrome

(require 'chord-skeleton)

(defun* chordx-new (&optional (chosk (chosk-new)) (inversion 0) (root (chrome-new)))
  (list chosk inversion root))
;;(chordx-new '(M3 P5) 1)

(defalias 'chordx-chosk 'first)
(defalias 'chordx-inversion 'second)
(defalias 'chordx-root 'third)

;;; properties
(defun* chordx-type (chordx)
  "Returns the classification of set chord SC"
  (schordx-type (schordx-from-chordx chordx)))


;;; print / write
(defun* chordx-to-string (chordx &optional (print-style mu-default-print-style))
  "Apply print-style for chord-type as well"
  (format "%s%s"
    (chrome-to-string (chordx-root chordx) print-style)
    (chosk-info-chord-abbrev (chosk-info-from-chosk (chordx-chosk chordx)))))

(defun* chordx-from-string (s &optional (print-style mu-default-print-style))
  (let* ((chrome-string (string-match* (chrome-regexp print-style) s))
	 (inversion 0) ;;in this version
	 (chosk-info (chosk-info-from-chord-abbrev (substring s (length chrome-string)))))
    (chordx-new (chosk-info-chosk chosk-info) inversion (chrome-from-string chrome-string))))
;;(chordx-from-string "Dm" 'english)

(provide 'chordx)
