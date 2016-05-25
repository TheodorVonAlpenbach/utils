(require 'mb-utils-div)

(defconst chrome-bases
  '(C D E F G A B))

(defun* chb-transpose (pbc chb-transposer &optional (n 1))
  (mod (* n (+ pbc chb-transposer)) 7))

(defun chb-to-spc (pbc)
  (case pbc
    (0 0)
    (1 2)
    (2 4)
    (3 5)
    (4 7)
    (5 9)
    (6 11)))
;(mapcar #'chb-to-spc (mapcar #'chb-from-string '("C" "D" "E" "F" "G" "A" "B")))

(defun chb- (pbc1 pbc2)
  (- pbc1 pbc2))

;;; read/write
(defconst chb-string-styles
  '((english ("C" "D" "E" "F" "G" "A" "B"))
    (english-chord english)
    (german ("C" "D" "E" "F" "G" "A" "H"))
    (lilypond ("c" "d" "e" "f" "g" "a" "b"))
    (symbol german))) ;;derivation

(defun chb-string-style (print-style)
  (let ((res (tmap-0-1 print-style chb-string-styles)))
    (if (symbolp res)
      (chb-string-style res)
      res)))
;;(chb-string-style 'english-chord)

(defun* chb-from-string (chb-string &optional (print-style mu-default-print-style))
  (case print-style
    (lilypond (chb-from-string (upcase chb-string)))
    (otherwise 
     (let ((res (position chb-string (chb-string-style print-style) :test #'string-equal)))
       (if (not res)
	 (error "Argument '%s' is not a valid pitch base class string in style '%S'" chb-string print-style)
	 res)))))
;;(chb-from-string "g" 'lilypond)
;;(chb-from-string "g")

(defun* chb-to-string (pbc &optional (print-style mu-default-print-style))
  (nth pbc (chb-string-style print-style)))
;;(chb-to-string 6)
;;(chb-to-string 6 'english-chord)

(provide 'chrome-base)
