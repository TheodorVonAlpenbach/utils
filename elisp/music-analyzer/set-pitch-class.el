(require 'key)

(defun generate-closest-spc-list ()
  (loop for i below 12 collect 
	(let ((spc (mod (* 7 i) 12)))
	  (if (> spc 6)
	    (- spc 12)
	    spc))))
;;(generate-closest-spc-list) ==> (0 -5 2 -3 4 -1 6 1 -4 3 -2 5)

(defconst closest-spc-list
  (generate-closest-spc-list)
  "Nth element in list denotes the closest number of steps in the
circle of fifths to arrive at spc N from 0 E.g. if key K is D
flat (set pitch class 1), then the \"closest\" pitch class of set
pitch class 4 (\"E sound\") should be 
 (spc - spc(K) mod 12) traspositions of a fifth (negative values
corresponds to transpositions of fourths), ie.
 (nth (mod (- 4 1) 12) closest-spc-list)
 (nth 3 closest-spc-list) = -3, 
ie. 3 fourth transpositions from D flat, ie. F flat")

(defun spc-transpose (set-pitch-class n)
  "Transposes SET-PITCH-CLASS up N semitones. If N is negative,
it transposes SET-PITCH-CLASS |N| semitones down."
  (mod (+ set-pitch-class n) 12))

(defun spc-dominant (set-pitch-class)
  "Returns the dominant of SET-PITCH-CLASS"
  (spc-transpose set-pitch-class 7))

(defun spc-dominant-relation-p (spc1 spc2)
  "Returns t iff SPC1 is the dominant of SPC2. Name is perhaps a
bit confusing. This concerns a true dominant, while sometimes we
write dominant about dominant class"
  (= spc1 (spc-dominant spc2)))
;;(mapcar (bind #'spc-dominant-relation-p 0) (0-n 12))

;; conversions
(defun spc-from-spitch (spitch)
  (mod spitch 12))

(defun spc-from-chrome (chrome)
  "Converts CHROME to set pitch class"
  (spc-from-spitch (chrome-to-spitch chrome)))
;;(spc-from-chrome (chrome-new))

(defun* spc-to-chrome (spc &optional (reference-chrome (make-chrome)))
  "Returns closest pitch class of set pitch class SPC relative to REFERENCE-PC.
See constant `closest-spc-list' for a definition of 'closest'."
  (chrome-transpose reference-chrome (p-chrome (i-from-symbol 'P5))
		    (nth (mod (- spc (chrome-to-spitch reference-chrome)) 12)
			 closest-spc-list)))

(defun* spc-to-string (spc &optional (print-style mu-default-print-style))
  ;;for now
  (chrome-to-string-symbol pc print-style))

(provide 'set-pitch-class)
