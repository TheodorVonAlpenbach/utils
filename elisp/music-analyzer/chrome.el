;;; An alternative name for this could be CHROMA, ref
;;; Meindard Müller, p. 60
;;; http://en.wikipedia.org/wiki/Pitch_class
(require 'chrome-base)
(require 'accidentals)

(defstruct (chrome (:constructor chrome-new (&optional (base 0) (accidentals 0))))
  (base 0)
  (accidentals 0))
;;(make-chrome)

(defun* chrome-copy (chrome &key
			    (base (chrome-base chrome))
			    (accidentals (chrome-accidentals chrome)))
  (make-chrome :base base
	       :accidentals accidentals))
;;(chrome-copy (chrome-new) :base 3)

(defun* chrome-new (&optional (chrome-base 0) (accidentals 0))
  (make-chrome :chrome-base chrome-base :accidentals accidentals))
;;(chrome-new)

(defun chrome-s-value (pc)
  "Returns the S-value of pitch class PC.
S-value is the number identified with a chrome base when the
latter is ordered after the scale, starting at C. Thus the S-values of C, D, E, F, G,
A, B are 0, 1, 2, 3, 4, 5, 6 respectively."
  (chrome-base pc)) ;;property chrome-base is currently stored as an S-value

(defun chrome-q-value (pc)
  "Returns the Q-value of pitch class PC.
Q-value is the number identified with a chrome base when the
latter is ordered after rising fifth, starting at C. Thus the
Q-values of F, C, G, D, A, E, B are -1, 0, 1, 2, 3, 4, 5
respectively."
  (1- (mod (+ (* 2 (chrome-base pc))
	  1)
       7)))

(defun chrome-a-value (pc)
  "Returns the A-value of pitch class PC.
A-value identifies the number and type of accidentals of a pitch
class. The absolute value of an A-value gives the number of
accidentals, while a negative or positive number means all
accidentals are flats or sharps respectivelly."
  (chrome-accidentals pc)) ;;property accidentals is currently stored as an A-value

;; Shortcuts for S-, Q-, A-values
(defalias 'pcs 'chrome-s-value)
(defalias 'pcq 'chrome-q-value)
(defalias 'pca 'chrome-a-value)

;; Some S, Q, A arithmetics
(defun* s-add (s1 s2 &optional (n)) 
   "Adds S2 N times to S1.
Check if this is useful." 
  (mod (+ s1 (* n s2)) 7))

(defun* a-add (a1 a2 &optional (n)) 
  "Adds S2 N times to S1" 
  (+ a1 (* n a2)))

(defun chrome- (pc1 pc2)
  ""
  (chrome-new (- (chrome-base pc2) (chrome-base pc1))
	  (- (chrome-accidentals pc2) (chrome-accidentals pc1))))
;;(apply #'chrome- (mapcar #'chrome-from-string '("G#" "F"))) ==> (chrome -1 -1)

(defun* chrome-transpose (chrome chrome-interval &optional (n 1))
  "Transposes pitch class CHROME with CHROME-INTERVAL N times.
The S- and A-values of transposed notes are calculated as follows:
S(T(chrome)) = (S(chrome) + N * S(T)) mod 7
A(T(chrome)) = (A(chrome) + N * A(T)) + (Q(chrome) + N * Q(T) + 1) \ 7
TODO: check if this is faster than going through `p-transpose'"
  (chrome-new (+ (s-add (pcs chrome) (pcs chrome-interval) n))
	  (+ (pca chrome) (* n (pca chrome-interval))
	     (floor (+ (pcq chrome) (* n (pcq chrome-interval)) 1) 7))))
;;(chrome-to-string (chrome-transpose (chrome-new 0 0) (chrome-new 3 1)))

(defun chrome-generate-all ()
  (mapcar #'(lambda (x) (apply #'chrome-new x)) (vxw (a-b 0 6) (a-b -2 2))))
;;(chrome-generate-all)

(defun chrome-nalterate (pc n)
  (incf (chrome-accidentals pc) n)
  pc)
;;(let ((pc (chrome-new))) (list (chrome-alterate pc -1) pc))

(defun chrome-alterate (pc n)
  (chrome-nalterate (chrome-copy pc) n))
;;(let ((pc (chrome-new))) (list (chrome-alterate pc -1) pc))

(defun chrome-alteration (chrome1 chrome2)
  "Returns the alteration (an integer) from chrome1 to chrome2.
If chrome2 is not an alteration of chrome1, nil is returned."
  (and (= (chrome-base chrome1)
	  (chrome-base chrome2))
       (- (chrome-accidentals chrome1)
	  (chrome-accidentals chrome2))))

(defun chrome-alteration-p (chrome1 chrome2 &optional n)
  "Returns nil iff PITCH-CLASS2 is an N-alteration of PITCH-CLASS1.
If optional argument N is not specified, the method accepts both
-1 and 1 (single chromatic step downwards and upwards
respectively) as matching alteration values."
  (aif (chrome-alteration chrome1 chrome2)
    (if n
      (= it n) 
      (= (abs it) 1))))

;;; conversions
(defun chrome-to-spitch (chrome)
  "This is only a help class to avoid mod 12 too many times..."
  (+ (chb-to-spc (chrome-base chrome)) (chrome-accidentals chrome)))
;;(chrome-to-spitch (chrome-new))


;;; read/write
(defun n-tuple-to-string (n)
  (case n
    ((1) "single")
    ((2) "double")
    ((3) "triple")
    (otherwise (format "%d-tuple" n))))
;;(mapcar #'n-tuple-to-string (0-n 6))

(defun* chrome-to-string-symbol (pc &optional (print-style mu-default-print-style))
  (let* ((a (pca pc))
	 (is-H (= (pcs pc) 6))
	 (is-flat (< a 0))
	 (is-german-H (and is-H is-flat (eq print-style 'german)))
	 (pbc (chrome-base pc))
	 (chb-string (if is-german-H
		       "B"
		       (chb-to-string pbc print-style)))
	 (a-string (if (= a 0) "" 
		       (if is-flat 
			 (multiply-string (flats-print-style print-style) (- (if is-german-H -1 0) a))
			 (multiply-string (sharps-print-style print-style) a)))))
    (concat chb-string a-string)))
;;(chrome-to-string-symbol (chrome-new 6 -1) 'english-chord)

(defun chrome-to-string-lilypond (pc)
  (let* ((a (pca pc))
	 (chb-string (chb-to-string (chrome-base pc) 'lilypond))
	 (a-string (if (< a 0)
			 (multiply-string (flats-print-style 'lilypond) (abs a))
			 (multiply-string (sharps-print-style 'lilypond) (abs a)))))
    (concat chb-string a-string)))
;;(prin1 (mapcar #'chrome-to-string-lilypond (chrome-generate-all)))

(require 'print-style)
(defun* chrome-to-string (pc &optional (print-style mu-default-print-style))
  ;;for now
  (case print-style
    (lilypond (chrome-to-string-lilypond pc))
    (otherwise (chrome-to-string-symbol pc print-style))))
;;(prin1 (mapcar #'chrome-to-string (chrome-generate-all)))

(defun chrome-from-string-lilypond (pitch-string)
  (let* ((pbc (chb-from-string (substring pitch-string 0 1) 'lilypond))
	 (accidentals-strings (split-string-by-length (substring pitch-string 1) 2))
	 (num-is (count "is" accidentals-strings :test #'equal))
	 (num-es (count "es" accidentals-strings :test #'equal))
	 (accidentals-count (- num-is num-es)))
    (when (or (/= (+ 1 (* num-is 2) (* num-es 2)) (length pitch-string))
	      (nor (zerop num-is) (zerop num-es)))
      (error "Argument '%s' is not a valied Lilypond pitch string" pitch-string))
    (make-chrome :base pbc :accidentals accidentals-count)))
;;(chrome-from-string-lilypond "aises")

(defconst chrome-regexps
  '((english "[ABCDEFG][#b]*" )
    (lilypond "[abcdefg]\\(\\(es\\)|\\(as)\\)*" )))

(defun* chrome-regexp (&optional (style mu-default-print-style))
  (tmap-0-1 style chrome-regexps))
;;(chrome-regexp 'english)

(defun* chrome-from-string (pitch-symbol-or-string &optional (style mu-default-print-style))
  "Symbol style only"
  (let ((s (sstring pitch-symbol-or-string)))
    (case style
      (lilypond (chrome-from-string-lilypond s))
      (otherwise (let* ((number-of-accidentals (1- (length s)))
			(pbc (chb-from-string (substring s 0 1))))
		   (make-chrome 
		    :base pbc
		    :accidentals (if (> number-of-accidentals 0)
				   (* number-of-accidentals (if (eq (char s 1) ?#)
							      1 -1))
				   0)))))))
;;(mapcar #'chrome-from-string (list "C" 'D))

(defun* chromes-from-string (chromes-string-or-list &optional (style mu-default-print-style))
  "Symbol style only"
  (mapcar (bind #'chrome-from-string style)
	  (if (stringp chromes-string-or-list)
	    (split-string chromes-string-or-list)
	    chromes-string-or-list)))
;;(mapcar #'chromes-from-string (list "Eb A#" '(C D Eb)))

(provide 'chrome)
