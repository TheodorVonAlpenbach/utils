(require 'pitch)
(require 'note)

(defconst mu-intervals
  `((perfect-unison ,(chrome-new 0 0) P1)
    (augmented-unison ,(chrome-new 0 1) A1)
    (diminished-unison ,(chrome-new 0 -1) d1)

    (major-second ,(chrome-new 1 0) M2)
    (minor-second ,(chrome-new 1 -1) m2)
    (augmented-second ,(chrome-new 1 1) A2)
    (diminished-second ,(chrome-new 1 -2) d2)

    (major-third ,(chrome-new 2 0) M3)
    (minor-third ,(chrome-new 2 -1) m3)
    (augmented-third ,(chrome-new 2 1) A3)
    (diminished-third ,(chrome-new 2 -2) d3)

    (perfect-fourth ,(chrome-new 3 0) P4)
    (augmented-fourth ,(chrome-new 3 1) A4)
    (diminished-fourth ,(chrome-new 3 -1) d4)

    (perfect-fifth ,(chrome-new 4 0) P5)
    (augmented-fifth ,(chrome-new 4 1) A5)
    (diminished-fifth ,(chrome-new 4 -1) d5)

    (major-sixth ,(chrome-new 5 0) M6)
    (minor-sixth ,(chrome-new 5 -1) m6)
    (augmented-sixth ,(chrome-new 5 1) A6)
    (diminished-sixth ,(chrome-new 5 -2) d6)

    (major-seventh ,(chrome-new 6 0) M7)
    (minor-seventh ,(chrome-new 6 -1) m7)
    (augmented-seventh ,(chrome-new 6 1) A7)
    (diminished-seventh ,(chrome-new 6 -2) d7)
     )
  "Should employ tone structure (or perhaps a structure of its
  own) instead, since the octave part will be needed")

(defun i-copy (interval)
  (chrome-copy interval))

(defun i-inverted-p (interval)
  (< (p-octave interval) 0))

(defun i-number (interval)
  (+ (if (i-inverted-p interval) -1 1)
     (chrome-base (p-chrome interval))
     (* (p-octave interval) 7)))
;;(i-number (i-parse 'm-2))

(defun i-invert (interval)
  (p- (i-new) interval))
;;(i-invert (i-from-abbreviation "d-2"))

(defun i-abs (interval)
  "Returns an :UP-ward copy of interval"
  (if (i-inverted-p interval)
    (i-invert interval)
    (i-copy interval)))

(defun i-direction (interval)
  "Doesn't take octave into account"
  (if (< (p-octave interval) 0) :down
      (if (i-is interval 'P1o) nil :up)))
;;(mapcar #'i-direction (mapcar #'i-parse '(P1 m2 m-2)))

(defun i-semitones (interval)
  (p-ansi interval 0))
;;(i-semitones (i-parse 'M3))

(defun i-new (&optional x y)
  "Creates a new interval from pitches.
Method may be invoked with the following argument combinations:
\(i-new [NOTE-OR-PITCH1 [NOTE-OR-PITCH2]], which creates the
interval from the pitches in NOTE-OR-PITCH1 and NOTE-OR-PITCH2
respectively. If the method is called with one argument, it
returns the interval from C1 to the argument pitch. If called
with no arguments, the method returns P1 (perfect prime).

Alternatively the pitches can be called with a list of 0, 1 or 2
pitch arguments.

\n(fn [NOTE-OR-PITCH1 [NOTE-OR-PITCH2]] | NOTE-OR-PITCHES)"
  (cond ((and (listp x) x) (apply #'i-new x))
	((note-p x) (i-new (n-pitch x) y))
	((note-p y) (i-new x (n-pitch y)))
	(t (if x
	     (if y
	       (p-interval x y)
	       (p-copy x))
	     (if y
	       (p-copy y)
	       (p-new (chrome-new) 0))))))
;;(i-new (list (make-note)) (make-note))
;;(i-new (list (make-pitch) (make-pitch)))
;;(i-new (make-pitch) (make-pitch))
;;(i-new (make-pitch))
;;(i-new)


(defun is-new (pitches)
  (mapcar (bind #'apply #'i-new 1) (pairs pitches)))
;;(mapcar #'i-symbol (is-new (mapcar #'p-from-string '("C1" "C#1" "G1"))))

(defun i-alteration (interval)
  (chrome-accidentals (p-chrome interval)))
;;(i-alteration (i-new))

(defun i-base (interval)
  (p-copy interval :octave 0))
;;(i-symbol (i-base (p-new)))

(defun i-type (interval)
  (intern (string-match* "[^-[:digit:]]*" (i-abbrevation (i-parse interval)))))
;;(mapcar #'i-type '(P1 m2 A4))

(defun i-step (interval)
  (string-to-number (string-match* "[^-[:digit:]]*\\(-?[[:digit:]]*\\)"
				   (i-abbrevation (i-parse interval)) 1)))
;;(mapcar #'i-step '(P1 m2 A-4))

(defun i-nalterate (interval n)
  "See `p-nalterate'"
  (p-nalterate interval n))

(defun i-alterate (interval n)
  "See `p-alterate'"
  (p-alterate interval n))

(defun i= (interval1 interval2)
  (p= (i-parse interval1)
      (i-parse interval2)))
;;(i= (i-new) (i-new))

(defun i/= (interval1 interval2)
  (not (i= interval1 interval2)))

(defun i~ (interval1 interval2)
  (p= (i-base interval1) (i-base interval2)))
;;(i~ (i-parse 'P1) (i-parse 'P8))

(defun i< (interval1 interval2)
  (let* ((i1 (i-parse interval1))
	 (i2 (i-parse interval2))
	 (o1 (p-octave i1))
	 (o2 (p-octave i2))
	 (pc1 (p-chrome i1))
	 (pc2 (p-chrome i2))
	 (pbc1 (chrome-base pc1))
	 (pbc2 (chrome-base pc2))
	 (a1 (chrome-accidentals pc1))
	 (a2 (chrome-accidentals pc2)))
    (if (/= o1 o2)
      (< o1 o2)
      (if (/= pbc1 pbc2)
	(< pbc1 pbc2)
	(< a1 a2)))))

(defun i> (interval1 interval2) (i< interval2 interval1))

(defun* i-is (interval intervals &optional (test #'i=))
  (and interval
       (let ((interval* (i-parse interval))
	     (intervals* (i-parse intervals)))
	 (if (listp intervals*)
	   (find interval* intervals* :test test)
	   (funcall test interval* intervals*)))))
;;(i-is 'P1 'P8 #'i~)

(defun i-perfect-consonance-p (interval)
  (eq (elt (i-abbrevation interval) 0) ?P))
;;(mapcar #'i-perfect-consonance-p (mapcar #'second mu-intervals))

(defun i-imperfect-consonance-p (interval)
  (find (elt (i-abbrevation interval) 0) '(?m ?M)))
;;(count nil (mapcar #'i-imperfect-consonance-p (i-a-b 'P1 'P8)))
;;(length (i-a-b 'P1 'P8))

(defun i-parallel-p (interval &rest intervals)
  (apply #'equal* (mapcar #'i-direction (cons interval intervals))))

(defun i-skip-p (interval)
  "Returns the direction of interval iff it is a skip, else returns nil"
  (not (i-is interval '(P1 A1 d1 m2 M2 m-2 M-2))))

(defun i-step-p (interval)
  "Returns the direction of interval iff it is a step, else returns nil"
  (i-is interval '(m2 M2 m-2 M-2)))
;;(i-step-p 'P5)

;;; read/write
(defun i-from-abbreviation (interval-string)
  (let* ((inverse (string-match "-" interval-string))
	 (opos (string-match "[-[:digit:]]" interval-string))
	 (pbc* (1- (abs (string-to-number (substring* interval-string opos)))))
	 (pbc (mod pbc* 7))
	 (o (floor pbc* 7))
	 (alteration (string-case (substring* interval-string 0 opos)
		       ("P" 0) ("M" 0) ("m" -1) ("A" 1) 
		       ("d" (if (find pbc '(0 3 4))
				-1 -2))))
	 (i (p-new (chrome-new pbc alteration) o)))
    (if inverse (i-invert i) i)))
;;(i-from-abbreviation "P-1")

(defun i-from-symbol (interval-symbol)
  (i-from-abbreviation (symbol-name interval-symbol)))
;;(i-from-symbol 'P-12)

(defun i-alteration-symbol-name (interval)
  (let* ((iv (if (i-inverted-p interval) (i-invert interval) interval))
	 (alt (i-alteration iv)))
    (case (i-number iv)
      ((1 4 5) 
       (if (zerop alt) "P"
	   (if (> alt 0)
	     (make-string alt ?A)
	     (make-string (- alt) ?d))))
      (otherwise 
       (case (i-alteration iv)
	 (0 "M")  (-1 "m") (otherwise (if (> alt 0)
					(make-string alt ?A)
					(make-string (1- (- alt)) ?d))))))))

(defun i-alteration-symbol (interval)
  (intern (i-alteration-symbol-name interval)))
;;(i-alteration-symbol (i-parse 'A3))

(defun i-alteration-name (interval)
  (case (i-alteration-symbol interval)
    (P "perfect") (M "major") (m "minor")
    (a "augmented")
    (aa "doubly augmented")
    (dd "doubly diminished")))
;;(i-alteration-name (i-parse 'M3))

(defun i-abbrevation (interval)
  (format "%s%d" 
      (i-alteration-symbol-name interval) 
      (i-number interval)))
;;(i-abbrevation (i-from-abbreviation "d2"))

(defun i-symbol (interval)
  (intern (i-abbrevation interval)))
;;(i-symbol (i-new))

(defun i-parse (interval-object)
  (if (listp interval-object)
    (mapcar #'i-parse interval-object)
    (case (type-of interval-object)
      (symbol (i-from-symbol interval-object))
      (string (i-from-abbreviation interval-object))
      (vector interval-object))))
;;(i-parse (list 'P-12 "M3" (i-new)))

;;; test
(defun* i-a-b (&optional (from 'P-12) (to 'P12))
  "Returns a sorted list of all main intervals in [FROM TO].
Secondary augmented"
  (let* ((from* (i-parse from))
	 (to* (i-parse to))
	 (ofrom (p-octave from*))
	 (oto (p-octave to*)))
    (sort (loop for o from ofrom to oto
		append 
		(loop for mui in mu-intervals
		      for ibase = (second mui)
		      for i = (p-new ibase o)
		      for is = (i-symbol i)
		      if (and (not (i< i from*))
			      (not (i< to* i)))
		      collect i))
	  #'i<)))
;;(prin1 (mapcar #'i-symbol (i-a-b 'd-3 'P4)))

(provide 'interval)
