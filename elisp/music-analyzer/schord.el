(require 'chrome)
(require 'schordx)
(require 'set-pitch-class)

(defun schord-new ()
  '(0 4 7))

(defun* schord-invert (schord &optional (n 1))
  "Non-destructive."
  (rotate-list schord n))
;;(schord-invert (schord-new) 2)

(defun* schord-reduce (schord &optional (ordered t) (trivial nil))
  "Reduces set-chord SCHORD to a minimal form:
* the bass spitch is kept as first element
* duplicate spitches are removed
* resulting schord is transposed so that bass is 0
+ iff ORDERED is non nil, the result is sorted. 
+ Iff TRIVIAL is nil the bass spitch (which otherwise is always 0) is removed "
  (let ((res (mapcar (bind #'spc-transpose (- (first schord)))
		     (remove-duplicates schord :from-end t))))
    (when ordered (setq res (sort res #'<)))
    (unless trivial (setq res (rest res)))
    res))
;;(schord-reduce '(7 2 7 10) nil t)
;;(schord-to-schosk '(4 7 0) t nil)
;;(mapcar #'scs-from-sc '((7 2 7 10) (7 2 10) (2 2 6 9)))

(defun schord-type (schord)
  "Returns the classification of set chord SC"
  (schordx-type (schord-to-schordx schord)))
;;(mapcar #'schord-type '((7 2 7 10) (7 2 10) (2 2 6 9) (2 5 9 0) (5 0 2 9)))

(defun schord-typename (sc)
  (second (first (schord-type sc))))
;;(schord-typename '(7 2 7 10))

(defun schord-inversion (schord)
  "0 is root postition, 1 is first inversion etc"
  (schordx-inversion (schord-to-schordx schord)))
;;(schord-inversion '(4 7 0))

(defun schord-transpose (schord n)
  "Transposes set-chord N semitones up. See also `spc-transpose'"
  (mapcar (bind #'spc-transpose n) schord))
;;(schord-transpose '(4 7) 1)

(defun schord-root (schord)
  "Returns the root of set chord SC. "
  (schordx-root (schord-to-schordx schord)))
;;(mapcar #'schord-root '((7 2 7 10) (2 2 6 9) (2 5 9) (5 0 2 9)))

(defun schord-position-of-root (schord)
  (position (schord-root schord) schord))
;;(schord-position-of-root '(4 0 7))

(defun schord-as-schosk (schord)
  (loop with schosk = (copy-list schord)
	for i from 0
	for schosk = (schosk-invert schosk i)
	for schosk-info = (schosk-info-from-schosk schosk)
	if schosk-info 
	return (list schosk i)
	)
  (list  (schord-inversion schord) (schord-root schord)))
;;(schord-as-schosk '(4 7 0)) 

;;; Conversions
(defun schord-to-schosk (schord)
  (error "Dangerous")
  (schord-reduce schord t t))

(defun schord-to-schordx (schord)
  (loop with n = (length schord)
	for i below n
	for schord-inverted = (schord-invert schord i)
	for root = (first schord-inverted)
	for schosk = (schord-reduce schord-inverted)
	if (schosk-exists-p schosk)
	return (schordx-new schosk (mod (- i) n) root)))
;;(schord-to-schordx '(6 9 2))

(defun* schord-to-chord (schord &optional (reference-chrome (chrome-new)))
  (schordx-to-chord (schord-to-schordx schord) reference-chrome))
;;(schord-to-chord '(2 5 9) (chrome-new))

(defun* schord-from-schosk (schosk &optional (inversion 0) (root 0))
  (schord-invert (schord-transpose (cons 0 schosk) root) inversion))
;;(schord-from-schosk '(4 7) 1 2)

(defun schord-from-schordx (schordx)
  (apply #'schord-from-schosk schordx))
;;(schord-from-schordx (schordx-new '(4 7) 1 2))

(defun schord-from-chord (chord)
  (mapcar #'chrome-to-spitch chord))
;;(chord-to-schord (chord-new))

(defun schord-root-position (sc)
  (scs-to-sc (scs-from-sc sc) (schord-root sc)))
;;(schord-root-position '(7 2 7 10))
;;(mapcar #'schord-root-position '((7 2 7 10) (2 2 6 9) (2 5 9) (5 0 2 9)))

;;; next section concerns functions when the key is known (or assumed)
(defun spc-scale-position (spc key)
  "Returns the scale position of spc in KEY.
For instance a D in the d minor key has scale position 0 (tonic).
An F in the c minor key has scale position 3 (subdominant)."
  (position (spc-transpose spc (- (first key))) ;spc transposed to C
	    (second (set-scale (second key)))   ;C minor/major set scale
	    :test #'(lambda (x y) (if (listp y) (find x y) (= x y)))))
;;(mapcar (bind #'spc-scale-position '(2 major)) '(2 4 6 7 9 11 1 2))

(defun schord-scale-position (sc key)
  "Returns the scale position of sc in KEY.
For instance a d minor chord in the d minor key has scale
position 0 (tonic). An f minor chord in the c minor key has scale
position 3 (subdominant)."
  (awhen (schord-root sc) (spc-scale-position it key)))
;;(schord-scale-position (mapcar (bind #'+ 3) '(0 4 7)) '(2 minor))

(defun chord-to-sc (chord)
  "Transforms a NOTE based chord to a SET-CHORD Note that this
version discards the mposition argument of chord. This should
probably be included later."
  (mapcar #'p-set-chrome chord))

(defun n-chord-to-sc (notes)
  (chord-to-sc (mapcar #'n-pitch notes)))

(defun chords-to-scs (chords)
  "Transforms a list of NOTE based chord to a list of SET-CHORDs"
  (mapcar #'chord-to-sc chords))
;;(chords-to-scs (mvt-chords (mvt-test)))

(lexical-let ((test-set-chords nil))
  (defun test-scs (&optional reset)
    "Returns a list of a plausable set chords for test usage"
    (unless test-set-chords
      (setq test-set-chords (chords-to-scs (mvt-chords (mvt-test)))))
    test-set-chords))
;;(test-scs t)

(defun schord-root-difference (sc1 sc2)
  (mod (- (schord-root sc2) (schord-root sc1)) 12))


;;; type queries
(defun schord-type-p (sc type)
  "Is SC of type TYPE?"
  (scs-type-p (scs-from-sc sc) type))

(defun schord-tonic-p (sc)
  "May SC work as a tonic?"
  (or (schord-type-p sc 'major)
      (schord-type-p sc 'minor)))

(defun schord-triad-p (sc)
  "Returns nil iff SC is not a major or minor triad chord."
  (find (schord-typename sc) '(minor-triad major-triad)))

(defun schord-dominant-p (sc)
  "Must be dominant, ie. a major triad does not qualify"
  (find (schord-typename sc) '(dominant-seventh-without-fifth dominant-seventh-without-third dominant-seventh)))

(defun schord-dominantic-p (sc)
  "Returns nil iff SC is not a set chord that could functions as a dominant."
  (find (schord-typename sc) '(major-triad dominant-seventh dominant-seventh-without-fifth dominant-seventh-without-third)))
;;(schord-dominantic-p '(0 4 7 10))

(defun schord-diminished-p (sc)
  "Returns nil iff SC is not a set chord that is diminished"
  (find (schord-typename sc) '(diminished-triad diminished-seventh)))
;;(schord-diminished-p '(0 3 6))


;;; relation queries
(defun schord-dominant-fifth-relation-p (sc1 sc2)
  (and (spc-dominant-relation-p (schord-root sc1) (schord-root sc2))
       (schord-dominantic-p sc1)
       (schord-triad-p sc2)))

(defun schord-dominant-dim-relation-p (sc1 sc2)
  "Dm T."
  (and (= 1 (mod (schord-root-difference sc1 sc2) 3))
       (schord-diminished-p sc1)
       (schord-triad-p sc2))) 

(defun schord-dominants-submediant-relation-p (sc1 sc2)
  "Ds T"
  (or
   ;; minor (must be in 1st inversion? not for now)
   (and (= 9 (schord-root-difference sc1 sc2))
	(eq (schord-typename sc1) 'augmented-triad)
	(schord-triad-p sc2)) ;; allow for picardian resolution
   ;; major: must be in 1st inversion
   (and (= 8 (mod (schord-root-difference sc1 sc2) 3))
	(schord-type-p sc1 'minor) ;;allow for 7th? not for now
	(= (schord-inversion sc1) 1) ;;1st inversion
	(schord-type-p sc2 'major))))

(defun schord-dominantic-relation-p (sc1 sc2)
  (or (schord-dominant-fifth-relation-p sc1 sc2)
      (schord-dominant-dim-relation-p sc1 sc2)
      (schord-dominants-submediant-relation-p sc1 sc2)))
;;(schord-dominant-relation-p  '(7 9 1 4) '(5 9 2 2))
;;(schord-type '(0 4 7))

(defun schord-neapolitan-relation-p (sc1 sc2)
  (and (= (schord-root-difference sc1 sc2) 11)
       (schord-type-p sc1 'major)
       (schord-triad-p sc2)))

(defun schord-subdominant-relation-p (sc1 sc2)
  "Note the difference to subdominantic"
  (and (= (schord-root-difference sc1 sc2) 7)
       ;;allow alterations in this version
       (schord-tonic-p sc2)))

(defun schord-subsubdominant-relation-p (sc1 sc2)
  "Is SC1 the SS of SC2"
  (and (= (schord-root-difference sc1 sc2) 10)
       ;;allow alterations in this version
       (schord-tonic-p sc2)))

(defun schord-subdominantic-relation-p (sc1 sc2)
  (or (schord-subdominant-relation-p sc1 sc2)
      (schord-subsubdominant-relation-p sc1 sc2)
      (schord-neapolitan-relation-p sc1 sc2)))

(defun schord-relation (schord1 schord2)
  (schordx-relation (schord-to-schordx schord1)
		    (schord-to-schordx schord2)))

(defun* schord-to-string (schord &optional (print-style mu-default-print-style))
  (schordx-to-string (schord-to-schordx schord) print-style))
;;(mapcar #'schord-to-string '((7 2 7 10) (2 2 6 9) (2 5 9) (5 0 2 9)))

(defun* schord-from-string (s &optional (style mu-default-print-style))
  "Does not work. Need to have a pc regexp on strings"
  (schord-from-schordx (schordx-from-string s)))
;;(schord-from-string "Cm")

;;(schord-from-chord-string "D")
;;(schord-from-chord-string "C#d7")

(provide 'schord)
