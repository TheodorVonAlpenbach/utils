(require 'chrome)
(require 'set-chord-skeleton)
(require 'set-pitch-class)

(defun sc-type (sc)
  "Returns the classification of set chord SC"
  (scs-type (scs-from-sc sc)))
;;(mapcar #'sc-type '((7 2 7 10) (7 2 10) (2 2 6 9) (2 5 9 0) (5 0 2 9)))

(defun sc-typename (sc)
  (second (first (sc-type sc))))
;;(sc-typename '(7 2 7 10))

(defun sc-inversion (sc)
  "0 is root postition, 1 is first inversion etc"
  (second (sc-type sc)))
;;(sc-inversion '(4 7 0))

(defun sc-transpose (sc n)
  "Transposes set-chord N semitones up. See also `spc-transpose'"
  (mapcar (bind #'spc-transpose n) sc))
;;(sc-transpose '(4 7) 1)

(defun sc-root (sc)
  "Returns the root of set chord SC. "
  (let* ((type-res (sc-type sc))
	 (inversion (sc-inversion sc))
	 (sc-transposed (cons 0 (first (first type-res)))))
    (when type-res
      (spc-transpose (first sc) (- (nth inversion sc-transposed))))))
;;(mapcar #'sc-root '((7 2 7 10) (2 2 6 9) (2 5 9) (5 0 2 9)))

(defun schord-as-skeleton (schord)
  (list (skeleton schord) (sc-inversion schord) (sc-root schord)))
;;(schord-as-skeleton '(0 4 7))

(defun sc-root-position (sc)
  (scs-to-sc (scs-from-sc sc) (sc-root sc)))
;;(mapcar #'sc-root-position '((7 2 7 10) (2 2 6 9) (2 5 9) (5 0 2 9)))

;;; next section concerns functions when the key is known (or assumed)
(defun spc-scale-position (spc key)
  "Returns the scale position of spc in KEY.
For instance a D in the d minor key has scale position 0 (tonic).
An F in the c minor key has scale position 3 (subdominant)."
  (position (spc-transpose spc (- (first key))) ;spc transposed to C
	    (second (set-scale (second key)))   ;C minor/major set scale
	    :test #'(lambda (x y) (if (listp y) (find x y) (= x y)))))
;;(mapcar (bind #'spc-scale-position '(2 major)) '(2 4 6 7 9 11 1 2))

(defun sc-scale-position (sc key)
  "Returns the scale position of sc in KEY.
For instance a d minor chord in the d minor key has scale
position 0 (tonic). An f minor chord in the c minor key has scale
position 3 (subdominant)."
  (awhen (sc-root sc) (spc-scale-position it key)))
;;(sc-scale-position (mapcar (bind #'+ 3) '(0 4 7)) '(2 minor))

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

(defun sc-root-difference (sc1 sc2)
  (mod (- (sc-root sc2) (sc-root sc1)) 12))


;;; type queries
(defun sc-type-p (sc type)
  "Is SC of type TYPE?"
  (scs-type-p (scs-from-sc sc) type))

(defun sc-tonic-p (sc)
  "May SC work as a tonic?"
  (or (sc-type-p sc 'major)
      (sc-type-p sc 'minor)))

(defun sc-triad-p (sc)
  "Returns nil iff SC is not a major or minor triad chord."
  (find (sc-typename sc) '(minor-triad major-triad)))

(defun sc-dominant-p (sc)
  "Must be dominant, ie. a major triad does not qualify"
  (find (sc-typename sc) '(dominant-seventh-without-fifth dominant-seventh-without-third dominant-seventh)))

(defun sc-dominantic-p (sc)
  "Returns nil iff SC is not a set chord that could functions as a dominant."
  (find (sc-typename sc) '(major-triad dominant-seventh dominant-seventh-without-fifth dominant-seventh-without-third)))
;;(sc-dominantic-p '(0 4 7 10))

(defun sc-diminished-p (sc)
  "Returns nil iff SC is not a set chord that is diminished"
  (find (sc-typename sc) '(diminished-triad diminished-seventh)))
;;(sc-diminished-p '(0 3 6))


;;; relation queries
(defun sc-dominant-fifth-relation-p (sc1 sc2)
  (and (spc-dominant-relation-p (sc-root sc1) (sc-root sc2))
       (sc-dominantic-p sc1)
       (sc-triad-p sc2)))

(defun sc-dominant-dim-relation-p (sc1 sc2)
  "Dm T."
  (and (= 1 (mod (sc-root-difference sc1 sc2) 3))
       (sc-diminished-p sc1)
       (sc-triad-p sc2))) 

(defun sc-dominants-submediant-relation-p (sc1 sc2)
  "Ds T"
  (or
   ;; minor (must be in 1st inversion? not for now)
   (and (= 9 (sc-root-difference sc1 sc2))
	(eq (sc-typename sc1) 'augmented-triad)
	(sc-triad-p sc2)) ;; allow for picardian resolution
   ;; major: must be in 1st inversion
   (and (= 8 (mod (sc-root-difference sc1 sc2) 3))
	(sc-type-p sc1 'minor) ;;allow for 7th? not for now
	(= (sc-inversion sc1) 1) ;;1st inversion
	(sc-type-p sc2 'major))))

(defun sc-dominantic-relation-p (sc1 sc2)
  (or (sc-dominant-fifth-relation-p sc1 sc2)
      (sc-dominant-dim-relation-p sc1 sc2)
      (sc-dominants-submediant-relation-p sc1 sc2)))
;;(sc-dominant-relation-p  '(7 9 1 4) '(5 9 2 2))
;;(sc-type '(0 4 7))

(defun sc-neapolitan-relation-p (sc1 sc2)
  (and (= (sc-root-difference sc1 sc2) 11)
       (sc-type-p sc1 'major)
       (sc-triad-p sc2)))

(defun sc-subdominant-relation-p (sc1 sc2)
  "Note the difference to subdominantic"
  (and (= (sc-root-difference sc1 sc2) 7)
       ;;allow alterations in this version
       (sc-tonic-p sc2)))

(defun sc-subsubdominant-relation-p (sc1 sc2)
  "Is SC1 the SS of SC2"
  (and (= (sc-root-difference sc1 sc2) 10)
       ;;allow alterations in this version
       (sc-tonic-p sc2)))

(defun sc-subdominantic-relation-p (sc1 sc2)
  (or (sc-subdominant-relation-p sc1 sc2)
      (sc-subsubdominant-relation-p sc1 sc2)
      (sc-neapolitan-relation-p sc1 sc2)))

(defun sc-dominant-relations (scs)
  (loop for sc1 in (butlast scs)
	for sc2 in (rest scs)
	if (sc-dominant-relation-p (second sc1) (second sc2))
	collect (list sc1 sc2)))
;;(length (sc-dominant-relations (test-scs)))

(defun sc-to-string (sc)
  (format "%d%s" (sc-root sc) (scs-chord-symbol (scs-from-sc sc))))
;;(mapcar #'scs-from-sc '((7 2 7 10) (2 2 6 9) (2 5 9) (5 0 2 9)))
;;(mapcar #'sc-to-string '((7 2 7 10) (2 2 6 9) (2 5 9) (5 0 2 9)))

;;(sc-from-chord-string "D")
;;(sc-from-chord-string "C#d7")

(provide 'set-chords)
