(require 'movement)
(require 'voice-group)
(require 'mu-general)
(require 'schosk)

;; See Rohrmeier (https://drive.google.com/a/contango.no/?tab=co#folders/0B0tCbZB2ykMzbDFhNE04VFBXVlU)
;  for a discussion of segmentation methods

(defun* segmentation (x &optional (segmentation-method 'harmonic))
  "Segments X according to SEGMENTATION-METHOD, and returns the
modified X"
  (funcall (case segmentation-method
	     (total #'total-segmentation)
	     (dense #'dense-segmentation)
	     (metric #'metric-segmentation)
	     (harmonic #'harmonic-segmentation)
	     (mb #'mb-segmentation)) x))

(defun notes-mapc (x function &optional preserve-tree)
  "Returns a list of all notes
TODO: also accept a list of notes"
  (mapc function (notes x preserve-tree)))
;;(notes-mapc qwe #'(lambda (n) (setf (d-value (n-duration n)) (* 2 (d-value (n-duration n))))))
;;(movement-to-lilypond qwe :start t)
;;(setq qwe (mvt-submovement (mvt-test) 5 7))

(defun durations (x)
  "Returns a list of all durations in VOICE"
  (mapcar #'n-duration (notes x)))
;;(durations (mvt-test))

(defun lcm-duration (x)
  "Returns the least common multiplum of the durations in VOICES
TODO: Do better than this: 
In this version we split each duration to a puncutation vector
and finds the non-empty least element in those vectors.
See `duration.el'"
  (loop for i from 0 below 10 ;;10 is ridicously high, but we must avoid eternal loop
	for ds = (mapcar #'d-value (durations x)) then (mapcar (bind #'* 2) ds)
	for ds* = (remove-if #'zerop ds :key (bind #'mod 1.0))
	unless ds* return (/ 1.0 (expt 2 i))))
;;(lcm-duration (mvt-test))

(defun* notes-total-segmentation (ns &optional (unit (lcm-duration ns)))
  "Splits NS according to `total-segmentation'"
;;  (mapcar (bind #'n-split (bind #'d-split-total (lcm-duration ns))) ns)
  (mapcan #'(lambda (n) (n-split n (bind #'d-split-total unit))) ns))
;;(notes-total-segmentation (notes (v-test)))

(defun* total-segmentation (x &optional (unit (lcm-duration x)))
  "Destructive, so make a copy of X, if necessary.
This segmentation is the most rudimentary: it identifies the
least common duration factor in VOICE-GROUP, and then it splits
the voice-group in to chords of that duration. Could be useful as
a foundation for other segmentation methods"
  (loop	for v in (voices x)
	do (setf (v-notes v) (notes-total-segmentation (v-notes v) unit)))
  x)
;;(setf total (total-segmentation (mvt-copy sm)))
;;(setq qwe (mvt-submovement (mvt-test) 6 8))
;;(movement-to-lilypond total :title "Total segmentation")

;;;; metric
(defun metric-segmentation (x)
  (let* ((vs (voices x))
	 (total-segmentation x))
    (loop for v in (voices x)
	  for ns = (v-notes v)
	  for metric-notes = (loop for n in ns
				   for st = (n-start-time n)
				   for m = (mod* st 1)
				   if (zerop m) collect n)
;;	  for metric-notes = (copy-if #'zerop ns :key (compose (bind #'mod* 1) #'d-value #'n-duration))
	  do (mapc #'(lambda (n)
		       (setf (d-value (n-duration n)) 1.0)
		       (setf (n-tied n) nil)) 
		   metric-notes)
	  do (setf (v-notes v) metric-notes))
    x))
;;(setf metric (metric-segmentation (mvt-copy total)))
;;(movement-to-lilypond metric :title "Metric segmentation")
;;(setq qwe (mvt-submovement (mvt-test) 6 8))

;;;; dense
(defun nchords (x)
  (voices x))

(defun nchord-equal (x y)
  (every #'n-equal x y))

(defun dense-segmentation (x)
  (let* ((vs (voices x))
	 (vs-total (total-segmentation vs))
	 (nss (notes vs-total t))
	 (nchords (transpose nss))
	 (nchords-groups (group nchords :test #'nchord-equal))
	 (nchords* (loop for g in nchords-groups
			 for nchord = (first g)
			 for ns = (first (transpose g))
			 for d = (sum ns :key (compose #'d-value #'n-duration))
			 if (> (length g) 1)
			 do (loop for n in nchord 
				  do (setf (d-value (n-duration n)) d)
				  do (setf (n-tied n) nil))
			 collect nchord)))
    (mapcar* #'(lambda (v ns) (setf (v-notes v) ns)) vs (transpose nchords*))
    x))
;;(setf sm (mvt-submovement (mvt-test) 1 2))
;;(setf dense (dense-segmentation (mvt-copy sm)))
;;(movement-to-lilypond dense :title "Dense segmentation")


;;;; harmonic
(defun sinterval-dissonance-score (sinterval)
  (case (abs sinterval)
    ((1 11) -4) ;; minor seconds
    ((2 10 6) -1) ;; major seconds or tritones
    (t 0)))
;;(mapcar #'sinterval-dissonance-score (0-n 12))

(defun schord-intervals (schordx)
  "TODO: move this"
  (loop for r in (relations (cons 0 (schordx-schosk schordx)))
	collect (- (apply #'- r))))
;;(schord-intervals '(0 4 7))

(defun schordx-dissonance-score (schordx)
  (sum (mapcar #'sinterval-dissonance-score (schord-intervals schordx))))
;;(schord-dissonance-score '(0 1 4 7))

(defun nchord-to-chord (nchord)
  (mapcar #'n-chrome nchord))
;;(nchord-to-chord (list (n-new)))

(defun nchord-new ()
  (nreverse
   (list (n-new (p-new (chrome-new 4 0) 3) 0 (d-new))
	 (n-new (p-new (chrome-new 1 0) 4) 0 (d-new))
	 (n-new (p-new (chrome-new 4 0) 4) 0 (d-new))
	 (n-new (p-new (chrome-new 6 -1) 4) 0 (d-new)))))
;;(mapcar #'n-to-string (nchord-new))

(defun schordx-from-nchord (nchord)
  (schordx-from-chord (nchord-to-chord nchord)))
(defun schordx-from-nchord (nchord)
  (schordx-from-spcs (reverse (mapcar (compose #'chrome-to-spitch #'n-chrome) nchord))))
;;(schordx-from-nchord (nchord-new))

(defun nchord-dissonance-score (nchord)
  (schordx-dissonance-score (schordx-from-nchord nchord)))
;;(schord-dissonance-score '(0 1 4 7))

(defun harmonic-segmentation-best-nchord (nchords)
  "Returns one nchord from NCHORDS based on the following rules:
1. if first schord in NCHORDS is consonant or dominant-seventh,
it is returned 2. else return the least consonant schord in
NCHORDS. See Rohrmeier
https://drive.google.com/a/contango.no/?tab=co#folders/0B0tCbZB2ykMzbDFhNE04VFBXVlU"
  (if (or (zerop (nchord-dissonance-score (first nchords)))
	  (schordx-dominant-p (schordx-from-nchord (first nchords))))
    (first nchords)
    (minimum nchords #'> :key (compose #'schordx-dissonance-score #'schordx-from-nchord))))

(defun* nchord-same-beat (nchord1 nchord2 &optional (dvalue 1))
  "Returns t iif two consecutive chords nchord1 and nchord2 are
on the same metric level. Metric level length is defined by (the
optional argument) DVALUE as this many `d-unit's.
TODO: Still a bit unclear, so perhaps rewrite"
  (plusp (mod* (n-start-time (first nchord2)) dvalue)))
;;(nchord-same-beat nil (list (n-new (p-new) .5)) .5)

(defun hs-adjust-note (n)
  "Quick and dirty method used by harmonic-segmentation. Makes
sure that note N starts on a main beat, has one beat's length and
is not tied to the following note"
  (setf (n-dvalue n) 1.0) 
  (setf (n-start-time n)
	(floor (n-start-time n)))
  (setf (n-tied n) nil))

(defun hs-adjust-nchord (nchord) (mapc #'hs-adjust-note nchord))

(defun harmonic-segmentation (x)
  "Only handles fairly metric voice-groups"
  (let* ((vs (voices x))
	 (vs-total (total-segmentation vs))
	 (nchords (transpose (notes vs-total t)))
	 (nchord-groups (group nchords :test #'nchord-same-beat))
	 (nchords* (mapcar #'harmonic-segmentation-best-nchord nchord-groups)))
    (mapc #'hs-adjust-nchord nchords*)
    ;; adjustments done: now the result is transfered to X
    (mapcar* #'(lambda (v ns) (setf (v-notes v) ns)) vs (transpose nchords*))
    x))
;;(setf harmonic (harmonic-segmentation (mvt-copy qwe)))


;;;; mb-segmentation

(defconst mb-seg-chord-groups
  '((major-triadic (major-triad
		    dominant-seventh
		    major-triad-without-fifth
		    major-seventh
		    dominant-seventh-without-fifth
		    dominant-seventh-without-third
		    major-seventh-without-fifth
					;suspended-triad
					;suspended-dominant-seventh
					;suspended-seventh
					;suspended-seventh-without-fifth
					;major-ninth-suspension
					;dominant-seventh-flat-five
		    ))
    (minor-triadic (minor-triad
		    minor-triad-without-fifth
		    minor-seventh
		    minor-major-seventh
		    minor-seventh-without-fifth
		    minor-major-seventh-without-fifth
					;suspended-triad
					;suspended-dominant-seventh
					;suspended-seventh
					;suspended-seventh-without-fifth
					;minor-ninth-suspension
		    ))
    (diminished-triadic (diminished-triad
			 diminished-seventh
			 half-diminished-seventh
			 ))
    (augmented-triadic (augmented-triad
			augmented-major-seventh
					;augmented-seventh
			))))
;;(schordx-group '((3 7) 0 0) nil mb-seg-chord-groups)


(defun* nchord-more-preferred (nchord1 nchord2)
  "Returns t if nchord1 is more preferred, nil if nchord2 is more
 preferred, and 'na if incommensurable.
incommensurable: 
* neither are legal
* both are legal but belongs to different chord groups or have different roots
"
  (let*
      ((schordx1 (schordx-from-nchord nchord1))
       (schordx2 (schordx-from-nchord nchord2))
       (ct1 (schordx-type schordx1))
       (ct2 (schordx-type schordx2))
       (order1 (second (first (chord-type-groups ct1 nil mb-seg-chord-groups))))
       (order2 (second (first (chord-type-groups ct2 nil mb-seg-chord-groups)))))

    (if order1
      (if order2
	(if (not (mequal #'schordx-root schordx1 schordx2))
	  ;;both legal but not the same root:
	  'na
	  ;;both legal, same root: returns best chord
	  ;;if equal, both nil (which will be returned) and t are equally good return values!)
	  (l-explicit*< ct1 ct2 order1))
	t)
      (if order2
	;;only nchord2 legal
	nil
	;;neither is legal, we return the first (could perhaps be the 'n)
	t))))


(defun nchord-concat (&rest nchords)
  "Returns the first nchord in NCHORDS, but after modifying the
start-time and duration so that these properties together
corresponds to nchords as a whole. Assumes that NCHORDS perferct
consecutive. Typically, used in connections with chord
segmentation"
  (let* ((notes (mapcar #'first nchords))
	 (start-time (min-value notes :key #'n-start-time))
	 (d-value (sum notes :key (compose #'d-value #'n-duration)))
	 (test (if (= d-value 0.75) (error "!!")))
	 (tied (n-tied (min-element notes :key #'n-start-time :test #'>)))
	 (res (mapcar #'(lambda (n) 
			  (setf (n-start-time n) start-time)
			  (setf (d-value (n-duration n)) d-value)
			  (setf (n-tied n) tied)
			  n)
		      (first nchords))))
    res))


(defun nchord-p (x)
  (and (listp x)
       (every #'note-p x)))

(defun nchord-group-p (x)
  (and (listp x)
       (every #'nchord-p x)))
;;(nchord-group-p nil)

(defun* nchord-groups-reduce-1 (g1 g2)
  "Must return a chord group, i.e. a list of list of notes.
Write this later.
Note: N is ignored in this version"
  (let ((res 
	 (if (or (> (length g1) 1) (> (length g2) 1))
	   (nconc g1 g2)
	   (aif (nchord-more-preferred (first g1) (first g2))
	       (if (or (eq it 'na))
		 (nconc g1 g2)
		 (list (nchord-concat (first g1) (first g2)))) ;do period transfer from g2
	       (list (nchord-concat (first g2) (first g1)))))));do period transfer from g1
    (if (nchord-group-p res)
      res
      (error "Expected a nchord-group")))) 

(defun* nchord-groups-reduce (nchord-groups &optional (n 2))
  "Write this doc later.
Note: N is ignored in this version"
  (loop for gg in (cut nchord-groups n)
	for x = (apply #'nchord-groups-reduce-1 gg)
	if (nchord-group-p x) collect x
	else do (error "Expected a nchord-group")))

(defun mb-segmentation (x)
  "Flexible version of harmonic segmentation. Only handles fairly metric voice-groups
TODO: remove tildes"
  (let* ((vs (voices x))
	 (vs-total (total-segmentation vs))
	 (nchords (transpose (notes vs-total t)))
 	 (nchord-groups (mapcar #'list nchords)))
    (loop for dvalue = (n-dvalue (first (first (first nchord-groups))))
	  while (< dvalue 1)
	  do (setf nchord-groups (nchord-groups-reduce nchord-groups)))
    (mapcar* #'(lambda (v ns) (setf (v-notes v) ns)) vs (transpose (flatten nchord-groups 1)))
    x))

(require 'lilypond-conversion)
;;(movement-to-lilypond (dense-segmentation (mvt-copy (mvt-test))) :title "dense" :file "c:/Documents and Settings/matsb/My Documents/data/ly/dense.ly" :start t :view nil)
;;(movement-to-lilypond (mvt-segmentation (mvt-submovement (mvt-test) 1 2)) "segment" "c:/Documents and Settings/matsb/My Documents/data/ly/segment.ly" :start t :view nil)
;;(movement-to-lilypond (mvt-submovement (mvt-test) 1 5) "segment" "c:/Documents and Settings/matsb/My Documents/data/ly/segment.ly" :start t :view nil)

(provide 'segmentation)
