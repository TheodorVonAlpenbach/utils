(require '4-scales)

;;; chord is a list of integers sorted on diatonic-order and with
;;; integer represent bass tone at the 0th position

(defun chord= (chord1 chord2) (apply #'and* (mapcar* #'n= chord1 chord2)))

(defconst chordtypes
  '((major-triad (4 7))
    (minor-triad (3 7))
    (diminished-triad (3 6))
    (augmented-triad (4 8))
    (suspended-triad (5 7))
    (major-triad-without-fifth (4))
    (minor-triad-without-fifth (3))

    (major-ninth-suspension (2 4 7))
    (minor-ninth-suspension (2 3 7))

    (major-seventh (4 7 11))
    (minor-seventh (3 7 10))
    (dominant-seventh (4 7 10))
    (dominant-seventh-flat-five (4 6 10))
    (diminished-seventh (3 6 9))
    (half-diminished-seventh (3 6 10))
    (minor-major-seventh (3 7 11))
    (augmented-major-seventh (4 8 11))
    (augmented-seventh (4 8 10))
    (suspended-seventh (5 7 11))
    (suspended-dominant-seventh (5 7 10))

    (major-seventh-without-fifth (4 11))
    (minor-seventh-without-fifth (3 10))
    (dominant-seventh-without-fifth (4 10))
    (minor-major-seventh-without-fifth (3 11))
    (suspended-seventh-without-fifth (5 11)))
  "Format is (chord-typ-name (steps) function)")

(defconst chord-functions
  '((tonic T)
    (subdominant-submediant Ss)
    (tonic-mediant Tm)
    (subdominant S)
    (dominant D)
    (tonic-submediant Ts)
    (dominant-mediant Dm)))

(defun chord-function-base (root-position)
  (nth root-position chord-functions))
;;(chord-function 2)

(defun transpose-note (note n)
  (and note (mod (+ note n) 12)))
;;(transpose-note 2 -2)

(defun transpose-notes (note-list n)
(mapcar (lambda (x) (mod (+ x n) 12)) note-list))
;;(transpose-notes '(0 2 4 5 7 9 11) 1)

(defun chord-skeleton (chord)
  (rest (transpose-notes chord (- (first chord)))))
;;(chord-skeleton '(4 7 12))

(defun* ninvert-chord-skeleton (cs &optional (n 1))
  "Destructive"
  (chord-skeleton (nrotate-list (push 0 cs) n)))
;;(ninvert-chord-skeleton '(4 7) 2)

(defun* invert-chord-skeleton (cs &optional (n 1))
  (ninvert-chord-skeleton (copy-list cs) n))
;;(invert-chord-skeleton major-root 2)
;;(setq major-root '(4 7))

(defun cs-chordtype (cs)
  "CS is chord-skeleton"
  (loop for i to (length cs) 
	for chordtype = (find (invert-chord-skeleton cs (- i)) chordtypes :key #'second :test #'equal)
	if chordtype return (list chordtype i)))
;;(chord-functions (test-choral))
;;(chord-functions (c-extract (test-choral) 2 4))
;;(cs-chordtype '(6 9))

(defun chordtype (chord)
  (cs-chordtype (chord-skeleton chord)))
;;(chordtype '(7 9 2 5))

(defun chord-root (chord)
  (let ((ct (chordtype chord)))
    (when ct (nth (mod (- (second ct)) (length chord)) chord))))
;;(chord-root '(2 5 9))
;;(chordtype '(2 5 9))

(defun chord-root-position (chord key)
  (let ((root (transpose-note (chord-root chord) (- (first key)))))
    (when root
      (loop for n in (second (scale-type (second key))) ;scale notes
	    for i from 0				;position
	    if (if (listp n) (find root n) (= root n))
	    return i))))
;;(chord-root-position '(6 9 0 2) '(2 minor))
;;(chord-root-position '(5 8 0) '(0 minor))

(defun chord-function (chord key)
  (let ((crp (chord-root-position chord key)))
    (and crp (list (nth crp chord-functions) (chordtype chord)))))
;;(chord-function  '(7 9 2 5) '(2 minor))

(defun chord-function-l (chord ch)
  (chord-function chord (list (diatonic-order (c-key-note ch)) (c-key-mode ch))))
;;(chord-function  '(5 9 2) '(2 major))

(defun chord-functions (ch)
  "Should not be used since function is dependant on neighbouring chords"
  (mapcar (bind #'chord-function-l ch) (c-chords ch)))
;;(chord-functions (load-choral "c:/Users/Theodor/Documents/projects/UiO/MUS-1221/Mappeoppgave-3/Mappeoppgave-3.ly"))
;;(chord-functions (c-extract (test-choral) 2 20))
;;(c-chords (test-choral))

(defun lchord-to-chord (lch) 
  "Must be possible to make more efficient."
  (let ((chord nil)) 
    (loop for dn in (nreverse (mapcar #'diatonic-order lch))
	  do (push-unique dn chord))
    (setq chord (nreverse chord))
    (setf (rest chord) (sort* (rest chord) #'< :key #'(lambda (x) (mod (- x (first chord)) 12))))
    chord))
;;(lchord-to-chord (c-chord (test-choral) 1))


;;;; Text representation
(defun chord-function-symbol (chord-function)
  (second (first chord-function)))
;;(chord-function-symbol (chord-function  '(5 9 2) '(2 major)))

(defun chord-function-bass (chord-function)
  (let ((inversion (second (second chord-function))))
    (or (case inversion (0 "") (1 "_3") (2 "_5") (3 "_7") (else ""))
	"")))
;;(chord-function-bass (chord-function  '(5 9 2) '(2 major)))
;;(chord-function  '(5 9 2) '(2 major))((tonic T) ((minor-triad (3 7)) 1))

(defun chord-is-ninth (chordtype-name)
  (find chordtype-name '(major-ninth-suspension minor-ninth-suspension)))
;;(chord-function-is-ninth (chord-function  '(5 9 2 4) '(2 major)))
    
(defun chord-is-seventh (chordtype-name)  
  (find chordtype-name 
	'(major-seventh
	  minor-seventh
	  dominant-seventh
	  dominant-seventh-flat-five
	  diminished-seventh
	  half-diminished-seventh
	  minor-major-seventh
	  augmented-major-seventh
	  augmented-seventh
	  suspended-seventh
	  suspended-dominant-seventh
	  major-seventh-without-fifth
	  minor-seventh-without-fifth
	  dominant-seventh-without-fifth
	  minor-major-seventh-without-fifth)))
;;(chord-function-is-seventh (chord-function  '(5 9 0 2) '(2 major)))

(defun chord-is-fourth (chordtype-name)  
  (find chordtype-name 
	'(suspended-triad suspended-seventh suspended-dominant-seventh suspended-seventh-without-fifth)))
;;(chord-function-is-fourth (chord-function  '(7 9 2 4) '(2 major)))
;;chord-function: '((dominant D) ((suspended-dominant-seventh (5 7 10)) 3))

(defun chord-is-augmented (chordtype-skeleton)
  (find 8 chordtype-skeleton))

(defun chord-is-major (chordtype-skeleton)
  (find 4 chordtype-skeleton))

(defun chord-is-minor (chordtype-skeleton)
  (and (find 3 chordtype-skeleton)
       (find 7 chordtype-skeleton)))

(defun chord-is-diminished (chordtype-skeleton)
  (and (find 3 chordtype-skeleton)
       (find 6 chordtype-skeleton)))

(defun chord-is-+ (chord-root-position chordtype-skeleton key-mode)
  (or (and (eq key-mode 'major)
	   (chord-is-augmented chordtype-skeleton))
      (and (eq key-mode 'minor)
	   (or (and (= chord-root-position 0) (chord-is-major chordtype-skeleton))
	       (and (= chord-root-position 1) (chord-is-minor chordtype-skeleton))
	       (and (= chord-root-position 3) (chord-is-major chordtype-skeleton))
	       (and (= chord-root-position 5) (chord-is-augmented chordtype-skeleton))))))

(defun chord-is-o (chord-root-position chordtype-skeleton key-mode)  
  (or (and (eq key-mode 'major)
	   nil) ;;nothing in major at this point
      (and (eq key-mode 'minor)
	   (or (and (= chord-root-position 2) (chord-is-major chordtype-skeleton))
	       (and (= chord-root-position 4) (chord-is-minor chordtype-skeleton))
	       (and (= chord-root-position 6) (chord-is-major chordtype-skeleton))))))

(defun chord-step-modifier (chord-root-position chordtype-skeleton key-mode)
  (if (chord-is-+ chord-root-position chordtype-skeleton key-mode)
    "+"
    (chord-is-o chord-root-position chordtype-skeleton key-mode)
    "o"
    ""))

(defun chord-function-print (chord key)
  "The step modificators could better be put in the chord-function table"
  (let* ((ct (chordtype chord))
	 (ct-name (first (first ct)))
	 (ct-skeleton (second (first ct)))
	 (cf (chord-function chord key))
	 (s (chord-function-symbol cf))
	 (step-modificators ())
	 (mode-modicicator nil)
	 (bs (chord-function-bass cf))
	 (crp (chord-root-position chord key))
	 (key-mode ((second key))))
    (when (chord-is-fourth ct-name) (push 4 step-modificators))
    (when (string/= bs "_7")
      (when (chord-is-seventh ct-name)
	(push 7 step-modificators)))
    (when (chord-is-ninth ct-name) (push 9 step-modificators))
    (format "%s%s%s%s" 
      (chord-step-modifier crp ct-skeleton key-mode)
      s
      (concat* step-modificators :in "," :key #'number-to-string)
      bs)))
;;(chord-function-print '(0 2 6 9) '(2 minor))

(provide '4-chord)
