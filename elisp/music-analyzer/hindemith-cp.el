(require 'interval)
(require 'cp)

(lexical-let ((hcp nil)
	      (hcp-file "c:/Documents and Settings/matsb/My Documents/projects/UiO/Var-2012/2270-Satslaere2B/ex-Hindemith-cp1.ly"))
  (defun* hcp-test (&optional (refresh nil) (name nil) (file nil))
    (when file
      (setq hcp-file file)
      (setq refresh t))
    (when (or refresh (not hcp))
      (let* ((vgs (ly-parse-file hcp-file))
	     (vg (if name 
		   (vgs-find-named-vg vgs name)
		   (first vgs)))
	     (voices (vg-voices vg)))
	(setq hcp (cp-new voices (length voices) 'hindemith name))))
    hcp))
;;(hcp-test t "14-30")

(defun hcp-from-vg (voice-group)
  (let ((vs (vg-voices voice-group)))
    (cp-new vs (length vs) 'hindemith (vg-name voice-group))))

(defun hcps-from-file (filename)
  (mapcar #'hcp-from-vg (ly-parse-file filename)))

(defun hcp-check-hindemith-file (filename)
  (cps-check-print (hcps-from-file filename)))
;;(hcp-check-hindemith-file "c:/Documents and Settings/matsb/My Documents/projects/UiO/Var-2012/2270-Satslaere2B/ex-Hindemith-cp1.ly")

(defconst hcp-root-intervals '(P1 m3 M3 P5 P8))
(defun hcp-root-p (interval)
  (i-is (i-abs interval) hcp-root-intervals #'i~))

(defun hcp-model (cp)
  (if (= 1 (length (cp-voices cp)))
    (first (cp-voices cp))
    (find "model" (cp-voices cp) :test #'equal :key #'v-instrument)))

(defun hcp-2nd (cp)
  (find "2nd" (cp-voices cp) :test #'equal :key #'v-instrument))

(defun hcp-rules (cp)
  (if (= 1 (length (cp-voices cp)))
    (hcp-model-rules cp)
    (hcp-two-part-rules)))

(defun hcp-model-rules (cp)
  (loop for i from 1 to 13 nconc
	(funcall (intern (format "hcp-rule-%d" i))
		 (hcp-model cp))))

(defun hcp-two-part-rules (cp)
  (nconc
   (hcp-model-rules cp)
   (loop for i from 14 to 28 nconc
	 (funcall (intern (format "hcp-rule-%d" i)) cp))))

(defun hcp-rule-1 (voice)
  "Pitch range about an octave."
  (let ((ambitus (v-ambitus voice)))
    (when (i> (apply #'i-new ambitus) (i-parse 'P8))
      (list (format "Rule 1 violation: Too big ambitus: %s" (p2-to-string ambitus))))))

(defun hcp-rule-2 (voice)
  "Length not less than seven notes."
  (unless (>= (v-length voice) 7)
    (list "Rule 2 violation: Length less than seven notes")))

(defun hcp-rule-3 (voice)
  "First and last tones the same."
  (let ((p1 (n-pitch (v-first-note voice)))
	(p2 (n-pitch (v-last-note voice))))
  (unless (p= p1 p2)
    (list (format "Rule 3 violation: Last pitch (%s) is not the same as first (%s)"
	    (p-to-string p2) (p-to-string p1))))))

(defun hcp-rule-4 (voice)
  "Pitch direction to be changed after four tones at the most.
I.e. only three consecutive melodic intervals in the same direction is accepted."
  (loop for g in (group (pairs (v-notes voice)) 
			:key #'(lambda (x) (i-direction (apply #'n-interval x))))
	if (> (length g) 3)
	collect (format "Rule 4 violation: From note %s, more than four tones in the same direction"
		  (n-to-string (first (first g))))))

(defun* hcp-rule-5 (voice &optional (model-p t))
  "Only certain tones may precede the final tone."
  (unless (i-is (v-last-interval voice)
		(if model-p 
		  '(M-2 m-2 M2 m2 M-3 m-3 P4 P-5)
		  '(M-2 m-2 M2 m2 M-3 m-3 P4 P-5 P5 P-4)))
    (list (format "Rule 5 violation: Illegal last interval (%s) in %s" 
	    (i-abbrevation (v-last-interval voice)) (if model-p "model" "2nd voice")))))

(defun hcp-rule-6 (voice)
  "No tone-repetition."
  (nconc 
   (loop for g in (group (v-notes voice) :test #'p= :key #'n-pitch)
	 if (> (length g) 1)
	 collect (format "Rule 6 violation: Note repetition after note %s"
		   (n-to-string (first g))))
   ;; auxilliary notes
   (let* ((n-zip (zip (v-notes voice))))
     (loop for n in (sort*  (nconc (loop for g in (group (first n-zip) :test #'p= :key #'n-pitch)
					 if (> (length g) 1) collect (first g))
				   (loop for g in (group (second n-zip) :test #'p= :key #'n-pitch)
					 if (> (length g) 1) collect (first g)))
			    #'< :key #'n-start-time)
	   collect (format "Rule 6 warning: 'note - auxillary note - note' formation from note %s" (n-to-string n))))))

(require 'set-chords)
(defun* hcp-illegal-melodic-triad-p (triad &optional (model-p t))
  "Helper function for `hcp-rule-7'. Major and minor triads, with
  inversions, augmented triads and every chord containing a
  tritone is considered illegal."
  (or (and model-p
	   (aif (find (sc-typename (chord-to-sc triad)) '(major-triad minor-triad augmented-triad))
	     (format "%S" it)))
      (let* ((intervals (mapcar #'(lambda (x) (apply #'i-new x)) (relations triad)))
	     (res (or (i-is 'A4 intervals #'i~) (i-is 'd5 intervals #'i~))))
	(if res (format "contains %s" (i-abbrevation res))))))
;;(hcp-illegal-melodic-triad-p (mapcar #'p-from-string '("E1" "F#" "C1")))

(defun* hcp-rule-7 (voice &optional (model-p t))
  "No broken chords."
  (nconc 
   (loop for n-triad in (tuples (v-notes voice) 3)
	 for p-triad = (mapcar #'n-pitch n-triad)
	 for res = (hcp-illegal-melodic-triad-p p-triad model-p)
	 when res
	 collect (format "Rule 7 violation: Illegal melodic triad (%s) after note %s: %s"
		   res (n-to-string (first n-triad)) (p-to-string p-triad)))
   (loop for 4n in (tuples (v-notes voice) 4)
	 for 4p = (mapcar #'n-pitch 4n)
	 for res = (or (hcp-illegal-melodic-triad-p (remove-nth 1 4p) model-p)
		       (hcp-illegal-melodic-triad-p (remove-nth 2 4p) model-p))
	 when res
	 collect (format "Rule 7 warning: Bad four pitch sequence (%s) after note %s: %s"
		   res (n-to-string (first 4n)) (p-to-string 4p)))))

(defun hcp-rule-8 (voice)
  "No sequences."
  (loop for l from 3 to (/ (v-length voice) 2) 
	nconc 
	(loop for i below l
	      for seq-candidates = (cut (nthcdr i (v-notes voice)) l)
	      for seqs = (group seq-candidates :test #'equal :key #'(lambda (x) (mapcar #'i-number (is-new (mapcar #'n-pitch x)))))
	      nconc 
	      (loop for seq in seqs
		    if (> (length seq) 1)
		    collect (format "Rule 8 violation: Sequence starting at note %s: %s" 
			      (n-to-string (first (first seq))) (maptree (compose #'p-to-string #'n-pitch) seq))))))

(defun* hcp-rule-9 (voice &optional (model-p t))
  "No skip larger than a fifth for model. Up to sixth plus the octave is allowed for 2nd voice."
  (loop for 2n in (pairs (v-notes voice))
	for hi = (i-new 2n)
	if (if model-p
	     (i> hi 'P5)
	     (and (i> hi 'M6) 
		  (i/= hi 'P8))) 
	collect (format "Rule 9 violation: Illegal melodic interval %s from note %s in %s" 
		  (i-abbrevation hi) (n-to-string (first 2n)) (if model-p "model" "2nd voice"))))

(defun* hcp-rule-10 (voice &optional (model-p t))
  "No more than one skip in the same direction."
  (loop for 22n in (pairs (pairs (v-notes voice)))
	for 2hi = (mapcar #'i-new 22n)
	for triad = (mapcar #'n-pitch (cons (first (first 22n)) (second 22n)))
	if (and model-p
		(apply #'eq (mapcar #'i-direction 2hi))
		(apply #'all-true (mapcar #'(lambda (hi) (> (abs (i-number hi)) 2)) 2hi)))
	collect (format "Rule 10 violation: In model, starting from note %s, there are two leaps in the same direction (a %s and a %s)" 
		  (n-to-string (first (first 22n))) (i-abbrevation (first 2hi)) (i-abbrevation (second 2hi)))
	;; for 2nd voice, two skips in the same direction are allowed only if they make up a legal triad
	if (and (not model-p)
		(apply #'eq (mapcar #'i-direction 2hi))
		(apply #'all-true (mapcar #'(lambda (hi) (> (abs (i-number hi)) 2)) 2hi))
		(not (hcp-illegal-melodic-triad-p triad nil)))
	collect (format "Rule 10 violation: In model, starting from note %s, there are two leaps in the same direction (a %s and a %s)" 
		  (n-to-string (first (first 22n))) (i-abbrevation (first 2hi)) (i-abbrevation (second 2hi)))))

(defun hcp-rule-11 (voice)
  "No scale-passages."
  (loop for g in (group (pairs (v-notes voice)) 
			:test #'(lambda (x y) (= 4 (* (i-number (i-new x))
						      (i-number (i-new y))))))
	if (> (length g) 2)
	collect (format "Rule 11 violation: Starting from note %s, more than two step of a second in the same direction" 
		  (n-to-string (first (first g))))))

(defun hcp-rule-12 (voice)
  "No augmented or dimished progressions."
  (loop for 2n in (pairs (v-notes voice))
	for hi = (i-new 2n)
	if (find (i-alteration-symbol hi) '(A d))
	collect (format "Rule 12 violation: Notes %s and %s creates an illegal %s interval (%s)" 
		  (n-to-string (first 2n)) (n-to-string (second 2n)) (i-alteration-name hi) (i-abbrevation hi))))

			 
(defun hcp-rule-13 (voice)
  "No chromaticism."
  (nconc 
   (loop for g in (group (pairs (v-notes voice))
			 :key (compose #'i-semitones #'i-new)
			 :test #'(lambda (s1 s2) (and (= s1 s2)
						      (= (abs s1) 1))))
	 if (> (length g) 1)
	 collect (format "Rule 13 violation: Two chromatic steps in the same direction from note %s"
		   (n-to-string (first (first g))))

	 else if (apply #'n-alteration-p (first g))
	 collect (format "Rule 13 violation: Note %s was chromatic alterated to %s"
		   (n-to-string (first (first g))) (n-to-string (second (first g)))))

   (loop for 3n in (tuples (v-notes voice) 3)
	 if (n-alteration-p (first 3n) (third 3n))
	 ;;The intervening note(s) are too few in order to release the chromatic tension, according to Hindemith.
	 collect (format "Rule 13 violation: In note sequence (%s) there is an illegal chromatic alteration between first and last note (%s to %s)"
		   (notes-to-string 3n) (p-to-string (n-pitch (first 3n))) (p-to-string (n-pitch (third 3n)))))))

(defun hcp-rule-14 (cp)
  (unless (apply #'= (mapcar #'v-length (cp-voices cp)))
    (list "Rule 14 violation: Model and 2nd voice are not of the same length")))

(defun hcp-rule-15 (cp)
  "Rules for the second voice"
  (let ((2nd (hcp-2nd cp)))
    (nconc 
     ;; These rules applies for the second voice also
     (loop for i in '(1 6 8 10 12 13) nconc
	   (funcall (intern (format "hcp-rule-%d" i)) 2nd))
     ;; Same as rule 7, but for the 2nd voice, broken major and minor triads are allowed
     (hcp-rule-7 2nd nil)
     ;; Some more freedom for melodic intervals
     (hcp-rule-9 2nd nil)
     ;; Some more freedom for skips in the same direction
     (hcp-rule-10 2nd nil)
     ;; Some more freedom for last melodic interval
     (hcp-rule-5 2nd nil)
     ;; First and last pitch need only to be the same if 2nd voice is the lower voice
     (when (v< 2nd (hcp-model cp))
       (hcp-rule-3 2nd)))))

(defun hcp-rule-16 (cp)
  "No crossing of voices. Assumes two voices."
  (loop for 22hn in (pairs (cp-chords cp))
	if (apply #'neq* (remove nil (mapcar #'i-direction (mapcar #'i-new 22hn))))
	collect (format "Rule 16 violation: Voices cross after chord %s" 
		  (notes-to-string (first 22hn)))))

(defun hcp-rule-17 (cp)
  "Pitch-distance determined by the voices used"
  nil)

(defun hcp-rule-18 (cp)
  "The first and the last interval must have their lower tones as roots"
  (nconc
   (unless (hcp-root-p (i-new (mapcar #'v-last-note (cp-voices cp))))
     (list "Rule 18 violation: First interval is not in root position"))
   (unless (hcp-root-p (i-new (mapcar #'v-last-note (cp-voices cp))))
     (list "Rule 18 violation: Last interval is not in root position"))))

(defun hcp-not-root-intervals-at (hcp notes)
  "Returns all intervals at NOTES which are not in root position.
Helper function for `hcp-rule-19'"
  (remove-if #'hcp-root-p 
	     (mapcar (bind #'voices-chord-at-note (cp-voices hcp) 1) notes)
	     :key #'i-new))

(defun hcp-rule-19 (cp)
  "Intervals with upper tone as root must be used carefully.
This version only warns when the upper tone is a root and one of
the voices is at a highest or lowest pitch point. Only warnings
are issued from this rule."
  (nconc
   (loop for ch in (hcp-not-root-intervals-at cp (v-pitch-minimum-notes (hcp-model cp)))
	 collect (format "Rule 19 warning: Interval %s at a lowest pitch point %s in model is not in root position"
		   (i-abbrevation (i-new ch)) (p-to-string (n-pitch (first ch)))))
   (loop for ch in (hcp-not-root-intervals-at cp (v-pitch-maximum-notes (hcp-model cp)))
	 collect (format "Rule 19 warning: Interval %s at a highest pitch point %s in model is not in root position"
		   (i-abbrevation (i-new ch)) (p-to-string (n-pitch (first ch)))))
   (loop for ch in (hcp-not-root-intervals-at cp (v-pitch-minimum-notes (hcp-2nd cp)))
	 collect (format "Rule 19 warning: Interval %s at a lowest pitch point %s in 2nd voice is not in root position"
		   (i-abbrevation (i-new ch)) (p-to-string (n-pitch (second ch)))))
   (loop for ch in (hcp-not-root-intervals-at cp (v-pitch-maximum-notes (hcp-2nd cp)))
	 collect (format "Rule 19 warning: Interval %s at a highest pitch point %s in 2nd voice is not in root position"
		   (i-abbrevation (i-new ch)) (p-to-string (n-pitch (second ch)))))))

(defun hcp-rule-20 (cp)
  "At a close, a skip in one voice requires a half-step or a whole-step progression in the other voice"
  (let ((2hi (mapcar #'v-last-interval (cp-voices cp))))
    (nconc (if (and (i-skip-p (first 2hi)) 
		     (not (i-step-p (second 2hi))))
	      (format "Rule 20 violation: Lower voice end in a skip, while higher voice does not end in a step"))
	    (if (and (i-skip-p (second 2hi)) 
		     (not (i-step-p (first 2hi))))
	      (format "Rule 20 violation: Higher voice end in a skip, while lower voice does not end in a step")))))

(defconst hcp-bad-chords
  '(major-triad-without-fifth
    minor-triad-without-fifth
    major-triad
    minor-triad
    diminished-triad
    augmented-triad
    dominant-seventh))

(defconst hcp-secondary-sevenths
  '(major-seventh
    minor-seventh
    dominant-seventh-flat-five
    diminished-seventh
    half-diminished-seventh
    augmented-seventh))


(defun hcp-rule-21-chord-p (sc &optional include-secondary-sevenths-p)
  "2-4 notes formations classified as chords by Hindemith"
  (find (sc-typename sc) 
	(if include-secondary-sevenths-p
	  (conc hcp-bad-chords hcp-secondary-sevenths)
	  hcp-bad-chords)))

(defun i-tritone-p (interval)
  (i-is interval '(A4 d5) #'i~))

(defun i-augmented-p (interval)
  (i-is interval '(A1 A2 A3 A4 A5 A6 A7) #'i~))

(defun hcp-rule-21-base (22vn sc-typename)
  (let* ((2vn1 (first 22vn))
	 (2vn2 (second 22vn)))
    (when sc-typename
      (nconc
       (let ((na (first 2vn1))
	     (nb (second 2vn2)))
	 (nconc 
	  (when (i-tritone-p (i-new na nb))
	    (list (format "Rule 21 violation: At start-times %s and %s, chord formation (%s) containing a tritone (%s-%s)"
		    (n-start-time na) (n-start-time nb) sc-typename (p-to-string (n-pitch na)) (p-to-string (n-pitch nb)))))
	  (when (i-augmented-p (i-new na nb))
	    (list (format "Rule 21 violation: At start-times %s and %s, chord formation (%s) containing an augmented ... (%s-%s)"
		    (n-start-time na) (n-start-time nb) sc-typename (p-to-string (n-pitch na)) (p-to-string (n-pitch nb)))))))
       (let ((na (second 2vn1))
	     (nb (first 2vn2)))
	 (nconc 
	  (when (i-tritone-p (i-new na nb))
	    (list (format "Rule 21 violation: At start-times %s and %s, chord formation (%s) containing a tritone (%s-%s)"
		    (n-start-time na) (n-start-time nb) sc-typename (p-to-string (n-pitch na)) (p-to-string (n-pitch nb)))))
	  (when (i-augmented-p (i-new na nb))
	    (list (format "Rule 21 violation: At start-times %s and %s, chord formation (%s) containing an augmented ... (%s-%s)"
		    (n-start-time na) (n-start-time nb) sc-typename (p-to-string (n-pitch na)) (p-to-string (n-pitch nb)))))))))))

(defun hcp-rule-21 (cp)
  "No chord-formations of tritone chords or augmented triads; no interchange of intervals"
  (let ((chords (cp-chords cp)))
    (nconc
     (loop for 22vn in (pairs (butlast chords)) 
	   for ch = (apply #'conc 22vn)
	   for sc = (n-chord-to-sc ch)
	   for sc-typename = (hcp-rule-21-chord-p sc t)
	   for res = (hcp-rule-21-base 22vn sc-typename)
	   if res nconc res
	   else if sc-typename collect
	   (format "Rule 21 warning: At start-times %s and %s, formation of chord %s"
	     (n-start-time (first (first 22vn))) (n-start-time (first (second 22vn))) sc-typename))
     (let* ((22vn (last chords 2))
	    (ch (apply #'conc 22vn))
	    (sc (n-chord-to-sc ch)))
       (aif (hcp-rule-21-chord-p sc nil)
	 (list (format "Rule 21 violation: Last two intervals illegally forms chord %s" it))
	 (aif (hcp-rule-21-chord-p sc t)
	   (list (format "Rule 21 warning: Last two intervals forms chord %s" it))))))))

(defun hcp-rule-22 (cp)
  "No simultaneous leaps in the same direction"
  (loop for 22vn in (copy-if #'(lambda (2hi) (and (apply #'all-true (mapcar #'i-skip-p 2hi))
							(apply #'eq (mapcar #'i-direction 2hi))))
			     (pairs (cp-chords cp)) 
			     :key #'(lambda (22vn) (mapcar #'i-new (transpose 22vn))))
	collect (format "Rule 22 violation: Simultaneous leaps in the same direction from chord %s"
		  (notes-to-string (first 22vn)))))

(defun voices-relations-to-other-voices-indexes (voices)
  (relations (0-n (length voices)) nil t))
;;(voices-relations-to-other-voices-indexes '((s1 s1) (a1 a2) (t1 t2) (b1 b2)))

(defun voices-relations-to-other-voices-notes (voices)
  (voices-relations-to-other-voices-indexes))

(defun hcp-cross-relation (chord1 chord2 format-string)
  "Helper function for hcp-rule-23: Returns error messages for
any cross relation in chord1 and chord2. The FORMAT-STRING must
have have following slots: (note1 voice-index1 note2
voice-index2)"
  (loop for indexes in (voices-relations-to-other-voices-indexes chord1)
	for 2n = (mapcar* #'nth indexes (list chord1 chord2))
	for 2pc = (mapcar (compose #'p-chrome #'n-pitch) 2n)
	if (apply #'n-alteration-p 2n)
	collect (format format-string 
		  (note-to-string (first 2n)) (first indexes)
		  (note-to-string (second 2n)) (second indexes))))

(defun hcp-rule-23 (cp)
  "No cross-relations"
  (nconc (loop for 2ch in (pairs (cp-chords cp)) nconc
	       (hcp-cross-relation (first 2ch) (second 2ch) "Rule 23 violation: Cross relation between note %s in voice %d and note %s in voice %d"))
	 (loop for 2ch in (remove-nth 1 (tuples (cp-chords cp) 3)) nconc
	       (hcp-cross-relation (first 2ch) (second 2ch) "Rule 23 violation: Cross relation (distant) between note %s in voice %d and note %s in voice %d"))))

(defun hcp-rule-24 (cp)
  "No unison, octave, fifth or fourth parallels"
  (loop for 22vn in (pairs (cp-chords cp))
	for 2vi = (mapcar #'i-new 22vn)
	if (and (apply #'i= 2vi)
		(i-is (first 2vi) '(P1 P4 P5) #'i~))
	collect (format "Rule 24 violation: Illegal parallel (%s) from chord %s"
		  (i-abbrevation (i-new (first 22vn))) (notes-to-string (first 22vn)))))

(defun hcp-rule-25 (cp)
  "Parallel major thirds or minor sixths only with half-step progressions"
  (loop for 22vn in (pairs (cp-chords cp))
	for 2vi = (mapcar #'i-new 22vn)
	if (and (apply #'i= 2vi)
		(i-is (first 2vi) '(M3 m6) #'i~)
		(= 1 (i-new (first (transpose 22vn)))))
	collect (format "Rule 25 violation: Parallel %s's from chord %s"
		  (i-abbrevation hi) (notes-to-string (first 22vn)))))

(defun hcp-rule-26 (cp)
  "No covered unisons or octaves"
  (loop for 22vn in (copy-if #'(lambda (22vn)
				 (and (i-is (i-new (second 22vn)) 'P1 #'i~)
				      (apply #'eq (mapcar #'i-direction (mapcar #'i-new (transpose 22vn))))))
			     (pairs (cp-chords cp)))
	collect (format "Rule 26 violation: Covered octave parallel chord %s"
		  (notes-to-string (first 22vn)))))

(defun hcp-rule-27 (cp)
  "No covered fifths or fourths from smaller intervals in an ascending direction"
  (loop for 22vn in (pairs (cp-chords cp))
	if (and (i-is (i-new (second 22vn)) '(P4 P5) #'i~)
		(eq :up (apply #'eq (mapcar #'i-direction (mapcar #'i-new (transpose 22vn)))))
		(apply #'i< (mapcar #'i-new 22vn)))
	collect (format "Rule 27 violation: Covered ascending fifth/fourth parallel from smaller interval at chord %s"
		  (notes-to-string (first 22vn)))))

(defun hcp-rule-28 (cp)
  "No simultaneous highest or lowest pitch points"
  (let ((high1 (v-pitch-maximum-notes (hcp-model cp)))
	(low1 (v-pitch-minimum-notes (hcp-model cp)))
	(high2 (v-pitch-maximum-notes (hcp-2nd cp)))
	(low2 (v-pitch-minimum-notes (hcp-2nd cp))))
    (nconc 
     (loop for n in (intersection high1 high2 :key #'n-start-time)
	   collect (format "Rule 28 warning: Highest pitch point note %s in model is simultaneous with a highest pitch point in 2nd voice" (n-to-string n)))
     (loop for n in (intersection low1 high2 :key #'n-start-time)
	   collect (format "Rule 28 warning: Lowest pitch point note %s in model is simultaneous with a highest pitch point in 2nd voice" (n-to-string n)))
     (loop for n in (intersection high1 low2 :key #'n-start-time)
	   collect (format "Rule 28 warning: Highest pitch point note %s in model is simultaneous with a lowest pitch point in 2nd voice" (n-to-string n)))
     (loop for n in (intersection low1 low2 :key #'n-start-time)
	   collect (format "Rule 28 warning: Lowest pitch point note %s in model is simultaneous with a lowest pitch point in 2nd voice" (n-to-string n))))))

(provide 'hindemith-cp)