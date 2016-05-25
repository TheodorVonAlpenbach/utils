(require 'key)
(require 'bc-utils)

(defconst b1-rules
  (list   
   ;; General constraints
   #'pcs-in-mode
   #'n-whole
   ;; Regel 1
   #'vis-start
   #'vis-end
   ;; Regel 2
   #'vi-dissonance
   (list #'vis-imperfect-consonance-distribution .8)
   #'2pp-hidden-parallels
   #'pp-unison
   #'pcp-leading-tone-doubling
   ;; Regel 3
   #'pp-illegal-parallels
   ;; Regel 4
   (list #'2p-too-big-interval 'P15)
   #'2p-not-dim-nor-aug
   ;; Regel 5
   (list #'pps-too-long-parallel-sequence 3)
   ;; Regel 6
   (list #'hip-too-big-simultaneous-leaps 3)
   ;; Regel 7
   #'pp-too-big-interval
   ;; Regel 8
   #'pp-stemmekryss
   #'2p-repetition)
  "First species rules Bach style")

;;; General constraints in BC1
(defun pcs-in-mode (chromees context args)
  "No modulation is allowed. Standard alterations in modes are allowed. Also, last interval may always be a major third.
Note that this rule does not consider first and last note. A special rule should cover these cases."
  (loop for pc in (rest (butlast chromees))
	for i from 1
	if (not (chrome-in-mode pc (cchrome-key context)))
	collect (format "Pitch class %s (at position %d) is not accepted in key %s" 
		  (chrome-to-string chrome) i (k-to-string (cchrome-key context)))))

;;; General constraints in BC1
(defun n-whole (note context args)
  "All notes should be whole notes."
  (when (/= (n-dtime note) 4)
    "Note a whole note"))

;;; Grinde rules
(defun vis-start (intervals context args)
  "Regel 1.1a hos Grinde"
  (unless (b1-legal-start-or-end-interval
	   (first intervals)
	   (k-mode (cchrome-key context))
	   nil
	   (cchrome-super-p context))
    "Illegal start interval"))

(defun vis-end (intervals context args)
  "Regel 1.1b hos Grinde"
  (unless (b1-legal-start-or-end-interval
	   (first (last intervals))
	   (k-mode (cchrome-key context))
	   t
	   (cchrome-super-p context))
    "Illegal end interval"))

(defun vi-dissonance (vinterval context args)
  "Regel 1.2a hos Grinde"
  (unless (or (eq (i-direction vinterval) :down)
	      (i-is vinterval '(P1 m3 M3 P5 m6 M6) #'i~))
    "Dissonance"))

(defun vis-imperfect-consonance-distribution (vintervals context lowest-percentage)
  "Checks thate the ratio number-ofimperfect-consonance/number-of-intervals is above lowest-percentage.
Perfect consonances in first and last interval are not counted.
Regel 1.2b hos Grinde (en presisering av regel 2a)."
  (let* ((bad-positions (loop for vi in (rest (butlast vintervals 1))
			      for i from 1
			      if (not (i-imperfect-consonance-p vi))
			      collect i))
	 (imperfect-percentage (- 1 (/ (length bad-positions)
				       (* 1.0 (length vintervals))))))
    (when (< imperfect-percentage (first lowest-percentage))
      (format "Too few imperfect dissonances (%d%%) . Anomality positions are %s" 
	(round (* imperfect-percentage 100))
	(concat* bad-positions :in ", " :key #'number-to-string)))))

(defun 2pp-hidden-parallels (vppair1 vppair2 context args)
  "Checks if the vertical pitch pairs VPPAIR1 and VPPAIR2 are illegal parallels.
Regel 1.2c hos Grinde."
    (let ((i1 (apply #'i-new vppair1))
	  (i2 (apply #'i-new vppair2)))
      (and (i-is i2 '(P1 P5) #'i~)
	   (vppair2-same-direction vppair1 vppair2)
	   "Hidden parallels")))
;;(vppair2-hidden-parallels (list (p-new) (p-new (chrome-new 4))) (list (p-new (chrome-new 1)) (p-new (chrome-new 5))) nil)

(defun pps-unison (vppairs context args)
  "Returns nil iff there is no unison intervals in the vertical pitch pairs VPPAIRS, except for the first and last pair.
Regel 1.2d hos Grinde."
  (let ((res (loop for vppair in (rest (butlast vppairs 1))
		   for i from 1
		   if (apply #'p= vppair)
		   collect i)))
    (when res
      (format "Unisons on note %s" (concat* res :in ", " :key #'number-to-string)))))
;;(vppairs-unison (list (list (p-new) (p-new)) (list (p-new) (p-new)) (list (p-new) (p-new (chrome-new 1))) (list (p-new) (p-new)) (list (p-new) (p-new))) nil)

(defun pcp-leading-tone-doubling (vpcpair context args)
  "Returns nil iff there is no leading note doubling in the vertical pitch class pair VPCPAIR.
Assumes only two notes in INTERVAL.
Regel 1.2e hos Grinde."
  (when (apply #'eq* (k-leading-tone (cchrome-key context)) vpcpair)
    "Leading note doubling"))

(defun 2pp-illegal-parallels (vppair1 vppair2 context args)
  "Checks if the vertical pitch pairs VPPAIR1 and VPPAIR2 are illegal parallels.
Regel 1.3 hos Grinde."
  (when (i-is (vppair2-parallel-motion vppair1 vppair2)
	      '(P1 P5) #'i~)
     "Illegal parallels"))
;;(vppair2-illegal-parallels (list (p-new) (p-new (chrome-new 4))) (list (p-new (chrome-new 1)) (p-new (chrome-new 5))) nil)

(defun 2p-too-big-interval (pitch1 pitch2 context args)
  "Checks that the interval between PITCH1 and PITCH2 does not exceed an octave.
Regel 1.4a hos Grinde."
  (when (i< (i-parse 'P8) (i-new pitch1 pitch2))
    "Too big voice leap"))

(defun 2p-not-dim-nor-aug (pitch1 pitch2 context args)
  "Checks that the interval between PITCH1 and PITCH2 does not exceed an octave.
Regel 1.4b hos Grinde."
  (case (find (i-alteration (i-new pitch1 pitch2)) '(aug dim))
    (aug "Melodic interval is augmented")
    (dim "Melodic interval is diminished")))
;;(2p-not-dim-nor-aug (p-new) (p-new (chrome-new 3 1)) nil)

(defun pps-too-long-parallel-sequence (vppairs context max-parallel-sequence-number)
  "Checks if there are sequences of parallel VPPAIRS that exceeds MAX-PARALLEL-SEQUENCE-NUMBER.
Regel 1.5 hos Grinde."
  (loop for i from 1
	for l = (vppairs-parallel-sequence-length vppairs)
	while vppairs
	if (and l (> l (first max-parallel-sequence-number)))
	collect (format "Too long parallel sequence (%d) from note %d" l i)
	and do (setf vppairs (nthcdr (max 1 l) vppairs))
	else do (setf vppairs (rest vppairs))))

(defun hip-too-big-simultaneous-leaps (hipair context maxmin-leap)
  "Checks that when both voices leaps in parallel, not both leaps must be bigger than a third.
Regel 1.6 hos Grinde."
  (let ((leaps (mapcar #'i-step hipair)))
    (when (and (apply #'eq* (mapcar #'signum leaps)) ;same direction
	       (> (apply #'min* (mapcar #'abs leaps)) (first maxmin-leap)))
      "Too big simultaneous leaps")))

(defun pp-too-big-interval (vppair context max-interval)
  "Checks that the interval of vppair does not exceed MAX-INTERVAL.
Regel 1.7 hos Grinde."
  (when (i< (i-parse max-interval) (apply #'i-new vppair))
    "Too big interval between voices"))

(defun pp-stemmekryss (vppair context args)
  "Verifies that vertical pitch pair VPPAIR is not causing stemmekryss.
Regel 1.8a hos Grinde."
  (unless (apply #'p<= vppair) "Stemmekryss"))

(defun 2p-repetition (pitch1 pitch2 context args)
  "Verifies that PITCH1 and PITCH2 are different.
Regel 1.8b hos Grinde."
  (when (p= pitch1 pitch2) "Note repetition"))

(provide 'bc-rules1)
