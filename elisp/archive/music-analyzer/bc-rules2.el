;;; cp methods
(cl-defun cp-voice (cp voice-symbol)
  (cl-case voice-symbol
    (cf (first cp-voices))
    (cp (second cp-voices))
    (otherwise (error "Unknown voice tag %S" voice-symbol))))

(cl-defun cp-durations (cp voice-symbol)
  (mapcar #'n-duration (v-notes (cp-voice voice-symbol))))

(cl-defun cf-common-duration-value (cp)
  (apply #'equal* (cp-durations 'cf)))



(cl-defun pitch-repetition (cp)
  "Note repetition"
  (cl-loop for hppair in (cp-hppairs cp) 
	collect (cp-test (apply #'p= hppair))))

(cl-defun stemmekryss (cp)
  "Stemmekryss"
  (cl-loop for vppair in (vp-hppairs cp) 
	collect (cp-test (apply #'p> vppair))))

(cl-defun too-big-interval (cp max-interval)
  "Too big interval between voices"
  (cl-loop for vppair in (vp-hppairs cp) 
	collect (cp-test (i< (i-parse max-interval)
			     (apply #'i-new vppair)))))

(cl-defun too-big-simultaneous-leaps (hipair context maxmin-leap)
  "Checks that when both voices leaps in parallel, not both leaps must be bigger than a third.
Regel 1.6 hos Grinde."
  (let ((leaps (mapcar #'i-step hipair)))
    (when (and (apply #'eq* (mapcar #'cl-signum leaps)) ;same direction
	       (> (apply #'min* (mapcar #'abs leaps)) (first maxmin-leap))))
    "Too big simultaneous leaps"))


;;; General rules
(cl-defun pcs-in-mode (chromees voice-tag key)
  "No modulation is allowed. Standard alterations in modes are allowed. Also, last interval may always be a major third.
Note that this rule does not consider first and last note. A special rule should cover these cases."
  (cl-loop for pc in chromees
	for i from 1
	if (not (chrome-in-mode pc key))
	collect (format "In voice %S, at position %d: Pitch class %s is not accepted in key %s" 
		  voice-tag i (chrome-to-string chrome) (k-to-string key))))

(cl-defun cp-in-mode (cp)
  "No modulation is allowed. Standard alterations in modes are allowed. Also, last interval may always be a major third.
Note that this rule does not consider last note in the cp voice. A special rule should cover these cases."
  (append (pcs-in-mode (butlast (cp-pcs cp 'cp)) (cp-pcs cp 'cf))))

(cl-defun cp-legal-start-interval (cp)
  "Regel 1.1a hos Grinde"
  (unless (b1-legal-start-or-end-interval
	   (first (cp-vertical-intervals cp))
	   (cp-mode key)
	   nil
	   (cp-super-p cp))
    "Illegal start interval"))

(cl-defun cp-legal-end-interval (cp)
  "Regel 1.1b hos Grinde"
  (unless (b1-legal-start-or-end-interval
	   (first (last (cp-vertical-intervals cp)))
	   (cp-mode cp)
	   t
	   (cp-super-p cp))
    "Illegal end interval"))

;;; Check rests
(cl-defun 2nd-species-rests-cp (cp)
  (let ((cf-duration (cf-duration cp)))
    (if (not cf-duration)
      "Could not test cp rests because no entydig cf duration"
      (cl-loop for r in (cp-rests 'cp)
	    for st = (n-start-time r)
	    for d = (n-duration r)
	    if (not (zerop st))
	    collect (format "Illegal rest at start-time %d" st)
	    if (/= d (/ cf-duration 2))
	    collect (format "Illegal rest duration (%d) at start-time %d" d st)))))

(cl-defun rests-cf (cp)
  "Applies to all species"
  (when (cp-rests 'cf) ;should return nil
    ("Illegal rest in cantus firmus")))

(cl-defun 2nd-species-rests (cp)
  "Checks that there are no illegal rests in the second species counterpoint system CP.
Grinde 2.1.b."
  (append 2nd-species-rests-cp rests-cf))

;;; Check durations
(cl-defun duration-cf (cp)
  "Returns the common duration of each note in CP's cantus firmus."
  (unless (common-duration-value-cf cp)
      (list "All notes in cantus firmus must have the same duration")))

(cl-defun 2nd-species-duration-cp-last-note (cp)
  (let* ((d (n-duration (cp-final 'cp)))
	 (d-cf (common-duration-value-cf cp))
	 (d-cf/2 (/ d-cf 2)))
    (unless (or (= d d-cf) (= d d-cf/2))
      (list (format "Duration of last note in cp is %d. Expected either %d or %d" d d-cf d-cf/2)))))

(cl-defun 2nd-species-duration-cp-but-last-note (cp)
  "Checks that the durations are ok for the second species counterpoint voice"
  (cl-loop with d-legal = (/ (common-duration-value-cf cp) 2)
	for d in (butlast (cp-durations 'cp))
	for i from 1
	if (/= d d-legal)
	collect (format "Duration at position %d in cp is %d. Expected %d" i d d-legal)))

(cl-defun 2nd-species-duration-cp (cp)
  "Checks that the durations are ok for the second species counterpoint system CP"
  (append 2nd-species-duration-cp-but-last-note 2nd-species-duration-cp-last-note))

(cl-defun 2nd-species-duration (cp)
  "Checks that the durations are ok for the second species counterpoint system CP
Grinde 2.1.a."
  (append 2nd-species-duration-cp duration-cf))

;;; Check consonance rules

(cl-defun vi-dissonance (vertical-interval)
  (when (dissonance-p vertical-interval)
    (cp-report "Illegal dissonance" vertical-interval)))

(cl-defun vis-dissonances (vertical-intervals)
  (cl-loop for vi in vertical-intervals append (vi-dissonance vi)))

(cl-defun vi-unaccented-dissonance (vertical-interval)
 (when (and (dissonance-p vertical-interval)
	    (not (passing-note-p (vi-note vertical-interval 'cp))))
    (cp-report "Dissonance is not handled correctly" vertical-interval)))

(cl-defun vis-unaccented-dissonances (vertical-intervals)
  (cl-loop for vi in vertical-intervals append (vi-unaccented-dissonance vi)))

(cl-defun 2cp-dissonances (cp)
  (append (vis-dissonances (cp-vertical-intervals cp :filter '(accented)))
	  (vis-unaccented-dissonances (cp-vertical-intervals cp :filter '(unaccented)))))

