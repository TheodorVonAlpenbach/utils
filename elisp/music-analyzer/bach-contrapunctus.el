(require 'lilypond-parser)
(require 'movement)

;;; In voices, the first element (voice) is supposed to be the upper etc
(lexical-let ((mats-vgs nil)
	      (default-filename "c:/Documents and Settings/matsb/My Documents/projects/UiO/Var-2012/2270-Satslaere2B/ov-Grinde-bcp1.ly"))
  (defun test-vgs (&optional reset filename)
    (when (or (not mats-vgs) 
	      reset
	      filename)
      (setq mats-vgs (ly-parse-file (or filename default-filename)))
      (loop for vg in mats-vgs
	    for vs = (vg-voices vg)
	    do (progn
		 (setf (v-instrument (first vs)) "super")
		 (setf (v-instrument (second vs)) "c.f.")
		 (setf (v-instrument (third vs)) "sub"))))
    mats-vgs))
;(test-vgs t)
;(test-vgs)

(defstruct (cp-context :named (:conc-name cchrome-))
  key
  time-signature
  super-p
  voices
  pitches)

(defun cchrome-new (key time-signature super-p)
  (make-cp-context :key key :time-signature time-signature :super-p super-p))
;;(cchrome-new nil nil nil)

;;; utils
(require 'cp-utils)

;;; rule callers
(defun apply-voices-rule (voices rule context args)
  "Checks MOVEMENT against RULE and returns a list of violations"
  (funcall rule voices context))

(defun apply-notes-rule (voices rule context args)
  "Checks MOVEMENT against RULE and returns a list of violations"
  (loop for v in voices
	for res = (funcall rule (v-notes v) context args)
	if res collect (format "In voice %s: %s" (v-instrument v) res)))

(defun apply-note-rule (voices rule context args)
  "Checks MOVEMENT against RULE and returns a list of violations"
  (loop for v in voices
	append (loop for n in (v-notes v)
		     for res = (funcall rule n context args)
		     if res collect (format "In voice '%s', note %s: %s"
				      (v-instrument v) (n-to-string n) res))))

(defun apply-2notes-rule (voices rule context args)
  "Checks MOVEMENT against RULE and returns a list of violations"
  (loop for v in voices
	append (loop for hnp in (pairs (v-notes v))
		     for res = (funcall rule (first hnp) (second hnp) context args)
		     if res collect (format "In voice '%s', note %s: %s"
				      (v-instrument v) (n-to-string (first hnp)) res))))

(defun apply-pitch-rule (voices rule context args)
  ""
  (loop for v in voices
	append (loop for n in (v-notes v)
		     for res = (funcall rule (n-pitch n) context args)
		     if res collect (format "In voice '%s', note %s: %s"
				      (v-instrument v) (n-to-string n) res))))

(defun apply-2pitches-rule (voices rule context args)
  ""
  (loop for v in voices
	append (loop for hnp in (v-notes v)
		     for hpp in (mapcar #'note-pair-to-pitch-pair hnp)
		     for res = (funcall rule (first hpp) (second hpp) context args)
		     if res collect (format "In voice '%s', note %s: %s"
				      (v-instrument v) (n-to-string (first hnp)) res))))

(defun apply-pitch-pairs-rule (voices rule context args)
  ""
  (loop for vp in (relations voices)
	for pps = (mapcar #'note-pair-to-pitch-pair (apply #'2voices-to-note-pairs vp))
	for res = (funcall rule pps context args)
	if res collect (format "In voices %S: %s" (mapcar #'v-instrument vp) res)))

(defun apply-2pitch-pairs-rule (voices rule context args)
  ""
  (loop for vp in (relations voices)
	append (loop for 2np in (pairs (apply #'2voices-to-note-pairs vp))
		     for pp1 = (note-pair-to-pitch-pair (first 2np))
		     for pp2 = (note-pair-to-pitch-pair (second 2np))
		     for i from 1
		     for res = (funcall rule pp1 pp2 context args)
		     if res collect (format "In voices %S at position %d: %s" 
				      (mapcar #'v-instrument vp)
				      i
				      res))))

(defun apply-pitch-rule (voices rule context args)
  ""
  (loop for v in voices
	append (loop for n in (v-notes v)
		     for res = (funcall rule (n-pitch n) context args)
		     if res collect (format "In voice '%s', note %s: %s"
				      (v-instrument v) (n-to-string n) res))))

(defun apply-chrome-rule (voices rule context args)
  ""
  (loop for v in voices
	append (loop for n in (v-notes v)
		     for res = (funcall rule (n-chrome n) context args)
		     if res collect (format "In voice '%s', note %s: %s"
				      (v-instrument v) (n-to-string n) res))))

(defun apply-chrome-pairs-rule (voices rule context args)
  ""
  (loop for vp in (relations voices)
	for pcps = (mapcar #'note-pair-to-chrome-pair (apply #'2voices-to-note-pairs vp))
	for res = (funcall rule pcps context args)
	if res collect (format "In voices %S: %s" (mapcar #'v-instrument vp) res)))

(defun apply-chrome-pair-rule (voices rule context args)
  ""
  (loop for vp in (relations voices)
	for pcps = (mapcar #'note-pair-to-chrome-pair (apply #'2voices-to-note-pairs vp))
	append (loop for pcp in pcps
		     for res = (funcall rule pcp context args)
		     if res collect (format "In voices %S: %s" (mapcar #'v-instrument vp) res))))
      
(defun apply-vertical-intervals-rule (voices rule context args)
  "Works only in 1st specices counterpoint"
  (loop for vp in (relations voices)
	for vis = (apply #'2voices-to-vertical-intervals vp)
	for res = (funcall rule vis context args)
	if res collect (format "In voices %S: %s" (mapcar #'v-instrument vp) res)))

(defun apply-vertical-interval-rule (voices rule context args)
  "Works only in 1st specices counterpoint"
  (loop for vp in (relations voices)
	for vis = (apply #'2voices-to-vertical-intervals vp)
	append (loop for vi in vis
		     for i from 1
		     for res = (funcall rule vi context args)
		     if res collect (format "In voices %S at position %d: %s" 
				      (mapcar #'v-instrument vp) i res))))

(defun apply-horizontal-interval-pair-rule (voices rule context args)
  (loop for vp in (relations voices)
	append (loop for hi1 in (voice-to-horizontal-intervals (first vp))
		     for hi2 in (voice-to-horizontal-intervals (second vp))
		     for n from 1
		     for res = (funcall rule (list hi1 hi2) context args)
		     if res collect (format "In voices %S, for horizontal interval pair at position %d (%s and %s): %s" 
				      (mapcar #'v-instrument vp)
				      n (i-abbrevation hi1) (i-abbrevation hi2)
				      res))))

(defun rule-type (rule)
  (first (split-string (symbol-name rule) "-")))
;;(rule-type #'2notes-repetition)

(defun vs-apply-rule (voices rule context)
  (let* ((rule-fn (if (listp rule) (first rule) rule))
	 (rule-args (if (listp rule) (rest rule) nil)))
    (string-case (rule-type rule-fn)
		     ("voices" (apply-voices-rule voices rule-fn context rule-args))
		     ("ns" (apply-notes-rule voices rule-fn context rule-args))
		     ("n" (apply-note-rule voices rule-fn context rule-args))
		     ("2n" (apply-2notes-rule voices rule-fn context rule-args))
		     ("p" (apply-pitch-rule voices rule-fn context rule-args))
		     ("2p" (apply-2pitches-rule voices rule-fn context rule-args))
		     ("pps" (apply-pitch-pairs-rule voices rule-fn context rule-args))
		     ("2pp" (apply-2pitch-pairs-rule voices rule-fn context rule-args))
		     ("pc" (apply-chrome-rule voices rule-fn context rule-args))
		     ("pcp" (apply-chrome-pair-rule voices rule-fn context rule-args))
		     ("vis" (apply-vertical-intervals-rule voices rule-fn context rule-args))
		    ("vi" (apply-vertical-interval-rule voices rule-fn context rule-args))
		    ("hip" (apply-horizontal-interval-pair-rule voices rule-fn context rule-args)))))

(defun vs-apply-rules (voices rules context)
  (loop for r in rules
	for res = (vs-apply-rule voices r context)
	if res collect res))

(defun vg-apply-rules (voice-group rules)
  "voices must be a list of voices as follows (super-cp cf sub-cp)"
  (let* ((voices (vg-voices voice-group))
	 (super (find "super" voices :key #'v-instrument :test #'equal))
	 (cantus-firmus (find "c.f." voices :key #'v-instrument :test #'equal))
	 (sub (find "sub" voices :key #'v-instrument :test #'equal))
	 (cp-super (and super (list cantus-firmus super)))
	 (cp-sub (and sub (list sub cantus-firmus)))
	 (key (v-key cantus-firmus))
	 (time-signature (v-time-signature cantus-firmus))
	 (context (cchrome-new key time-signature nil)))
    (unless cantus-firmus
      (error "Cantus firmus is not provided!"))
    (list
     (when super 
       (setf (cchrome-super-p context) t)
       (cons 'super (vs-apply-rules cp-super rules context)))
     (when sub 
       (setf (cchrome-super-p context) nil)
       (cons 'sub (vs-apply-rules cp-sub rules context))))))
;;(vg-apply-rules (first (test-vgs nil)) b1-rules)

(defun vgs-apply-rules (voice-groups rules)
  (loop for vg in voice-groups
	collect (cons (vg-name vg) (vg-apply-rules vg rules))))
;;(vgs-apply-rules (test-vgs nil) b1-rules)

(defun mvt-apply-rules (movement rules)
  (vs-apply-rules (vg-voices (first (mvt-voice-groups movement))) rules))

(provide 'bach-contrapunctus)
