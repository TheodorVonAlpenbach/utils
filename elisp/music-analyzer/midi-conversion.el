;;;; Simple to and from MIDI converter
;;;; Uses the emidi package
(require 'midi-file)
(require 'movement)
(require 'mu-globals)
(require 'interval)

(defun me-find-next-note-start (events)
  ""
  (member-if #'(lambda (e)
		 (and (eq (me-subsubtype e) 'note-on)
		      (> (note-on-velocity (me-subsubevent e)) 0)))
	     events))
;;(me-find-next-note-start (nthcdr 6 (test-events)))

(defun me-find-matching-note-end (note-on events)
  (member-if #'(lambda (e)
		 (or (and (eq (me-subsubtype e) 'note-off)
			  (string-equal 
			   (note-off-pitch (me-subsubevent e))
			   (note-on-pitch note-on)))
		     (and (eq (me-subsubtype e) 'note-on)
			  (= (note-on-velocity (me-subsubevent e)) 0))))
	     events))
;;(let ((events (me-find-next-note-start (nthcdr 4 (test-events))))) (list (first events) (first (me-find-matching-note-end (me-subsubevent (first events)) (rest events)))))

(defun me-find-notes (events)
  (loop for result-list = (me-find-next-note-start events) then (me-find-next-note-start result-list)
	for note-start = (first result-list)
	for result-list = (rest result-list)
	while note-start
	collect (list note-start
		      (first (me-find-matching-note-end (me-subsubevent note-start) result-list)))))
;;(prin1 (me-find-notes (test-events)))

(defun parse-ansi-pitch (x))

(defun create-note-from-midi-note (midi-note)
  (make-note
   :pitch (p-from-string (note-on-pitch (me-subsubevent (first midi-note))))
   :start-time (me-delta-time (first midi-note))
   :duration (d-new (- (me-delta-time (second midi-note))
		       (me-delta-time (first midi-note))))))

(defun midi-track-to-voice (mt)
  "Converts a MIDI track to a VOICE object.
TODO include instrument."
  (make-voice :notes (mapcar #'create-note-from-midi-note
			     (me-find-notes (mt-events mt)))))

(defun mode-root (midi-accidentals mode)
  (chrome-transpose-iv (make-chrome) 'perfect-fifth
		   (if (eq mode 'major)
		     midi-accidentals
		     (+ midi-accidentals 3))))
;;(chrome-to-string (mode-root 0 'minor))

(defun create-key-from-accidentals (midi-accidentals mode)
  (make-key
   :root (mode-root midi-accidentals mode)
   :mode mode))

(defun create-key-from-midi-key (ks)
  (create-key-from-accidentals (key-signature-accidentals ks) (key-signature-mode ks)))

(defun create-time-from-midi-key (ks)
  (create-key-from-accidentals (key-signature-accidentals ks) (key-signature-mode ks)))

(defun n-modify-to-orignal-pitch (n key)
  (let ((key-pitch (find (n-set-chrome n) 
			 (flatten (k-scale key))
			 :key #'spc-to-chrome)))
    (when key-pitch
      (setf (p-chrome (n-pitch n)) key-pitch))))
;;(n-modify-to-orignal-pitch (first (v-notes (nth 0 (vg-voices (mvt-voice-group mats))))) (make-key :mode 'major))

(defmacro v-do-notes (voice n &rest body)
  `(loop for x in (v-notes ,voice)
	 do (setf ,n x)
	 do (progn ,@body)))
;;(v-do-notes (nth 0 (vg-voices (mvt-voice-group mats))) n (n-to-string n 'lilypond))

(defmacro vg-do-notes (voice-group n &rest body)
  `(loop for v in (vg-voices ,voice-group)
	 do (v-do-notes v n ,@body)))
;;(vg-do-notes (mvt-voice-group mats) n (insert (n-to-string n 'lilypond)))

(defmacro mvt-do-notes (mvt n &rest body)
  `(vg-do-notes (mvt-voice-group ,mvt) n ,@body))
;;(mvt-do-notes mats n (insert (n-to-string n 'lilypond)))

(defun midi-file-to-movement (midi-file)
  "Parses MIDI-FILE into an MA movement object. 
The parser is simplified and is applying the following assumptions:
* only one simple voice per track (i.e. only one frequency at a time)
  except the first track, which should contain time signature, key and tempo
* only one instrument per track
* notes are ended with note-off events (not note-on set to 0 velocity
* constant time signature, key and tempo
* ignores dynamics
DOES: 
o split notes with clusy duration into several tied notes:
  E.g. 1.25 ~ 4 + 16
TODO:
* split notes that breaks bar lines
* handle rests"
  (let* ((mf (if (stringp midi-file) (read-midi-file midi-file) midi-file))
	 (td (td-value (mf-time-division mf)))
	 (meta-track (first (mf-tracks mf)))
	 (me-key-signature (mt-find-first-event meta-track 'key-signature))
	 (me-time-signature (mt-find-first-event meta-track 'midi-time-signature))
	 (me-set-tempo (mt-find-first-event meta-track 'set-tempo))
	 (voices (mapcar #'midi-track-to-voice
			 (mapcar (bind #'midi-track-with-absolute-times (/ 1.0 td))
				 (rest (mf-tracks mf)))))
	 (voice-group (make-voice-group :voices voices))
	 (time-signature (if me-time-signature 
			   (make-time-signature 
			    :length (midi-time-signature-numerator (me-subsubevent me-time-signature))
			    :unit (expt 2 (midi-time-signature-denominator (me-subsubevent me-time-signature))))))
	 (key nil
;;	  (if me-key-signature (create-key-from-midi-key (me-subsubevent me-key-signature)))
	  )
	 (upbeat (vg-deduce-upbeat voice-group time-signature)))

    ;;pitch-filter TODO: this one is very simple; a more thorogh one
    ;;based on functional anaylysis should be provided
    (when key
      (vg-do-notes voice-group n (n-modify-to-orignal-pitch n key)))

    (when upbeat
      ;; shift notes again so first start time is 0
      (setf voice-group (vg-shift-start-time voice-group)))

    ;;duration filter (with a setf method)
    (vg-split-notes voice-group (list (bind #'d-split-illegal-duration (mapcar #'first duration-map-lilypond))
				      (bind #'d-split-at-constant 4))) ;;TODO: this should be adapted to time-signature

    (let ((mvt (make-movement
		:tempo (if me-set-tempo (set-tempo-to-bpm (me-subsubevent me-set-tempo)))
		:voice-groups (list voice-group))))
      (mvt-set-time-signature mvt time-signature)
      (mvt-set-key mvt key)
      (mvt-set-upbeat mvt upbeat)
      ;;spot repetitions: note: only whole measures (justified for upbeat) may be repeated
      ;;optionally a minium excerpt should be specified (avoiding ridiculusly short repeats)
      ;;DEBUG THIS
      (setf debug-mvt (copy-movement mvt))
      ;;(setf (mvt-repeats debug-mvt) (mvt-find-maximum-repeat-span mvt)) this works
      (setf (mvt-repeats debug-mvt)
	    (list (mvt-find-maximum-repeat-span debug-mvt))) ;; TODO: try to find several
      (when (mvt-repeats debug-mvt)
	(setf debug-mvt
	      (loop for repeat in (mvt-repeats debug-mvt)
		    do (apply #'mvt-remove-measures debug-mvt repeat)
		    finally return debug-mvt)))
      debug-mvt)))
;;(mvt-test)
;;(midi-file-to-movement (test-midi))
;;(midi-file-to-movement (test-midi 1))

(provide 'midi-conversion)
