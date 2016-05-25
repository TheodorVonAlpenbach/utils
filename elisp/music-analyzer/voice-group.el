(require 'voice)

(defstruct (voice-group :named (:conc-name vg-))
  (name)
  (voices)) ;list of voices

(defun vg-test ()
  (mvt-voice-group (mvt-test)))

(defun vg-new (voices &optional name)
  (make-voice-group :voices voices :name name))
;;(vg-voices (vg-new '(adsf ølkj )))

(defun vg-copy (vg)
  "Overrides default copy since this can't copy deep in voice-groups"
  (vg-new (mapcar #'v-copy (vg-voices vg))
	  (vg-name vg)))

(defun voice-group= (vg1 vg2)
  (apply #'all-true (mapcar* #'voice= (vg-voices vg1) (vg-voices vg2))))

(defun voices-p (x)
  "Returns t iif X is a list of `voice' elements"
  (and (listp x) (every #'voice-p x)))

 (defun vg-time-signature (vg)
  "TODO: return a warning if not all voices in VG has same ts"
  (v-time-signature (first (vg-voices vg))))
;;(vg-time-signature (test-vg))

(defun vg-set-time-signature (vg time-signature)
  "TODO: return a warning if not all voices in VG has same ts"
  (loop for v in (vg-voices vg)
	do (setf (v-time-signature v) time-signature)))
;;(vg-set-time-signature (test-vg) (ts-new))

(defun vg-key (vg)
  "TODO: return a warning if not all voices in VG has same ts"
  (v-key (first (vg-voices vg))))
;;(vg-key (test-vg))

(defun vg-set-key (vg key)
  "TODO: return a warning if not all voices in VG has same ts"
  (loop for v in (vg-voices vg)
	do (setf (v-key v) key)))
;;(vg-set-key (test-vg) (ts-new))

(defun vg-upbeat (vg)
  "TODO: return a warning if not all voices in VG has same ts"
  (v-upbeat (first (vg-voices vg))))
;;(vg-upbeat (test-vg))

(defun vg-set-upbeat (vg upbeat)
  "TODO: return a warning if not all voices in VG has same ts"
  (loop for v in (vg-voices vg)
	do (setf (v-upbeat v) upbeat)))
;;(vg-set-upbeat (test-vg) (ts-new))

(defun* vg-deduce-upbeat (vg &optional (time-signature (vg-time-signature vg)))
  (v-deduce-upbeat (first (vg-voices vg)) time-signature))
;;(vg-deduce-upbeat (test-vg))

(defun vgs-find-named-vg (voice-groups name)
  (find name voice-groups :key #'vg-name :test #'equal))

(defun vg-start-time (voice-group)
  "Returns the start-time of the first note in VOICE-GROUP"
  (apply #'min (mapcar #'v-start-time (vg-voices voice-group))))

(defun vg-end-time (voice-group)
  "Returns the end-time of the last note in VOICE-GROUP"
  (apply #'max (mapcar #'v-end-time (vg-voices voice-group))))
;;(vg-end-time (test-vg))

(defun* vg-shift-start-time (voice-group &optional (shift (- (vg-start-time voice-group))))
  "Returns a copy of VOICE-GROUP where the start-time of all its notes has been reduced by SHIFT.
If SHIFT is nil the notes will be shifted so that the first note has start-time 0."
  (make-voice-group :voices (loop for v in (vg-voices voice-group)
				  collect (v-shift-start-time v shift))))
;;(vg-shift-positions (test-vg))

(defun vg-next-chord-position (voice-group mposition)
  (awhen (minimum (mapcar #'(lambda (v) (v-note-after v mposition t)) (vg-voices voice-group))
		  :test #'(lambda (x y) (if x (if y (< (n-start-time x) (n-start-time y)) x) y)))
    (n-start-time it)))
;;(vg-next-chord-position (test-vg) 40.7)
;;(vg-end-time (test-vg))

(defun vg-chord-positions (voice-group)
  (loop for mposition = (vg-next-chord-position voice-group -1) then (vg-next-chord-position voice-group mposition)
	while mposition collect mposition))
;;(vg-chord-positions (test-vg))

(defun voices-chord-at (voices mposition)
  (reverse (mapcar (bind #'v-note-at mposition) voices)))

(defun vg-chord-at (voice-group mposition)
  (voices-chord-at (vg-voices voice-group)))
;;(mapcar (bind #'vg-chord-at (test-vg) 1) (vg-chord-positions (test-vg)))

(defun voices-chord-at-note (voices note)
  (voices-chord-at voices (n-start-time note)))

(defun vg-chord-at-note (voice-group note)
  (voices-chord-at-note (vg-voices voice-group) note))

(defun vg-chords (voice-group)
  (mapcar (bind #'vg-chord-at voice-group 1)
	  (vg-chord-positions voice-group)))
;;(vg-chords (test-vg))

(defun* vg-section (voice-group begin-pos end-pos)
  "Returns a copy of VOICE-GROUP excluding the notes with start-time outside the interval [BEGIN-POS END-POS).
Non-destructive."
  (make-voice-group :voices (loop for v in (vg-voices voice-group)
				  collect (v-section v begin-pos end-pos))))

(defun* vg-equal (vg1 vg2)
  "Is this necessary, or will equal handle this?"
  )

(defun vg-delete-notes-in-interval (voice-group begin-pos end-pos)
  "Deletes measures from BEGIN-POS to and including END-POS in VOICE-GROUP.
Destructive."
  (vg-copy voice-group :voices (loop for v in-ref (vg-voices voice-group)
				     collect (v-delete-notes-in-interval v begin-pos end-pos))))
;;(vg-delete-notes-in-interval (vg-copy (test-vg)) 1 8)
;;(vg-delete-notes-in-interval qvg 1 40)
;;(setq qvg (vg-copy (test-vg)))

(defun vg-remove-notes-in-interval (voice-group begin-pos end-pos)
  "Deletes measures from BEGIN-POS to and including END-POS in VOICE-GROUP.
Destructive."
  (make-voice-group :voices (loop for v in (vg-voices voice-group)
				  collect (v-remove-notes-in-interval v begin-pos end-pos))))
;;(vg-to-string (vg-remove-notes-in-interval (test-vg) 1 50) 'lilypond)

(defun vg-split-notes (voice-group d-split-functions)
  "Splits a note into a row of similar but tied notes
such the sum of the durations of the resulting notes equals the duration of the input note."
  (loop for v in (vg-voices voice-group)
	do (v-split-notes* v d-split-functions)))

;;; read/write
(defun* voices-to-string (voices &optional (print-style mu-default-print-style))
  (case print-style
    (otherwise (concat* (mapcar (bind #'v-to-string print-style) voices)
			:in "\n"))))

(defun* vg-to-string (vg &optional (print-style mu-default-print-style))
  (concat (vg-name vg) "\n" (voices-to-string (vg-voices vg) print-style)))

(provide 'voice-group)
