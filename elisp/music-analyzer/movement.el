(require 'voice-group)
(require 'key)
(require 'time-signature)

;TODO? voice-groups -> voice-group
(cl-defstruct (movement :named (:conc-name mvt-))
  (voice-groups)
  (repeats) ;list of repeats
  (tempo))

(cl-defun vgs-p (x)
  "Returns t iif X is a list of `voice-group' elements"
  (and (listp x) (every #'voice-group-p x)))

(cl-defun mvt-time-signature (mvt)
  (vg-time-signature (mvt-voice-group mvt)))

(cl-defun mvt-set-time-signature (mvt time-signature)
  (vg-set-time-signature (mvt-voice-group mvt) time-signature))

(cl-defun mvt-upbeat (mvt)
  (vg-upbeat (mvt-voice-group mvt)))

(cl-defun mvt-set-upbeat (mvt upbeat)
  (vg-set-upbeat (mvt-voice-group mvt) upbeat))

(cl-defun mvt-key (mvt)
  (vg-key (mvt-voice-group mvt)))

(cl-defun mvt-set-key (mvt key)
  (vg-set-key (mvt-voice-group mvt) key))

(cl-defun mvt-new (&optional voice-groups repeats tempo)
  (make-movement :voice-groups voice-groups :repeats repeats :tempo tempo))

(cl-defun mvt-voice-group (movement)
  "Short-cut for first VG (only using one vg in this version anyway)"
  (first (mvt-voice-groups movement)))


(cl-defun mvt-copy (movement &key 
			   (voice-groups (mvt-voice-groups movement))
			   (repeats (mvt-repeats movement))
			   (tempo (mvt-tempo movement)))
  "Doesn't work, it seems"
  (mvt-new (mapcar #'vg-copy voice-groups)
	   (copy-tree repeats)
	   tempo))
;;(mvt-copy (mvt-test 1)) 

(cl-defun movement= (mvt1 mvt2)
  (and (equal (mvt-tempo mvt1) (mvt-tempo mvt2))
       (equal (mvt-time-signature mvt1) (mvt-time-signature mvt2))
       (equal (mvt-upbeat mvt1) (mvt-upbeat mvt2))
       (equal (mvt-key mvt1) (mvt-key mvt2))
       (voice-group= (mvt-voice-group mvt1) (mvt-voice-group mvt2))
       (equal (mvt-repeats mvt1) (mvt-repeats mvt2))))

(cl-defun mvt-deduce-upbeat (mvt)
  (vg-deduce-upbeat (mvt-voice-group mvt)))
;;(mvt-deduce-upbeat (mvt-test))

(cl-defun upbeat-to-string (u &optional (print-style mu-default-print-style))
    (error "%S not implemented"))

(cl-defun mvt-to-string (mvt &optional (print-style mu-default-print-style))
  (cl-case print-style
    ((english lilypond) (format "%S %s %S %s\n%s\n%S" 
			  (mvt-tempo mvt)
			  (ts-to-string (mvt-time-signature mvt) print-style)
			  (mvt-upbeat mvt)
			  (k-to-string (mvt-key mvt))
			  (vg-to-string (mvt-voice-group mvt) print-style)
			  (mvt-repeats mvt)))
    (otherwise (error "Not implemented!"))))


;;; queries
(cl-defun mvt-start-time (movement)
   "Returns the start-time of the first note in MOVEMENT"
  (vg-start-time (mvt-voice-group movement)))

(cl-defun mvt-end-time (movement)
   "Returns the end-time of the last note in MOVEMENT"
  (vg-end-time (mvt-voice-group movement)))
;;(mvt-end-time (mvt-test))

(cl-defun mvt-last-bar (movement)
  "Returns the last bar (a number) in MOVEMENT."
  (/ (mvt-end-time movement)
     (ts-length (mvt-time-signature movement))))
;;(mvt-last-bar (mvt-test))

(cl-defun mvt-shift-start-time (movement &optional (shift (- (mvt-start-time movement))))
  "Returns a copy of MOVEMENT where the start-time of all its notes has been reduced by SHIFT.
If SHIFT is nil the notes will be shifted so that the first note has start-time 0."
  (mvt-copy movement
	    :voice-groups (cl-loop for vg in (mvt-voice-groups movement)
				collect (vg-shift-start-time vg shift))))

(cl-defun mvt-time-signature (mvt)
  (vg-time-signature (first (mvt-voice-groups mvt))))

(cl-defun mvt-bar-position (mvt bar &optional (with-upbeat t))
  "Assumes all voices has the same time-signature."
  (let ((pos-with-upbeat (* (1- bar) (ts-length (mvt-time-signature mvt)))))
    (if with-upbeat 
      pos-with-upbeat
      (+ pos-with-upbeat (mvt-upbeat mvt)))))
;;(mvt-bar-position mats 1 nil)

(cl-defun mvt-submovement (movement from-bar to-bar &optional (with-upbeat t))
  "Returns a copy of MOVEMENT but only the bars [FROM-BAR TO-BAR). If with-upbeat is not nil, the result will contain the upbeat of FROM-BAR but not the upbeat to TO-BAR."
  (let ((new-mvt (mvt-copy movement)))
    ;; remove repeats not strictly within [from-bar to-bar]
    (setf (mvt-repeats new-mvt)
	  (remove* (list from-bar to-bar) (mvt-repeats new-mvt)
	    :test #'(lambda (x y) (i-within x y t))))
    (setf (first (mvt-voice-groups new-mvt))
	  (vg-section (mvt-voice-group new-mvt)
		      (mvt-bar-position new-mvt from-bar with-upbeat)
		      (mvt-bar-position new-mvt to-bar with-upbeat)))
    (mvt-shift-start-time new-mvt)))
;;(mvt-submovement (mvt-test) 1 2)

(cl-defun mvt-measure (movement bar &optional (with-upbeat t))
  "Returns a section of MOVEMENT (as a movement struct)"
  (mvt-submovement movement bar (1+ bar) with-upbeat))
;;(mvt-measure (mvt-test) 1)

(cl-defun mvt-find-maximum-repeat-span (mvt)
  "Returns (mp1 mp2) for maximum repeat span"
  (let* ((n (mvt-last-bar mvt)) 
	 (mid-bar (floor (/ n 2))))
    (cl-loop for nbars from mid-bar downto 1
	  for ores = (cl-loop for from-bar1 from 1
			   for to-bar1 from (+ from-bar1 nbars) to (- (1+ n) nbars)
			   for submvt1 = (mvt-submovement mvt from-bar1 to-bar1)
			   for res1 = (cl-loop for from-bar2 from to-bar1
					    for to-bar2 from (+ to-bar1 nbars) to (1+ n)
					    for submvt2 = (mvt-submovement mvt from-bar2 to-bar2)
					    when (movement= submvt1 submvt2) 
					    return (list from-bar1 from-bar2))
			   when res1 return res1)
	  when ores return ores)))
;;(mvt-find-maximum-repeat-span (mvt-test))

(cl-defun mvt-delete-measures (movement from-bar to-bar)
  "Returns a copy of movement but only measures from FROM-BAR to, but not including, TO-BAR in movement MOVEMENT.
Destructive.
TODO: why copy movement? It should be destructive."
  (let ((vg (vg-delete-notes-in-interval (mvt-voice-group movement)
				     (mvt-bar-position movement from-bar)
				     (mvt-bar-position movement to-bar))))
    (setf (mvt-voice-group movement) vg))
  movement)

(cl-defun mvt-remove-measures (movement from-bar to-bar)
  "Returns a copy of movement but only measures from FROM-BAR to, but not including, TO-BAR in movement MOVEMENT.
Destructive.
TODO: why copy movement? It should be destructive."
  (mvt-copy movement :voice-groups (list (vg-remove-notes-in-interval 
					  (mvt-voice-group movement)
					  (mvt-bar-position movement from-bar)
					  (mvt-bar-position movement to-bar)))))
;;(mvt-remove-measures qmvt 1 14)

(cl-defun mvt-chords (movement)
  (vg-chords (mvt-voice-group movement)))
;;(mvt-chords (mvt-test))

(let ((mats nil))
  (cl-defun mvt-test (&optional reset)
    (when (or reset (not mats))
      (setq mats (midi-file-to-movement (test-midi 1))))
    mats))
;;(mvt-test t)
;;(mvt-test)

(provide 'movement)
