(require 'note)

;;;; TODO: voice is in fact notes + instrument, so every method in voice regarding notes should have a notes counterpart.
;;;; unfortunatly, there isn't a oo framework in Elisp so every method must currently be written out for each "class"
;;;; an idea would be to write a macro that generates all methods from "superclass", except those that is already defined ("poor man's Override")
;;;; another idea is to implement a CLOS like framework in Elisp (e.g. defobject defmethod etc), but I guess that' not so easy

;;;; MP or MPOSITION is music position corresponding for instance to start-time, not list position

(defstruct (voice :named (:conc-name v-))
  (instrument)
  (notes)  ; a list of notes---this is, in effect, a superclass of voice
  (time-signature)
  (upbeat (d-new 0))
  (key))

(defun notes-p (x)
  "Returns t iif X is a list of `note' elements"
  (and (listp x) (every #'note-p x)))
;;(notes-p (list (n-new)))

(defun* v-new (&optional notes (time-signature (ts-new)) (key (k-new)) instrument)
  (make-voice :instrument instrument :time-signature time-signature :key key :notes notes))
;;(v-new)

(defun* v-test (&optional (n 0))
  (nth n (vg-voices (vg-test))))
;;(v-test)
(defun notes-test ()
  (v-notes (v-test)))
;;(notes-test)

;;; constructors etc
(defun notes-copy (notes) (mapcar #'n-copy notes))

;;True copy method of voice.  "
(defun* v-copy (voice &key
		      (instrument (v-instrument voice))
		      (notes (v-notes voice))
		      (time-signature (v-time-signature voice))
		      (upbeat (v-upbeat voice))
		      (key (v-key voice)))
  "Returns a copy of VOICE where all the notes also are copies of the respective notes in VOICE.
copy-voice only copies the head of the notes list."
  (make-voice :instrument instrument 
	      :notes (notes-copy notes)
	      :time-signature (copy-time-signature time-signature)
	      :upbeat (copy-duration upbeat)
	      :key (copy-key key)))
;;(v-copy (make-voice :instrument 'qwe :key (k-new)))

(defun voice= (v1 v2)
  (and (equal (v-instrument v1) (v-instrument v2))
       (apply #'all-true (mapcar* #'note= (v-notes v1) (v-notes v2)))))

(defun* v-bar-position (voice bar &optional (with-upbeat t))
  "Assumes all voices has the same time-signature."
  (let ((pos-with-upbeat (* (1- bar) (ts-length (v-time-signature voice)))))
    (if with-upbeat 
      pos-with-upbeat
      (+ pos-with-upbeat (v-upbeat voice)))))

;;; print
(defun* instrument-to-string (instrument &optional (print-style mu-default-print-style))
  (if (eq print-style 'lilypond) 
    "" 
    (if (stringp instrument)
      instrument
      (format "%S" instrument))))

;;; queries
(defun v-length (voice)
  "Returns the number of notes in VOICE"
  (length (v-notes voice)))

(defun notes-first (notes) 
  "Returns the first note in NOTES"
  (first notes))
(defun v-first-note (voice)
  "Returns the first note in VOICE"
  (notes-first (v-notes voice)))

(defun notes-last (notes) 
  "Returns the last note in NOTES"
  (first (last notes)))
;;(notes-last (list (n-new) (n-new)))
(defun v-last-note (voice)
  "Returns the last note in VOICE"
  (notes-last (v-notes voice)))

(defun* notes-calculate-start-times (notes &optional (init-start-time 0))
  "Doesn't handle units"
  (loop for n in-ref notes
	for start-time = init-start-time then next-start-time
	for next-start-time = (+ start-time (n-dtime n))
	do (setf (n-start-time n) start-time))
  notes)
(defun* v-calculate-start-times (voice &optional (init-start-time 0))
  "Doesn't handle units"
  (notes-calculate-start-times (v-notes voice) init-start-time)
  voice)

(defun notes-start-time (notes)
  "Returns the start-time of the first note in NOTES"
  (n-start-time (first notes)))
(defun v-start-time (voice)
  "Returns the start-time of the first note in VOICE"
  (notes-start-time (v-notes voice)))
;;(v-start-time (v-test))

(defun notes-end-time (notes)
  "Returns the end-time of the last note in NOTES"
  (n-end-time (notes-last notes)))
(defun v-end-time (voice)
  "Returns the end-time of the last note in VOICE"
  (notes-end-time (v-notes voice)))
;;(v-end-time (v-test))

(defun notes-note-after (notes mposition &optional strictly-after)
  "Returns the first note in NOTES sounding after MPOSITION.
If strictly-after is nil, the note may also start on MPOSITION."
  (find-if (bind (if strictly-after #'> #'>=) mposition) notes :key #'n-start-time))
(defun v-note-after (voice mposition &optional strictly-after)
  "Returns the first note in VOICE sounding after MPOSITION.
If strictly-after is nil, the note may also start on MPOSITION."
  (notes-note-after (v-notes voice) mposition strictly-after))
;;(v-note-after (v-test) 1 t)

(defun notes-note-at (notes mposition)
  "Returns the note in NOTES sounding at MPOSITION.
Note that a notes sounds in the interval [start-time end-time)."
  (find-if (bind #'<= mposition) notes :key #'n-start-time :from-end t))
(defun v-note-at (voice mposition)
  "Returns the note in VOICE sounding at MPOSITION.
Note that a notes sounds in the interval [start-time end-time)."
  (notes-note-at (v-notes voice) mposition))
;;(v-note-at (v-test) 1)

(defun notes-from (notes mposition)
  "Returns the portion of NOTES starting at or after MPOSITION"
  (member-if (bind #'>= mposition) notes :key #'n-start-time))

(defun notes-position (notes mposition &optional strictly-after)
  "Returns the element position of the first note in VOICE starting at or after MPOSITION.
If STRICTLY-AFTER is not nil, the position of the first note
starting after MPOSITION is returned."
  (position-if (bind (if strictly-after #'> #'>=) mposition) notes :key #'n-start-time))
(defun v-position (voice mposition &optional strictly-after)
  "Returns the element position of the first note in VOICE starting at or after MPOSITION.
If STRICTLY-AFTER is not nil, the position of the first note
starting after MPOSITION is returned."
  (notes-position (v-notes voice) mposition strictly-after))
;;(v-position (v-test) 26)

(defun* v-deduce-upbeat (voice &optional (time-signature (v-time-signature voice)))
  "Deduces if VOICE has upbeat. This is typically used in the
initial phase of midi conversion when a start-time > 0 could be
found. A movement should, finally, have start-time 0."
  (unless (zerop (v-start-time voice)) 
    (d-new (- (ts-length time-signature)
	      (v-start-time voice)))))
;;(v-deduce-upbeat (v-test))

;;; ACTIONS
(defun* notes-shift-start-time (notes &optional (shift (notes-start-time notes)))
  "Returns a list of notes where the start-time of each of its notes has been reduced by SHIFT compared with NOTES.
If SHIFT is nil the notes will be shifted so that the first note has start-time 0."
  (loop for n in notes
	collect (n-shift-start-time n shift)))

(defun* v-shift-start-time (voice &optional (shift (- (v-start-time voice))))
  "Returns a copy of VOICE where the start-time of each of its notes has been reduced by SHIFT.
If SHIFT is nil the notes will be shifted so that the first note has start-time 0."
  (v-copy voice :notes (notes-shift-start-time (v-notes voice) shift)))
;;(v-shift-start-time (v-shift-start-time (v-test) 12341324))

(defun notes-section (notes begin-pos end-pos)
  "Returns the portion of NOTES with start-time in the interval [BEGIN-POS END-POS).
Non-destructive."
  (let ((res (loop for n in notes
		   if (within (n-start-time n) (interval-co begin-pos end-pos))
		   collect (copy-note n))))
    (when (and res (> (notes-end-time res) end-pos))
      ;; cut last note if sustained longer than the given interval
      (let ((n (notes-last res)))
	(setf (n-duration n)
	      (d-new (- end-pos (n-start-time n))
		     (d-unit (n-duration n)))))
      ;; and tie it to mark that it has been cut
      (setf (n-tied (notes-last res)) t))
    res))
(defun* v-section (voice begin-pos &optional (end-pos (v-end-time voice)))
  "Returns a voice with the portion of VOICE's notes with start-time in the interval [BEGIN-POS END-POS).
Non-destructive."
  (v-copy voice :notes (notes-section (v-notes voice) begin-pos end-pos)))
;;(v-section (v-test) 0 5)

;;; TODO: the following method is clever
;;; however fast; a clearer approach is to introduce a general method woring on notes as a set:
;;; (notes-interval-select (notes inteval operation))
;;; where interval is one of ()(][)[], and operation one of
;;; within, overlap, disjoint

(defun notes-delete-in-interval (notes begin-pos end-pos)
  "Removes the NOTES' content in [BEGIN-POS END-POS). Destructive."
;;(n0 -> n1 -> n2 ... -> nl -> nl+1 ... -> nN)
;; 0     1     2         l     l+1         N   :list-positions
;;          |begin-pos       |end-pos
;;            (n2                      ... nN) :nsb
;;                            (nl+1    ... nN) :nse
;;             2                               :start-position
;;(n0 ->                   ->  nl+1 ... -> nN) :result
  (let* ((shift (- end-pos begin-pos))
	 (notes-b (notes-from notes begin-pos))
	 (notes-e (notes-from notes end-pos))
	 (start-pos (- (length notes)
		       (length notes-b))))
    ;;the notes after removal section must be shifted by the duration amount that is to be removed
    (setf notes-e (notes-shift-start-time notes-e (- shift)))
    ;;remove section
    (setf (nthcdr start-pos notes) notes-e)
    notes))

(defun v-delete-notes-in-interval (voice begin-pos end-pos)
  "Removes the voice's notes with start-time within [BEGIN-POS END-POS). Destructive."
  "Removes all notes in the measures from FROM-BAR to and including TO-BAR in VOICE. Destructive."
  (notes-delete-in-interval (v-notes voice) begin-pos end-pos)
  voice)
;;(v-delete-notes-in-interval qv 1 4)
;;(setq qv (v-copy (v-test)))

(defun notes-remove-in-interval (notes begin-pos end-pos)
  (notes-delete-in-interval (notes-copy notes) begin-pos end-pos))
;;(notes-to-string (notes-remove-in-interval (v-notes (v-test)) 0 30) 'lilypond)

(defun v-remove-notes-in-interval (voice begin-pos end-pos)
  "Removes the voice's notes with start-time within [BEGIN-POS END-POS). Destructive."
  "Removes all notes in the measures from FROM-BAR to and including TO-BAR in VOICE. Destructive."
  (v-copy voice :notes (notes-remove-in-interval (v-notes voice) begin-pos end-pos)))
;;(v-remove-notes-in-interval (v-test) 0 2)

(defun notes-split-at-mposition (notes mposition)
  "Cuts list of NOTES in two. Consider renaming"
  (list-split notes (notes-position notes mposition)))
;;(notes-split-at-mposition (v-notes (v-test)) 16)

(defun v-split-at-mposition (voice mposition)
  "Splits VOICE in two at MPOSITION. Consider renaming. 
Can easily be generalized.
TODO: look at the split methods in midi-conversion.el"
  (loop for notes in (notes-split-at-mposition (v-notes voice) mposition)
	collect (v-copy voice :notes notes)))
;;(mapcar #'v-to-string (v-split-at-mposition (v-test) 16))

;; split notes
(defun notes-split-notes (notes d-split-function)
  "Splits a note that crosses a measure bar"
  (loop for n in notes
	append (n-split n d-split-function)))
;(notes-split-notes (list (make-note :duration (d-new 2.5))) (bind #'d-split-illegal-duration (mapcar #'first duration-map-lilypond)))

(defun v-split-notes (voice d-split-function)
  "Splits a note into a row of similar but tied notes
such the sum of the durations of the resulting notes equals the duration of the input note."
  (setf (v-notes voice) (notes-split-notes (v-notes voice) d-split-function)))
;(v-split-notes (v-section (v-test) 1 2) (bind #'d-split-at-constant 4))

(defun v-split-notes* (voice d-split-functions)
  (loop for fn in d-split-functions
	do (v-split-notes voice fn)))

;; other queries
(defun v-accented-start-times (voice)
  "Generates a sorted list of floats where each element represents the start time of an accented beat"
  (loop for st = (+ upbeat 0.0) then (+ st (ts-length (v-time-signature voice)))
	while (< st (v-end-time voice)) 
	collect st))
;;(v-accented-start-times (v-test))

(defun v-pitch-maximum-notes (voice)
  "Returns a list of all occurences of notes with the maximum pitch within VOICE."
  (minimums (v-notes voice) #'p> :key #'n-pitch))

(defun v-pitch-minimum-notes (voice)
  "Returns a list of all occurences of notes with the minimum pitch within VOICE."
  (minimums (v-notes voice) #'p< :key #'n-pitch))

(defun v-pitch-maximum (voice)
  "Returns the maximum pitch within VOICE."
  (n-pitch (first (v-pitch-maximum-notes voice))))

(defun v-pitch-minimum (voice)
  "Returns the minimum pitch within VOICE."
  (n-pitch (first (v-pitch-minimum-notes voice))))

(defun v-ambitus (voice)
  "Returns the ambitus of VOICE as a pair (MIN MAX), 
where MIN and MAX is the minimum and maximum pitch respectively within voice."
  (list (v-pitch-minimum voice) (v-pitch-maximum voice)))

(defun* v-last-interval (voice &optional (n 1))
  "Returns the last melodic interval in VOICE"
  (apply #'n-interval (last (v-notes voice) 2)))

(defun v-average-set-pitch (voice)
  (/ (apply #'+ (mapcar (compose #'p-ansi #'n-pitch) (v-notes voice))) 
     (* 1.0 (v-length voice))))

(defun v< (voice1 voice2)
  "Returns the t iff VOICE1 is generally lower than VOICE2"
  (< (v-average-set-pitch voice1) (v-average-set-pitch voice2)))

(defun* notes-to-string (notes &optional (print-style mu-default-print-style))
  (concat* (loop for n in notes
		 collect (n-to-string n print-style))
	   :in (if (eq print-style 'lilypond) " " " ")))

(defun* v-to-string (voice &optional (print-style mu-default-print-style))
  (let ((args (remove-if #'null (list (instrument-to-string (v-instrument voice) print-style)
				       (k-to-string (v-key voice) print-style)
				       (ts-to-string (v-time-signature voice) print-style)
				       (notes-to-string (v-notes voice) print-style)))))
    (if (eq print-style 'lilypond)
      (concat* args :in "\n")
      (concat* args :pre "(" :in "\n" :suf ")"))))
;;(v-to-string (v-new) 'lilypond)


(provide 'voice)
