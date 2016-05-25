(require 'midi-track)
(require 'midi-time-division)

(defstruct (midi-file (:type list) :named (:conc-name mf-))
  (original-file)
  (header-tag)
  (format-type)
  (time-division)
  (tracks))

(defun read-midi-file (file)
  "Reads a MIDI file into an Emacs Lisp model.
The MIDI header always starts with the string 'MThd'. The second
element, 6, is just the size of the three last elements in the
header. Format is 0, 1, 2."
  (with-file-readonly file
    (hexl-mode)
    (hexl-beginning-of-buffer 1)
    (let* ((header-tag (bytes-to-string (read-bytes 4)))
	  (header-size (bytes-to-int (read-bytes 4)))
	  (format-type (bytes-to-int (read-bytes 2)))
	  (number-of-tracks (bytes-to-int (read-bytes 2)))
	  (time-division (bytes-to-time-division (read-bytes 2)))
	  (tracks (loop for i below number-of-tracks
			collect (read-midi-track))))
      (make-midi-file
       :original-file file
       :header-tag header-tag
       :format-type format-type
       :time-division time-division
       :tracks tracks))))
;;(read-midi-file "c:/Documents and Settings/matsb/My Documents/projects/UiO/project/bach-chorals/midi/000206b_.mid")
;;(file-to-bytes "c:/Documents and Settings/matsb/My Documents/projects/UiO/midi-files/bach-chorals/000206b_.mid")

(defun write-midi-file (mf file)
  (bytes-to-file (midi-file-to-bytes mf) file))
;;(write-midi-file (read-midi-file "c:/emacs-22.1/site-lisp/mb-lisp/midi/test.midi") "c:/emacs-22.1/site-lisp/mb-lisp/midi/test-new.midi")
;;(read-midi-file "c:/emacs-22.1/site-lisp/mb-lisp/midi/test-new.midi")

(defun midi-file-to-bytes (mf)
  "Converts midi-file MF to bytes.
``write-midi-file''
`write-midi-file'
write-midi-file"
  (append (string-to-bytes (mf-header-tag mf))
	  (int-to-bytes 6 4)
	  (int-to-bytes (mf-format-type mf) 2)
	  (int-to-bytes (length (mf-tracks mf)) 2)
	  (time-division-to-bytes (mf-time-division mf))
	  (flatten (mapcar #'midi-track-to-bytes (mf-tracks mf)))))

;;; print
(defun midi-file-to-string (mf)
  (apply #'concat 
	 (format "\n")
	 (format "Orignal file: %s\n" (mf-original-file mf))
	 (format "Format type: %d\n" (mf-format-type mf))
	 (format "%s\n" (time-division-to-string (mf-time-division mf))) 
	 (loop for track in (mf-tracks mf)
	       for track-number from 1
	       collect (midi-track-to-string track track-number))))
;;(midi-file-to-string (test-midi))

;;; manipulations
(defun mf-set-tempo (mf bpm)
  "Sets tempo in mf to BPM"
  (let* ((mt (first (mf-tracks mf)))
	 (mes (midi-meta-events (mt-events mt)))
	 (ste (me-subsubevent (first (mme-subevents mes '(set-tempo))))))
    (if ste
      (setf-tempo-bpm ste bpm)
      (error "Couldn't set tempo in midi file MF. The MF lacks an
      existing set-tempo event. This version doesn't support
      adding a new one"))))

;;; queries
(defun midi-file-ticks-per-beat (mf)
  (mf-time-division mf))

(defun get-midi-meta-events (mt &optional type)
  (remove-if-not #'(lambda (x)
	       (and (eq (me-subtype x) 'midi-meta-event)
		    (or (not type)
			(eq (struct-type (mme-sub-event (me-sub-event x))) type))))
	   (mt-events track)))
;;(midi-meta-events (first (midi-file-tracks (test-midi))))
;;(midi-meta-events (first (midi-file-tracks (test-midi))) 'set-tempo)

(defun find-meta-event (track)
  (copy-if #'(lambda (x) (eq (struct-type (midi-event-sub-event x)) 'mide-meta-event))
	     (midi-track-events (first (midi-file-tracks track)))))


;;; test functions
(defun* test-midi-filename (&optional (n 1)) 
  (nth n (list (concat *mb-lisp-dir* "midi/test.midi")
	       (concat *mb-lisp-dir* "midi/data/000206b_.mid")
	       "c:/Documents and Settings/matsb/My Documents/projects/UiO/project/bach-chorals/midi/000206b_.mid"
	       "c:/emacs-22.1/site-lisp/mb-lisp/midi/test.midi")))
;;(test-midi-filename)

(defun* test-midi (&optional (n 1)) 
  (read-midi-file (test-midi-filename n)))
;;(test-midi 1)

(defun test-midi-edit-instruments (file nfile)
  (let* ((mf (read-midi-file file))
	 (mt1 (second (mf-tracks mf)))
	 (es (mt-events mt1))
	 (npce (make-program-change :instrument 'violin))
	 (nce (make-midi-channel-event :channel 0 :sub-event npce))
	 (ne (make-midi-event :delta-time 0 :sub-event nce)))
    (list-insert ne 1 es)
    (write-midi-file mf nfile)))

(defun test-midi-stupid-copy (file nfile)
  (write-midi-file (read-midi-file file) nfile))
;;(test-midi-stupid-copy "c:/emacs-22.1/site-lisp/mb-lisp/midi/test.midi" "c:/emacs-22.1/site-lisp/mb-lisp/midi/new.midi")

(defun* test-midi-set-tempo (bpm filename &optional (nfile (file-name-append filename (format "-%d" bpm))))
  "Sets new tempo in midi FILE. Result is flushed to NFILE."
  (let ((mf (read-midi-file filename)))
    (mf-set-tempo mf bpm)
    (write-midi-file mf nfile)))
;;(test-midi-set-tempo 110 "c:/Documents and Settings/matsb/My Documents/projects/UiO/Host-2012/2710/Oppgave1/Oppgave1-edit.midi")
;;(test-midi-set-tempo 110 "c:/Documents and Settings/matsb/My Documents/My Files/Coro/odk/arr/koraler/505/continuo-505.midi")
;;(test-midi-set-tempo 90 "c:/Documents and Settings/matsb/My Documents/My Files/Coro/odk/arr/koraler/505/chorus-505.midi")

(provide 'midi-file)
