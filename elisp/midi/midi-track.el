(require 'midi-event)

(cl-defstruct (midi-track (:type list) :named (:conc-name mt-))
  (header-tag)
  (events))

(cl-defun test-track (&optional (track-number 1)) (nth track-number (mf-tracks (test-midi))))
;;(test-track)

;;; read/write
(cl-defun read-midi-track ()
  "Reads a MIDI track into the corresponding struct in the Midi model.
The MIDI header always starts with the string 'MTrk'."
  (let* ((track-header-tag (bytes-to-string (read-bytes 4)))
	 (track-size (bytes-to-int (read-bytes 4)))
	 (track-stop-position (+ track-size (read-position) -1))
	 (events (cl-loop while (< (read-position) track-stop-position)
		       collect (read-midi-event))))
    (make-midi-track :header-tag track-header-tag :events events)))

(cl-defun midi-track-to-bytes (track)
  (let ((track-bytes (flatten (apply #'append (mapcar #'midi-event-to-bytes (mt-events track))))))
    (append (string-to-bytes (mt-header-tag track))
	    (int-to-bytes (length track-bytes) 4)
	    track-bytes)))

;;; print
(cl-defun midi-track-to-string (mt track-number)
  (apply #'concat
	 (format "Track %d\n" track-number)
	 (cl-loop for me in (mt-events mt)
	       collect (midi-event-to-string me))))
;;(midi-track-to-string (test-track) 1)

;;; queries
(cl-defun midi-track-with-absolute-times (mt &optional (factor 1))
  "Returns track MT with delta-times substituted with absolute times"
  (cl-loop for e in (mt-events mt)
	for delta-time = (me-delta-time e)
	for absolute-time = 0 then (+ absolute-time delta-time)
	do (setf (me-delta-time e) 
		 (* factor absolute-time)))
  mt)
;;(midi-track-to-string (midi-track-with-absolute-times (test-track)) 1)

(cl-defun mt-find-first-event (mt subsubtype)
  (find subsubtype (mt-events mt) :key #'me-subsubtype))
;;(mt-find-first-event (test-track 0) 'set-tempo)

(provide 'midi-track)
