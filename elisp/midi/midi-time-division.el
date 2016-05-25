(defstruct (time-division (:type list) :named (:conc-name td-))
  (type) ;'ticks-per-beat or 'frames-per-second
  (value))

(defun bytes-to-time-division (bytes)
  "Converts a list of two BYTES to a time-division struct.
First bit defines time division type while the remaining 15 bits
defines the value itself."
  (let* ((td-int (bytes-to-int bytes))
	 (type-bit (nth-bit td-int 16))
	 (value (logand td-int #x7FFF)))
    (make-time-division
     :type (nth type-bit '(frames-per-second ticks-per-beat))
     :value value)))

(defun time-division-to-bytes (td)
  "Converts a time-division struct to MIDI file bytes."
  (int-to-bytes (+ (* (position (td-type td)
				  '(frames-per-second ticks-per-beat))
			(expt 2 15))
		     (td-value td))
		2))
;;(time-division-to-bytes (mf-time-division (test-midi)))

(defun time-division-to-string (td)
  (format "Time division: %d %S" (td-value td) (td-type  td)))
;;(time-division-to-string (mf-time-division (test-midi)))

(provide 'midi-time-division)