(require 'midi-constants)

(defstruct (midi-meta-event (:type list) :named (:conc-name mme-))
  (subevent))

(defun mme-subtype (mme) (struct-type (mme-subevent mme)))

(defun mme-subevents (mme-list subevent-types)
  (copy-if (bind #'member subevent-types) mme-list :key #'me-subsubtype))
;;(filter-mme-subevents (midi-meta-events (mt-events (test-track 0))) '(set-tempo))


;;; subevent types
(defstruct (sequence-number (:type list) :named)
  (msb)
  (lsb))

(defstruct (text-event (:type list) :named)
  (text))

(defstruct (copyright-notice (:type list) :named)
  (text))

(defstruct (sequence-or-track-name (:type list) :named)
  (text))

(defstruct (instrument-name (:type list) :named)
  (text))

(defstruct (lyrics (:type list) :named)
  (text))

(defstruct (midi-marker (:type list) :named) ;;NB! prefixed with midi-, else it would tamper crucial Emacs functions
  (text))

(defstruct (cue-point (:type list) :named)
  (text))

(defstruct (other-text-event (:type list) :named)
  (text))

(defstruct (midi-channel-prefix (:type list) :named)
  (channel))

(defstruct (end-of-track (:type list) :named)
  )

(defstruct (set-tempo (:type list) :named)
  (ms-per-quarter-note))

(defun set-tempo-to-bpm (st)
  (and st (/ 60 
	     (/ (set-tempo-ms-per-quarter-note st)
		1000000.0))))

(defun setf-tempo-bpm (st bpm)
  "Sets the set-tempo value from a BPM value The method converts
input argument BPM (beats per minute), converts it to
ms/quarter-note and assigns this value to ST"
  (setf (set-tempo-ms-per-quarter-note st)
	(round (/ (* 60 1000000) bpm))))

;; TODO! NB! Handling of this event is a bit simplified. The fram rate
;; is hidden in the hour byte as follows:
;; 0rrhhhhh
;; where for rr:  00=24 fps, 01=25 fps, 10=30 fps (drop frame), 11=30 fps
;; what is drop frame?
(defstruct (smpte-offset (:type list) :named)
  (hours)
  (minutes)
  (seconds)
  (frames)
  (sub-frames))

(defstruct (midi-time-signature (:type list) :named)
  (numerator)
  (denominator)
  (metronome-pulse)
  (32nd-notes-per-quarter-note))

(defstruct (key-signature (:type list) :named)
  (accidentals)
  (mode))

(defstruct (sequencer-specific (:type list) :named)
  (blob))


;;; Reader functions
(defun create-sequence-number (bytes)
  (make-sequence-number :msb (first bytes) :lsb (second bytes)))

(defun create-text-event (bytes)
  (make-text-event :text (bytes-to-string bytes)))

(defun create-copyright-notice (bytes)
  (make-copyright-notice :text (bytes-to-string bytes)))

(defun create-sequence-or-track-name (bytes)
  (make-sequence-or-track-name :text (bytes-to-string bytes)))

(defun create-instrument-name (bytes)
  (make-instrument-name :text (bytes-to-string bytes)))

(defun create-lyrics (bytes)
  (make-lyrics :text (bytes-to-string bytes)))

(defun create-marker (bytes)
  (make-marker :text (bytes-to-string bytes)))

(defun create-cue-point (bytes)
  (make-cue-point :text (bytes-to-string bytes)))

(defun create-other-text-event (bytes)
  (make-other-text-event :text (bytes-to-string bytes)))

(defun create-midi-channel-prefix (bytes)
  (make-midi-channel-prefix :channel (first bytes)))

(defun create-end-of-track (bytes)
  (make-end-of-track))

(defun create-set-tempo (bytes)
  (make-set-tempo :ms-per-quarter-note (bytes-to-int bytes)))

(defun create-smpte-offset (bytes)
  (make-smpte-offset
   :hours (first bytes)
   :minutes (second bytes)
   :seconds (third bytes)
   :frames (fourth bytes)
   :sub-frames (fifth bytes)))

(defun create-midi-time-signature (bytes)
  (make-midi-time-signature
   :numerator (first bytes)
   :denominator (second bytes)
   :metronome-pulse (third bytes)
   :32nd-notes-per-quarter-note (fourth bytes)))
;;(create-midi-time-signature nil)

(defun create-key-signature (bytes)
  (make-key-signature
     :accidentals (signed-byte-to-int (first bytes))
     :mode (midi-mode (second bytes))))

(defun create-sequencer-specific (bytes)
  (make-sequencer-specific :blob bytes))

(defun create-mme-subevent-function (subevent)
  (intern-soft (format "create-%s" (symbol-name subevent))))

(defun read-midi-meta-event (type-id bytes)
  (make-midi-meta-event
   :subevent (funcall (create-mme-subevent-function (mme-type type-id))
		       bytes)))
;;(read-midi-meta-event 1 2)


;;; Converters to byte lists
(defun sequence-number-to-bytes (e) 
  (list 2 (sequence-number-msb) (sequence-number-lsb)))

(defun text-event-to-bytes (e)
  (list (length (text-event-text e))
	(string-to-bytes (text-event-text e))))

(defun copyright-notice-to-bytes (e)
  (list (length (copyright-notice-text e))
	(string-to-bytes (copyright-notice-text e))))

(defun sequence-or-track-name-to-bytes (e)
  (list (length (sequence-or-track-name-text e))
	(string-to-bytes (sequence-or-track-name-text e))))

(defun instrument-name-to-bytes (e)
  (list (length (instrument-name-text e))
	(string-to-bytes (instrument-name-text e))))

(defun lyrics-to-bytes (e)
  (list (length (lyrics-text e))
	(string-to-bytes (lyrics-text e))))

(defun midi-marker-to-bytes (e)
  (list (length (midi-marker-text e))
	(string-to-bytes (midi-marker-text e))))

(defun cue-point-to-bytes (e)
  (list (length (cue-point-text e))
	(string-to-bytes (cue-point-text e))))

(defun other-text-event-to-bytes (e)
  (list (length (other-text-event-text e))
	(string-to-bytes (other-text-event-text e))))

(defun midi-channel-prefix-to-bytes (e)
  (list 1 (midi-channel-prefix-channel e)))

(defun end-of-track-to-bytes (e)
  (list 0))

(defun set-tempo-to-bytes (e)
  (list 3 (int-to-bytes (set-tempo-ms-per-quarter-note e) 3)))

(defun smpte-offset-to-bytes (e)
  (list 5 
	(smpte-offset-hours e)
	(smpte-offset-minutes e)
	(smpte-offset-seconds e)
	(smpte-offset-frames e)
	(smpte-offset-sub-frames e)))

(defun midi-time-signature-to-bytes (e)
  (list 4 
	(midi-time-signature-numerator e)
	(midi-time-signature-denominator e)
	(midi-time-signature-metronome-pulse e)
	(midi-time-signature-32nd-notes-per-quarter-note e)))

(defun key-signature-to-bytes (e)
  (list 2 
	(int-to-signed-byte (key-signature-accidentals e))
	(midi-mode-byte (key-signature-mode e))))

(defun sequencer-specific-to-bytes (e)
  (list (length (sequencer-specific-blob e))
	(sequencer-specific-blob e)))

(defun midi-meta-event-to-bytes (mme)
    (append (list #xff (mme-type-id (mme-subtype mme)))
	    (funcall (struct-to-bytes-function (mme-subtype mme)) (mme-subevent mme))))

;;; print
(defun sequence-number-to-string (sn)
  (format "MSB=%d LSB=%d" (sequence-number-msb sn) (sequence-number-lsb sn)))

(defun text-event-to-string (e)
  (text-event-text e))

(defun copyright-notice-to-string (e)
  (copyright-notice-text e))

(defun sequence-or-track-name-to-string (e)
  (sequence-or-track-name-text e))

(defun instrument-name-to-string (e)
  (instrument-name-text e))

(defun lyrics-to-string (e)
  (lyrics-text e))

(defun midi-marker-to-string (e)
  (midi-marker-text e))

(defun cue-point-to-string (e)
  (cue-point-text e))

(defun other-text-event-to-string (e)
  (other-text-event-text e))

(defun midi-channel-prefix-to-string (mc)
  (format "%d" (midi-channel-prefix-channel mc)))

(defun end-of-track-to-string (eot) "")

(defun set-tempo-to-string (st)
  (format "%d" (set-tempo-ms-per-quarter-note st)))

(defun smpte-offset-to-string (offset)
  (format "%02d:%02d:%02d frames=%d subframes=&d"
    (smpte-offset-hours offset)
    (smpte-offset-minutes offset)
    (smpte-offset-seconds offset)
    (smpte-offset-frames offset)
    (smpte-offset-sub-frames offset)))

(defun midi-time-signature-to-string (ts)
  (format "%d/%d MM=:%d 32nds/quarters=%d"
    (midi-time-signature-numerator ts)
    (midi-time-signature-denominator ts)
    (midi-time-signature-metronome-pulse ts)
    (midi-time-signature-32nd-notes-per-quarter-note ts)))

(defun key-signature-to-string (ks)
  (format "%s %S"
    (midi-key-to-chrome (key-signature-accidentals ks) (key-signature-mode ks))
    (key-signature-mode ks)))
;;(key-signature-to-string (make-key-signature :accidentals -4 :mode 'minor))

(defun sequencer-specific-to-string (ss)
  (format "%S" (sequencer-specific-blob ss)))

(defun midi-meta-event-to-string (mme)
  (format "%S %s" 
    (struct-type (mme-subevent mme))
    (funcall (struct-to-string-function (struct-name (mme-subevent mme)))
	     (mme-subevent mme))))

(provide 'midi-meta-event)