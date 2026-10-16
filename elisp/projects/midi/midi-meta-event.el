(require 'midi-constants)

(cl-defstruct (midi-meta-event (:type list) :named (:conc-name mme-))
  (subevent))

(cl-defun mme-subtype (mme) (struct-type (mme-subevent mme)))

(cl-defun mme-subevents (mme-list subevent-types)
  (copy-if (bind #'member subevent-types) mme-list :key #'me-subsubtype))
;;(filter-mme-subevents (midi-meta-events (mt-events (test-track 0))) '(set-tempo))


;;; subevent types
(cl-defstruct (sequence-number (:type list) :named)
  (msb)
  (lsb))

(cl-defstruct (text-event (:type list) :named)
  (text))

(cl-defstruct (copyright-notice (:type list) :named)
  (text))

(cl-defstruct (sequence-or-track-name (:type list) :named)
  (text))

(cl-defstruct (instrument-name (:type list) :named)
  (text))

(cl-defstruct (lyrics (:type list) :named)
  (text))

(cl-defstruct (midi-marker (:type list) :named) ;;NB! prefixed with midi-, else it would tamper crucial Emacs functions
  (text))

(cl-defstruct (cue-point (:type list) :named)
  (text))

(cl-defstruct (other-text-event (:type list) :named)
  (text))

(cl-defstruct (midi-channel-prefix (:type list) :named)
  (channel))

(cl-defstruct (end-of-track (:type list) :named)
  )

(cl-defstruct (set-tempo (:type list) :named)
  (ms-per-quarter-note))

(cl-defun set-tempo-to-bpm (st)
  (and st (/ 60 
	     (/ (set-tempo-ms-per-quarter-note st)
		1000000.0))))

(cl-defun setf-tempo-bpm (st bpm)
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
(cl-defstruct (smpte-offset (:type list) :named)
  (hours)
  (minutes)
  (seconds)
  (frames)
  (sub-frames))

(cl-defstruct (midi-time-signature (:type list) :named)
  (numerator)
  (denominator)
  (metronome-pulse)
  (32nd-notes-per-quarter-note))

(cl-defstruct (key-signature (:type list) :named)
  (accidentals)
  (mode))

(cl-defstruct (sequencer-specific (:type list) :named)
  (blob))


;;; Reader functions
(cl-defun create-sequence-number (bytes)
  (make-sequence-number :msb (first bytes) :lsb (second bytes)))

(cl-defun create-text-event (bytes)
  (make-text-event :text (bytes-to-string bytes)))

(cl-defun create-copyright-notice (bytes)
  (make-copyright-notice :text (bytes-to-string bytes)))

(cl-defun create-sequence-or-track-name (bytes)
  (make-sequence-or-track-name :text (bytes-to-string bytes)))

(cl-defun create-instrument-name (bytes)
  (make-instrument-name :text (bytes-to-string bytes)))

(cl-defun create-lyrics (bytes)
  (make-lyrics :text (bytes-to-string bytes)))

(cl-defun create-marker (bytes)
  (make-marker :text (bytes-to-string bytes)))

(cl-defun create-cue-point (bytes)
  (make-cue-point :text (bytes-to-string bytes)))

(cl-defun create-other-text-event (bytes)
  (make-other-text-event :text (bytes-to-string bytes)))

(cl-defun create-midi-channel-prefix (bytes)
  (make-midi-channel-prefix :channel (first bytes)))

(cl-defun create-end-of-track (bytes)
  (make-end-of-track))

(cl-defun create-set-tempo (bytes)
  (make-set-tempo :ms-per-quarter-note (bytes-to-int bytes)))

(cl-defun create-smpte-offset (bytes)
  (make-smpte-offset
   :hours (first bytes)
   :minutes (second bytes)
   :seconds (third bytes)
   :frames (fourth bytes)
   :sub-frames (fifth bytes)))

(cl-defun create-midi-time-signature (bytes)
  (make-midi-time-signature
   :numerator (first bytes)
   :denominator (second bytes)
   :metronome-pulse (third bytes)
   :32nd-notes-per-quarter-note (fourth bytes)))
;;(create-midi-time-signature nil)

(cl-defun create-key-signature (bytes)
  (make-key-signature
     :accidentals (signed-byte-to-int (first bytes))
     :mode (midi-mode (second bytes))))

(cl-defun create-sequencer-specific (bytes)
  (make-sequencer-specific :blob bytes))

(cl-defun create-mme-subevent-function (subevent)
  (intern-soft (format "create-%s" (symbol-name subevent))))

(cl-defun read-midi-meta-event (type-id bytes)
  (make-midi-meta-event
   :subevent (funcall (create-mme-subevent-function (mme-type type-id))
		       bytes)))
;;(read-midi-meta-event 1 2)


;;; Converters to byte lists
(cl-defun sequence-number-to-bytes (e) 
  (list 2 (sequence-number-msb) (sequence-number-lsb)))

(cl-defun text-event-to-bytes (e)
  (list (length (text-event-text e))
	(string-to-bytes (text-event-text e))))

(cl-defun copyright-notice-to-bytes (e)
  (list (length (copyright-notice-text e))
	(string-to-bytes (copyright-notice-text e))))

(cl-defun sequence-or-track-name-to-bytes (e)
  (list (length (sequence-or-track-name-text e))
	(string-to-bytes (sequence-or-track-name-text e))))

(cl-defun instrument-name-to-bytes (e)
  (list (length (instrument-name-text e))
	(string-to-bytes (instrument-name-text e))))

(cl-defun lyrics-to-bytes (e)
  (list (length (lyrics-text e))
	(string-to-bytes (lyrics-text e))))

(cl-defun midi-marker-to-bytes (e)
  (list (length (midi-marker-text e))
	(string-to-bytes (midi-marker-text e))))

(cl-defun cue-point-to-bytes (e)
  (list (length (cue-point-text e))
	(string-to-bytes (cue-point-text e))))

(cl-defun other-text-event-to-bytes (e)
  (list (length (other-text-event-text e))
	(string-to-bytes (other-text-event-text e))))

(cl-defun midi-channel-prefix-to-bytes (e)
  (list 1 (midi-channel-prefix-channel e)))

(cl-defun end-of-track-to-bytes (e)
  (list 0))

(cl-defun set-tempo-to-bytes (e)
  (list 3 (int-to-bytes (set-tempo-ms-per-quarter-note e) 3)))

(cl-defun smpte-offset-to-bytes (e)
  (list 5 
	(smpte-offset-hours e)
	(smpte-offset-minutes e)
	(smpte-offset-seconds e)
	(smpte-offset-frames e)
	(smpte-offset-sub-frames e)))

(cl-defun midi-time-signature-to-bytes (e)
  (list 4 
	(midi-time-signature-numerator e)
	(midi-time-signature-denominator e)
	(midi-time-signature-metronome-pulse e)
	(midi-time-signature-32nd-notes-per-quarter-note e)))

(cl-defun key-signature-to-bytes (e)
  (list 2 
	(int-to-signed-byte (key-signature-accidentals e))
	(midi-mode-byte (key-signature-mode e))))

(cl-defun sequencer-specific-to-bytes (e)
  (list (length (sequencer-specific-blob e))
	(sequencer-specific-blob e)))

(cl-defun midi-meta-event-to-bytes (mme)
    (append (list #xff (mme-type-id (mme-subtype mme)))
	    (funcall (struct-to-bytes-function (mme-subtype mme)) (mme-subevent mme))))

;;; print
(cl-defun sequence-number-to-string (sn)
  (format "MSB=%d LSB=%d" (sequence-number-msb sn) (sequence-number-lsb sn)))

(cl-defun text-event-to-string (e)
  (text-event-text e))

(cl-defun copyright-notice-to-string (e)
  (copyright-notice-text e))

(cl-defun sequence-or-track-name-to-string (e)
  (sequence-or-track-name-text e))

(cl-defun instrument-name-to-string (e)
  (instrument-name-text e))

(cl-defun lyrics-to-string (e)
  (lyrics-text e))

(cl-defun midi-marker-to-string (e)
  (midi-marker-text e))

(cl-defun cue-point-to-string (e)
  (cue-point-text e))

(cl-defun other-text-event-to-string (e)
  (other-text-event-text e))

(cl-defun midi-channel-prefix-to-string (mc)
  (format "%d" (midi-channel-prefix-channel mc)))

(cl-defun end-of-track-to-string (eot) "")

(cl-defun set-tempo-to-string (st)
  (format "%d" (set-tempo-ms-per-quarter-note st)))

(cl-defun smpte-offset-to-string (offset)
  (format "%02d:%02d:%02d frames=%d subframes=&d"
    (smpte-offset-hours offset)
    (smpte-offset-minutes offset)
    (smpte-offset-seconds offset)
    (smpte-offset-frames offset)
    (smpte-offset-sub-frames offset)))

(cl-defun midi-time-signature-to-string (ts)
  (format "%d/%d MM=:%d 32nds/quarters=%d"
    (midi-time-signature-numerator ts)
    (midi-time-signature-denominator ts)
    (midi-time-signature-metronome-pulse ts)
    (midi-time-signature-32nd-notes-per-quarter-note ts)))

(cl-defun key-signature-to-string (ks)
  (format "%s %S"
    (midi-key-to-chrome (key-signature-accidentals ks) (key-signature-mode ks))
    (key-signature-mode ks)))
;;(key-signature-to-string (make-key-signature :accidentals -4 :mode 'minor))

(cl-defun sequencer-specific-to-string (ss)
  (format "%S" (sequencer-specific-blob ss)))

(cl-defun midi-meta-event-to-string (mme)
  (format "%S %s" 
    (struct-type (mme-subevent mme))
    (funcall (struct-to-string-function (struct-name (mme-subevent mme)))
	     (mme-subevent mme))))

(provide 'midi-meta-event)
