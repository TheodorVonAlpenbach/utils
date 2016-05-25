(require 'midi-pitch)
(require 'midi-constants)

;;; channel events
;; length: 3 bytes (always)
;; format: (midi-channel-event-type midi-channel parameter-1 parameter-2)

(defstruct (midi-channel-event (:type list) :named (:conc-name mce-))
  (channel)
  (subevent))

;;; queries
(defun mce-subtype (mce) (struct-type (mce-subevent mce)))

(defun filter-mce-subevents (mce-list subevent-types)
  (copy-if (bind #'member subevent-types) mce-list :key #'mce-subtype))
;;(filter-mce-subevents (midi-channel-events (mt-events (test-track))) '(note-on))

(defstruct (note-off (:type list) :named)
  (pitch)
  (velocity))

(defstruct (note-on (:type list) :named)
  (pitch)
  (velocity))

;;TODO
(defstruct (aftertouch (:type list) :named))

(defstruct (controller (:type list) :named)
  (type)
  (value))

(defstruct (program-change (:type list) :named)
  (instrument))

;;TODO
(defstruct (channel-aftertouch (:type list) :named))

(defstruct (pitch-bend (:type list) :named))


;;; Reader functions: all create-<subevent> must take to byte parameters
(defun create-note-off (pitch-byte velocity)
  (make-note-off :pitch (midi-pitch-string pitch-byte) :velocity velocity))

(defun create-note-on (pitch-byte velocity)
  (make-note-on :pitch (midi-pitch-string pitch-byte) :velocity velocity))

(defun create-controller (type-id value)
  (make-controller :type (midi-controller-type type-id) :value value))

(defun create-program-change (instrument-id &option obsolete-parameter)
  (make-program-change :instrument (midi-instrument-name instrument-id)))

(defun create-mce-subevent-function (subevent)
  (intern-soft (format "create-%s" (symbol-name subevent))))

(defun read-midi-channel-event (status-byte)
  (let ((sb (split-byte status-byte)))
    (make-midi-channel-event 
     :channel (second sb) 
     :subevent (funcall (create-mce-subevent-function (mce-type (first sb)))
			 (read-byte) (read-byte)))))
;;(read-midi-file "c:/emacs-22.1/site-lisp/mb-lisp/midi/test.midi")

;;; Converters to byte lists
(defun note-off-to-bytes (e)
  (list (midi-pitch-byte (note-off-pitch e))
	(note-off-velocity e)))

(defun note-on-to-bytes (e)
  (list (midi-pitch-byte (note-on-pitch e))
	(note-on-velocity e)))

(defun controller-to-bytes (e)
  (list (midi-controller-type-id (controller-type e))
	(controller-value e)))

(defun program-change-to-bytes (e)
  (list (midi-instrument-id (program-change-instrument e))
	0))

(defun midi-channel-event-to-bytes (mce)
  (append (list (join-ints (mce-type-id (mce-subtype mce))
			   (mce-channel mce)))
	  (funcall (struct-to-bytes-function (mce-subtype mce)) 
		   (mce-subevent mce))))
;;(midi-channel-event-to-bytes (make-midi-channel-event :channel 2 :subevent (make-note-on :pitch "E7" :velocity 100)))

;;; print
(defun note-off-to-string (off)
  (format "%s %d" (note-off-pitch off) (note-off-velocity off)))

(defun note-on-to-string (on)
  (format "%s %d" (note-on-pitch on) (note-on-velocity on)))

(defun controller-to-string (c)
  (format "%S %d" (controller-type c) (controller-value c)))

(defun program-change-to-string (pc)
  (format "%d" (program-change-instrument pc)))

(defun midi-channel-event-to-string (mce)
  (format "%S %s" 
    (struct-type (mce-subevent mce))
    (funcall (struct-to-string-function (struct-name (mce-subevent mce)))
	     (mce-subevent mce))))

(provide 'midi-channel-event)
