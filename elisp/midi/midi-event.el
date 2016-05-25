(require 'midi-channel-event)
(require 'midi-meta-event)

(defstruct (midi-event (:type list) :named (:conc-name me-))
  "Represent a general midi event."
  (delta-time)
  (subevent))

(defun test-events () (mt-events (test-track)))

;;; queries
(defun me-subtype (me) (struct-type (me-subevent me)))

;;; read/write
(defun read-midi-event ()
  (let ((delta-time (first (read-variable-length-integer)))
	(status-byte (read-byte)))
    (make-midi-event
     :delta-time delta-time
     :subevent (if (< status-byte #xf0)
		  (read-midi-channel-event status-byte)
		  (if (= status-byte #xff)
		    (let* ((type-id (read-byte))
			  (event-length (first (read-variable-length-integer)))
			  (event-bytes (read-bytes event-length)))
		      (read-midi-meta-event type-id event-bytes))
		    (error "read-midi-system-exclusive-event is not implemented!"))))))
;;(read-midi-file "c:/Documents and Settings/matsb/My Documents/projects/UiO/midi-files/bach-chorals/000206b_.mid")

(defun struct-to-bytes-function (struct)
  (intern-soft (format "%s-to-bytes" (if (symbolp struct) (symbol-name struct) struct))))

(defun midi-event-to-bytes (e)
  (append (int-to-variable-length-quantity (me-delta-time e))
	  (funcall (struct-to-bytes-function (struct-name (me-subevent e)))
		   (me-subevent e))))

;;; print
(defun struct-to-string-function (struct)
  (intern-soft (format "%s-to-string" (if (symbolp struct) (symbol-name struct) struct))))

(defun midi-event-to-string (me)
  (format "%6d %s\n" 
    (me-delta-time me)
    (funcall (struct-to-string-function (struct-name (me-subevent me)))
	     (me-subevent me))))

(defun midi-subsubtype (e) "Error: Use me-subsubtype instead")
(defun midi-subsubevent (e) "Error: Use me-subsubevent instead")

;;; object queries
(defun me-subsubtype (e)
  "Probably obsolete"
  (case (me-subtype e)
    ((midi-channel-event) (mce-subtype (me-subevent e)))
    ((midi-meta-event) (mme-subtype (me-subevent e)))
    ((midi-system-exclusive-event) (mse-subtype (me-subevent e)))))
;;(mapcar #'me-subsubtype (mt-events (test-track)))

(defun me-subsubevent (e)
  "Returns the subevent of E's subevent"
  (case (me-subtype e)
    ((midi-channel-event) (mce-subevent (me-subevent e)))
    ((midi-meta-event) (mme-subevent (me-subevent e)))
    ((midi-system-exclusive-event) (mse-subevent (me-subevent e)))))
;;(mapcar #'me-subsubevent (mt-events (test-track)))


;;; object related queries
(defun midi-channel-events (events &optional channel)
  "Extracts from EVENTS all midi channel events for CHANNEL (0-15).
If CHANNEL is not specified, it returns the midi channel events
from all channels. Note. This methods discards
program (instrument) changes."
  (copy-if #'(lambda (me) 
	       (and (eq (me-subtype me) 'midi-channel-event)
		    (or (not (numberp channel))
			(= (mce-channel (me-subevent me))
			   channel))))
	   events))
;;(midi-channel-events (mt-events (test-track 1)) 0)

(defun midi-meta-events (events)
  "Extracts from EVENTS all midi meta events."
  (copy-if #'(lambda (me) (eq (me-subtype me) 'midi-meta-event))
	   events))
;;(midi-meta-events (mt-events (test-track 1)))

(provide 'midi-event)
