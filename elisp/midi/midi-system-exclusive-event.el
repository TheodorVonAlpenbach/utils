(defstruct (midi-system-exclusive-event (:type list) :named (:conc-name mse-))
  "Represent a system exclusive MIDI event."
  (subevent))

(defun midi-system-exclusive-event-to-bytes (e)
  (error "midi-system-exclusive-event-to-bytes is not implemented!"))