(require 'duration)
(require 'time-signature)

(defstruct (mposition (:type list) :named (:conc-name mp-))
  (absolute-position)		;duration object
  (time-signature))

(defun mp-new (absolute-position time-signature)
  (make-mposition :absolute-position absolute-position
		  :time-signature time-signature))

(defun mp-measure (mposition)
  (/ (mp-absolute-position mposition)
     (ts-length (mp-time-signature mposition))))

(defun mp-position-in-measure (mposition)
  (mod (mp-absolute-position mposition) 
       (ts-length (mp-time-signature mposition))))

(defun mp-measure-remainder (mposition)
  (- (ts-length (mp-time-signature mposition))
     (mp-position-in-measure mposition)))

(defun mp-beat (mposition)
  (floor (mp-position-in-measure mposition)))

(defun mp-position-in-beat (mposition)
  (mod (mp-position-in-measure mposition)
       (mp-beat mposition)))

(defun* mp-to-string (mposition &optional (print-style mu-default-print-style))
  (format "m%d:%d.%d" (mp-measure mposition) (mp-beat mposition) (mp-position-in-beat mposition)))

(provide 'mposition)