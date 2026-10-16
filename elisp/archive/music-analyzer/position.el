(require 'duration)
(require 'time-signature)

(cl-defstruct (mposition (:type list) :named (:conc-name mp-))
  (absolute-position)		;duration object
  (time-signature))

(cl-defun mp-new (absolute-position time-signature)
  (make-mposition :absolute-position absolute-position
		  :time-signature time-signature))

(cl-defun mp-measure (mposition)
  (/ (mp-absolute-position mposition)
     (ts-length (mp-time-signature mposition))))

(cl-defun mp-position-in-measure (mposition)
  (mod (mp-absolute-position mposition) 
       (ts-length (mp-time-signature mposition))))

(cl-defun mp-measure-remainder (mposition)
  (- (ts-length (mp-time-signature mposition))
     (mp-position-in-measure mposition)))

(cl-defun mp-beat (mposition)
  (floor (mp-position-in-measure mposition)))

(cl-defun mp-position-in-beat (mposition)
  (mod (mp-position-in-measure mposition)
       (mp-beat mposition)))

(cl-defun mp-to-string (mposition &optional (print-style mu-default-print-style))
  (format "m%d:%d.%d" (mp-measure mposition) (mp-beat mposition) (mp-position-in-beat mposition)))

(provide 'mposition)
