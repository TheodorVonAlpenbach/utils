;;http://en.wikipedia.org/wiki/Time_signature

(cl-defstruct (time-signature :named (:conc-name ts-))
  (length 4)
  (unit 4))
;;(make-time-signature)

(cl-defun ts-new (&optional (length 4) (unit 4))
  (make-time-signature :length length :unit unit))

(cl-defun ts-to-string (ts &optional (print-style mu-default-print-style))
  (when ts 
    (cl-case print-style
      (otherwise (format "%d/%d" (ts-length ts) (ts-unit ts))))))
;;(ts-to-string (make-time-signature))

(cl-defun ts-from-string (ts-string &optional (print-style mu-default-print-style))
  (cl-case print-style
    (otherwise (apply #'ts-new (mapcar #'string-to-int (split-string ts-string "/"))))))
;;(ts-from-string "4/4")

(provide 'time-signature)
