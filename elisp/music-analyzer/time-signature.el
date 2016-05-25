;;http://en.wikipedia.org/wiki/Time_signature

(defstruct (time-signature :named (:conc-name ts-))
  (length 4)
  (unit 4))
;;(make-time-signature)

(defun* ts-new (&optional (length 4) (unit 4))
  (make-time-signature :length length :unit unit))

(defun* ts-to-string (ts &optional (print-style mu-default-print-style))
  (when ts 
    (case print-style
      (otherwise (format "%d/%d" (ts-length ts) (ts-unit ts))))))
;;(ts-to-string (make-time-signature))

(defun* ts-from-string (ts-string &optional (print-style mu-default-print-style))
  (case print-style
    (otherwise (apply #'ts-new (mapcar #'string-to-int (split-string ts-string "/"))))))
;;(ts-from-string "4/4")

(provide 'time-signature)