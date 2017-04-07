(in-package :numerics-utils)

;;; Safe handlers
(defun exp-safe (exponent)
  (restart-case (exp exponent)
    (use-value (value)
      :report "Use a new value."
      :interactive read-new-value
      value)
    (round-floating-point-underflow-to-zero () 0)))

(defun expt-safe (base-number power-number)
  (restart-case (expt base-number power-number)
    (use-value (value)
      :report "Use a new value."
      :interactive read-new-value
      value)
    (round-floating-point-underflow-to-zero () 0)))

(defun read-new-value ()
  (format t "Enter a new value: ")
  (multiple-value-list (eval (read))))

(defun safe-op (fn &rest args)
  (restart-case (apply fn args)
    (use-value (value)
      :report "Use a new value."
      :interactive read-new-value
      value)
    (round-floating-point-underflow-to-zero () 0)))

(defun safe-+ (&rest args)
  (restart-case (reduce #'+ args)
    (use-value (value)
      :report "Use a new value."
      :interactive read-new-value
      value)
    (round-floating-point-underflow-to-zero () 0)))

(defun safe-* (&rest args)
  (restart-case (reduce #'* args)
    (use-value (value)
      :report "Use a new value."
      :interactive read-new-value
      value)
    (round-floating-point-underflow-to-zero () 0)
    (save-multiplication-outflow-by-log ()
      (if (zerop (reduce #'min args))
	  0 (exp-safe (reduce #'+ (mapcar #'log args)))))))
;;(handles-outflow (#'save-multiplication-outflow-by-log) (safe-* 1E-20 1E-20 2E20))

(defun round-floating-point-underflow-to-zero (c)
  (declare (ignore c))
  (invoke-restart 'round-floating-point-underflow-to-zero))
(defun save-multiplication-outflow-by-log (c)
  (declare (ignore c))
  (invoke-restart 'save-multiplication-outflow-by-log))

(defmacro handles-outflow ((&optional restart) &body body)
  "Handles FLOATING-POINT-UNDERFLOW conditions in BODY with RESTART"
  `(handler-bind ((floating-point-underflow
		   (or ,restart #'round-floating-point-underflow-to-zero)))
     ,@body))
;;(handles-outflow (#'round-floating-point-underflow-to-zero) (+))

;;(handler-bind ((floating-point-underflow #'save-multiplication-outflow-by-log))
;;  (loop for i below 5 collect (safe-* 1E-20 1E-20 2E20 i)))
