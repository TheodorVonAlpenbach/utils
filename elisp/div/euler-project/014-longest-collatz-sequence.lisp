(defparameter *014-next-vector*)

(defun next-collatz (n)
  (evenp n) (/ n 2) (1+ (* 3 n)))

(defun len)

(defun handle-nth-collatz (n)
  (when (zerop (svref *014-next-vector* n))
    (setf (svref *014-next-vector* n) (next-collatz n))))

(defun 014-solution (&optional (n 1000000))
  (setf *014-next-vector* (make-array n :element-type 'integer))
  (loop for i from 1 below n do (handle-nth-collatz i)))
;;(time (013-solution))
;; => "5537376230"
;; 5537376230390876637302048746832985971773659831892672

