(cl-defun rest-space (n space &optional (world 1024))
  (- world (* (1+ n) space)))
;;(rest-space 3 10)

;;(defun make-1d (start length) (list :start start :length length))
(defun make-1d (start length) (list start length))

;;(defun 1d-start (1d) (getf res :start))
(defalias '1d-start #'first)
(defalias '1d-length #'second)

(cl-defun 1d-align (objects &key (left-space nil) (right-space nil) (world 1024))
  "Returns a list of 1Ds of equal length that fill out the space between the N spaces of length SPACE"
  (let ((space (/ (- world (cl-reduce #'+ objects :key #'length)) (1+ (length objects)))))
    (loop for x in objects
       for l = (1d-length x)
       for start = space then (+ start space l)
       collect (make-1d start l))))
;;(1d-align '((0 10) (0 10)))

(cl-defun 1d-interspaces (n space &optional (world 1024))
  "Returns a list of 1Ds of equal length that fill out the space between the N spaces of length SPACE"
  (let ((l (/ (rest-space n space world) n)))
    (loop for i below n
       for sp = space then (+ sp space l)
       collect (make-1d sp l))))

(cl-defun 1d-transform (x offset scale)
  "Moves 1D object X OFFSET units"
  (make-1d ))

(cl-defun 1d-move (x offset)
  "Moves 1D object X OFFSET units"
  (let ((res (copy-list x)))
    (incf (1d-start res) offset)
    res))
;;(1d-move (first (1d-interspaces 3 10)) 1)

(provide 'mb-utils-geometry)
