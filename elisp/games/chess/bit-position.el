;;; Bit position
(defun bp-from-snumber (snumber)
  (if (< snumber 28)
    (list (expt 2 snumber) 0 0)
    (if (< snumber 56)
      (list 0 (expt 2 (- snumber 28)) 0)
      (list 0 0 (expt 2 (- snumber 56))))))
;;(bp-from-snumber 4)

(defconst bp-empty '(0 0 0))

(defun bp (square-description)
  (bp-from-snumber (snumber square-description)))
;;(bp nil)

(defun bp-add (&rest bit-positions)
  (if bit-positions
    (apply #'mapcar* #'logior bit-positions)
    bp-empty))
;;(bp-add (bp-from-snumbers (fibonacci-numbers 10)) (bp-from-snumbers (fibonacci-numbers 10)))

(defun bp-add* (bit-positions)
  (apply #'bp-add bit-positions))
;;(bp-add* (list (bp-from-snumbers (fibonacci-numbers 4)) (bp-from-snumbers '(4))))

(defun bp-from-snumbers (snumbers)
  (bp-add* (mapcar #'bp-from-snumber snumbers)))
;;(bp-from-snumbers '(4 5))

(defun bps (square-descriptions)
  (bp-add* (mapcar #'bp square-descriptions)))
;;(bps '(a1 b1 nil))

(defun bp-to-snumbers (bit-position)
  (append
   (loop for i from 0 below 28
	 for bp = (first bit-position) then (lsh bp -1)
	 if (not (zerop (logand bp 1))) collect i)
   (loop for i from 28 below 56
	 for bp = (second bit-position) then (lsh bp -1)
	 if (not (zerop (logand bp 1))) collect i)
   (loop for i from 56 below 64
	 for bp = (third bit-position) then (lsh bp -1)
	 if (not (zerop (logand bp 1))) collect i)))
;;(bp-to-snumbers (bp-add (bp-from-snumbers (fibonacci-numbers 10))))

(defun bp-print (bp)
  (print-board-positions (bp-to-snumbers bp)))

