(require 'chess-utils)

;;; SQUARE
(defmacro make-square (row column) `(list ,row ,column))

(defalias 'square-column 'first
  "Returns square's column")
(defalias 'square-row 'second
  "Returns square's row")
(defalias 'square-copy 'copy-list
  "Returns a copy of square")

(defun square-from-sname (sname)
  (let* ((sname* (upcase sname)))
    (list (- (char sname* 0) ?A)
	  (- (char sname* 1) ?1))))
;;(mapcar #'square-from-sname '("A1" "B1" "A2" "B2" "C3" "D4" "E5" "H8"))

(defun square-from-snumber (snumber)
  (nreverse (floor* snumber 8)))
;;(mapcar #'square-from-snumber '(0 1 8 9 18 27 36 63))

(defun square (square-description)
  (if (stringp square-description)
    (square-from-sname square-description)
    (if (listp square-description)
      square-description
      (if (integerp square-description)
	(square-from-snumber square-description)
	(if (symbolp square-description)
	  (square (symbol-name square-description)))))))
;;(mapcar #'square (list "B2" 0 'C3 'd4 36 "f6" 54 '(7 7)))

(defun square-to-snumber (square)
  (+ (* (second square) 8)
     (first square)))
;;(square (square-to-snumber '(0 1)))

(defun square-reflect-y (square)
  "Reflects by 0 row"
  (list (- (first square)) (second square)))

(defun square-reflect-x (square)
  "Reflects by 0 column"
  (list (first square) (- (second square))))

(defun* square-rotate90 (square &optional (n 1))
  "Rotates 90 degrees counter-clockwise around A1 (0 0)"
  (case (mod n 4)
    (0 (copy-list square))
    (1 (list (- (second square)) (first square)))
    (2 (list (- (first square)) (- (second square))))
    (3 (list (second square) (- (first square))))))

(defun square-outside-board-p (square)
  (or (< (first square) 0)
      (< (second square) 0)
      (> (first square) 7)
      (> (second square) 7)))

(defun* square-translate (square translation &optional (nil-if-outside-board t))
  "Reflects by 0 column"
  (let ((virtual-square (mapcar* #'+ square translation)))
   (if (and nil-if-outside-board (square-outside-board-p virtual-square))
     nil
     virtual-square)))
;;(square-translate '(10 -7) '(-6 6))

;;; square number ie. 0-63
(defun snumber (square-description)
  (square-to-snumber (square square-description)))
;;(mapcar #'snumber '("A1" "B1" "A2" "B2" "C3" "D4" "E5" "H8"))

(defun snumber-to-row-number (snumber)
  (/ snumber 8))
(defun snumber-to-column-number (snumber)
  (mod snumber 8))

(defun snumber-on-same-column-p (snumber1 snumber2)
  (zerop (mod (- snumber1 snumber2) 8)))
;;(snumber-on-same-column-p 8 17)

(defun snumber-on-ne-diagonal (row-number)
  (nth row-number ne-main-diagonal))

;;; sname ie. a1, b2 etc
(defun sname (square-description)
  (let ((square (square square-description)))
    (format "%S%d"
      (+ ?a (first square))
      (1+ (second square)))))
;;(mapcar #'sname '("A1" "B1" "A2" "C3" "D4" "E5" H8))

(defun* squares (&optional (dimensions '(8 8)))
  (loop for row below (square-row dimensions)
	collect (loop for column below (square-column dimensions)
		     collect (list column row))))
;;(squares '(3 4))

(defun* square-invert (square &optional (rows 8))
  "Inverts square"
  (let ((res (square-copy square)))
    (setf (square-row res) (- rows (square-row res) 1))
    res))
;;(mapcar #'square-invert (squares))

(defun* square-light-p (square &optional (first-square-is-dark-p t))
  "Returns t iff SQUARE color is light. Assumes that first
square (A1) is dark, as in chess. The latter assumption can
however be reversed by setting optional argument
FIRST-SQUARE-IS-DARK-P to nil"
  (eq (oddp (apply #'+ square))
      first-square-is-dark-p))
;;(mapcar (bind #'square-light-p nil) (squares))

(provide 'chess-square)
