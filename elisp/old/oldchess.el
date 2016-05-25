(defun generate-e (x))
(defun generate-w (x))
(defun generate-n (x))
(defun generate-s (x))
(defun generate-knight-positions (x))

(defun shift-rowwise (x n)
  (if (integerp x) 
    (+ x (* n 8))
    (mapcar (bind #'shift-rowwise n) x)))

(defun shift-columnwise (x n)
  (if (integerp x) 
    (+ x n)
    (mapcar (bind #'shift-columnwise n) x)))

(defun below-ne-diagonal-p (x)
  (< (board-row-number x) (board-column-number x)))
;;(mapcar #'below-ne-diagonal-p (0-n 63))

(defun below-nw-diagonal-p (x)
  (< (board-column-number x) (board-row-number x)))
;;(prin1 (mapcar #'below-nw-diagonal-p (0-n 63)))

;;; obsolete?
(defconst ne-main-diagonal (first lower-ne-diagonals))
(defconst nw-main-diagonal (first lower-nw-diagonals))
(defconst 1-row (a-b 0 7))
(defconst a-column (a-b 0 56 8))


;;; Board should be represented as 4 integers, using 16 bits of each:
;;; '(0 0 0 0) ;emptyboard
;;; '(65536 0 0 65536) ;startup position

(defun ne-transpose-square (square)
  (nreverse square))
;;(ne-transpose-square '(3 2))

(defun nw-transpose-square (square)
  (list (- 7 (second square)) (- 7 (first square))))
;;(nw-transpose-square '(1 4))

(defun c-transpose-square (square)
  (list (first square) (- 7 (second square))))
;;(c-transpose-square '(1 4))

(defun ne-transpose-position (position-number)
  (position-number-from-square (ne-transpose-square (square-from-position-number position-number))))
;;(ne-transpose-position 1)

(defun nw-transpose-position (position-number)
  (position-number-from-square (nw-transpose-square (square-from-position-number position-number))))

(defun c-transpose-position (position-number)
  (position-number-from-square (c-transpose-square (square-from-position-number position-number))))
;;(c-transpose-position 12)

(defconst lower-right-diagonals
  (loop for c from 0 to 7 collect (a-b c (* (- 8 c) 8) 9)))
(defconst upper-right-diagonals 
  (maptree #'ne-transpose-position lower-right-diagonals))
(defconst right-diagonals 
  (append (reverse lower-right-diagonals) (rest upper-right-diagonals)))
;;(print-board-positions (nth 15 right-diagonals))

(defconst lower-left-diagonals
  (maptree #'c-transpose-position lower-right-diagonals))
(defconst upper-left-diagonals
  (maptree #'c-transpose-position upper-right-diagonals))
(defconst left-diagonals 
  (append (reverse lower-nw-diagonals) (rest upper-nw-diagonals)))
;;(print-board-positions (nth 3 left-diagonals))

(defun generate-diagonal (position-number up-p left-p)
  "Generates a list of position-numbers corresponding to the
diagonal from POSITION-NUMBER and going into the direction
defined by UP-P and LEFT-P."
  (let ((diagonal (find position-number 
			(if (eq up-p left-p) left-diagonals right-diagonals)
			:test #'member)))
    (member position-number (if up-p diagonal (reverse diagonal)))))
;;(print-board-positions (generate-diagonal 17 nil nil))

(defun generate-pn-diagonals (up-p left-p)
  (apply #'vector (loop for i below (* 8 8) collect (generate-diagonal i up-p left-p))))

(defconst pn-nw-diagonals (generate-pn-diagonals t t))
(defconst pn-sw-diagonals (generate-pn-diagonals nil t))
(defconst pn-ne-diagonals (generate-pn-diagonals t nil))
(defconst pn-se-diagonals (generate-pn-diagonals nil nil))
;;(print-board-positions (aref pn-se-diagonals 27))

(defun generate-e-row (position-number)
  (a-b position-number (1- (fixnum-ceiling (1+ position-number) 8))))
(defun generate-w-row (position-number)
  (a-b (fixnum-floor position-number 8) position-number))
(defun generate-n-column (position-number)
  (maptree #'ne-transpose-position (generate-e-row (ne-transpose-position position-number))))
(defun generate-s-column (position-number)
  (maptree #'ne-transpose-position (generate-w-row (ne-transpose-position position-number))))
;;(print-board-positions (generate-s-column 0))

(defun generate-pn-lines (function)
  (apply #'vector (loop for i below (* 8 8) collect (funcall function i))))
(defconst pn-e-rows (generate-pn-lines #'generate-e-row))
(defconst pn-w-rows (generate-pn-lines #'generate-w-row))
(defconst pn-n-columns (generate-pn-lines #'generate-n-column))
(defconst pn-s-columns (generate-pn-lines #'generate-s-column))

;;; Bit position
(defun bp-from-position-number (position-number)
  (if (< position-number 28)
    (list (expt 2 position-number) 0 0)
    (if (< position-number 56)
      (list 0 (expt 2 (- position-number 28)) 0)
      (list 0 0 (expt 2 (- position-number 56))))))
;;(bp-from-position-number 4)

(defun bp (square-description)
  (if square-description
    (bp-from-position-number (position-number-from-square (square square-description)))
    bp-emtpy))
;;(bp nil)

(defconst bp-emtpy '(0 0 0))

(defun bp-add (&rest bit-positions)
  (apply #'mapcar* #'logior bit-positions))
;;(bp-add nil bp-emtpy)

(defun bp-from-position-numbers (position-numbers)
  (apply #'bp-add (mapcar #'bp-from-position-number position-numbers)))
;;(bp-from-position-numbers '(4 5))

(defun bps (square-descriptions)
  (apply #'bp-add (mapcar #'bp square-descriptions)))
;;(bps '(()))
;;(bps '(a1 b1))

(defun bp-to-position-numbers (bit-position)
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
;;(bp-to-position-numbers (bp-add (bp-from-position-numbers (fibonacci-numbers 10))))

;;; new generating approach using squares
(defconst knight-template (list (make-square 2 1) (make-square 1 2)))
(defconst king-template (list (make-square 0 1) (make-square 1 1)))

(defun square-virtual-range (template)
  (loop for i below 4 
	append (mapcar (bind #'square-rotate90 i) template)))
;;(square-virtual-range square-knight-template)

(defun square-range (square virtual-range)
  (remove nil (remove-duplicates (mapcar (bind #'square-translate square) virtual-range))))
;;(print-board-squares (let ((x '(1 1))) x (square-range x (square-virtual-range square-knight-template))))

(defun bp-range-vector (virtual-range)
  (apply #'vector
	 (loop for i below (* 8 8) 
	       collect (bps (square-range (square-from-position-number i) virtual-range)))))

(defun bp-print (bp)
  (print-board-positions (bp-to-position-numbers bp)))

(defconst knight-ranges (bp-range-vector (square-virtual-range knight-template)))
(defconst king-ranges (bp-range-vector (square-virtual-range king-template)))
;;(bp-print (aref king-ranges 64))

(defconst nw-diagonals (apply #'vector (mapcar #'bp-from-position-numbers pn-nw-diagonals)))
(defconst se-diagonals (apply #'vector (mapcar #'bp-from-position-numbers pn-se-diagonals)))
(defconst ne-diagonals (apply #'vector (mapcar #'bp-from-position-numbers pn-ne-diagonals)))
(defconst sw-diagonals (apply #'vector (mapcar #'bp-from-position-numbers pn-sw-diagonals)))

(defconst e-rows (apply #'vector (mapcar #'bp-from-position-numbers pn-e-rows)))
(defconst w-rows (apply #'vector (mapcar #'bp-from-position-numbers pn-w-rows)))
(defconst n-columns (apply #'vector (mapcar #'bp-from-position-numbers pn-n-columns)))
(defconst s-columns (apply #'vector (mapcar #'bp-from-position-numbers pn-s-columns)))

(defun king-range (position-number) (aref king-ranges position-number))
(defun knight-range (position-number) (aref knight-range position-number))
(defun bishop-range (position-number)
  (bp-add (aref nw-diagonals position-number)
	  (aref se-diagonals position-number)
	  (aref ne-diagonals position-number)
	  (aref sw-diagonals position-number)))
(defun rook-range (position-number)
  (bp-add (aref e-rows position-number)
	  (aref w-rows position-number)
	  (aref n-columns position-number)
	  (aref s-columns position-number)))
(defun queen-range (position-number)
  (bp-add (rook-range position-number)
	  (bishop-range position-number)))
;;(print-board-positions (bp-to-position-numbers (queen-range 0)))

(defun bp (square-description)
  (if square-description
    (bp-from-snumber (snumber square-description))
    bp-empty))

(defun cb-join-connectors (connector-a connector-b)
  "Joins connector-a and connector-b. Obsolete?"
  (list (first connector-a) (last connector-b)))

(defun cb-remove-connector (connector-a connector-mid connector-b)
  "Joins connector-a and connector-b"
  (setf (second connector-a) (second connector-mid))
  (setf (first connector-b) (first connector-mid)))

(defun chess-square-clear (cs cb-squares)
  (setf chess-square-content nil)
  (let* ((cs chess-square-connections)
	 (cs-lines (first cs))
	 (cs-row (first cs-lines))
	 (cs-column (second cs-lines))
	 (cs-diagonals (second cs))
	 (cs-ne (first cs-diagonals))
	 (cs-nw (second cs-diagonals)))
    (apply #'cbc-connect cb-squares cs-row)
    (apply #'cbc-connect cb-squares cs-column)
    (apply #'cbc-connect cb-squares cs-ne)
    (apply #'cbc-connect cb-squares cs-nw)))

(defun cb-clear-square (cb snumber)
  (setf chess-square-content nil)
  (loop for i below 4
	for connector-mid = (nth i (cb-connections cb snumber))
	for connector-a = (nth i (cb-connections cb (first connector-mid)))
	for connector-b = (nth i (cb-connections cb (second connector-mid)))
	do (cb-remove-connector connector-a connector-mid connector-b)))

(defun cb-insert-square (cb snumber piece)
  (setf chess-square-content piece)
  (loop for i below 4
	for connector-mid = (nth i (cb-connections cb snumber))
	for connector-a = (nth i (cb-connections cb (first connector-mid)))
	for connector-b = (nth i (cb-connections cb (second connector-mid)))
	do (cb-remove-connector connector-a connector-mid connector-b)))
(defstruct chess-square
  "Holds content and relations to other squares. Connections is a tree
\(((e w) (n s)) ((ne sw) (nw se)))"
  content ;piece
  connections)


(defstruct chess-board
  squares)

(defstruct 
  (chess-side
   (:conc-name cs-)
   (:constructor nil)
   (:constructor 
    make-chess-side (piece-descriptions side
					&aux 
					(pieces (mapcar (bind #'new-chess-piece side) piece-descriptions)))
    make-white (piece-descriptions &aux 
				   (pieces (pieces (mapcar (bind #'new-chess-piece side) piece-descriptions)))
				   (side 'white))
    make-black (piece-descriptions &aux 
				   (pieces (pieces (mapcar (bind #'new-chess-piece side) piece-descriptions)))
				   (side 'black))))
  
  (pieces white-pieces)
  (side white)
  (may-castle t))



(defun chess-board-move (cb from-snumber to-snumber)
  (let* ((from-square (aref squares from-snumber))
	(to-square (aref squares to-snumber))
	(content (chess-board-content from-square)))
    (chess-square-clear from-square)
    (chess-square-add to-square content)))