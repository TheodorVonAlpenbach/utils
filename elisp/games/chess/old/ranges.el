;;; For knight and king, the template range can be generated from
;;; skeleton (ie. template sans all rotation invariants)
(defconst knight-skeleton (list (make-square 1 2) (make-square 2 1)))
(defconst king-skeleton (list (make-square 1 0) (make-square 1 1)))

(cl-defun template-from-skeleton (template)
  (cl-loop for i below 4 
	append (mapcar (bind #'square-rotate90 i) template)))
;;(template-from-skeleton ne-template)

(cl-defun range-from-square (square template)
  (remove nil (remove-duplicates (mapcar (bind #'square-translate square) template))))
;;(range-from-square '(7 0) ne-template)
;;(print-board-squares (let ((x '(1 1))) x (range-from-square x ne-template)))

(cl-defun bp-range-vector (range-template &optional include-start-square)
  "Generate 64-element vector of every range on the board"
  (apply #'vector
	 (cl-loop with squares = (if include-start-square
				(cons '(0 0) range-template)
				range-template)
	       for i below (* 8 8) collect (bps (range-from-square (square i) squares)))))
;;(print-board-positions (bp-to-snumbers (aref (bp-range-vector ne-template t) 0)))

;;; For long distance pieces 
(defconst ne-template (cl-loop for i from 1 below 8 collect (make-square i i)))
(defconst nw-template (mapcar (bind #'square-rotate90 1) ne-template))
(defconst sw-template (mapcar (bind #'square-rotate90 2) ne-template))
(defconst se-template (mapcar (bind #'square-rotate90 3) ne-template))

(defconst e-template (cl-loop for i from 1 below 8 collect (make-square i 0)))
(defconst n-template (mapcar (bind #'square-rotate90 1) e-template))
(defconst w-template (mapcar (bind #'square-rotate90 2) e-template))
(defconst s-template (mapcar (bind #'square-rotate90 3) e-template))

;;Diagonal ranges without and with start square (snumber)
(defconst ne-ranges (list (bp-range-vector ne-template) (bp-range-vector ne-template t)))
(defconst nw-ranges (list (bp-range-vector nw-template) (bp-range-vector nw-template t)))
(defconst sw-ranges (list (bp-range-vector sw-template) (bp-range-vector sw-template t)))
(defconst se-ranges (list (bp-range-vector se-template) (bp-range-vector se-template t)))

;;Line ranges without and with start square (snumber)
(defconst e-ranges (list (bp-range-vector e-template) (bp-range-vector e-template t)))
(defconst n-ranges (list (bp-range-vector n-template) (bp-range-vector n-template t)))
(defconst w-ranges (list (bp-range-vector w-template) (bp-range-vector w-template t)))
(defconst s-ranges (list (bp-range-vector s-template) (bp-range-vector s-template t)))

(defconst knight-ranges (bp-range-vector (template-from-skeleton knight-skeleton)))
(defconst king-ranges (bp-range-vector (template-from-skeleton king-template)))
(defconst bishop-ranges (list ne-ranges nw-ranges sw-ranges se-ranges))
(defconst rook-ranges (list e-ranges n-ranges w-ranges s-ranges))
(defconst queen-ranges (append bishop-ranges rook-ranges))

;;The ranges (not sure if this is useful for anything)
(cl-defun king-range (snumber) (aref king-ranges snumber))
(cl-defun knight-range (snumber) (aref knight-ranges snumber))

(cl-defun rbq-range (ranges &optional include-start-square)
  (bp-add* (cl-loop for x in ranges
		 for r = (if include-start-square (second x) (first x))
		 collect (aref r snumber))))

(cl-defun rook-range (snumber &optional include-start-square)
  (rbq-range rook-ranges include-start-square))
(cl-defun bishop-range (snumber &optional include-start-square)
  (rbq-range bishop-ranges include-start-square))
(cl-defun queen-range (snumber &optional include-start-square)
  (rbq-range queen-ranges include-start-square))
;;(print-board-positions (bp-to-snumbers (queen-range 10 nil)))

(cl-defun actual-range (ranges snumber bp-player bp-opponent)
  (let* ((bp-range (aref (first ranges) snumber)))))

;; Bit positions? What a waste of time!!
;; «Premature optimization is the root of all evil»

;; Don't use bp. Use snumbers instead

;; (cl-loop for sn in range
;;       for x = (board-ref board sn)
;;       if (not x) collect x into res
;;       else return (if (opponent-piece-p x)
;; 		    (cons x res) res))

;; Maybe "slow", but it is simple and it should work!
