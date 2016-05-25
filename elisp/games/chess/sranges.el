(require 'chess-square)
(require 'board-matrix)
(require 'chess-side)

;;; For knight and king, the template range can be generated from
;;; skeleton (ie. template sans all rotation invariants)
(defconst knight-skeleton (list (make-square 1 2) (make-square 2 1)))
(defconst king-skeleton (list (make-square 1 0) (make-square 1 1)))
(defconst pawn-template (list (make-square 0 1) (make-square -1 1) (make-square 1 1)))

(defun template-from-skeleton (template)
  (loop for i below 4 
	append (mapcar (bind #'square-rotate90 i) template)))
;;(template-from-skeleton ne-template)


(defun range-from-square (square template)
  (remove nil (remove-duplicates (mapcar (bind #'square-translate square) template))))
;;(range-from-square '(1 1) ne-template)
;;(print-board-squares (let ((x '(1 1))) x (range-from-square x ne-template)))


(defun srange-list (range-template &optional include-start-square)
  (loop with squares = (if include-start-square
			 (cons '(0 0) range-template)
			 range-template)
	for i below (* 8 8) collect (mapcar #'snumber (range-from-square (square i) squares))))

(defun srange-vector-pawn ()
  (let ((res (srange-list pawn-template)))
    (loop for i from 8 below 16 ;second row
	  do (push (+ i 16) (nth i res)))
    (apply #'vector res)))
;;(print-board-positions (aref (srange-vector-pawn) 53))

(defun srange-vector (range-template &optional include-start-square)
  (apply #'vector (srange-list range-template include-start-square)))
;;(print-board-positions (aref (srange-vector ne-template t) 0))



(defconst knight-ranges (srange-vector (template-from-skeleton knight-skeleton)))
(defconst king-ranges (srange-vector (template-from-skeleton king-skeleton)))
(defconst pawn-ranges (srange-vector-pawn))
;;(print-board-positions (aref pawn-ranges 53))

;;; For long distance pieces 
(defconst ne-template (loop for i from 1 below 8 collect (make-square i i)))
(defconst nw-template (mapcar (bind #'square-rotate90 1) ne-template))
(defconst sw-template (mapcar (bind #'square-rotate90 2) ne-template))
(defconst se-template (mapcar (bind #'square-rotate90 3) ne-template))

(defconst e-template (loop for i from 1 below 8 collect (make-square i 0)))
(defconst n-template (mapcar (bind #'square-rotate90 1) e-template))
(defconst w-template (mapcar (bind #'square-rotate90 2) e-template))
(defconst s-template (mapcar (bind #'square-rotate90 3) e-template))

;;Diagonal ranges without and with start square (snumber)
(defconst ne-ranges (srange-vector ne-template))
(defconst nw-ranges (srange-vector nw-template))
(defconst sw-ranges (srange-vector sw-template))
(defconst se-ranges (srange-vector se-template))

;;Line ranges without and with start square (snumber)
(defconst e-ranges (srange-vector e-template))
(defconst n-ranges (srange-vector n-template))
(defconst w-ranges (srange-vector w-template))
(defconst s-ranges (srange-vector s-template))
;;(aref e-ranges 0)

(defconst bishop-ranges (list ne-ranges nw-ranges sw-ranges se-ranges))
(defconst rook-ranges (list e-ranges n-ranges w-ranges s-ranges))
(defconst queen-ranges (append bishop-ranges rook-ranges))

;;The ranges (not sure if this is useful for anything)
(defun king-range (snumber) (aref king-ranges snumber))
(defun knight-range (snumber) (aref knight-ranges snumber))

(defun rbq-range (ranges &optional include-start-square)
  (bp-add* (loop for x in ranges
		 for r = (if include-start-square (second x) (first x))
		 collect (aref r snumber))))

(defun rook-range (snumber &optional include-start-square)
  (rbq-range rook-ranges include-start-square))
(defun bishop-range (snumber &optional include-start-square)
  (rbq-range bishop-ranges include-start-square))
(defun queen-range (snumber &optional include-start-square)
  (rbq-range queen-ranges include-start-square))
;;(print-board-positions (bp-to-snumbers (queen-range 10 nil)))

(defun crop-range (sfrom srange side cb)
  (loop for sto in srange
	for cm = (cm-new cb sfrom sto)
	while cm collect cm into res
	if (cm-capture cm) return res
	finally return res))

(defun actual-rbq-range (sfrom sranges side cb)
  (loop for r in sranges
	append (crop-range sfrom (aref r sfrom) side cb)))

(defun actual-kn-range (sfrom sranges side cb) 
  (loop for sto in (aref sranges sfrom) 
	for cm = (cm-new cb sfrom sto)
	if cm collect cm))

(defun actual-p-range (sfrom sranges side cb) 
  (loop for sto in (aref sranges sfrom) 
	for cm = (cm-new-pawn cb sfrom sto)
	if cm collect cm))

(defun actual-range (piece cb)
  (let* ((side (chess-piece-side piece))
	 (sfrom (chess-piece-snumber piece)))
    (sb-set cb sfrom piece)
    (case (chess-piece-type piece)
      (queen (actual-rbq-range sfrom queen-ranges side cb))
      (rook (actual-rbq-range sfrom rook-ranges side cb))
      (bishop (actual-rbq-range sfrom bishop-ranges side cb))
      (king (actual-kn-range sfrom king-ranges side cb))
      (knight (actual-kn-range sfrom knight-ranges side cb))
      (pawn (actual-p-range sfrom pawn-ranges side cb)))))

(require 'chess-move)
(defun test-actual-range (piece)
  (let* ((cp (cp-new))
	 (cb (cp-board cp))) 
    (actual-range piece cb)))
;;(cm-range-print (test-actual-range (new-chess-piece "a2" 'white)))