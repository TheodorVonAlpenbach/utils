(require 'chess-utils)

(defconst board-matrix-start
  (maptree #'symbol-to-char
	   (nreverse
	    '((r n b q k b n r)
	      (p p p p p p p p)
	      (· · · · · · · ·)
	      (· · · · · · · ·)
	      (· · · · · · · ·)
	      (· · · · · · · ·)
	      (P P P P P P P P)
	      (R N B Q K B N R)))))

(defun* make-board-matrix (&optional (blank-symbol ?·))
  "Returns a list of list"
  (mapcar #'copy-list (make-list 8 (make-list 8 blank-symbol))))
;;(bm-print (make-board-matrix))

(defun bm-setf (board-matrix square value)
  (setf (nth (first square) (nth (second square) board-matrix)) value)
  board-matrix)
;;(bm-print (bm-setf board-matrix-start '(1 1) (char "Y" 0)))

(defun bm-square (board-matrix square)
  "Make this a settable macro"
  (nth (second square) (nth (first square) board-matrix)))
;;(bm-square board-matrix-start '(0 0))

(defun bm-print (bm)
  "Ugly, but it works."
  (concat* (mapcar #'(lambda (x) (string* x :in 32)) (reverse bm))
	   :pre "\n  A B C D E F G H\n  ---------------\n8|"
	   :infun #'(lambda (i) (format "|%d\n%d|" (- 8 i) (- 7 i)))
	   :suf "|1\n  ---------------\n  A B C D E F G H\n"))
;;(print-board-matrix board-matrix-start)


(defun* print-board-positions (positions &optional (symbols '(?X ?·)))
  "Obsolete?"
  (let ((bm (make-board-matrix (second symbols))))
    (loop for position in positions
	  do (bm-setf bm (square-from-snumber position) (first symbols)))
    (bm-print bm)))

(defun* print-board-squares (squares &optional (symbols '(?X ?·)))
  "Obsolete?"
  (print-board-positions (mapcar #'square-to-snumber squares)  symbols))

(provide 'board-matrix)